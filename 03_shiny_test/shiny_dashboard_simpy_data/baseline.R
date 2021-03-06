cat("Loading packages...\n")
needed_packages <- c("dplyr", "magrittr", "ggplot2", "lubridate", "scales",
                     "RColorBrewer", "wesanderson", "microbenchmark", "reshape2",
                     "readr", "xts", "zoo", "caTools", "jsonlite",
                     "broom", "getopt", "RCurl")
installed_packages <- installed.packages()[,"Package"]

# ����� ����������� ������� ���������, ��� �� ������ ������ �����������
is_installed <- needed_packages  %in% installed_packages
# ���� ����������� �� ���, �� ����� ������
if (! all(is_installed)){
  need_install <- needed_packages[! is_installed]
  cat(paste("In order to use this script you need to install these R packages:",
            need_install))
  q(status = 1)
}

# ������� ��� �������� ������ � ����������� ������ ���������
LoadPkg <- function(LibName){
  suppressMessages(require(LibName, character.only = TRUE))
}

# ��������� ������ �� ������ needed_packages
is_loaded <- sapply(needed_packages, LoadPkg)
# ����� ������ ��� ��������� �������� �������
if (! all(is_loaded)){
  failed_load <- needed_packages[! is_loaded]
  cat(paste("Failed to load these R packages:",
            failed_load))
  q(status = 1)
}

# WARN: essentially perfect fit: summary may be unreliable
# options(warn = 2)

source("funcs.R") # ��������� ����������� �������

# �� ���� -- ��������� �� ��������� timestamp � value �
# ������ timestamp'�� ��� ���������� ������� �����
baseline <- function(subdata, forecast_timestapms){
  
  
  # �������� �������������� �������
#   subdata$timestamp <-
#     dmy_hms(subdata$timestamp, truncated = 3, tz = "Europe/Moscow") #��������� ��������� ������� ������
  # ������ �� ����
  # s_date <- ymd("2015-04-01", tz = "Europe/Moscow")
  # e_date <- s_date + days(60)
  # subdata %<>% filter(s_date <= timestamp & timestamp <= e_date)
  # � ������ ��������� 15-�� ��������� ����������� ��� �������� ���������
  # subdata <- generate.discrete(subdata)
  
  # ��� ��������� � ��������� ���� �������� ������ �� ����
  # http://stackoverflow.com/questions/18503177/r-apply-function-on-specific-dataframe-columns
  # round_date -- ��������� � ��� �������, �� ����� ��������, ��� ���� ��������� ����.
  # + ������� ��������� �� ������� ����������
  # http://stackoverflow.com/questions/10705328/extract-hours-and-seconds-from-posixct-for-plotting-purposes-in-r
  # using lubridate::hour and lubridate::minute
  subdata %<>% mutate(date = floor_date(timestamp, "day"),
                      nwday = wday(timestamp),
                      hgroup = hgroup.enum(timestamp),
                      textdate = format(date, format = "%d.%m (%a)"))
  subdata$textdate <- as.factor(subdata$textdate)
  
  # ������ �������������� �� ������������ ����������
  # �� ������� �������� ������������� ���������� �� ����
  subdata %<>% precalc_df()
  
  #==================  �������� � ������������� �����������, ����� ��������� =============
  
  # ������� ������� 'integr', value' � 'ratio' � �������� �� � ����� ����� ��������
  # �������� ������, ���� ����, ���� �����
  subdata %<>% mutate(ratio = value / mean_value)
  
  # ����� ��������� ������� 'subdata.f(iltered)', ����������� ���������� �� ��������� ����
  # ����� ���������� �� ���� ������. 1 - �����������, 7 - �������
  days.stat <- subdata %>%
    group_by(nwday) %>%
    summarise(mean_integr = mean(integr), sd_integr = sd(integr)) %>%
    arrange(nwday)
  
  subdata.f <-
    subdata %>%
    dplyr::filter(integr >= days.stat$mean_integr[nwday] - 1.9 * days.stat$sd_integr[nwday] &
                    integr <= days.stat$mean_integr[nwday] + 1.9 * days.stat$sd_integr[nwday])
  
  # ����� ���� ��� �� ��������������!!
  subdata <- subdata.f
  
  # *********************** ���������� �������� ******************************
  print("Start Math")
  # Start the clock!
  ptm <- proc.time()
  
  # ======================================================================
  # ������ ��� �������� � �������, ���� ���������, ��� ��������� � �������� ���������� ������� (�����)
  # ���� ����������. C������, ��� � �������� �������� �������� ����� ���� �������� �� �����.
  # ����������� �� ��������� ����� ���� � ��������, � ����� ���� ������� � � ����������� ���������.
  
  # ��� �������� ��������� �� ��������� ��������� (����\���\���� ����) 
  # ������ ������� � ������ ������ y=f(time)
  
  # ������� �������-�������(3-5) ��� ������� ������������������ (������ ������)
  # � ������ ����, ��� �������� ���������� �� 0 �� ��������, ������������� ��������� ������ �������� ���. 
  # �� ���� �����, ��� ��� ����� ������� ���������.
  
  print('regression analysis started')
  
  regr_hdata <- NULL 
  days.fit <- NULL # ������ ���������� ������������� ������
  
  for (prognosis.nwday in 1:7) # ���� ������ �� ������� �� ����� ����������� �������. 1 - ����, 7 - �������
  {
    # transform(nwday=ifelse(nwday==3 | nwday==5, 4, nwday)) %>%
    # dplyr::mutate() works the same way as plyr::mutate() and similarly to
    # base::transform(). The key difference between mutate() and transform() is
    # that mutate allows you to refer to columns that you just created:
    # https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
    
    # ������� �������-������� ��� ������� ������������������ (������ ������)
    df8 <- subdata %>%
      # mutate(nwday=ifelse(nwday==3 | nwday==5, 4, nwday)) %>%
      filter(nwday == prognosis.nwday) %>%
      arrange(date)
    
    
    df7 <- df8 %>%
      group_by(date, textdate, integr) %>%
      summarise(mean = mean(mean_value), std.dev = sd(mean_value)) %>%  # � ����� ������ mean(value) !!!
      arrange(date)
    
    # ��� ����� ������� ����� �� ��������
    # ����������, � ���������� �� ������ ��� ���������� ���������?
    r.depth <- dim(df7)[1]
    # print("���� ������ - %s. ���������� ��� ��� �������� ��������� - %s", wdnames[prognosis.nwday], r.depth)
    
    # �������� ����������
    # http://stackoverflow.com/questions/21677923/how-to-remove-selected-r-variables-without-having-to-type-their-names
    # ������ � ��������� ����������
    # http://stackoverflow.com/questions/17218404/shoud-i-get-a-habit-of-removing-unused-variables-in-r
    
    # �������, ��� ��� ��������� �������� ����������������� � ��������� ������������������ (����������)
    # boxplot ��������� ��������� ��� ��������� ����� ������� �� ��������� � �� ��������.
    # ��� ��������� ���������� ������������ ����������� ���������� x
    # ������, ��������� �� ����������� ����� ���� ���������, �� ����� ����� ���������� ����������� k
    
    # ******************** ��������� �������� ��������� **********************
    
    # ��������� ��������� �������� ���������
    # http://www.tatvic.com/blog/linear-regression-using-r/
    # http://www.theanalysisfactor.com/r-tutorial-4/
    # http://blog.yhathq.com/posts/r-lm-summary.html
    nwday_fit <- lm(data = df7, formula = mean ~ date)
    # summary(days.fit)
    # � ������ ������� ������� � ���� ������ � ������
    days.fit <- rbind(days.fit,
                      data_frame(
                        nwday = as.character(prognosis.nwday),
                        intercept = as.numeric(coef(nwday_fit)[1]),
                        slope = as.numeric(coef(nwday_fit)[2]),
                        r2 = as.numeric(summary(nwday_fit)$r.squared)
                      ))
    
    
    # ******************** ��������� �������� ��������� **********************
    # ��������� ��������� ��� ������� ����
    # ���������� do(): https://cran.r-project.org/web/packages/dplyr/README.html
    # http://stackoverflow.com/questions/22182442/dplyr-how-to-apply-do-on-result-of-group-by
    regr_day <- select(df8, nwday, hgroup, date, ratio) %>%
      group_by(hgroup) %>%
      #do(text = tfun(.))
      do(lm_eqn(.)) %>%
      filter(!is.na(slope)) # ������ ��� ������� ��� ��� ������������� ����������
    
    # � ������ ������� ������� � ���� ������ � ������
    regr_day$nwday <- prognosis.nwday
    #req <- plyr::ddply(df8, .(hgroup), lm_eqn)
    
    regr_hdata %<>% bind_rows(regr_day) # �������� ���������� �� ��� ������ ���
    # regr_hdata <- rbind(regr_hdata, regr_day) # https://stat.ethz.ch/pipermail/r-help/2006-June/107734.html
  }
  
  print('regression analysis finished')
  
  # Stop the clock
  pts <- proc.time() - ptm
  # print(pts)
  print("End Math")
  # *********************** ���������� ����������� ******************************
  
  
  reconstruct.df_fast <- function(timestamps){
    sample_df <- data_frame(timestamp = timestamps)
    # ���������� ������ ��������� �� timestamp
    sample_df %<>% mutate(p.hour.l = hgroup.enum(timestamp),
                          p.hour.r = hgroup.enum(timestamp + minutes(15)),
                          p.date = floor_date(timestamp, "day"),
                          nwday = wday(timestamp))
    # ���������� �� ���� ������ � ������������ �������������� ������
    days.fit %<>% select(nwday, intercept, slope) %>%
      mutate(nwday = as.numeric(nwday))
    
    sample_df %<>% left_join(days.fit, by = "nwday") %>%
      mutate(av.load = slope * as.numeric(p.date) + intercept) %>%
      select(-slope, -intercept)
    # browser()
    
    regr_hdata %<>% select(nwday, hgroup, intercept, slope)
    sample_df %<>% left_join(regr_hdata, by = c("nwday", "p.hour.l" = "hgroup")) %>%
      mutate(modif.l = slope * as.numeric(p.date) + intercept) %>%
      select(-slope, -intercept)
    
    sample_df %<>% left_join(regr_hdata, by = c("nwday", "p.hour.r" = "hgroup")) %>%
      mutate(modif.r = slope * as.numeric(p.date) + intercept) %>%
      select(-slope, -intercept)
    
    sample_df %<>% mutate(dm = floor(minute(timestamp) / 15) * 15,
                          walk = difftime(timestamp, (floor_date(timestamp, "hour") + minutes(dm)), units = "mins"),
                          modif = modif.l + (modif.r - modif.l) * (as.numeric(walk) / 15),
                          p.load = av.load * modif)
    
    return(sample_df$p.load)
  }
  
  sampledata <- subdata
  
  ### ������������� � ������� ������� reconstruct.point ��� ������ ����� ��������
  # system.time(baseline.val <- lapply(sampledata$timestamp, reconstruct.point))
  # ���������� ~ 60�� �� �����
  
  ### ������������� � ������� ������� reconstruct.df
  baseline.val <- reconstruct.df_fast(forecast_timestapms)
  return(baseline.val)
  
#   sampledata$baseline <- baseline.val
#   
#   # ������� ������� � ������ ������� ���������� ��������
#   dev <- .15 # 15%
#   #mindev <- max(df$baseline, na.rm = TRUE) * .05
#   #print(mindev)
#   mindev <- 3 # ������� � ���������� ���������. ������������ �������� � ��� 100%
#   
#   sampledata %<>% mutate(low = pmax(0, (baseline - pmax(baseline * dev, mindev))),
#                          up  = baseline + pmax(baseline * dev, mindev),
#                          dev_delta = pmax(baseline * dev, mindev)) # DEBUG
#   export_data <- sampledata %>% select(timestamp, baseline, low, up)
#   return(export_data)
}
    
