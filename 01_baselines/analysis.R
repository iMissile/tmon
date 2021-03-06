#!/usr/bin/env Rscript
# TODO ��������� ����������������� ������� ��� ������������ �����

# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding
rm(list=ls()) # ������� ��� ����������

############## �������� �� ��������� ##############
importURL <- "data/bandwidth.JSON" # ��������� ����
importURL <- "http://192.168.144.105:8080/HFS/bandwidth.JSON" # ���-������
exportURL <- "http://192.168.144.105:8080/HFS/"
###################################################

cat("Loading packages...\n")
needed_packages <- c("dplyr", "magrittr", "ggplot2", "lubridate", "scales",
                     "RColorBrewer", "wesanderson", "microbenchmark", "reshape2",
                     "readr", "xts", "zoo", "caTools", "jsonlite", "logging",
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

options(warn = 2)


basicConfig()
addHandler(writeToFile, logger="", file="adaptiveB.log")


# ��������� ��������� �� ��������� ������ �� �������� �� �������� � �������� ������
# http://www.r-bloggers.com/including-arguments-in-r-cmd-batch-mode/
# http://stackoverflow.com/questions/14167178/passing-command-line-arguments-to-r-cmd-batch
# http://stackoverflow.com/questions/4547789/command-line-arguments-in-bash-to-rscript
# �������� ������ �� Linux ������� ��������� �������:
# Rscript "analysis.R" --args "importURL=... exportURL=..." (���� ��������� �� Linux!!!!!!)

# ������������ ���������� ��������� ������ (����� getopt)
# https://cran.r-project.org/web/packages/getopt/getopt.pdf
cmdSpec = matrix(c(
  "importURL", "i", 2, "character",
  "exportURL", "e", 2, "character",
  "help" , "h", 0, "logical"), byrow=TRUE, ncol=4)

cmdArgs <- getopt(cmdSpec)

# ����������� ���������
# if help was asked for print a friendly message
# and exit with a non-zero error code
if (!is.null(cmdArgs$help)){
  cat(getopt(cmdSpec, usage=TRUE))
  q(status=1)
}

# ���� ������� �� ���� �������� ������ ��������� --importURL � --exportURL,
# �� ���������� �������� �� ��������� (������� ��� �������)
if (is.null(cmdArgs$importURL)){
  logwarn("Using default value for importURL (%s)", importURL)
} else {
  importURL <- cmdArgs$importURL
  loginfo("Received importURL (%s)", importURL)
}

if (is.null(cmdArgs$exportURL)){
  logwarn("Using default value for exportURL (%s)", exportURL)
} else {
  exportURL <- cmdArgs$exportURL
  loginfo("Received exportURL (%s)", exportURL)
}


source("funcs.R") # ��������� ����������� �������, http://adv-r.had.co.nz/Functions.html
# R Performance. http://adv-r.had.co.nz/Performance.html


# �������� ������ �� ����� �������� �������������� �������� � ������������� �������� (������ ������) ���
# ������������ ���������� ������ (25 ���������)

# ��� ��������� ��������, ��������� ������� ��� ����������������� �������. 
# ���� ����� ���, �� ���������� ��������� ������ �� ������ ����� � ���������� �� � ���� ������� � ����
# http://stackoverflow.com/questions/21370132/r-data-formats-rdata-rda-rds-etc
# http://www.fromthebottomoftheheap.net/2012/04/01/saving-and-loading-r-objects/

# ������������ �������� CSV-���� � ������ JSON
if (FALSE){
  CSVrawdata.filename = "./data/bandwidth.csv" # �������� ����������� � %, ��������� 1 �������
  dataCSV <- read_csv(CSVrawdata.filename)
  dataCSV %<>% select(timestamp, value = bandwidth_in) # %>% head(2)
  params <- list(name = "Bandwidth In (% of interface load)")
  json_out <- toJSON(list(config = params, data = dataCSV),
                     na="null", pretty=TRUE, auto_unbox=TRUE, digits=8)
  write(json_out, file="data/bandwidth.JSON")
}

# ��������� JSON
loginfo("Loading JSON from %s", importURL)
# ������� ��������� JSON � rawdata. ��� ������� ����� � ��� ������ � ���������� ����������
tryCatch({
    rawdata <- fromJSON(importURL)
  }, error = function(err){
    logerror(paste("�� ������� ��������� JSON. �������� ������:",err))
    q(status=1)
  })

# �������� �������� � ������ �� ���������� JSON
params <- rawdata$config
subdata <- rawdata$data

# �������� �������������� �������
subdata$timestamp <-
  dmy_hms(subdata$timestamp, truncated = 3, tz = "Europe/Moscow") #��������� ��������� ������� ������
# ������ �� ����
# s_date <- ymd("2015-04-01", tz = "Europe/Moscow")
# e_date <- s_date + days(60)
# subdata %<>% filter(s_date <= timestamp & timestamp <= e_date)
# � ������ ��������� 15-�� ��������� ����������� ��� �������� ���������
subdata <- generate.discrete(subdata)

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
df_integr <- precalc_df()

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
loginfo("Start Math")
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

loginfo('regression analysis started')

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
  loginfo("���� ������ - %s. ���������� ��� ��� �������� ��������� - %s", wdnames[prognosis.nwday], r.depth)
  
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

loginfo('regression analysis finished')

# Stop the clock
pts <- proc.time() - ptm
# print(pts)
loginfo("End Math")
# *********************** ���������� ����������� ******************************

# ������� �������-������� ��� ������� ������������������ (������ ������)
# ���������� ������ ����������� �������� � ��������� �� ����
##### plot_mean_integr()

# ******************** ��������� ��������� ������������� � ��������� **********************
# ����������� ������ ������ ���� �� �����, �����, 24 �������. ��� X - �����, ��� Y - ���������� ��������
##### plot_regr_hdata()

# ===============================================================================
# ������������ ������� ����� ������� �����

reconstruct.point <- function (predict.date) {
  # � �������� �������� ��������� ����������� ���� � ����� �� ������� ���������� �������� ������
  # �� ������ �������� ������� ��������

  # ������������� �� ���� ������ � ��� ��� ������ �����.
  # ���������, ��� ���� ������ �����. ����� ������������
  # http://stackoverflow.com/questions/1169248/r-function-for-testing-if-a-vector-contains-a-given-element
  # wday(predict.date) %in% c(3:5)
  # any(wday(predict.date)== c(3:5))
  # print(wday(predict.date))
  # print(">")
  
  nwday <- wday(predict.date)
  # ��-���
  # ��� ������������� �������� �� ����, � �������� ���� ������� ��������� �������� ���
  # browser()
  #p.date <- floor_date(predict.date, "day") + hours(12) # ��������� ���� � �������� 12 �����
  p.date <- floor_date(predict.date, "day")
  # p.hour <- hour(predict.date) # ���� ������� �� ���������
  p.hour.l <- hgroup.enum(predict.date)
  p.hour.r <- hgroup.enum(predict.date + minutes(15)) # ������ ������� ������� �� ���� ��� ����� ������
  
  
  ## paste("����", p.date, ", ����� �������", p.hour.l, ", ������ �������", p.hour.r)
  
  # �������� ���������� �������� ��������� ��������� ������� �� �������� ��� ���������� ���
  # ���� ���� ��������
  day_fit <- days.fit[days.fit$nwday == nwday, ]
  av.load <- as.numeric(day_fit$slope) * as.numeric(p.date) + as.numeric(day_fit$intercept)

  
  # �������� ������ ���� ���������� ����������� ��������.
  # ����������� ����� �������
  # �� &&, � &
  regr.row <- regr_hdata[regr_hdata$hgroup == p.hour.l & regr_hdata$nwday == nwday, ] # �� ��������, ��� ������������� ������ ������������� �� ������� ������� + ���������� ���� � 1
  # regr_hdata[p.hour + 1, ] # �� ��������, ��� ������������� ������ ������������� �� ������� ������� + ���������� ���� � 1
  modif.l <- regr.row$slope * as.numeric(p.date) + regr.row$intercept

  # ����������� ������ �������
  regr.row <- regr_hdata[regr_hdata$hgroup == p.hour.r & regr_hdata$nwday == nwday, ] # �� ��������, ��� ������������� ������ ������������� �� ������� ������� + ���������� ���� � 1
  # regr_hdata[p.hour + 1, ] # �� ��������, ��� ������������� ������ ������������� �� ������� ������� + ���������� ���� � 1
  modif.r <- regr.row$slope * as.numeric(p.date) + regr.row$intercept
  
  #akon
  #print(nwday)
  #print(p.hour.l)
  #print(modif.l)
  #print(modif.r)
  #end akon
  
  
  # ������ ��������� �� 15-�� �������� ��������� � ���������
  dm <- floor(lubridate::minute(predict.date) / 15) * 15
  walk <- difftime(predict.date, (floor_date(predict.date, "hour") + minutes(dm)), units = "mins")
  
  modif <- modif.l + (modif.r - modif.l) * (as.numeric(walk) / 15)
  p.load <- av.load * modif
  #p.load <-  modif
  
  # ���� �� ��������� �
  # ����� ����, �� ��������� ����, ��������� � ���������, ����� ��� 5-��
  # �������� ��������� �������� � ���� ������ ���������. ��-�� ����� ��������� ���������, ���� ������ �������������.
  
  as.numeric(p.load) # ���� �������� NA. �� �� �������� melt. Can't melt data.frames with non-atomic 'measure' columns
}


# ���������������� ������� ������������ �� ������
# �� ���� ����� ������ timestamps
reconstruct.df <- function(timestamps){
  
  # �� ���� ����� ������ � ����������� ����� ������
  Calc_av_load <- function(df){
    nw_day <- df$nw_day[1]
    day_fit <- days.fit %>% filter(nwday == nw_day)
    df %<>% mutate(av.load = day_fit$slope * as.numeric(p.date) + day_fit$intercept)
    return(df)
  }
  
  # �� ���� ����� ������ � ����������� ����� ������ � ������� ����������
  Calc_hour_factor <- function(df){
    nw_day <- df$nw_day[1]
    p.hour.l <- df$p.hour.l[1]
    p.hour.r <- df$p.hour.r[1]
    regr.row.l <- regr_hdata %>% filter(hgroup == p.hour.l & nwday == nw_day)
    regr.row.r <- regr_hdata %>% filter(hgroup == p.hour.r & nwday == nw_day)
    #browser()
    df %<>% mutate(modif.l = regr.row.l$slope * as.numeric(p.date) + regr.row.l$intercept,
                   modif.r = regr.row.r$slope * as.numeric(p.date) + regr.row.r$intercept,
                   dm = floor(minute(timestamp) / 15) * 15,
                   walk = difftime(timestamp, (floor_date(timestamp, "hour") + minutes(dm)), units = "mins"),
                   modif = modif.l + (modif.r - modif.l) * (as.numeric(walk) / 15),
                   p.load = av.load * modif)
    df %<>% select(timestamp, p.load)
    return(df)
  }
  
  sample_df <- data_frame(timestamp = timestamps)
  # ���������� ������ ��������� �� timestamp
  sample_df %<>% mutate(p.hour.l = hgroup.enum(timestamp),
                        p.hour.r = hgroup.enum(timestamp + minutes(15)),
                        p.date = floor_date(timestamp, "day"),
                        nw_day = wday(timestamp))
  # ���������� �� ���� ������ � ������������ ������������� ������
  sample_df %<>% group_by(nw_day) %>% do(Calc_av_load(.)) %>% ungroup()
  # ���������� �� ���� ������ � ������� ������� ��� �������� ���������� ������������
  sample_df %<>% group_by(nw_day, p.hour.l) %>% do(Calc_hour_factor(.)) %>% ungroup()
  # �������� ������ ������� � ��������� �� timestamp
  sample_df %<>% select(timestamp, p.load) %>% arrange(timestamp)
  return(sample_df$p.load)
}


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

predict.date <- dmy_hm("01.04.2015 0:30", tz = "Europe/Moscow")

# �������� ������ �� ����� ������ ������, ������� �������������� �������
# subdata %<>% mutate()
# 
# tf <- function(m){
# print(m)
# print("<")
# }
# # 
# df <- data.frame(x = 1, y = 1:10)
# m <- lapply(df$y, tf)

#subdata["baseline"] <-
# http://nicercode.github.io/guides/repeating-things/
# https://cran.r-project.org/doc/manuals/R-intro.html#Lists-and-data-frames

loginfo("Reconstructing baseline")
# !!!! lapply �� �������� ��� ["timestamp"], �� ���� ��� $timestamp
# ���� �������� ��������� ����� dput. � ������ $ �������� ������, � ������ [""] �������� data.frame

# --- ������������� ������ �� ������������
# sampledata <- dplyr::sample_frac(subdata, 0.2, replace = FALSE) #20%
sampledata <- subdata

### ������������� � ������� ������� reconstruct.point ��� ������ ����� ��������
# system.time(baseline.val <- lapply(sampledata$timestamp, reconstruct.point))
# ���������� ~ 60�� �� �����

### ������������� � ������� ������� reconstruct.df
baseline.val <- reconstruct.df_fast(sampledata$timestamp)

sampledata$baseline <- baseline.val

# ������� ������� � ������ ������� ���������� ��������
dev <- .15 # 15%
#mindev <- max(df$baseline, na.rm = TRUE) * .05
#print(mindev)
mindev <- 3 # ������� � ���������� ���������. ������������ �������� � ��� 100%

sampledata %<>% mutate(low = pmax(0, (baseline - pmax(baseline * dev, mindev))),
         up  = baseline + pmax(baseline * dev, mindev),
         dev_delta = pmax(baseline * dev, mindev)) # DEBUG

# reconstruct.point(predict.date)
# print(wday(predict.date))
# day_raw_plot(predict.date+weeks(3)+days(1), data = sampledata)

# ������� JSON
export_params <- list(name = params$name, line = "baseline", fill = c("low", "up"))
export_data <- sampledata %>% select(timestamp, baseline, low, up)
export_json <- toJSON(list(config = export_params, data = export_data),
                      pretty=TRUE, auto_unbox=TRUE, digits=8, na="null")

loginfo("Exporting JSON to %s", exportURL)
# ������� ��������� JSON � rawdata. ��� ������� ����� � ��� ������ � ���������� ����������
tryCatch({
    postform <- postForm(exportURL,"fileData" = fileUpload(contents = export_json,
                                                         contentType = "application/json",
                                                         filename = "export_test.JSON"))
  }, error = function(err){
    logerror(paste("�� ������� ������������� JSON. �������� ������:", err))
    q(status=1)
  })

loginfo("Execution completed")

#----
# s_date <- ymd("2015-04-18", tz = "Europe/Moscow")
# e_date <- s_date + days(1) #17
# testdata <- dplyr::filter(sampledata, s_date < timestamp & timestamp < e_date)
# 
# baseline_raw_plot(testdata)
# 
# jsonF <- toJSON(testdata, pretty=TRUE)
# write(jsonF, file="export.JSON")

# stop("Manual end")
# # =================================================
# 
# mm <- filter(df_baseline, abs((baseline-value)/baseline)>.3 & value > 10)
# 
# 
# s_date <- ymd("2015-04-01", tz = "Europe/Moscow")
# e_date <- s_date + days(10) #17
# testdata <- dplyr::filter(subdata, s_date < timestamp & timestamp < e_date)
# 
# gp <- baseline_raw_plot(testdata, 12.3)
# 
# # Open a new png device to print the figure out to (or use tiff, pdf, etc).
# png(filename = "07. adaptive baseline (history).png", width = 11312, height = 8000, units = 'px')
# print(gp) #end of print statement
# dev.off() #close the png device to save the figure.
# 
# write.table(subdata %>% select(timestamp, value, baseline), file = "adaptive_baseline.csv", sep = ",", col.names = NA, qmethod = "double")
# 
# #write.xlsx(x = sample.dataframe, file = "test.excelfile.xlsx",
# #           sheetName = "TestSheet", row.names = FALSE)
# 
