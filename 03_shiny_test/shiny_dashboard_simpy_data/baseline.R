cat("Loading packages...\n")
needed_packages <- c("dplyr", "magrittr", "ggplot2", "lubridate", "scales",
                     "RColorBrewer", "wesanderson", "microbenchmark", "reshape2",
                     "readr", "xts", "zoo", "caTools", "jsonlite",
                     "broom", "getopt", "RCurl")
installed_packages <- installed.packages()[,"Package"]

# Перед выполнением скрипта проверяем, все ли нужные пакеты установлены
is_installed <- needed_packages  %in% installed_packages
# Если установлены не все, то выдаём ошибку
if (! all(is_installed)){
  need_install <- needed_packages[! is_installed]
  cat(paste("In order to use this script you need to install these R packages:",
            need_install))
  q(status = 1)
}

# Функция для загрузки пакета с подавлением вывода сообщений
LoadPkg <- function(LibName){
  suppressMessages(require(LibName, character.only = TRUE))
}

# Загружаем пакеты по списку needed_packages
is_loaded <- sapply(needed_packages, LoadPkg)
# Выдаём ошибку при неудачной загрузке пакетов
if (! all(is_loaded)){
  failed_load <- needed_packages[! is_loaded]
  cat(paste("Failed to load these R packages:",
            failed_load))
  q(status = 1)
}

# WARN: essentially perfect fit: summary may be unreliable
# options(warn = 2)

source("funcs.R") # загружаем определения функций

# На вход -- датафрейм со столбцами timestamp и value и
# вектор timestamp'ов для построения базовой линии
baseline <- function(subdata, forecast_timestapms){
  
  
  # проводим постпроцессинг колонок
#   subdata$timestamp <-
#     dmy_hms(subdata$timestamp, truncated = 3, tz = "Europe/Moscow") #допустима неполнота входных данных
  # Фильтр по дате
  # s_date <- ymd("2015-04-01", tz = "Europe/Moscow")
  # e_date <- s_date + days(60)
  # subdata %<>% filter(s_date <= timestamp & timestamp <= e_date)
  # а теперь нашпигуем 15-ти минутными интервалами для проверки алгоритма
  # subdata <- generate.discrete(subdata)
  
  # для просмотра в табличном виде загрубим данные по дням
  # http://stackoverflow.com/questions/18503177/r-apply-function-on-specific-dataframe-columns
  # round_date -- округляет в обе стороны, не очень подходит, нам надо округлять вниз.
  # + сделаем раскладку по часовым интервалам
  # http://stackoverflow.com/questions/10705328/extract-hours-and-seconds-from-posixct-for-plotting-purposes-in-r
  # using lubridate::hour and lubridate::minute
  subdata %<>% mutate(date = floor_date(timestamp, "day"),
                      nwday = wday(timestamp),
                      hgroup = hgroup.enum(timestamp),
                      textdate = format(date, format = "%d.%m (%a)"))
  subdata$textdate <- as.factor(subdata$textdate)
  
  # делаем предвычисления по интегральным параметрам
  # из функции получаем распределение интегралов по дням
  subdata %<>% precalc_df()
  
  #==================  загрузка и предобработка закончилась, пошла аналитика =============
  
  # добавим колонки 'integr', value' и 'ratio' с которыми мы и будем далее работать
  # выбираем трафик, либо вход, либо выход
  subdata %<>% mutate(ratio = value / mean_value)
  
  # сразу посчитаем выборку 'subdata.f(iltered)', исключающую аномальные по интегралу даты
  # имеем статистику по дням недели. 1 - воскресенье, 7 - суббота
  days.stat <- subdata %>%
    group_by(nwday) %>%
    summarise(mean_integr = mean(integr), sd_integr = sd(integr)) %>%
    arrange(nwday)
  
  subdata.f <-
    subdata %>%
    dplyr::filter(integr >= days.stat$mean_integr[nwday] - 1.9 * days.stat$sd_integr[nwday] &
                    integr <= days.stat$mean_integr[nwday] + 1.9 * days.stat$sd_integr[nwday])
  
  # чтобы весь код не перелопачивать!!
  subdata <- subdata.f
  
  # *********************** математика началась ******************************
  print("Start Math")
  # Start the clock!
  ptm <- proc.time()
  
  # ======================================================================
  # прежде чем говорить о средних, надо убедиться, что измерений в заданный промежуток времени (сутки)
  # было достаточно. Cчитаем, что в качестве критерия проверки может быть интеграл за сутки.
  # несхождение по интегралу может быть и выбросом, а может быть связано и с недостатком измерений.
  
  # для линейной регрессии по различным критериям (день\час\доля часа) 
  # делаем выборку и строим график y=f(time)
  
  # выберем вторник-четверг(3-5) для большей репрезентативности (данных больше)
  # с учетом того, что значения изменяются от 0 до десятков, анализировать дисперсию смысла большого нет. 
  # мы итак знаем, что там будут большие колебания.
  
  print('regression analysis started')
  
  regr_hdata <- NULL 
  days.fit <- NULL # список посуточных регрессионных кривых
  
  for (prognosis.nwday in 1:7) # день недели на который мы будем расчитывать прогноз. 1 - Вскр, 7 - Суббота
  {
    # transform(nwday=ifelse(nwday==3 | nwday==5, 4, nwday)) %>%
    # dplyr::mutate() works the same way as plyr::mutate() and similarly to
    # base::transform(). The key difference between mutate() and transform() is
    # that mutate allows you to refer to columns that you just created:
    # https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
    
    # выберем вторник-четверг для большей репрезентативности (данных больше)
    df8 <- subdata %>%
      # mutate(nwday=ifelse(nwday==3 | nwday==5, 4, nwday)) %>%
      filter(nwday == prognosis.nwday) %>%
      arrange(date)
    
    
    df7 <- df8 %>%
      group_by(date, textdate, integr) %>%
      summarise(mean = mean(mean_value), std.dev = sd(mean_value)) %>%  # а стоял просто mean(value) !!!
      arrange(date)
    
    # тут нужно ставить вилку на проверку
    # определяем, а достаточно ли данных для построения регрессии?
    r.depth <- dim(df7)[1]
    # print("День недели - %s. Количество дат для суточной регрессии - %s", wdnames[prognosis.nwday], r.depth)
    
    # удаление переменных
    # http://stackoverflow.com/questions/21677923/how-to-remove-selected-r-variables-without-having-to-type-their-names
    # работа с локальным контекстом
    # http://stackoverflow.com/questions/17218404/shoud-i-get-a-habit-of-removing-unused-variables-in-r
    
    # считаем, что все измерения являются последовательными в выбранной последовательности (тавтология)
    # boxplot интересно поглядеть для понимания общей картины по значениям и их разбросу.
    # для регрессии необходимо использовать непрерывную переменную x
    # однако, измерения по пропущенным датам надо учитывать, от этого может измениться коэффициент k
    
    # ******************** посчитаем суточную регрессию **********************
    
    # попробуем посчитать линейную регрессию
    # http://www.tatvic.com/blog/linear-regression-using-r/
    # http://www.theanalysisfactor.com/r-tutorial-4/
    # http://blog.yhathq.com/posts/r-lm-summary.html
    nwday_fit <- lm(data = df7, formula = mean ~ date)
    # summary(days.fit)
    # и теперь добавим колонку с днем недели к данным
    days.fit <- rbind(days.fit,
                      data_frame(
                        nwday = as.character(prognosis.nwday),
                        intercept = as.numeric(coef(nwday_fit)[1]),
                        slope = as.numeric(coef(nwday_fit)[2]),
                        r2 = as.numeric(summary(nwday_fit)$r.squared)
                      ))
    
    
    # ******************** посчитаем суточную регрессию **********************
    # посчитаем регрессию для каждого часа
    # Используем do(): https://cran.r-project.org/web/packages/dplyr/README.html
    # http://stackoverflow.com/questions/22182442/dplyr-how-to-apply-do-on-result-of-group-by
    regr_day <- select(df8, nwday, hgroup, date, ratio) %>%
      group_by(hgroup) %>%
      #do(text = tfun(.))
      do(lm_eqn(.)) %>%
      filter(!is.na(slope)) # убрали все строчки где нет регрессионных параметров
    
    # и теперь добавим колонку с днем недели к данным
    regr_day$nwday <- prognosis.nwday
    #req <- plyr::ddply(df8, .(hgroup), lm_eqn)
    
    regr_hdata %<>% bind_rows(regr_day) # добавили вычисления по еще одному дню
    # regr_hdata <- rbind(regr_hdata, regr_day) # https://stat.ethz.ch/pipermail/r-help/2006-June/107734.html
  }
  
  print('regression analysis finished')
  
  # Stop the clock
  pts <- proc.time() - ptm
  # print(pts)
  print("End Math")
  # *********************** математика закончилась ******************************
  
  
  reconstruct.df_fast <- function(timestamps){
    sample_df <- data_frame(timestamp = timestamps)
    # определяем нужные параметры по timestamp
    sample_df %<>% mutate(p.hour.l = hgroup.enum(timestamp),
                          p.hour.r = hgroup.enum(timestamp + minutes(15)),
                          p.date = floor_date(timestamp, "day"),
                          nwday = wday(timestamp))
    # группируем по дням недели и прогнозируем среднесуточный трафик
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
  
  ### реконструкция с помощью функции reconstruct.point для каждой точки отдельно
  # system.time(baseline.val <- lapply(sampledata$timestamp, reconstruct.point))
  # получается ~ 60мс на точку
  
  ### реконструкция с помощью функции reconstruct.df
  baseline.val <- reconstruct.df_fast(forecast_timestapms)
  return(baseline.val)
  
#   sampledata$baseline <- baseline.val
#   
#   # считаем верхний и нижний уровень допустимых значений
#   dev <- .15 # 15%
#   #mindev <- max(df$baseline, na.rm = TRUE) * .05
#   #print(mindev)
#   mindev <- 3 # считаем в абсолютных значениях. Максимальная загрузка у нас 100%
#   
#   sampledata %<>% mutate(low = pmax(0, (baseline - pmax(baseline * dev, mindev))),
#                          up  = baseline + pmax(baseline * dev, mindev),
#                          dev_delta = pmax(baseline * dev, mindev)) # DEBUG
#   export_data <- sampledata %>% select(timestamp, baseline, low, up)
#   return(export_data)
}
    
