# ��������� ���� ������ ���������� � ����������� (1), ������� ������ � ���������� ���� ��� �������� �����������
wdnames <-
  c(
    "�����������",
    "�����������",
    "�������",
    "�����",
    "�������",
    "�������",
    "�������"
  )



# =================================================
date_format_tz <- function(format = "%Y-%m-%d", tz = "UTC") {
  function(x) format(x, format, tz=tz)
}


# ����� �� ������ ��� ������������ ���������� ��� ��������� ��������� ������ ��
# ���������� �������, ���� ���� �������� ������ ����� ���������?
hgroup.enum <- function(date){
  hour(date) * 100 + floor(minute(date) / 15)
}

precalc_df <- function(subdata) {
  # ��������� �������� �������� ��� ������ ������, ����� ����� ����� ����������� ����������
  # ���� �� ������������� ������ ���� � ���������
  # �������������� ������� ��������
  # �������, ��� ��������� ����������, ����� ��������� �������� y(x) ������� ��������
  # https://chemicalstatistician.wordpress.com/2013/12/14/conceptual-foundations-and-illustrative-examples-of-trapezoidal-integration-in-r/
  # http://stackoverflow.com/questions/24813599/finding-area-under-the-curve-auc-in-r-by-trapezoidal-rule
  # http://svitsrv25.epfl.ch/R-doc/library/caTools/html/trapz.html
  df0 <- subdata %>%
    group_by(date) %>%
    # �������� ������� �������� �� ������, trapz �� ������ caTools
    # ����� ������� ������� �������� �� ��� ��� ������������ ������� ���������
    summarise(
      integr = trapz(timestamp, value),
      # ������-��, ��� ������� ������� �������. �� �� �� �������, ��� ���� ����� ������ �������. ��������� �������� �� ����� ������
      # mean = mean(value),
      mean_value = integr/(24*60) #!!!!!
    )
  subdata <- dplyr::left_join(subdata, df0, by = "date")
  #browser()
  return(subdata)
}

# ������� ��� ����������� 
# http://stackoverflow.com/questions/28162486/display-regression-slopes-for-multiple-subsets-in-ggplot2-facet-grid
# http://stackoverflow.com/questions/19699858/ggplot-adding-regression-line-equation-and-r2-with-facet
lm_eqn = function(df){
  # 1. [ggplot2 Quick Reference: geom_abline](http://sape.inf.usi.ch/quick-reference/ggplot2/geom_abline), ������� ���� [�����](http://docs.ggplot2.org/current/geom_abline.html)
  # - slope - (required) slope of the line (the "a" in "y=ax+b")
  # - intercept - (required) intercept with the y axis of the line (the "b" in "y=ax+b").
  
  # print(df)
  # browser()

  # ����� ��� ���� ���� ���������, ��� ��������� ����� ���������
  m = lm(formula = ratio ~ date, data = df);
  # ����� ������������� ��������, ��� ��� ������� ������ ����� ����� Slope �������� �������� NA
  
  #print(m)
  #dm <<- m
  #dd <<- coefficients(m)
  intercept <- as.numeric(coef(m)[1])
  slope <- as.numeric(coef(m)[2])
  r2 <- as.numeric(summary(m)$r.squared)
#   intercept <- signif(coef(m)[1], digits = 2)
#   slope <- signif(coef(m)[2], digits = 2)
#   r2 <- signif(summary(m)$r.squared, 3)
  #lm_label <- as.character(paste("y=", slope, "*x", intercept, ", r2=", r2, sep = ''))
  lm_label <- sprintf("y=%.2e*x%+.2f, r2=%.2f", signif(slope, 2), signif(intercept, 2), signif(r2,3))
  # ������ ��������� ���������� �� �����������
  x <- min(df$date)
  y <- max(df$ratio)
  # https://cran.r-project.org/web/packages/dplyr/vignettes/data_frames.html
  dplyr::data_frame(x, y, lm_label, slope, intercept, r2)
}

tfun = function(df){
  print(df)
}

generate.discrete <- function(df) {
  # �� ���� �������� df � ���������
  # timestamp = POSIXct
  # value
  
  # �� ��������, ��� ���� ������������ �������������� ������..., �� ��� �������� �������
  # � ������ ��������� ������� ������������ �� 15-�� �������� �����������
  # http://stackoverflow.com/questions/16011790/add-missing-xts-zoo-data-with-linear-interpolation-in-r
  # http://www.noamross.net/blog/2014/2/10/using-times-and-dates-in-r---presentation-code.html
  # seq(dmy("01-01-2015"), dmy("02-01-2015"), "15 mins")
  # seq(round_date(min(df$timestamp), "hour"), max(df$timestamp), "15 mins")

  # ZOO: Z's Ordered Observations
  # ������� ������������������ ��������� ���������� �� ������� ������ ������������
  # ������� ��������� ��� �� ������
  tseq <- seq(round_date(min(df$timestamp), "hour"), max(df$timestamp), "15 mins")
  # �� ������ �������, ������ �� ���������� ��������� > 15 ���, �� ��������� ������� ����� :(
  # IN
  tt <- zoo(df$value, order.by = df$timestamp)
  # na.approx\na.spline
  v_seq <- na.approx(object = tt, xout = tseq)
  
  # ���������� zoo ������� � data.frame
  # http://stackoverflow.com/questions/14064097/r-convert-between-zoo-object-and-data-frame-results-inconsistent-for-different
  # df2b <- data.frame(timestamp=time(t2), t2, check.names=FALSE, row.names=NULL)
  
  # ��������� ��� ������ �����, ���������� ���� ������������ data.frame
  # Use Function time() to return all dates corresponding to a series index(z) or equivalently
  # Use Function coredata() to return all values corresponding to a series index(z) or equivalently
  r.dev <- runif(length(v_seq), 0.99, 1.01) # ��������� ��������� ��������� �������
  testdata <-
    dplyr::data_frame(
      timestamp = tseq,
      value = coredata(v_seq) * r.dev 
    )
  
  testdata
}
