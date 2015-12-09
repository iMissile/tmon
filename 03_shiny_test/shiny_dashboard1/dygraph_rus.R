# Русификация функции из официального репозитория dygraphs
# https://github.com/danvk/dygraphs/blob/master/src/dygraph-utils.js
axisLabelFormatter <- "function formatAxisLabel(date, granularity) {
  function zeropad(x) {
    if (x < 10) return '0' + x; else return '' + x;
  };
  function hmsString_(hh, mm, ss) {
    var ret = zeropad(hh) + ':' + zeropad(mm);
    if (ss) {
      ret += ':' + zeropad(ss);
    }
    return ret;
  };
  var year = date.getFullYear(),
      month = date.getMonth(),
      day = date.getDate(),
      hours = date.getHours(),
      mins = date.getMinutes(),
      secs = date.getSeconds(),
      millis = date.getSeconds();
  var SHORT_MONTH_NAMES_ = ['Янв', 'Фев', 'Мар', 'Апр', 'Май', 'Июн',
                            'Июл', 'Авг', 'Сен', 'Окт', 'Ноя', 'Дек'];
  if (granularity >= Dygraph.DECADAL) {
    return '' + year;
  } else if (granularity >= Dygraph.MONTHLY) {
   return SHORT_MONTH_NAMES_[month] + '&#160;' + year;
  } else {
    var frac = hours * 3600 + mins * 60 + secs + 1e-3 * millis;
    if (frac === 0 || granularity >= Dygraph.DAILY) {
      // e.g. '21 Jan' (%d%b)
      return zeropad(day) + '&#160;' + SHORT_MONTH_NAMES_[month];
    } else {
      return hmsString_(hours, mins, secs);
    }
  }
  return 'тест';}"

Recode <- function(str){
  # str <- iconv(str, from="CP1251", to="UTF-8")
  str <- iconv(str, from="CP1251", to="UTF-8")
  return(str)
}

axisLabelFormatter <- Recode(axisLabelFormatter)