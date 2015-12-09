library(dygraphs)
library(xts)

dygraph(xts(data$value, order.by=data$timestamp))

# How to format dygraphs labels in R
# http://stackoverflow.com/questions/28486510/how-to-format-dygraphs-labels-in-r-comma-separate-thousands-place

# How to format date and time in Dygraphs legend according to user locale
# http://stackoverflow.com/questions/32148482/how-to-format-date-and-time-in-dygraphs-legend-according-to-user-locale

axisLabelFormatter <- "function formatAxisLabel(d, gran) {
                        return d.toLocaleDateString('de-DE', {month: 'short'});}"

axisLabelFormatter <- "function formatAxisLabel(d, gran) {
                          var options = {weekday: 'long'};
                          return d.toLocaleDateString('de-DE', options);}"

axisLabelFormatter <- "function formatAxisLabel(d, gran) {
var options = {weekday: 'long'};
return d.toLocaleDateString('ru-RU', options);}"

axisLabelFormatter <- "function formatAxisLabel(d, gran) {
var month_short = new Intl.DateTimeFormat({month: 'short'});
return month_short.format(d);}"

axisLabelFormatter <- "function formatAxisLabel(d, gran) {
return 'тест';}"

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

axisLabelFormatter <- iconv(axisLabelFormatter, from="CP1251", to="UTF-8")
axisLabelFormatter <- iconv(axisLabelFormatter, from="CP1251", to="UTF-8")

main <- "Тест Dygraph"
main <- iconv(main, from="CP1251", to="UTF-8")
main <- iconv(main, from="CP1251", to="UTF-8")
ylab <- "Профиль нагрузки"
ylab <- iconv(ylab, from="CP1251", to="UTF-8")
ylab <- iconv(ylab, from="CP1251", to="UTF-8")

# Русификация функции из официального репозитория dygraphs + moment.js
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
  
  moment.locale('ru');
  var SHORT_MONTH_NAMES_ = moment.monthsShort('-MMM-');
  
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

dygraph(xts(data$value, order.by=data$timestamp), main=main, ylab=ylab) %>%
  dyAxis("x", axisLabelFormatter = axisLabelFormatter)
