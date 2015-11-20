# построение прогноза для временного ряда на основе массива одномерных регрессий
# по результатам тестирования дает менее точный результат, чем для одной
# многомерной регрессии (см. regr_type1.R )

library(lubridate)

subdata1=subdata
#subdata1$nwday=as.factor(subdata1$nwday)
#subdata1$hgroup=as.factor(subdata1$hgroup)

subdata1$week=week(subdata1$timestamp)

h1=100*(seq(0:23)-1)
h2=c(0,1,2,3)
hgroup=sort(expand.grid(h1,h2)[,1]+expand.grid(h1,h2)[,2])


all_t=nrow(subdata1)
test_t=7*24*4 #672 15-ти минутных интервала в неделе
train_t=all_t-test_t

regr <- data.frame('nwday'=integer(), 
                      'hgroup'=character(0), 
                      'b0'=double() , 
                      'b1'=double(),
                  # 'b0_smooth'=double() , 
                  # 'b1_smooth'=double(),                   
                      stringsAsFactors=FALSE) 
#regr_smooth=regr

subdata1_train=subdata1[(1:train_t),]

i=1
for (i_nwday in 1:7) {
  for (i_hgroup in hgroup) {
    z=(subdata1_train[subdata1_train$nwday==i_nwday &subdata1_train$hgroup==i_hgroup,])
    a=lm(value~week,data=z)
    regr[i,'nwday']=i_nwday
    regr[i,'hgroup']=i_hgroup
    regr[i,'b0']=as.numeric(a$coefficients[1])
    regr[i,'b1']=as.numeric(a$coefficients[2])
    i=i+1
  }
}

#regr_smooth <- data.frame(nwday=regr$nwday, 
#                   hgroup=regr$hgroup, 
#                   b0=loess(regr$b0~seq(1:(7*length(hgroup))),span=1/100)$fitted, 
#                   b1=loess(regr$b1~seq(1:(7*length(hgroup))),span=1/100)$fitted,
#                   stringsAsFactors=FALSE) 
                   
#regr_smooth$nwday=regr$nwday
#regr_smooth$hgroup=regr$hgroup

#сглаживание
regr$b0_smooth=loess(regr$b0~seq(1:(7*length(hgroup))),span=1/10)$fitted
regr$b1_smooth=loess(regr$b1~seq(1:(7*length(hgroup))),span=1/10)$fitted



subdata1$fit_value=NA
subdata1$fit_value_smooth=NA
for (i in 1:nrow(subdata1)) {
  c1=as.character(subdata1[i,'nwday'])
  c2=as.character(subdata1[i,'hgroup'])
  #print (c1)
  #print (c2)
  a=regr[regr[,'nwday']==c1 & regr[,'hgroup']==c2,]
  subdata1[i,'fit_value']=(a$b0+a$b1*subdata1[i,'week'])
  subdata1[i,'fit_value_smooth']=(a$b0_smooth+a$b1_smooth*subdata1[i,'week'])
}


#par(mfrow=c(2,1))
#plot(subdata1[c(2000:2800),]$value,  ylab='value',col='black',type='l')
#par(new=TRUE)

#lines(subdata1[c(2000:2800),]$fit_value,col='blue',type='l')
#lines(subdata1[c(2000:2800),]$fit_value_smooth,col='red')


#вычисление ошибки на тестовых данных
err=abs(tail(subdata1$value,test_t)-tail(subdata1$fit_value,test_t))
SM=sum(err)
SS=sum(err*err)

err_smooth=abs(tail(subdata1$value,test_t)-tail(subdata1$fit_value_smooth,test_t))
SM_smooth=sum(err_smooth)
SS_smooth=sum(err_smooth*err_smooth)