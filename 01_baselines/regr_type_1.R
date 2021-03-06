# ���������� �������� ��� ���������� ���� �� ������ ����������� ���������
# �� ����������� ������������ ���� �������� ���������, �����, ��� ��� ������� ���������� ���������


library(lubridate)

subdata2=subdata
subdata2$nwday=as.factor(subdata2$nwday)
subdata2$hgroup=as.factor(subdata2$hgroup)
subdata2$week=week(subdata2$timestamp)

# ��� ������ ����������� �� ������������� � �������� �����
# �������� ����� - (��������) ��������� ������, ������������� ����� - ��� ������, 
# �� ����������� ��������� ������
all_t=nrow(subdata2)
test_t=7*24*4 #672 15-�� �������� ��������� � ������
train_t=all_t-test_t

# � ������ �������� �������� ����������� ��������� � ���������������� nwday � hgroup
# (week ��� ���� �� ������������).
# � ���������� ���������� ���� ��������� ���� c1 � ������ ������������� c2,
# ������ ������� �������� ������������� �����-�� ���������� ���� {nwday; hgroup}

model_1=lm(formula = value ~ nwday:hgroup:week, data = subdata2[(1:(train_t)),])
c1=as.numeric(model_1$coefficients[1])
c2=as.numeric(model_1$coefficients[paste('nwday',subdata2[c(1:all_t),]$nwday,':hgroup',
                                         subdata2[c(1:all_t),]$hgroup,':week',sep='')])

predict=c1+ifelse(is.na(c2),0,c2)*subdata2[c(1:all_t),]$week

#����������� ������������� ���������
c2_smooth=loess(as.numeric(c2)~seq(1:all_t),span=1/100)
predict_smooth=c1+ifelse(is.na(c2_smooth$fitted),0,c2_smooth$fitted)*subdata2[c(1:all_t),]$week

#��� �������� ����� ������������ ��� predict, ��� � predict_smooth - ��� ���� ������������ ��������

#�������
#plot(subdata2$value[1900:2876], type="l")
#lines(predict[1900:2876], type="l", col='blue')
#lines(predict_smooth[1900:2876],col='red')

#���������� ������ �� �������� ������
err=abs(tail(subdata2$value,test_t)-tail(predict,test_t))
SM=sum(err)
SS=sum(err*err)

err_smooth=abs(tail(subdata2$value,test_t)-tail(predict_smooth,test_t))
SM_smooth=sum(err_smooth)
SS_smooth=sum(err_smooth*err_smooth)