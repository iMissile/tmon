#JSON test
# Проблема: Внимание! При работе ф-ции toJSON 49.0 преобразуется в 49. Алексей Марин говорит, что это может быть потенциальной 
# проблемой. Но мне кажется, что это не так серьезно, если учесть на "той" стороне.

library(RCurl)
library(jsonlite)

#test0=getURL('http://tadvisor.dev.tsft.ru/metrica/data/hrNKFiRT/')
#test1=fromJSON(test0)
test1=fromJSON("JSON_test2")
test2=test1
test2=test1$data
test2$datetime=(strptime((test1$data$datetime), "%Y-%m-%dT%H:%M:%S"))
test2$datetime=format(test2$datetime,"%Y-%m-%dT%H:%M:%S")
#test2$value=test2$value*2
test2$value=test2$value
test1$data=test2
test1$data[1,1]=49.0 #изменил одно значение для проверки

#jsonF <- toJSON(test1, pretty=TRUE)
jsonF <- toJSON(test1,na="null", auto_unbox=TRUE, digits=6)
# Внимание! При работе ф-ции toJSON 49.0 преобразуется в 49
write(jsonF, file="export_test.JSON")