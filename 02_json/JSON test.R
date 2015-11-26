#тестирование экспорта и импорта на/с веб-сервер
#JSON test


library(RCurl)
library(jsonlite)

geturl=getURL('http://127.0.0.1:8080/HFS/export_111.JSON')
#test4=fromJSON(geturl)

#test1=fromJSON("JSON_test2")
test1=fromJSON('http://127.0.0.1:8080/HFS/export_111.JSON')
test2=test1$data[is.na(test1$data$value),2]
test2=test2[order( rank(test2))]
jsonF <- toJSON(test2,na="null", auto_unbox=TRUE, digits=6)
#write(jsonF, file="export_12.JSON")
postform=postForm("http://127.0.0.1:8080/HFS/","fileData" = 
                    fileUpload( contents = jsonF))

postform=postForm("http://127.0.0.1:8080/HFS/a.php", param="a")



library(httr)
r <- POST("http://127.0.0.1:8080/HFS/", 
          body = "Tim O'Reilly, Archbishop Huxley")

#--------------------------------------------------------------------
#-- с паролями и POST (нужно для авторизации)
#your.username <- 'AKononyhin'
#your.password <- '123456'

#curl = getCurlHandle()
#curlSetOpt(
#  followlocation = TRUE ,
#  autoreferer = TRUE ,
#  curl = curl
#) 

# list parameters to pass to the website (pulled from the source html)
#params <-
#  list(
#    'username' = 'AKononyhin',
#    'password' = '123456'
#  )


#getForm('http://tadvisor.dev.tsft.ru/accounts/login/?next=/forecast/&username=\"AKononyhin\"&Password=\"123456\"')


# logs into the form
#html = postForm('http://tadvisor.dev.tsft.ru/', .params = params, curl=curl, style="POST")
#html = postForm('http://tadvisor.dev.tsft.ru/accounts/login/?next=/forecast/', 'Username'='AKononyhin', 'Password'='123456', style="HTTPPOST")
#html
#-------------------------------------------------------------------------

