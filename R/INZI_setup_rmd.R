#SETUP PAPAER TEMPLATE VIA knitr

INZI_setup_rmd<-function(filename = 'AutoiNZi Report',content = '```{r} \n
plot(iris)
```',DIR = 'E:/'){
  if(DIR =='wd'|DIR=='getwd') DIR=getwd
  message(paste0('File saved to : ', DIR))
  message(paste0(filename,'rmd'))
  hd='---
title: \"AutoiNZi Generated Report"
author: \"AutoiNZi\"
date: \"DATE\"
output: html_document
---
'
  cont=paste0(hd,content)
  cat(cat(file = paste0(DIR,filename,'.rmd'),cont))
}

INZI_pasteCodeViaRmd<-function(code,lang='r'){
s1='```{'
s2=lang
s3='}
'
s4='```
'
paste0(s1,s2,s3,code,s4)
}

INZI_pasteBlockViaRmd<-function(code,lang='r',main='title',type='##'){
s1='```{'
s2=lang
s3='}
'
s4='```
'
ss=paste0(s1,s2,s3,code,s4)
paste0(type,main,'
',ss)
}
