'%QTS%'<-function(a,b) INZI_QTS(a,b)
'%BIV%'<-function(a,b) INZI_BIV(a,b)
'%INF%'<-function(a,b) INZI_INF(a,b)


INZI_BIV<-function(a,b=c('graph','3d'),dopar=F){
  if(dopar==F){
    for(i in b){
      get(paste0('biv_',i),pos = 'AutoiNZi')(data)
    }
  }
}


#iris %BIV% c('cor','graph','3d')

INZI_QTS<-function(a,b) UseMethod('INZI_QTS')
INZI_QTS.data.frame<-function(a,b){}
INZI_QTS.ts<-function(a.b){}
INZI_QTS.inzqts<-function(a,b){}
