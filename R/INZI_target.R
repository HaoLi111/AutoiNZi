#Auto data Tidying with Certain selected target

#Time series quarterly data e.g. 2001Q4 11.5 ..

INZI_target_QTS<-function(data=mydata,target=NA,dfname='myqts',pos=.GlobalEnv,assign=F,report=T){
  INZI_getQTSstarting<-function(data){
    return(c(as.numeric(substr(data[1,1],1,4)),as.numeric(substr(data[1,1],6,6))))
  }
  if(is.na(target)) target = 2#cleverer way of selecting target should be encouraged
  #prepare starting vector
  starting=c(as.numeric(substr(data[1,1],1,4)),as.numeric(substr(data[1,1],6,6)))#2012Q1 ->c(2012,1)
  myqts=ts(data[,target],start=starting,frequency = 4)
  #report procedure
  if(report==T){
    print(paste0('Starting with quarter',starting[2],' in ',starting[1]))
    print('Convert data to R ts(timeseries)')
  }
  if(assign==T){
    assign(dfname,myqts,pos=pos)
  }else{
    return(myqts)
  }
}#returning a time series obj

INZI_target<-function(data=mydata,target=NA,dfname='mybiv',pos=.GlobalEnv,assign=F,completeonly=T,report=T){
  if(is.na(target[1])){
    #select from correlation <further dev>
  }
  mybiv=data[,target]
  #1 - dependent
  #2 - independent
  #3 - reference and extension studies
  if(completeonly==T) mybiv<-mybiv[complete.cases(mybiv),]
  if(report==T){
    print('Independent:')
    ifelse(is.character(target),print(target[1]),print(colnames(data)[target[1]]))
    print('Dependent:')
    ifelse(is.character(target),print(target[2]),print(colnames(data)[target[2]]))
    if(length(target)==3){
      print('Reference:')
      ifelse(is.character(target),print(target[3]),print(colnames(data)[target[3]]))
    }
    print(summary(mybiv))
  }
  if(assign==T){
    assign(dfname,mybiv,pos=pos)
  }else{
    return(mybiv)
  }
}#bivariate data

#INZI_target_INF<-function(data=mydata,target=NA,dfname='myinf',pos=.GlobalEnv,assign=F,completeonly=T,report=T){
