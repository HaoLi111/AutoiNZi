biv_graph<-function(data,engine='base'){
  if(engine=='base'){
    plot(data[,1],data[,2],type='p',xlab=colnames(data)[1],ylab=colnames(data)[2])
  }#else if(engine=='ggplot2')
}


#biv_linear

biv_fit2d<-function(data,engine='base',report=T){
  if(engine=='base'){

    fitf<-lm(eval(parse(text = paste0(colnames(data)[2],'~',
                                      colnames(data)[1]))),data=data)
    if(report==T){
      print('The linear regression suggest that:')
      print(paste0('$$',colnames(data)[2],'\\simeq',
                   as.numeric(fitf$coefficients[1]),'+(',
                   as.numeric(fitf$coefficients[2]),')*',
                   colnames(data)[1],'$$'))
      return(fitf)
    }
    return(fitf)
  }#else if(engine=='MFVN')
}

biv_scatter2d<-function(data,engine ='base',report = T){
  if(engine=='base'){
    plot(data[,1],data[,2],xlab =colnames(data)[1],ylab=colnames(data)[2],
         main=paste0('Scatterplot of ',colnames(data)[2],
                     ' with respect to ',colnames(data)[1]
                     ) )
  }else if(engine=='ggplot2'){

    g=qplot(get(x=colnames(data)[1]),y=get(colnames(data)[2]),xlab=colnames(data)[1],
            ylab=colnames(data)[2],data =data,geom =c('point','rug'),col = get(x=colnames(data)[ncol(data)])          )
    try(g <- g+geom_rug(col = get(x=colnames(data)[4])))
    g
  }
}
biv_scatter2dfit<-function(data,engine ='base',report = T){
  if(subset==T){
  require("ggplot2")
  g=qplot(get(x=colnames(data)[1]),y=get(colnames(data)[2]),xlab=colnames(data)[1],
          ylab=colnames(data)[2],data =data,geom =c('point','rug','smooth'),
          col = get(x=colnames(data)[ncol(data)]),method='lm' )
  try(g <- g+geom_rug(col = get(x=colnames(data)[4])))
  g
  }
}





INZI_interest_simple<-function(data){
  print('Brainstorm output for Pre-modelling to pick the variable you are interested in')

  summary(data)
  try({
    biv_cor(data)
  })
  plot(data)
  ndata=data[sapply(data[1,],is.numeric)]
  inf_box(ndata,target = 1:ncol(ndata))
}

biv_cor<-function(data){
  COR=cor(data[sapply(data[1,],is.numeric)])
  print(symnum(COR))
  COR
}
#biv_cor(iris)

