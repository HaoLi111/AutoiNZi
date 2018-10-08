biv_graph<-function(data,engine='base'){
  if(engine=='base'){
    plot(data[,1],data[,2],type='p',xlab=colnames(data)[1],ylab=colnames(data)[2])
  }#else if(engine=='ggplot2')
}

biv_scatter3d<-function(data,engine='scatterplot3d'){
  if(engine=='scatterplot3d'){
    require(scatterplot3d)

    layout(matrix(1:2,1))
    scatterplot3d(data[,1],data[,3],data[,2],
                  xlab=colnames(data)[1],ylab=colnames(data)[2],
                  zlab = colnames(data)[2],highlight.3d = T)
    scatterplot3d(data[,3],data[,1],data[,2],
                  xlab=colnames(data)[3],ylab=colnames(data)[1],
                  zlab = colnames(data)[2],highlight.3d = T)

    layout(matrix(1,1,1))
    title(paste0('3D Scatterplot of ',colnames(data)[2],
                 ' with respect to ',colnames(data)[1],
                 ' and ',colnames(data)[3]))
  }else if(engine=='rgl'){
    require(rgl)
    plot3d(data[,1],data[,3],data[,2],
           xlab=colnames(data)[1],ylab=colnames(data)[2],
           zlab = colnames(data)[2])
  }#else if(engine=='gnuplot')
}

biv_fit3d<-function(data,engine='base',report=T){
  if(engine=='base'){

    fitf<-lm(eval(parse(text = paste0(colnames(data)[2],'~',
                                      colnames(data)[1],'+',
                                      colnames(data)[3]))),data=data)
    if(report==T){
      print('The linear regression suggest that:')
      print(paste0('$$',colnames(data)[2],'\\simeq',
                   as.numeric(fitf$coefficients[1]),'+(',
                   as.numeric(fitf$coefficients[2]),')*',
                   colnames(data)[1],'+(',
                   as.numeric(fitf$coefficients[3]),')*',
                   colnames(data)[3],'$$'))
    }
    fitf
  }#else if(engine=='MFVN')
}
#(biv_fit3d(iris))$coefficients
#names((biv_fit3d(iris))$coefficients[1])
#as.numeric((biv_fit3d(iris))$coefficients[1])
biv_cor<-function(data){
  COR=cor(data[sapply(data[1,],is.numeric)])
  print(symnum(COR))
  COR
}
