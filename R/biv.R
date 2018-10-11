biv_graph<-function(data,engine='base'){
  if(engine=='base'){
    plot(data[,1],data[,2],type='p',xlab=colnames(data)[1],ylab=colnames(data)[2])
  }#else if(engine=='ggplot2')
}






#biv_3d

biv_3d<-function(data,report =T){

  if(report==T) print(paste0('For better prediction of',
                             colnames(data)[2],
                             ' take the effect of ',
                             colnames(data)[3],
                             ' into account.'))

  biv_scatter3d(data)

  if(report==T) print('Perform a linear regression')
  fitf = biv_fit3d(data,report=report)
  m =biv_lmMat3d(data,fitf)
  biv_persp3d(data,m)

}
biv_scatter3d<-function(data,engine='scatterplot3d'){
  if(engine=='scatterplot3d'){
    layout(matrix(1:2,1))
    scatterplot3d :: scatterplot3d(data[,1],data[,3],data[,2],
                  xlab=colnames(data)[1],ylab=colnames(data)[2],
                  zlab = colnames(data)[2],highlight.3d = T)
    scatterplot3d :: scatterplot3d(data[,3],data[,1],data[,2],
                  xlab=colnames(data)[3],ylab=colnames(data)[1],
                  zlab = colnames(data)[2],highlight.3d = T)

    layout(matrix(1,1,1))
    title(paste0('3D Scatterplot of ',colnames(data)[2],
                 ' with respect to ',colnames(data)[1],
                 ' and ',colnames(data)[3]))
  }else if(engine=='rgl'){
    rgl :: plot3d(data[,1],data[,3],data[,2],
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
      return(fitf)
    }
    return(fitf)
  }#else if(engine=='MFVN')
}
#(biv_fit3d(iris))$coefficients
#names((biv_fit3d(iris))$coefficients[1])
#as.numeric((biv_fit3d(iris))$coefficients[1])

biv_lmMat3d=function(data,fitf,report=T,res1=30,res2=30,engine = 'base'){
  base1 = seq(from =min(data[,1]),to = max(data[,1]),length.out = res1);base2 = seq(from = min(data[,3]),to = max(data[,3]),length.out=res2)
  m = matrix(NA,res1,res2)
  if(report==T) print('Calculate the estimation by the model.')
  #call gradient
  m0 = as.numeric(fitf$coefficients[1]);m1= as.numeric(fitf$coefficients[2]);m2 = as.numeric(fitf$coefficients[3])
  for(i in 1:res1){
    for(j in 1:res2){
      m[i,j]<-as.numeric(m0+ m1*base1[i] + m2*base2[j])
    }
  }
  m
}
#biv_lmMat3d(iris,biv_fit3d(iris))
biv_persp3d<-function(data,m,report=T,engine = 'base'){
  m=as.matrix(m)
  layout(matrix(1:2,1))
  base1 = seq(from =min(data[,1]),to = max(data[,1]),length.out = nrow(m))
  base2 = seq(from =min(data[,3]),to = max(data[,3]),length.out = ncol(m))
  if(report ==T) print('The relation of variables suggected by this model can be visualized
                       as a plane in 3D space.')
  persp(x=base1,y=base2,m,xlab = colnames(data)[1],ylab = colnames(data)[3],
        zlab =colnames(data)[2],shade = .6)
  persp(x=base2,y=base1,t(m),xlab = colnames(data)[3],ylab =  colnames(data)[1],
        zlab=colnames(data)[2],shade = .6)
  layout(matrix(1,1,1))

  title(paste0('lm Prediction of ',colnames(data)[2],
               ' with respect to ',colnames(data)[1],
               ' and ',colnames(data)[3]))
}#
#m = biv_lmMat3d(iris,biv_fit3d(iris))
#biv_persp3d(iris,m)



biv_cor<-function(data){
  COR=cor(data[sapply(data[1,],is.numeric)])
  print(symnum(COR))
  COR
}
#biv_cor(iris)

