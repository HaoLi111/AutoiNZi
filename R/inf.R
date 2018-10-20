#inf

#data tidy to dataframe
inf_summary<-function(data,engine='base',target =1,
                      report = T,bins=30){
  if(report ==T){
    print('')
  }
  print(summary(data[,target]))
  if(engine=='base'){
    hist(data[,target],prob = T,breaks=bins-1,xlab=colnames(data)[target],
         col = 'blue',main = paste('Histogram of',colnames(data)[target]))
    lines(density(data[,target],na.rm = T),col = 'red',lty=2)
    rug(jitter(data[,target]),col = 'red')
    #see also Data Mining with R by Luis Torgo
  }else if(engine=='ggplot2'){
    require('ggplot2')
    qplot(data=data,x=get(colnames(data)[target]),xlab=colnames(data)[target],geom ='histogram',
          bins=bins)+geom_rug(alpha = .5)

  }

  #lines(density(data))
  #try(print(stem(data)))
}
#inf_summary(iris,engine = 'ggplot2')
inf_box<-function(data,target=1,engine='base'){
  if(engine=='base'){
    boxplot(data[,target],ylab = colnames(data)[target],col = 'blue')
    try(rug(jitter(data[,target]),col='red',side = 2));abline(h=mean(data[,target],na.rm=T),lty=2,col = 'red')
    title(paste('Boxplot of',colnames(data)[target]))
    #text(y = mean(data[,target],na.rm = T),paste('Mean:',mean(data[,target],na.rm = T)),
     #    side = 2)
    }
}
inf_box(iris)

inf_facet<-function(data,facet=NA,
                    plot=T,report=T){
  if(is.na(facet)){
    facet=ncol(data)
  }else if(length(facet==2)){
    #Use Lattice and Hmisc
  }
  #Now facet is 1

}
