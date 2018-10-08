#Setup, load data


INZI_setup<-function(wd='E:/',fl='data',dataname='mydata',type='.csv',
                     header=T,swd = F,assign = F,
                     pos = .GlobalEnv){
  if(swd==T){
    setwd(wd)
    print(paste0('R Workspace set to',wd,'put the data file in'))
  }
  print('default data name is')
  print('data.csv')
  print('The file loaded :')
  #prepare file name
  filename=paste0(wd,fl,type)
  print(filename)
  #prepare file to mydt
  if(type=='.csv'){
    mydt=read.csv(filename,header=header)
  }else if(type=='.txt'){
    mydt=read.table(filename,header=header)
  }else if(type=='xls'|type=='xlsx'){
    #NOT TESTED
    #read xls and xlsx from
    #https://www.r-bloggers.com/read-excel-files-from-r/

    require(gdata)
    mydt = read.xls (filename, sheet = 1, header = header)
  }
  #assign or return
  if(assign==T){
    print(paste0('Assigned to', dataname,'in',pos))
    assign(filename,mydt,pos=pos)
  }else{
    print('Return')
    return(mydt)
  }
}
#

