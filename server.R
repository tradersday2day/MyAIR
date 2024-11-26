rm(list=ls())
ddm<<-1
tj<<-1
csym<<-1
ups<<-1
uc<<-1
j1<<-1
o1<<-1
vstacc<<-read.table(file="vstacc.csv",header=TRUE,sep=",") 

testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}



cave <- function(x, c1, c2) 
{
  if(x[c1]< x[c2])
  {
    0
  }else
  {
    1
  }
}

detectSupportResistance <- function(timeSeries, tolerance=1, nChunks=10, nPoints=3, plotChart=TRUE)
{
  #detect maximums and minimums
  N = length(timeSeries)
  stp = floor(N / nChunks)
  minz = array(0.0, dim=nChunks)
  whichMinz = array(0, dim=nChunks)
  maxz = array(0.0, dim=nChunks)
  whichMaxz = array(0, dim=nChunks)
  for(j in 1:(nChunks-1)) 
  { 
    lft = (j-1)*stp + 1  #left and right elements of each chunk
    rght = j*stp
    whichMinz[j] = which.min(timeSeries[lft:rght]) + (lft-1)
    minz[j] = min(timeSeries[lft:rght])
    whichMaxz[j] = which.max(timeSeries[lft:rght]) + (lft-1)
    maxz[j] = max(timeSeries[lft:rght])
  }   
  #last chunk
  lft = j*stp + 1  #left and right elements of each chunk
  rght = N
  whichMinz[nChunks] = which.min(timeSeries[lft:rght]) + (lft-1)
  minz[nChunks] = min(timeSeries[lft:rght])
  whichMaxz[nChunks] = which.max(timeSeries[lft:rght]) + (lft-1)
  maxz[nChunks] = max(timeSeries[lft:rght])
  
  result = list()
  result[["minima"]] = NULL
  result[["minimaAt"]] = NULL
  result[["maxima"]] = NULL
  result[["maximaAt"]] = NULL
  span = tolerance*(max(maxz) - min(minz))
  
  rang = order(minz)[1:nPoints]
 # if((minz[rang[nPoints]] - minz[rang[1]]) <= span)
  {
    result[["minima"]] = minz[rang[1:nPoints]]
    result[["minimaAt"]] = whichMinz[rang[1:nPoints]]
  } 
  
  rang = order(maxz, decreasing = TRUE)[1:nPoints]
 # if((maxz[rang[1]] - maxz[rang[nPoints]]) <= span)
  {
    result[["maxima"]] = maxz[rang[1:nPoints]]
    result[["maximaAt"]] = whichMaxz[rang[1:nPoints]]
  }

   
  return(result)    
}


eval_with_timeout <- function(expr, envir = parent.frame(), timeout, on_timeout = c("error", "warning", "silent")) {
  # substitute expression so it is not executed as soon it is used
  expr <- substitute(expr)
  
  # match on_timeout
  on_timeout <- match.arg(on_timeout)
  
  # execute expr in separate fork
  myfork <- parallel::mcparallel({
    eval(expr, envir = envir)
  }, silent = FALSE)
  
  # wait max n seconds for a result.
  myresult <- parallel::mccollect(myfork, wait = FALSE, timeout = timeout)
  # kill fork after collect has returned
  tools::pskill(myfork$pid, tools::SIGKILL)
  tools::pskill(-1 * myfork$pid, tools::SIGKILL)
  
  # clean up:
  parallel::mccollect(myfork, wait = FALSE)
  
  # timeout?
  if (is.null(myresult)) {
    if (on_timeout == "error") {
      stop("reached elapsed time limit")
    } else if (on_timeout == "warning") {
      warning("reached elapsed time limit")
    } else if (on_timeout == "silent") {
      myresult <- NA
    }
  }
  
  # move this to distinguish between timeout and NULL returns
  myresult <- myresult[[1]]
  
  if ("try-error" %in% class(myresult)) {
    stop(attr(myresult, "condition"))
  }
  
  # send the buffered response
  return(myresult)
}

x  <- c("i1","i2","i3","i4","i5","i6","i7","i8","i9")

testc=combinations(n=9,r=1,v=x,repeats.allowed = FALSE)

tesd1=cbind(testc,testc,testc,testc,testc,testc,testc,testc,testc)


testc=combinations(n=9,r=2,v=x,repeats.allowed = FALSE)

tesd2=cbind(testc,testc,testc,testc,testc[,1])


testc=combinations(n=9,r=3,v=x,repeats.allowed = FALSE)

tesd3=cbind(testc,testc,testc)


testc=combinations(n=9,r=4,v=x,repeats.allowed = FALSE)

tesd4=cbind(testc,testc,testc[,1])



testc=combinations(n=9,r=5,v=x,repeats.allowed = FALSE)

tesd5=cbind(testc,testc[,1:4])

testc=combinations(n=9,r=6,v=x,repeats.allowed = FALSE)
tesd6=cbind(testc,testc[,1:3])

testc=combinations(n=9,r=7,v=x,repeats.allowed = FALSE)
tesd7=cbind(testc,testc[,1:2])


testc=combinations(n=9,r=8,v=x,repeats.allowed = FALSE)
tesd8=cbind(testc,testc[,1])

testc=combinations(n=9,r=9,v=x,repeats.allowed = FALSE)
tesd9=cbind(testc)


testd=rbind(tesd1,tesd2,tesd3,tesd4,tesd5,tesd6,tesd7,tesd8,tesd9)

testr<- apply( testd[ , ] , 1 , paste , collapse = "+" )



cest=as.data.frame(1)
maxIter <<- 0
maxIter1 <<- 0
maxIter2 <<- 0
maxIter3 <<- 0
gel=read.table(file="gel.csv",header=TRUE,sep=",") 
Ninf<<-2
Ninf1<<-2


gts1<-1
gsts1<-1
gts<-1
gsts<-1
Nhinf<<-2


smd=c('xxx',
      '172.23',
      '(0.58%)',
      '170.25',
      '13.12M/14.26M')

smd=as.data.frame(smd)
smd=t(smd)
smd=as.data.frame(smd)
gest=read.table(file="mest.csv",header=FALSE,sep=",") 

colnames(gest)=c("symbol","Date","Open","High","Low","close","volume")
gest=as.data.frame(gest)
peaks <- function(x, halfWindowSize) {
  
  windowSize <- halfWindowSize * 2 + 1
  windows <- embed(x, windowSize)
  
  
  localMaxima <- max.col(windows, "first") == halfWindowSize + 1
  
  return(c(rep(FALSE, halfWindowSize), localMaxima, rep(FALSE, halfWindowSize)))
}


peaks1 <- function(x, halfWindowSize) {
  
  windowSize <- halfWindowSize * 2 + 5
  windows <- embed(x, windowSize)
  
  
  localMaxima <- max.col(windows, "first") == halfWindowSize + 5
  
  return(c(rep(FALSE, halfWindowSize), localMaxima, rep(FALSE, halfWindowSize)))
}


supres <-   function() 
{
  
  test= sqldf('SELECT distinct * FROM hest where symbol not in("NSE:XXX") order by Date desc ',drv='SQLite')
  
  allsym=as.data.frame(as.matrix(hest$symbol))
  
  
  colnames(allsym)=c("symbol")
  
  yoursym2=as.character(allsym[1,1])
  
  rest= sqldf('SELECT distinct * FROM test where symbol not in("NSE:XXX") order by Date desc ',drv='SQLite')
  
  
  
  test=as.data.frame(rest)
  mm1=1:(nrow(hest)-150)
  mm2=1:(nrow(hest)-150)
  m1=1:(nrow(hest)-150)
  m11=1:(nrow(hest)-150)
  m2=1:(nrow(hest)-150)
  m22=1:(nrow(hest)-150)
  zy1=1:(nrow(hest)-150)
  zy2=1:(nrow(hest)-150)
  zy11=1:(nrow(hest)-150)
  zy22=1:(nrow(hest)-150)
  mxb=1:(nrow(hest)-150)
  kr=1:(nrow(hest)-150)
  lr=1:(nrow(hest)-150)
  mr=1:(nrow(hest)-150)
  nr=1:(nrow(hest)-150)
  brk=1:(nrow(hest)-150)
  dly3=11
  zx111=1:(nrow(hest)-150)
  zy122=1:(nrow(hest)-150)
  zx=1:(nrow(hest)-150)
  zy=1:(nrow(hest)-150)
  dly2=-250
  sup=1:(nrow(hest)-150)
  res=1:(nrow(hest)-150)
  resls=1:(nrow(hest)-150)
  supls=1:(nrow(hest)-150)
  dun=1:(nrow(hest)-150)
  dun[1:(nrow(hest)-150)]=0
  don=1:(nrow(hest)-150)
  don[1:(nrow(hest)-150)]=0
  for(ch in 1:(nrow(hest)-150))
  {
    
    test <- rest[((nrow(hest)-149)-ch):nrow(hest),]
    
    test=sqldf('SELECT distinct * FROM test  order by Date asc ',drv='SQLite')
    
    test <- test[((nrow(test)-149)):nrow(test),]

    print(ch)
    Lines <-  sqldf('SELECT  Date as Date ,open as Open , high as High, low as Low, close as Close,  volume FROM test order by Date desc',drv='SQLite')
    
    
    Lines$Open=gsub("\\.", ",", Lines$Open)
    
    
    Lines$High=gsub("\\.", ",", Lines$High)
    
    
    Lines$Low=gsub("\\.", ",", Lines$Low)
    
    
    Lines$Close=gsub("\\.", ",", Lines$Close)
    
    
    df=sapply(Lines, function(x) gsub("\"", "", x))
    
    df1=as.data.frame(df,quote=FALSE)
    
    
    connectionString <- paste0(df1$Date,";",df1$Open,";",df1$High,";",df1$Low,";",df1$Close,";",df1$volume)
    
    names= paste0("Date;Open;High;Low;Close;Volume")
    
    
    names=as.data.frame(names)
    connectionString=as.data.frame(connectionString)
    
    colnames(connectionString)=c("names")
    
    dd=rbind(names,connectionString)
    
    
    connectionString <- lapply(dd, as.character)
    
    df=sapply(connectionString, function(x) gsub("\"", "", x))
    
    
    
    
    
    conn <- textConnection(df)
    stock=as.xts(read.zoo(conn, sep=';', tz="US/Eastern", dec=",", header=TRUE,
                          format='%Y-%m-%d', index.column=1))
    
    stock1= (stock)
    
    
    
    close(conn)
    
    
    
    
    stock2=as.matrix(stock1)
    stock2=stock1[,1:5]
    
    stock2=as.data.frame(stock2)
    
    colnames(stock2) <- c("Open", "High", "Low", "Close","Volume")
    
    stock3=as.matrix(stock2)
    
    stock3=stock3[,1:5]
    
    
    
    stock3=as.xts(stock3,tz="EST")
    .index(stock3)[1:nrow(stock3)] <-(.index(stock3)[1:nrow(stock3)])+10.5*3600
    
    
    mals=as.matrix(test$close)
    mals=as.matrix(test$close)
    
    mals=as.numeric(mals[,1]) 
    
    hals=as.matrix(test$High)
    
    hals=as.numeric(hals[,1]) 
    
    lals=as.matrix(test$Low)
    
    lals=as.numeric(lals[,1]) 
    
    vcols=as.matrix(test$volume)
    
    vcols=as.numeric(vcols[,1]) 
    
    
    
    mals=mals[1:length(mals)]
    nx=-(1:length(mals))
    
    bals=mals[-nx]
    
    nx=-(length(mals):1)
    
    x <- nx
    
    nx=-(1:length(mals))
    y <- bals[-nx]
    v <- vcols[-nx]
    
    
    k=1:10
    l=1:10
    m=1:10
    n=1:10
    brk=1:10
    
    m1=1:10
    m11=1:10
    m2=1:10
    m22=1:10
    zy1=1:10
    zy2=1:10
    zy11=1:10
    zy22=1:10
    mxb=1:10
    zx111=1:10
    zy122=1:10
    zx=1:10
    zy=1:10
    sup=1:10
    res=1:10
    resls=1:10
    supls=1:10 
    

      library(extRemes)
      library(xts)
      
      
      
      ams=stock3
      
      
      vv=to.weekly(stock3)
      bbb<-stock3
      vv=bbb$High
      
      ress=detectSupportResistance(vv, tolerance=1, nChunks=10, nPoints=10, plotChart=TRUE)
      
      maxams <- ams$High
      
      
      maxams=as.matrix(as.data.frame(maxams))
      
      minams <- ams$Low
      
      minams=as.matrix(as.data.frame(minams))
      xt=1:nrow(maxams)
      i=1 
      j=1
      for(i in i:nrow(maxams))
      { 
        for(j in j:nrow(stock3))
        {
          if(maxams[i]==hals[j])
          {
            xt[i]=j
            
            break
          }
          else
          {
            
          }
          
        }
      }
      
      
      
      yt=1:nrow(minams)
      i=1 
      j=1
      for(i in i:nrow(minams))
      { 
        for(j in j:nrow(stock3))
        {
          if(minams[i]==lals[j])
          {
            yt[i]=j
            
            break
          }
          else
          {
            
          }
          
        }
      }
      
      
      
      pks11=sort(c(ress$maximaAt,ress$minimaAt))
      
      vv=bbb$Low
      
      ress=detectSupportResistance(vv, tolerance=1, nChunks=10, nPoints=10, plotChart=TRUE)
      
      pksl2=sort(c(ress$maximaAt,ress$minimaAt))
      
      print(pks11)
      print(pksl2)
      dpks111= which(xt == max(pks11))
      dpksl22=which(yt == max(pksl2))
      
      
      
      dpks11=xt[((dpks111):length(xt))]
      dpksl2=yt[((dpksl22):length(yt))]
      
      if(length(dpks11)<2)
      {  dpks11=xt[((dpks111-1):length(xt))]
      
      
      }
      
      if(length(dpksl2)<2)
      { 
        dpksl2=yt[((dpksl22-1):length(yt))]
        
      }
      
      
      
      
      x1=(length(hals)-(pks11[length(pks11)]))-(length(hals)-(pks11[length(pks11)-1])) 
      y1= hals[pks11[length(pks11)]]- hals[pks11[length(pks11)-1]]
      m1=y1/x1
      
      
      
      x2=(length(lals)-(pksl2[length(pksl2)]))-(length(lals)-(pksl2[length(pksl2)-1])) 
      y2= lals[pksl2[length(pksl2)]]- lals[pksl2[length(pksl2)-1]]
      m2=y2/x2
      
      
      x1=(length(hals)-(pks11[length(pks11)-1]))-(length(hals)-(pks11[length(pks11)-2])) 
      y1= hals[pks11[length(pks11)-1]]- hals[pks11[length(pks11)-2]]
      m11=y1/x1
      
      
      x2=(length(lals)-(pksl2[length(pksl2)-1]))-(length(lals)-(pksl2[length(pksl2)-2])) 
      y2= lals[pksl2[length(pksl2)-1]]- lals[pksl2[length(pksl2)-2]]
      m22=y2/x2
      
      
      
      
      k=m1
      
      l=m2
      
      m=m11
      
      n=m22
      
      
      
      
      zy1=hals[pks11[length(pks11)]]-m1*(length(hals)-(pks11[length(pks11)])) 
      
      
      
      zy11=hals[pks11[length(pks11)-1]]-m11*(length(hals)-(pks11[length(pks11)-1])) 
      
      
      zy22=lals[pksl2[length(pksl2)-1]]-m22*(length(lals)-(pksl2[length(pksl2)-1])) 
      
      
      
      zy2=lals[pksl2[length(pksl2)]]-m2*(length(lals)-(pksl2[length(pksl2)])) 
      zx=pks11[length(pks11)]
      zy=pksl2[length(pksl2)]
      
      zx111=pks11[length(pks11)-1]
      zy122=pksl2[length(pksl2)-1]
      mxb=bals[length(bals)]
      res=hals[pks11[length(pks11)]]
      sup=lals[pksl2[length(pksl2)]]
      resls=hals[pks11[length(pks11)-1]]
      supls=lals[pksl2[length(pksl2)-1]]
      

      ch11=c(hals[pks11])
      ch12=c(lals[pksl2])
      ch111=c(hals[dpks11])
      ch112=c(lals[dpksl2])
      
      
      

        if(((((ch11[length(ch11)]-ch11[length(ch11)-1])*100)/ch11[length(ch11)-1])>(0.5))&&((((ch12[length(ch12)]-ch12[length(ch12)-1])*100)/ch12[length(ch12)-1])>(0.5)))             
          
        {
          if((length(ch111)<=3)&&(length(ch112)<=3))
          { 
            if(((((ch111[length(ch111)]-ch111[length(ch111)-1])*100)/ch111[length(ch111)-1])>(-1.5))&&((((ch112[length(ch112)]-ch112[length(ch112)-1])*100)/ch112[length(ch112)-1])>(-1.5)))            
            {
              print("down-up reversal")
              dun[ch]=1
            }
          }
        }
 
      if((length(ch111)>3)||(length(ch112)>3))
      {
        if(length(dpks11)>2)
        { md= min(dpks11)
        mx= max(dpks11)
        dpks11=c(md,mx)
        
        }
        
        
        if(length(dpksl2)>2)
        {
          md= min(dpksl2)
          mx= max(dpksl2)
          dpksl2=c(md,mx)
          
        }
        
        
        
        ch111=c(hals[dpks11])
        ch112=c(lals[dpksl2])
        
        if(((((ch111[length(ch111)]-ch111[length(ch111)-1])*100)/ch111[length(ch111)-1])>=(2))&&((((ch112[length(ch112)]-ch112[length(ch112)-1])*100)/ch112[length(ch112)-1])>=(2)))            
        {
          print("down-up reversal")
          dun[ch]=1
        }
      }
      
      
      
      
      vv=to.monthly(stock3)
      bbb<-stock3
      vv=bbb$High
      
      ress=detectSupportResistance(vv, tolerance=1, nChunks=5, nPoints=5, plotChart=TRUE)
      
      maxams <- ams$High
      
      
      maxams=as.matrix(as.data.frame(maxams))
      
      minams <- ams$Low
      
      minams=as.matrix(as.data.frame(minams))
      xt=1:nrow(maxams)
      i=1 
      j=1
      for(i in i:nrow(maxams))
      { 
        for(j in j:nrow(stock3))
        {
          if(maxams[i]==hals[j])
          {
            xt[i]=j
            
            break
          }
          else
          {
            
          }
          
        }
      }
      
      
      
      yt=1:nrow(minams)
      i=1 
      j=1
      for(i in i:nrow(minams))
      { 
        for(j in j:nrow(stock3))
        {
          if(minams[i]==lals[j])
          {
            yt[i]=j
            
            break
          }
          else
          {
            
          }
          
        }
      }
      
      
      
      pks11=sort(c(ress$maximaAt,ress$minimaAt))
      
      vv=bbb$Low
      
      ress=detectSupportResistance(vv, tolerance=1, nChunks=5, nPoints=5, plotChart=TRUE)
      
      pksl2=sort(c(ress$maximaAt,ress$minimaAt))
      
      print(pks11)
      print(pksl2)
      dpks111= which(xt == max(pks11))
      dpksl22=which(yt == max(pksl2))
      
      
        
      dpks11=xt[((dpks111):length(xt))]
      dpksl2=yt[((dpksl22):length(yt))]
      
      if(length(dpks11)<2)
      {  dpks11=xt[((dpks111-1):length(xt))]
      
      
      }
      
      if(length(dpksl2)<2)
      { 
        dpksl2=yt[((dpksl22-1):length(yt))]
        
      }
      
      
      
      
      x1=(length(hals)-(pks11[length(pks11)]))-(length(hals)-(pks11[length(pks11)-1])) 
      y1= hals[pks11[length(pks11)]]- hals[pks11[length(pks11)-1]]
      m1=y1/x1
      
      
      
      x2=(length(lals)-(pksl2[length(pksl2)]))-(length(lals)-(pksl2[length(pksl2)-1])) 
      y2= lals[pksl2[length(pksl2)]]- lals[pksl2[length(pksl2)-1]]
      m2=y2/x2
      
      
      x1=(length(hals)-(pks11[length(pks11)-1]))-(length(hals)-(pks11[length(pks11)-2])) 
      y1= hals[pks11[length(pks11)-1]]- hals[pks11[length(pks11)-2]]
      m11=y1/x1
      
      
      x2=(length(lals)-(pksl2[length(pksl2)-1]))-(length(lals)-(pksl2[length(pksl2)-2])) 
      y2= lals[pksl2[length(pksl2)-1]]- lals[pksl2[length(pksl2)-2]]
      m22=y2/x2
      
      
      
      
      k=m1
      
      l=m2
      
      m=m11
      
      n=m22
      
      
      
      
      zy1=hals[pks11[length(pks11)]]-m1*(length(hals)-(pks11[length(pks11)])) 
      
      
      
      zy11=hals[pks11[length(pks11)-1]]-m11*(length(hals)-(pks11[length(pks11)-1])) 
      
      
      zy22=lals[pksl2[length(pksl2)-1]]-m22*(length(lals)-(pksl2[length(pksl2)-1])) 
      
      
      
      zy2=lals[pksl2[length(pksl2)]]-m2*(length(lals)-(pksl2[length(pksl2)])) 
      zx=pks11[length(pks11)]
      zy=pksl2[length(pksl2)]
      
      zx111=pks11[length(pks11)-1]
      zy122=pksl2[length(pksl2)-1]
      mxb=bals[length(bals)]
      res=hals[pks11[length(pks11)]]
      sup=lals[pksl2[length(pksl2)]]
      resls=hals[pks11[length(pks11)-1]]
      supls=lals[pksl2[length(pksl2)-1]]
      
      
      ch11=c(hals[pks11])
      ch12=c(lals[pksl2])
      ch111=c(hals[dpks11])
      ch112=c(lals[dpksl2])
      
      
      
      
      if(((((ch11[length(ch11)]-ch11[length(ch11)-1])*100)/ch11[length(ch11)-1])>(0.5))&&((((ch12[length(ch12)]-ch12[length(ch12)-1])*100)/ch12[length(ch12)-1])>(0.5)))             
        
      {
        if((length(ch111)<=3)&&(length(ch112)<=3))
        { 
          if(((((ch111[length(ch111)]-ch111[length(ch111)-1])*100)/ch111[length(ch111)-1])>(-1.5))&&((((ch112[length(ch112)]-ch112[length(ch112)-1])*100)/ch112[length(ch112)-1])>(-1.5)))            
          {
            print("down-up reversal")
            dun[ch]=1
          }
        }
      }
      
      if((length(ch111)>3)||(length(ch112)>3))
      {
        if(length(dpks11)>2)
        { md= min(dpks11)
        mx= max(dpks11)
        dpks11=c(md,mx)
        
        }
        
        
        if(length(dpksl2)>2)
        {
          md= min(dpksl2)
          mx= max(dpksl2)
          dpksl2=c(md,mx)
          
        }
        
        
        
        ch111=c(hals[dpks11])
        ch112=c(lals[dpksl2])
        
        if(((((ch111[length(ch111)]-ch111[length(ch111)-1])*100)/ch111[length(ch111)-1])>=(2))&&((((ch112[length(ch112)]-ch112[length(ch112)-1])*100)/ch112[length(ch112)-1])>=(2)))            
        {
          print("down-up reversal")
          don[ch]=1
        }
      }
      
     
      
      
  }
  
  
  
  kojl<-(don) 
  kjl<-(dun) 
  print(kojl[(length(kojl)-150):length(kojl)])
  print(kjl[(length(kjl)-150):length(kjl)])
  
  kjl<- as.matrix(kjl)
  
  kojl<- as.matrix(kojl)
  
  kjl1=kjl+kojl
  
  print(kjl1[(length(kjl1)-150):length(kjl1)])
  
  for(i in 1:nrow(kjl1)){
    
    
    if (as.numeric(kjl1[i,1])<(1))
      
    {
      
      
      kjl1[i,1]=0
    } else{
      
      kjl1[i,1]=1
    }}
  
  kjl1<- as.matrix(kjl1)
  
  kjl1=c(kjl1)
  
  return(kjl1)
  
} 


symb <-   function() 
  {
    
    test= sqldf('SELECT distinct * FROM hest where symbol not in("NSE:XXX") order by Date desc ',drv='SQLite')
    
    allsym=as.data.frame(as.matrix(hest$symbol))
    
    
    colnames(allsym)=c("symbol")
    
    yoursym2=as.character(allsym[1,1])

    rest= sqldf('SELECT distinct * FROM test where symbol not in("NSE:XXX") order by Date desc ',drv='SQLite')
    
    
    
    test=as.data.frame(rest)
    mm1=1:(nrow(hest)-150)
    mm2=1:(nrow(hest)-150)
    m1=1:(nrow(hest)-150)
    m11=1:(nrow(hest)-150)
    m2=1:(nrow(hest)-150)
    m22=1:(nrow(hest)-150)
    zy1=1:(nrow(hest)-150)
    zy2=1:(nrow(hest)-150)
    zy11=1:(nrow(hest)-150)
    zy22=1:(nrow(hest)-150)
    mxb=1:(nrow(hest)-150)
    kr=1:(nrow(hest)-150)
    lr=1:(nrow(hest)-150)
    mr=1:(nrow(hest)-150)
    nr=1:(nrow(hest)-150)
    brk=1:(nrow(hest)-150)
    dly3=11
    zx111=1:(nrow(hest)-150)
    zy122=1:(nrow(hest)-150)
    zx=1:(nrow(hest)-150)
    zy=1:(nrow(hest)-150)
    dly2=-250
    sup=1:(nrow(hest)-150)
    res=1:(nrow(hest)-150)
    resls=1:(nrow(hest)-150)
    supls=1:(nrow(hest)-150)
    dun=1:(nrow(hest)-150)
    dun[1:(nrow(hest)-150)]=0
    don=1:(nrow(hest)-150)
    don[1:(nrow(hest)-150)]=0
    for(ch in 1:(nrow(hest)-150))
    {
      
      test <- rest[((nrow(hest)-149)-ch):nrow(rest),]
      
      test=sqldf('SELECT distinct * FROM test  order by Date asc ',drv='SQLite')
      
      test <- test[((nrow(test)-149)):nrow(test),]
      
      print(ch)
      Lines <-  sqldf('SELECT  Date as Date ,open as Open , high as High, low as Low, close as Close,  volume FROM test order by Date desc',drv='SQLite')
      
      
      Lines$Open=gsub("\\.", ",", Lines$Open)
      
      
      Lines$High=gsub("\\.", ",", Lines$High)
      
      
      Lines$Low=gsub("\\.", ",", Lines$Low)
      
      
      Lines$Close=gsub("\\.", ",", Lines$Close)
      
      
      df=sapply(Lines, function(x) gsub("\"", "", x))
      
      df1=as.data.frame(df,quote=FALSE)
      
      
      connectionString <- paste0(df1$Date,";",df1$Open,";",df1$High,";",df1$Low,";",df1$Close,";",df1$volume)
      
      names= paste0("Date;Open;High;Low;Close;Volume")
      
      
      names=as.data.frame(names)
      connectionString=as.data.frame(connectionString)
      
      colnames(connectionString)=c("names")
      
      dd=rbind(names,connectionString)
      
      
      connectionString <- lapply(dd, as.character)
      
      df=sapply(connectionString, function(x) gsub("\"", "", x))
      
      
      
      
      
      conn <- textConnection(df)
      stock=as.xts(read.zoo(conn, sep=';', tz="US/Eastern", dec=",", header=TRUE,
                            format='%Y-%m-%d', index.column=1))
      
      stock1= (stock)
      
      
      
      close(conn)
      
      
      
      
      stock2=as.matrix(stock1)
      stock2=stock1[,1:5]
      
      stock2=as.data.frame(stock2)
      
      colnames(stock2) <- c("Open", "High", "Low", "Close","Volume")
      
      stock3=as.matrix(stock2)
      
      stock3=stock3[,1:5]
      
      
      
      stock3=as.xts(stock3,tz="EST")
      .index(stock3)[1:nrow(stock3)] <-(.index(stock3)[1:nrow(stock3)])+10.5*3600
      
      mals=as.matrix(test$close)
      
      mals=as.numeric(mals[,1]) 
      
      hals=as.matrix(test$High)
      
      hals=as.numeric(hals[,1]) 
      
      lals=as.matrix(test$Low)
      
      lals=as.numeric(lals[,1]) 
      
      vcols=as.matrix(test$volume)
      
      vcols=as.numeric(vcols[,1]) 
      
      
      
      mals=mals[1:length(mals)]
      nx=-(1:length(mals))
      
      bals=mals[-nx]
      
      nx=-(length(mals):1)
      
      x <- nx
      
      nx=-(1:length(mals))
      y <- bals[-nx]
      v <- vcols[-nx]
      
      
      
      
      
      library(extRemes)
      library(xts)
      
      
      
      ams=stock3
      
      
      maxams <- apply.weekly(ams$High, max)
      
      maxams=as.matrix(as.data.frame(maxams))
      
      minams <- apply.weekly(ams$Low, min)
      
      minams=as.matrix(as.data.frame(minams))
      xt=1:nrow(maxams)
      i=1 
      j=1
      for(i in i:nrow(maxams))
      { 
        for(j in j:nrow(stock3))
        {
          if(maxams[i]==hals[j])
          {
            xt[i]=j
            
            break
          }
          else
          {
            
          }
          
        }
      }
      
      
      
      yt=1:nrow(minams)
      i=1 
      j=1
      for(i in i:nrow(minams))
      { 
        for(j in j:nrow(stock3))
        {
          if(minams[i]==lals[j])
          {
            yt[i]=j
            
            break
          }
          else
          {
            
          }
          
        }
      }
      
      
      
      pks11=xt
      pksl2=yt
      
      
      ha=2
      hrals=hals
      for(ha in 2:length(pks11))
      {
        if((((hals[pks11[ha]]-hals[pks11[ha-1]])*100)/(hals[pks11[ha-1]]))>0)
        {
          hrals[pks11[ha]]=1
        }else
        {
          hrals[pks11[ha]]=0
        }
      }
      
   
      Me=as.numeric(hrals[pks11])
      chan=cbind(pks11,Me)
      
      chan=as.matrix(chan)
      
      chan[1,2]=0
      
      kk=as.data.frame(chan)
      
      updn <- kk
      
      updn3=rbind(updn[nrow(updn),])
      
      updn3$Me=0
      
      updn=rbind(updn,updn3)
      
      ix1 <- which(updn$Me == 0)
      
      updn1 <- c(diff(ix1))
      
      ix <- which(updn1 != 1)
      BID=as.data.frame(as.matrix(c(ix1[ix])))
      ix0 <- which(updn1 == 1)
      MBID=as.data.frame(as.matrix(c(ix1[ix])))
      
      if(max(MBID)==max(BID))
      {
        
      } else      {
        BID=  rbind(BID,max(MBID))
      }
      
      
      kk=as.data.frame(chan)
      
      updn <- kk
      
      updn3=rbind(updn[nrow(updn),])
      
      updn3$Me=1
      
      updn=rbind(updn,updn3)
      
      ix1 <- which(updn$Me == 1)
      
      updn1 <- c(diff(ix1))
      
      ix <- which(updn1 != 1)
      SID=as.data.frame(as.matrix(c(ix1[ix])))
      ix0 <- which(updn1 == 1)
      MSID=as.data.frame(as.matrix(c(ix1[ix])))
      
      if(max(MSID)==max(SID))
      {
        
      } else      {
        SID=  rbind(SID,max(MSID))
      }
      
      
      
      bfpnts=chan[BID$V1+1,]
      bfpnts=as.data.frame(bfpnts)
      sfpnts=chan[SID$V1,]
      sfpnts=as.data.frame(sfpnts)
      cfpnts=sqldf('SELECT  a.* from bfpnts a join sfpnts b  on a.pks11=b.pks11  ',drv='SQLite')
      
      cfpnts= cfpnts$pks11
      
      cfpnts=as.data.frame(cfpnts)
      fpnts=rbind(bfpnts,sfpnts)
      fpnts= fpnts$pks11
      
      fpnts=as.data.frame(fpnts)
      pks11=sqldf('SELECT distinct fpnts from fpnts   order by fpnts asc',drv='SQLite')
      pks11=as.numeric(t(pks11))
      
      
      
      ha=2
      lrals=lals
      for(ha in 2:length(pksl2))
      {
        if((((lals[pksl2[ha]]-lals[pksl2[ha-1]])*100)/(lals[pksl2[ha-1]]))>0)
        {
          lrals[pksl2[ha]]=1
        }else
        {
          lrals[pksl2[ha]]=0
        }
      }
      
      Me=as.numeric(lrals[pksl2])
      chan=cbind(pksl2,Me)
      
      chan=as.matrix(chan)
      
      chan[1,2]=0
      
      kk=as.data.frame(chan)
      
      updn <- kk
      
      updn3=rbind(updn[nrow(updn),])
      
      updn3$Me=0
      
      updn=rbind(updn,updn3)
      
      ix1 <- which(updn$Me == 0)
      
      updn1 <- c(diff(ix1))
      
      ix <- which(updn1 != 1)
      BID=as.data.frame(as.matrix(c(ix1[ix])))
      ix0 <- which(updn1 == 1)
      MBID=as.data.frame(as.matrix(c(ix1[ix])))
      
      if(max(MBID)==max(BID))
      {
        
      } else      {
        BID=  rbind(BID,max(MBID))
      }
      
      
      kk=as.data.frame(chan)
      
      updn <- kk
      
      updn3=rbind(updn[nrow(updn),])
      
      updn3$Me=1
      
      updn=rbind(updn,updn3)
      
      ix1 <- which(updn$Me == 1)
      
      updn1 <- c(diff(ix1))
      
      ix <- which(updn1 != 1)
      SID=as.data.frame(as.matrix(c(ix1[ix])))
      ix0 <- which(updn1 == 1)
      MSID=as.data.frame(as.matrix(c(ix1[ix])))
      
      if(max(MSID)==max(SID))
      {
        
      } else      {
        SID=  rbind(SID,max(MSID))
      }
      
      
      
      bfpnts=chan[BID$V1+1,]
      bfpnts=as.data.frame(bfpnts)
      sfpnts=chan[SID$V1,]
      sfpnts=as.data.frame(sfpnts)
      cfpnts=sqldf('SELECT  a.* from bfpnts a join sfpnts b  on a.pksl2=b.pksl2  ',drv='SQLite')
      
      cfpnts= cfpnts$pksl2
      
      cfpnts=as.data.frame(cfpnts)
      fpnts=rbind(bfpnts,sfpnts)
      fpnts= fpnts$pksl2
      
      fpnts=as.data.frame(fpnts)
      pksl2=sqldf('SELECT distinct fpnts from fpnts  order by fpnts asc',drv='SQLite')
      pksl2=as.numeric(t(pksl2))
   
      
      dpks111= which(xt == max(pks11))
      dpksl22=which(yt == max(pksl2))
      
      dpks11=xt[((dpks111):length(xt))]
      dpksl2=yt[((dpksl22):length(yt))]
      
      
      if(length(dpks11)<2)
      {  dpks11=xt[((dpks111-1):length(xt))]
      
      
      }
      
      if(length(dpksl2)<2)
      { 
        dpksl2=yt[((dpksl22-1):length(yt))]
        
      }
      
      if(length(dpks11)>2)
      { md= min(dpks11)
      mx= max(dpks11)
      dpks11=c(md,mx)
      
      }
    
      
      if(length(dpksl2)>2)
      {
        md= min(dpksl2)
        mx= max(dpksl2)
        dpksl2=c(md,mx)
        
      }
      
      xx1=(length(hals)-(dpks11[length(dpks11)]))-(length(hals)-(dpks11[length(dpks11)-1])) 
      yy1= hals[dpks11[length(dpks11)]]- hals[dpks11[length(dpks11)-1]]
      m1=yy1/xx1
      
      xx2=(length(lals)-(dpksl2[length(dpksl2)]))-(length(lals)-(dpksl2[length(dpksl2)-1])) 
      yy2= lals[dpksl2[length(dpksl2)]]- lals[dpksl2[length(dpksl2)-1]]
      m2=yy2/xx2
      
     
   
      ch11=c(hals[dpks11],  hals[length(hals)])
      ch12=c(lals[dpksl2],  lals[length(lals)])
     
 
      
      if((((ch11[length(ch11)-1]))==ch11[length(ch11)]))
      {
        ch11[length(ch11)-1]=ch11[length(ch11)-2]
      }else
      {
       
        
      }
      
      if((((ch12[length(ch12)-1]))==ch12[length(ch12)]))
      {
        ch12[length(ch12)-1]=ch12[length(ch12)-2]
      }else
      {
        
        
      }
      
      
      print(ch11)
      print(ch12)
      
      if((((ch11[length(ch11)-1]))<ch11[length(ch11)])&&(((ch12[length(ch12)-1]))<ch12[length(ch12)]))
      {
        
        if((((ch11[length(ch11)])-ch11[length(ch11)-1])*100/(ch11[length(ch11)]))>0)
        {
          dun[ch]=1
          # print("squeeze")
        }
        
        #print("down-up reversal")
      } else if((((ch11[length(ch11)-1]))>ch11[length(ch11)])&&(((ch12[length(ch12)-1]))>ch12[length(ch12)]))
        
      {#print("up-down reversal")
        dun[ch]=0
      } else if((((ch11[length(ch11)-1]))<ch11[length(ch11)])&&(((ch12[length(ch12)-1]))>ch12[length(ch12)]))
        
      { 
       # print("break out")
      } else if((((ch11[length(ch11)-1]))>ch11[length(ch11)])&&(((ch12[length(ch12)-1]))<ch12[length(ch12)]))
        
        
      { if((((ch11[length(ch11)-1])-ch11[length(ch11)])*100/(ch11[length(ch11)-1]))>1)
      {
      #  dun[ch]=0
       # print("squeeze")
      }
      }
      
      mals=as.matrix(test$close)
      
      mals=as.numeric(mals[,1]) 
      
      
      
      vcols=as.matrix(test$volume)
      
      vcols=as.numeric(vcols[,1]) 
      
      
      
      mals=mals[1:length(mals)]
      nx=-(1:length(mals))
      
      bals=mals[-nx]
      
      nx=-(length(mals):1)
      
      x <- nx
      
      nx=-(1:length(mals))
      y <- bals[-nx]
      v <- vcols[-nx]
      
    
      ams=stock3
      maxams <- apply.monthly(ams$Close, max)
      
      maxams=as.matrix(as.data.frame(maxams))
      
      minams <- apply.monthly(ams$Close, min)
      
      xt=1:nrow(maxams)
      i=1 
      j=1
      for(i in i:nrow(maxams))
      { 
        for(j in j:nrow(stock3))
        {
          if(maxams[i]==mals[j])
          {
            xt[i]=j
            
            break
          }
          else
          {
            
          }
          
        }
      }
      
      
      
      yt=1:nrow(minams)
      i=1 
      j=1
      for(i in i:nrow(minams))
      { 
        for(j in j:nrow(stock3))
        {
          if(minams[i]==mals[j])
          {
            yt[i]=j
            
            break
          }
          else
          {
            
          }
          
        }
      }
      
      
      
      pks11=xt
      pksl2=yt
      
      
      dpks111= which(xt == max(pks11))
      dpksl22=which(yt == max(pksl2))
      
      dpks11=xt[((dpks111):length(xt))]
      dpksl2=yt[((dpksl22):length(yt))]
      
      
      if(length(dpks11)<2)
      {  dpks11=xt[((dpks111-1):length(xt))]
      
      
      }
      
      if(length(dpksl2)<2)
      { 
        dpksl2=yt[((dpksl22-1):length(yt))]
        
      }
      
      if(length(dpks11)>2)
      { md= min(dpks11)
      mx= max(dpks11)
      dpks11=c(mx,md)
      
      }
      
      
      if(length(dpksl2)>2)
      {
        md= min(dpksl2)
        mx= max(dpksl2)
        dpksl2=c(mx,md)
        
      }
      
      
      
      ch11=c(hals[dpks11],  hals[length(hals)])
      ch12=c(lals[dpksl2],  lals[length(lals)])
      
      
      
      if((((ch11[length(ch11)-1]))==ch11[length(ch11)]))
      {
        ch11[length(ch11)-1]=ch11[length(ch11)-2]
      }else
      {
        
        
      }
      
      if((((ch12[length(ch12)-1]))==ch12[length(ch12)]))
      {
        ch12[length(ch12)-1]=ch12[length(ch12)-2]
      }else
      {
        
        
      }
      
      
      print(ch11)
      print(ch12)
      
      if((((ch11[length(ch11)-1]))<ch11[length(ch11)])&&(((ch12[length(ch12)-1]))<ch12[length(ch12)]))
      {
        
        if((((ch11[length(ch11)])-ch11[length(ch11)-1])*100/(ch11[length(ch11)]))>0)
        {
          don[ch]=1
          # print("squeeze")
        }
        
        #print("down-up reversal")
      } else if((((ch11[length(ch11)-1]))>ch11[length(ch11)])&&(((ch12[length(ch12)-1]))>ch12[length(ch12)]))
        
      {#print("up-down reversal")
        don[ch]=0
      } else if((((ch11[length(ch11)-1]))<ch11[length(ch11)])&&(((ch12[length(ch12)-1]))>ch12[length(ch12)]))
        
      { 
      # print("break out")
      } else if((((ch11[length(ch11)-1]))>ch11[length(ch11)])&&(((ch12[length(ch12)-1]))<ch12[length(ch12)]))
        
        
      { if((((ch11[length(ch11)-1])-ch11[length(ch11)])*100/(ch11[length(ch11)-1]))>1)
      {
        #don[ch]=0
      }
      } 
      
      
      
      
    }
    

    

    for(o in  1:200)  
    {
     dun1=dun
    for(ka in 2:(length(dun)))
    {
      if((dun[[ka]]==dun[[ka-1]])||(dun[[ka]]==2))
      {
        dun1[ka]=dun[ka-1]
      }else
      {
        dun1[ka]=dun[ka]
      }
    }
    

    dun=dun1
    }
    
    for(ka in  1:200)  
    {
      don1=don
      for(ka in 2:(length(don)))
      {
        if((don[[ka]]==don[[ka-1]])||(don[[ka]]==2))
        {
          don1[ka]=dun[ka-1]
        }else
        {
          don1[ka]=dun[ka]
        }
      }
      
      
      don=don1
    }
    kojl<-(don) 
    kjl<-(dun) 
    print(kojl[(length(kojl)-150):length(kojl)])
    print(kjl[(length(kjl)-150):length(kjl)])
    
    kjl<- as.matrix(kjl)
    
    kojl<- as.matrix(kojl)
    
    kjl1=kjl+kojl
    
    for(i in 1:nrow(kjl1)){
      
      
      if (as.numeric(kjl1[i,1])<(2))
        
      {
        
        
        kjl1[i,1]=0
      } else{
        
        kjl1[i,1]=1
      }}
    
    kjl1<- as.matrix(kjl1)
    
    kjl1=c(kjl1)
    
    kjl1=c(kjl1)
    return(kjl1)
    
  } 
  


symb1 <-   function() 
{
  
  test= sqldf('SELECT distinct * FROM mest where symbol not in("NSE:XXX") order by Date desc ',drv='SQLite')
  
  allsym=as.data.frame(as.matrix(mest$symbol))
  
  
  colnames(allsym)=c("symbol")
  
  yoursym2=as.character(allsym[1,1])
  
  rest= sqldf('SELECT distinct * FROM test where symbol not in("NSE:XXX") order by Date desc ',drv='SQLite')
  
  
  
  test=as.data.frame(rest)
  mm1=1:(nrow(mest)-150)
  mm2=1:(nrow(mest)-150)
  m1=1:(nrow(mest)-150)
  m11=1:(nrow(mest)-150)
  m2=1:(nrow(mest)-150)
  m22=1:(nrow(mest)-150)
  zy1=1:(nrow(mest)-150)
  zy2=1:(nrow(mest)-150)
  zy11=1:(nrow(mest)-150)
  zy22=1:(nrow(mest)-150)
  mxb=1:(nrow(mest)-150)
  kr=1:(nrow(mest)-150)
  lr=1:(nrow(mest)-150)
  mr=1:(nrow(mest)-150)
  nr=1:(nrow(mest)-150)
  brk=1:(nrow(mest)-150)
  dly3=11
  zx111=1:(nrow(mest)-150)
  zy122=1:(nrow(mest)-150)
  zx=1:(nrow(mest)-150)
  zy=1:(nrow(mest)-150)
  dly2=-250
  sup=1:(nrow(mest)-150)
  res=1:(nrow(mest)-150)
  resls=1:(nrow(mest)-150)
  supls=1:(nrow(mest)-150)
  dun=1:(nrow(mest)-150)
  dun[1:(nrow(mest)-150)]=2
  don=1:(nrow(mest)-150)
  don[1:(nrow(mest)-150)]=2
  for(ch in 1:(nrow(mest)-150))
  {
    
    test <- rest[((nrow(mest)-149)-ch):nrow(mest),]
    
    test=sqldf('SELECT distinct * FROM test  order by Date asc ',drv='SQLite')
    
    test <- test[((nrow(test)-149)):nrow(test),]
    
    print(ch)
    Lines <-  sqldf('SELECT  Date as Date ,open as Open , high as High, low as Low, close as Close,  volume FROM test order by Date desc',drv='SQLite')
    
    
    Lines$Open=gsub("\\.", ",", Lines$Open)
    
    
    Lines$High=gsub("\\.", ",", Lines$High)
    
    
    Lines$Low=gsub("\\.", ",", Lines$Low)
    
    
    Lines$Close=gsub("\\.", ",", Lines$Close)
    
    
    df=sapply(Lines, function(x) gsub("\"", "", x))
    
    df1=as.data.frame(df,quote=FALSE)
    
    
    connectionString <- paste0(df1$Date,";",df1$Open,";",df1$High,";",df1$Low,";",df1$Close,";",df1$volume)
    
    names= paste0("Date;Open;High;Low;Close;Volume")
    
    
    names=as.data.frame(names)
    connectionString=as.data.frame(connectionString)
    
    colnames(connectionString)=c("names")
    
    dd=rbind(names,connectionString)
    
    
    connectionString <- lapply(dd, as.character)
    
    df=sapply(connectionString, function(x) gsub("\"", "", x))
    
    
    
    
    
    conn <- textConnection(df)
    stock=as.xts(read.zoo(conn, sep=';', tz="US/Eastern", dec=",", header=TRUE,
                          format='%Y-%m-%d %H:%M:%S', index.column=1))
    
    stock1= to.minutes(stock)
    
    
    
    close(conn)
    
    
    stock2=as.matrix(stock1)
    stock2=stock1[,1:4]
    
    stock2=as.data.frame(stock2)
    
    colnames(stock2) <- c("Open", "High", "Low", "Close")
    
    stock3=as.matrix(stock2)
    
    stock3=stock3[,1:4]
    
    
    
    stock3=as.xts(stock3,tz="EST")
    
    
    
    .index(stock3)[1:nrow(stock3)] <-(.index(stock3)[1:nrow(stock3)])+5*3600
    
    
    mals=as.matrix(test$close)
    mals=as.matrix(test$close)
    
    mals=as.numeric(mals[,1]) 
    
    hals=as.matrix(test$High)
    
    hals=as.numeric(hals[,1]) 
    
    lals=as.matrix(test$Low)
    
    lals=as.numeric(lals[,1]) 
    
    vcols=as.matrix(test$volume)
    
    vcols=as.numeric(vcols[,1]) 
    
    
    
    mals=mals[1:length(mals)]
    nx=-(1:length(mals))
    
    bals=mals[-nx]
    
    nx=-(length(mals):1)
    
    x <- nx
    
    nx=-(1:length(mals))
    y <- bals[-nx]
    v <- vcols[-nx]
    
    
    
    
    
    library(extRemes)
    library(xts)
    
    ams=stock3
    
    
    maxams= period.apply(ams$High, endpoints(ams$High, "hours"), max)
    maxams=as.matrix(as.data.frame(maxams))
    
    minams <- period.apply(ams$Low, endpoints(ams$Low, "hours"), min)
    minams=as.matrix(as.data.frame(minams))
    xt=1:nrow(maxams)
    i=1 
    j=1
    for(i in i:nrow(maxams))
    { 
      for(j in j:nrow(stock3))
      {
        if(maxams[i]==hals[j])
        {
          xt[i]=j
          
          break
        }
        else
        {
          
        }
        
      }
    }
    
    
    
    yt=1:nrow(minams)
    i=1 
    j=1
    for(i in i:nrow(minams))
    { 
      for(j in j:nrow(stock3))
      {
        if(minams[i]==lals[j])
        {
          yt[i]=j
          
          break
        }
        else
        {
          
        }
        
      }
    }
    
    
    
    pks11=xt
    pksl2=yt
    
    
    
    
    dpks111= which(xt == max(pks11))
    dpksl22=which(yt == max(pksl2))
    
    dpks11=xt[((dpks111):length(xt))]
    dpksl2=yt[((dpksl22):length(yt))]
    
    
    if(length(dpks11)<2)
    {  dpks11=xt[((dpks111-1):length(xt))]
    
    
    }
    
    if(length(dpksl2)<2)
    { 
      dpksl2=yt[((dpksl22-1):length(yt))]
      
    }
    
    if(length(dpks11)>2)
    { md= min(dpks11)
    mx= max(dpks11)
    dpks11=c(mx,md)
    
    }
    
    
    if(length(dpksl2)>2)
    {
      md= min(dpksl2)
      mx= max(dpksl2)
      dpksl2=c(mx,md)
      
    }
    
    xx1=(length(hals)-(dpks11[length(dpks11)]))-(length(hals)-(dpks11[length(dpks11)-1])) 
    yy1= hals[dpks11[length(dpks11)]]- hals[dpks11[length(dpks11)-1]]
    m1=yy1/xx1
    
    xx2=(length(lals)-(dpksl2[length(dpksl2)]))-(length(lals)-(dpksl2[length(dpksl2)-1])) 
    yy2= lals[dpksl2[length(dpksl2)]]- lals[dpksl2[length(dpksl2)-1]]
    m2=yy2/xx2
    
    
    
    ch11=c(hals[dpks11],  hals[length(hals)])
    ch12=c(lals[dpksl2],  lals[length(lals)])
    
    
    
    if((((ch11[length(ch11)-1]))==ch11[length(ch11)]))
    {
      ch11[length(ch11)-1]=ch11[length(ch11)-2]
    }else
    {
      
      
    }
    
    if((((ch12[length(ch12)-1]))==ch12[length(ch12)]))
    {
      ch12[length(ch12)-1]=ch12[length(ch12)-2]
    }else
    {
      
      
    }
    
    
    print(ch11)
    print(ch12)
    
    if((((ch11[length(ch11)-1]))<ch11[length(ch11)])&&(((ch12[length(ch12)-1]))<ch12[length(ch12)]))
    {
      
      if((((ch11[length(ch11)])-ch11[length(ch11)-1])*100/(ch11[length(ch11)]))>0)
      {
        dun[ch]=1
        # print("squeeze")
      }
      
      #print("down-up reversal")
    } else if((((ch11[length(ch11)-1]))>ch11[length(ch11)])&&(((ch12[length(ch12)-1]))>ch12[length(ch12)]))
      
    {#print("up-down reversal")
      dun[ch]=0
    } else if((((ch11[length(ch11)-1]))<ch11[length(ch11)])&&(((ch12[length(ch12)-1]))>ch12[length(ch12)]))
      
    { 
      # print("break out")
    } else if((((ch11[length(ch11)-1]))>ch11[length(ch11)])&&(((ch12[length(ch12)-1]))<ch12[length(ch12)]))
      
      
    { if((((ch11[length(ch11)-1])-ch11[length(ch11)])*100/(ch11[length(ch11)-1]))>1)
    {
      # dun[ch]=0
      # print("squeeze")
    }
    }
    
    
    
    
    
  }
  
  
  
  
  for(o in  1:20)  
  {
    dun1=dun
    for(ka in 2:(length(dun)))
    {
      if((dun[[ka]]==dun[[ka-1]])||(dun[[ka]]==2))
      {
        dun1[ka]=dun[ka-1]
      }else
      {
        dun1[ka]=dun[ka]
      }
    }
    
    
    dun=dun1
  }
  

  kjl<-(dun) 
  
  
  kjl<- as.matrix(kjl)
  
  kjl=c(kjl)
  

  return(kjl)
  
} 




zigz1 <-   function() 
{
  
  
  test= sqldf('SELECT distinct * FROM mest where symbol not in("NSE:XXX") order by Date desc ',drv='SQLite')
  
  allsym=as.data.frame(as.matrix(mest$symbol))
  
  
  colnames(allsym)=c("symbol")
  
  yoursym2=as.character(allsym[1,1])
  
  rest= sqldf('SELECT distinct * FROM test where symbol not in("NSE:XXX") order by Date desc ',drv='SQLite')
  
  
  
  test=as.data.frame(rest)
  mm1=1:(nrow(mest)-150)
  mm2=1:(nrow(mest)-150)
  m1=1:(nrow(mest)-150)
  m11=1:(nrow(mest)-150)
  m2=1:(nrow(mest)-150)
  m22=1:(nrow(mest)-150)
  zy1=1:(nrow(mest)-150)
  zy2=1:(nrow(mest)-150)
  zy11=1:(nrow(mest)-150)
  zy22=1:(nrow(mest)-150)
  mxb=1:(nrow(mest)-150)
  kr=1:(nrow(mest)-150)
  lr=1:(nrow(mest)-150)
  mr=1:(nrow(mest)-150)
  nr=1:(nrow(mest)-150)
  brk=1:(nrow(mest)-150)
  dly3=11
  zx111=1:(nrow(mest)-150)
  zy122=1:(nrow(mest)-150)
  zx=1:(nrow(mest)-150)
  zy=1:(nrow(mest)-150)
  dly2=-250
  sup=1:(nrow(mest)-150)
  res=1:(nrow(mest)-150)
  resls=1:(nrow(mest)-150)
  supls=1:(nrow(mest)-150)
  dun=1:(nrow(mest)-150)
  dun[1:(nrow(mest)-150)]=0
  don=1:(nrow(mest)-150)
  don[1:(nrow(mest)-150)]=0
  for(ch in 1:(nrow(mest)-150))
  {
    
    test <- rest[((nrow(mest)-149)-ch):nrow(mest),]
    
    test=sqldf('SELECT distinct * FROM test  order by Date asc ',drv='SQLite')
    
    test <- test[((nrow(test)-149)):nrow(test),]
    
    vest<-    test
    zz <- ZigZag( vest[,c("Low", "close")] ,change=1  ) 
    print(zz)
    ln=length(zz[is.na(zz)]) 
    
    #  zz[length(zz)-(ln+1)]=zz[length(zz)-(ln)]
    print(zz[length(zz)-(ln)])
    print(zz[length(zz)-(ln+1)])
    
    if((((zz[length(zz)-(ln)]-zz[length(zz)-(ln+1)]))*100)/(zz[length(zz)-(ln+1)])>=0)
    {
      dun[ch]=1
    }
    
  }
  
  
  
  kojl<-(dun) 
  
  print(kojl[(length(kojl)-150):length(kojl)])
  
  
  kjl1=c(kojl)
  
  return(kjl1)
  
} 

zigz <-   function(test) 
{ 
  hest=test
  test= sqldf('SELECT distinct * FROM hest where symbol not in("NSE:XXX") order by Date desc ',drv='SQLite')
  
  allsym=as.data.frame(as.matrix(hest$symbol))
  
  
  colnames(allsym)=c("symbol")
  
  yoursym2=as.character(allsym[1,1])
  
  rest= sqldf('SELECT distinct * FROM test where symbol not in("NSE:XXX") order by Date desc ',drv='SQLite')
  
  
  
  test=as.data.frame(rest)
  mm1=1:(nrow(hest)-150)
  mm2=1:(nrow(hest)-150)
  m1=1:(nrow(hest)-150)
  m11=1:(nrow(hest)-150)
  m2=1:(nrow(hest)-150)
  m22=1:(nrow(hest)-150)
  zy1=1:(nrow(hest)-150)
  zy2=1:(nrow(hest)-150)
  zy11=1:(nrow(hest)-150)
  zy22=1:(nrow(hest)-150)
  mxb=1:(nrow(hest)-150)
  kr=1:(nrow(hest)-150)
  lr=1:(nrow(hest)-150)
  mr=1:(nrow(hest)-150)
  nr=1:(nrow(hest)-150)
  brk=1:(nrow(hest)-150)
  dly3=11
  zx111=1:(nrow(hest)-150)
  zy122=1:(nrow(hest)-150)
  zx=1:(nrow(hest)-150)
  zy=1:(nrow(hest)-150)
  dly2=-250
  sup=1:(nrow(hest)-150)
  res=1:(nrow(hest)-150)
  resls=1:(nrow(hest)-150)
  supls=1:(nrow(hest)-150)
  dun=1:(nrow(hest)-150)
  dun[1:(nrow(hest)-150)]=0
  don=1:(nrow(hest)-150)
  don[1:(nrow(hest)-150)]=0
  withProgress(message = 'loading...', value = 0, 
  { 
  for(ch in 1:(nrow(hest)-150))
  {  
    
    incProgress(1/(nrow(hest)-150), detail = paste("Doing part", (nrow(hest)-150)-ch))
    {  
    print(ch)
    test <- rest[((nrow(hest)-149)-ch):nrow(hest),]
    
    test=sqldf('SELECT distinct * FROM test  order by Date asc ',drv='SQLite')
    
    test <- test[((nrow(test)-149)):nrow(test),]
    
   vest<-    test
   zz <- ZigZag( vest[,c("High", "High")] ,change=1  ) 
   print(zz)
    ln=length(zz[is.na(zz)]) 
    
  #  zz[length(zz)-(ln+1)]=zz[length(zz)-(ln)]
    print(zz[length(zz)-(ln)])
    print(zz[length(zz)-(ln+1)])
    
    if((((zz[length(zz)-(ln)]-zz[length(zz)-(ln+1)]))*100)/(zz[length(zz)-(ln+1)])>=0)
    {
      dun[ch]=1
    }
    
     }
  }})
  
  
  
  kojl<-(dun) 
 
  print(kojl[(length(kojl)-150):length(kojl)])

  
    kjl1=c(kojl)
  
  return(kjl1)
  
} 


smacheck <-   function() 
{
  
  best<-sqldf('SELECT distinct * FROM hest where  symbol not in("NSE:XXX") and close>0  order by Date asc ',drv='SQLite')
  
  
  uals=as.matrix(best$close)
  
  uals=as.numeric(uals[,1]) 
  
  
  nx=-(1:(nrow(best)))
  
  
  bals=uals[-nx]
  
  
  gelsma50=SMA(bals,50)
  gelsma50=as.data.frame(gelsma50)
  
  
  GELSMA50=as.numeric(as.matrix(gelsma50$gelsma50))
  
  
  
  GELSMA50[is.na(GELSMA50)]=bals[1:sum(is.na(GELSMA50))]
  
  GELSMA50[is.na(GELSMA50)]=bals[sum(is.na(GELSMA50))+1]
  
  
  nx=-(1:length(GELSMA50))
  
  
  GELSMA50=GELSMA50[-nx]
  
  
  ha=1
  
  hals=GELSMA50
  chals=hals
  for(ha in 1:length(GELSMA50))
  {
    if((((hals[[ha]]-bals[[ha]])*100)/(hals[[ha]]))<0)
    {
      chals[[ha]]=1
    }else
    {
      chals[[ha]]=0
    }
  }
  
  
  
  kojl<-(chals) 
  
  
  print(kojl[(length(kojl)-150):length(kojl)])
  
  
  kjl1=c(kojl)
  
  return(kjl1)
  
  
} 


findValleys <-   function(x, thresh=0) {
  pks <- which(diff(sign(diff(x, na.pad=FALSE)),na.pad=FALSE) > 0) + 2
  if( !missing(thresh) ) {
    if(sign(thresh) > 0)
      thresh <- -thresh
    pks[x[pks-1]-coredata(x[pks]) < thresh]
  } else pks
}




findValleys1 <-
  function(x, thresh=0) {
    pks <- which((diff(x, na.pad=FALSE)) >0) +1
    if( !missing(thresh) ) {
      if(sign(thresh) < 0)
        thresh <- -thresh
      pks[x[pks-1]-coredata(x[pks]) > thresh]
    } else pks
  }

pattern.db <- function() 
{
  # page 12
  patterns = list()
  
  #*****************************************************************
  # You can reference E1,E2,...,En and t1,t2,...,tn in pattern formula
  #****************************************************************** 	
  # Head-and-shoulders (HS)
  #****************************************************************** 	
  pattern = list()
  pattern$len = 5
  pattern$start = 'max'
  pattern$formula = expression(
    # E3 > E1, E3 > E5
    E3 > E1 &
      E3 > E5 &
      
      # E1 and E5 are within 1.5 percent of their average
      abs(E1 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
      abs(E5 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
      
      # E2 and E4 are within 1.5 percent of their average
      abs(E2 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2 &
      abs(E4 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2
  )
  pattern$plot = expression({
    lines(c(d1,d2,d3,d4,d5), c(E1,E2,E3,E4,E5), lwd=10, col=col)
    text(d3, E3, 'HS', adj=c(0.5,-0.5), xpd=TRUE)		
  })								
  patterns$HS = pattern		
  
  #*****************************************************************
  # Inverted Head-and-shoulders (IHS)
  #****************************************************************** 	
  pattern = list()
  pattern$len = 5
  pattern$start = 'min'
  pattern$formula = expression(
    # E3 < E1, E3 < E5
    E3 < E1 &
      E3 < E5 &
      
      # E1 and E5 are within 1.5 percent of their average
      abs(E1 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
      abs(E5 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
      
      # E2 and E4 are within 1.5 percent of their average
      abs(E2 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2 &
      abs(E4 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2
  )
  pattern$plot = expression({
    lines(c(d1,d2,d3,d4,d5), c(E1,E2,E3,E4,E5), lwd=10, col=col)
    text(d3, E3, 'IHS', adj=c(0.5,1), xpd=TRUE)		
  })						
  patterns$IHS = pattern		
  
  #*****************************************************************
  # Broadening tops (BTOP)
  #****************************************************************** 		
  pattern = list()
  pattern$len = 5
  pattern$start = 'max'
  pattern$formula = expression(
    # E1 < E3 < E5
    E1 < E3 &
      E3 < E5 &		
      E2 > E4
  )
  pattern$plot = expression({
    beta = lm(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients
    lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=10, col=col)	
    lines(c(d2,d4), c(E2,E4), lwd=10, col=col)
    text(d3, min(E2,E4), 'BTOP', adj=c(0.5,1), xpd=TRUE)
  })				
  patterns$BTOP = pattern		
  
  #*****************************************************************
  # Broadening bottoms (BBOT)
  #****************************************************************** 		
  pattern = list()
  pattern$len = 5
  pattern$start = 'min'
  pattern$formula = expression(
    # E1 > E3 > E5
    E1 > E3 &
      E3 > E5 &		
      E2 < E4
  )		
  pattern$plot = expression({
    beta = lm(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients
    lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=10, col=col)	
    lines(c(d2,d4), c(E2,E4), lwd=10, col=col)
    text(d3, max(E2,E4), 'BBOT', adj=c(0.5,0), xpd=TRUE)
  })		
  patterns$BBOT = pattern		
  
  #*****************************************************************
  # Triangle tops (TTOP)
  #****************************************************************** 		
  pattern = list()
  pattern$len = 5
  pattern$start = 'max'
  pattern$formula = expression(
    # E1 > E3 > E5
    E1 > E3 &
      E3 > E5 &		
      E2 < E4
  )
  pattern$plot = expression({
    beta = lm(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients
    lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=10, col=col)
    lines(c(d2,d4), c(E2,E4), lwd=10, col=col)
    text(d3, min(E2,E4), 'TTOP', adj=c(0.5,1), xpd=TRUE)
  })						
  patterns$TTOP = pattern		
  
  #*****************************************************************
  # Triangle bottoms (TBOT)
  #****************************************************************** 		
  pattern = list()
  pattern$len = 5
  pattern$start = 'min'
  pattern$formula = expression(
    # E1 < E3 < E5
    E1 < E3 &
      E3 < E5 &		
      E2 > E4
  )
  pattern$plot = expression({
    beta = lm(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients		
    lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=10, col=col)
    lines(c(d2,d4), c(E2,E4), lwd=10, col=col)
    text(d3, max(E2,E4), 'TBOT', adj=c(0.5,0), xpd=TRUE)
  })				
  patterns$TBOT = pattern		
  
  #*****************************************************************
  # Rectangle tops (RTOP)
  #****************************************************************** 		
  pattern = list()
  pattern$len = 5
  pattern$start = 'max'
  pattern$formula = expression({
    avg.top = (E1+E3+E5)/3
    avg.bop = (E2+E4)/2
    
    # tops E1,E3,E5 are within 0.75 percent of their average
    abs(E1 - avg.top) < 0.75/100 * avg.top &
      abs(E3 - avg.top) < 0.75/100 * avg.top &
      abs(E5 - avg.top) < 0.75/100 * avg.top &
      
      # bottoms E2,E4 are within 0.75 percent of their average
      abs(E2 - avg.bop) < 0.75/100 * avg.bop &
      abs(E4 - avg.bop) < 0.75/100 * avg.bop &
      
      # lowest top > highest bottom
      min(E1,E3,E5) > max(E2,E4)
  })
  pattern$plot = expression({
    avg.top = (E1+E3+E5)/3
    avg.bop = (E2+E4)/2
    
    lines(c(d1,d3,d5), rep(avg.top,3), lwd=10, col=col)
    lines(c(d2,d4), rep(avg.bop,2), lwd=10, col=col)
    text(d3, min(E2,E4), 'RTOP', adj=c(0.5,-0.5), xpd=TRUE)
  })						
  patterns$RTOP = pattern		
  
  #*****************************************************************
  # Rectangle bottoms (RBOT)
  #****************************************************************** 		
  pattern = list()
  pattern$len = 5
  pattern$start = 'min'
  pattern$formula = expression({
    avg.top = (E2+E4)/2
    avg.bop = (E1+E3+E5)/3
    
    # tops E2,E4 are within 0.75 percent of their average
    abs(E2 - avg.top) < 0.75/100 * avg.top &
      abs(E4 - avg.top) < 0.75/100 * avg.top &
      
      # bottoms E1,E3,E5 are within 0.75 percent of their average		
      abs(E1 - avg.bop) < 0.75/100 * avg.bop &
      abs(E3 - avg.bop) < 0.75/100 * avg.bop &
      abs(E5 - avg.bop) < 0.75/100 * avg.bop &
      
      # lowest top > highest bottom
      min(E2,E4) > max(E1,E3,E5)
  })
  pattern$plot = expression({
    avg.top = (E2+E4)/2
    avg.bop = (E1+E3+E5)/3
    
    lines(c(d1,d3,d5), rep(avg.bop,3), lwd=10, col=col)
    lines(c(d2,d4), rep(avg.top,2), lwd=10, col=col)
    text(d3, max(E2,E4), 'RBOT', adj=c(0.5,0), xpd=TRUE)
  })						
  patterns$RBOT = pattern		
  
  #*****************************************************************
  # Analyzing Chart Patterns: Double Top And Double Bottom
  # http://www.investopedia.com/university/charts/charts4.asp
  #*****************************************************************
  # Double tops (DTOP), note in E and t first one is excluded
  #****************************************************************** 		
  pattern = list()
  pattern$len = 3
  pattern$start = 'max'
  pattern$formula = expression({
    # Ea = max(E), ta = t[which.max(E)]
    second.top = max(E)
    second.top.t = t[which.max(E)]
    avg = (E1 + second.top)/2
    
    # E1 and Ea are within 1.5 percent of their average
    abs(E1         - avg) < 1.5/100 * avg &
      abs(second.top - avg) < 1.5/100 * avg &
      
      # ta - t1 > 22
      second.top.t - t1 > 22
  })
  pattern$plot = expression({
    second.top = max(E)
    second.top.d = d[which.max(E)]
    avg = (E1 + second.top)/2
    
    points(c(d1, second.top.d), c(E1, second.top), pch=2, lwd=2) 
    lines(c(d1, second.top.d), rep(avg, 2), lwd=10, col=col)
    text(d2, avg, 'DTOP', adj=c(0.5,-0.5), xpd=TRUE)
  })
  pattern$last.point = expression(t[which.max(E)])
  patterns$DTOP = pattern		
  
  #*****************************************************************
  # Double bottoms (DBOT)
  #****************************************************************** 		
  pattern = list()
  pattern$len = 3
  pattern$start = 'min'
  pattern$formula = expression(
    # E1 and Ea = min(E) are within 1.5 percent of their average
    abs(E1 -         (E1+min(E))/2) < 1.5/100 * (E1+min(E))/2 &
      abs(max(E[-1]) - (E1+min(E))/2) < 1.5/100 * (E1+min(E))/2 &
      
      # ta - t1 > 22, ta = t[which.min(E)]
      t[which.min(E)] - t1 > 22
  )
  pattern$plot = expression({
    second.bot = min(E)
    second.bot.d = d[which.min(E)]
    avg = (E1 + second.bot)/2
    
    points(c(d1, second.bot.d), c(E1, second.bot), pch=2, lwd=2) 
    lines(c(d1, second.bot.d), rep(avg, 2), lwd=10, col=col)
    text(d2, avg, 'DBOT', adj=c(0.5,1), xpd=TRUE)
  })	
  pattern$last.point = expression(t[which.min(E)])
  patterns$DBOT = pattern		
  
  
  # add name and convert start to +1/-1
  for(i in 1:length(patterns)) {
    patterns[[i]]$name = names(patterns)[i]
    patterns[[i]]$start = ifelse(patterns[[i]]$start == 'max', 1, -1)	
  }
  
  return(patterns)	
}



findpw <- function(mals) 
{
  history = as.vector(((mals)))
  window.len = 60
  patterns = pattern.db()
  
  
  found.patterns = c()
  for(t in length(mals) : (length(history)-1)) {
    sample = history[(t - window.len + 1):t]		
    obj = find.extrema( sample )	
    
    if(length(obj$data.extrema.loc) > 0) {
      out =  find.patterns(obj, patterns = patterns, silent=F, plot=F)  
      
      if(length(out)>0) found.patterns = rbind(found.patterns,cbind(t,out,t-window.len+out))			
    }
    if( t %% 10 == 0) cat(t, 'out of', length(history), '\n')
  }
  
  if(is.null(found.patterns)=="TRUE")
  {
    found.patterns=1
    
  }else{
    colnames(found.patterns) = c('t','start','end','tstart','tend')
  }
  return(found.patterns)
}




find.patterns <- function
(
  obj, 	# extrema points
  patterns = pattern.db(), 
  silent=T, 
  plot=T
) 
{
  data = obj$data
  mhat = obj$mhat
  extrema.dir = obj$extrema.dir
  data.extrema.loc = obj$data.extrema.loc
  n.index = length(data.extrema.loc)
  
  if(is.xts(mhat)) {
    dates = as.Date(index(obj$mhat))
  } else {
    dates = 1:length(data)
  }		
  
  # Semi-transparent orange color
  col = col.add.alpha('orange', alpha=150)
  
  
  out = out.rownames = c()
  
  # search for patterns
  for(i in 1:n.index) {
    
    for(pattern in patterns) {
      
      # check same sign
      if( pattern$start * extrema.dir[i] > 0 ) {
        
        # check that there is suffcient number of extrema to complete pattern
        if( i + pattern$len - 1 <= n.index ) {
          
          # create environment to check pattern: E1,E2,...,En; t1,t2,...,tn
          envir.data = c(data[data.extrema.loc][i:(i + pattern$len - 1)], 
                         data.extrema.loc[i:(i + pattern$len - 1)])									
          names(envir.data) = c(paste('E', 1:pattern$len, sep=''), 
                                paste('t', 1:pattern$len, sep=''))
          envir.data = as.list(envir.data)					
          
          # double top/bottom patterns require all extrema [we will exclude the first point(E1/t1)]
          #envir.data$E = data[data.extrema.loc][i:n.index]
          envir.data$E = data[data.extrema.loc][-c(1:i)]
          envir.data$t = data.extrema.loc[-c(1:i)]
          
          # check if pattern was found
          if( eval(pattern$formula, envir = envir.data) ) {
            if(!silent) 
              
              if(plot & !is.null(pattern$plot)) {						
                temp = dates[data.extrema.loc[i:(i + pattern$len - 1)]]									
                names(temp) = paste('d', 1:pattern$len, sep='')
                envir.data = c( envir.data, temp )
                envir.data$d = dates[data.extrema.loc[-c(1:i)]]
                envir.data$col = col
                
                eval(pattern$plot, envir = envir.data)
              }
            
            # record 
            out.rownames = c(out.rownames, pattern$name)
            out = rbind(out, c(data.extrema.loc[i], 
                               ifelse(is.null(pattern$last.point),
                                      data.extrema.loc[(i + pattern$len - 1)], 
                                      eval(pattern$last.point, envir = envir.data)
                               )))
          }
        }
      }		
    }	
  }
  
  if(length(out)>0) {
    colnames(out) = c('start','end')
    rownames(out) = out.rownames
  }
  return(out)
}


col.add.alpha <- function
(
  col, 		# color(s)
  alpha=150	# alpha
) 
{
  rgb(t(col2rgb(col)), alpha=alpha, maxColorValue = 255)	
}

find.extrema <- function(
  x	# time series
) 
{
  if(is.xts(x)) {
    y = as.vector( Cl(x) )
  } else {
    y = x
  }		
  n = length(y)
  t = 1:n
  
  # Fit kernel
  # stat.epfl.ch/files/content/sites/stat/files/users/MdC/notes_3.pdf
  h = h.select(t, y, method = 'cv')
  temp = sm.regression(t, y, h=h, display = 'none')
  mhat = approx(temp$eval.points, temp$estimate, t, method='linear')$y
  
  # page 15
  # find locations of local maxima and minima in mhat
  temp = diff(sign(diff(mhat)))
  loc = which( temp != 0 ) + 1
  loc.dir = -sign(temp[(loc - 1)])
  
  # check
  # temp = cbind(mhat[(loc - 1)],mhat[(loc)],mhat[(loc + 1)])
  # cbind(round(temp,2), loc.dir, apply(temp, 1, which.max), apply(temp, 1, which.min))
  
  
  # page 16
  # find locations of local maxima and minima in original data with in +1/-1 local maxima and minima in mhat
  temp = c( y[1], y, y[n] )
  temp = cbind(temp[loc], temp[(loc + 1)], temp[(loc + 2)])
  max.index = loc + apply(temp, 1, which.max) - 2
  min.index = loc + apply(temp, 1, which.min) - 2
  data.loc = ifelse(loc.dir > 0, max.index, min.index)
  data.loc = ifelse(data.loc < 1, 1, ifelse(data.loc > n, n, data.loc))
  
  if(is.xts(x)) mhat = as.xts(mhat, index(x))
  
  return(list(data = y, mhat = mhat, extrema.dir = loc.dir,
              mhat.extrema.loc = loc, data.extrema.loc = data.loc))
}



rtd3 <- function(symbol,j){
  
  add="+share+price"
  
  tickers <-symbol
  tickers=tolower(tickers)
  
  base <- "https://www.google.com/search?q="
  
  
  
  post="&rlz=1C1CHBD_enUS803US803"
  
  #Pasting them together to make the API call. The result should look like this if you view it:"https://api.intrinio.com/prices?ticker=AAPL"
  call1 <- paste(base,"=", tickers,add,post, sep="")
  
  
  
  html <- getURL(call1, followlocation = TRUE)
  
  # parse html
  doc = htmlParse(html, asText=TRUE)
  
  
  plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  
  
  
  text=data.frame(plain.text)
  tryCatch({
    
    levels(text$plain.text ) <- tolower(levels(text$plain.text ))
    
    
  },
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
  
  cid=grepl("google finance", text$plain.text)
  bcid=1*cid
  text$ID=seq.int(nrow(text))
  
  frstc=cbind(bcid,text)
  
  frstd=as.matrix(frstc)
  frstd=as.data.frame(frstd)
  
  frstd=frstd[!(is.na(frstd$plain.text) ), ]
  
  
  frstd=frstd[!(is.null(frstd$plain.text) ), ]
  
  
  frstd=frstd[!((frstd$plain.text==" ") ), ]
  
  
  frstd=frstd[!((frstd$plain.text=="0") ), ]
  
  
  frstd=frstd[!((frstd$plain.text=="-") ), ]
  
  frstd$cle=gsub("[^0-9A-Za-z///' ]", "", frstd$plain.text)
  
  
  rng<-  sqldf('SELECT * FROM frstd where length(cle) <> 0 ',drv='SQLite')
  
  
  
  
  nrng=rng[!is.na(as.numeric(rng$cle)),] 
  
  nrng$ID=seq.int(nrow(nrng))
  colnames(nrng)=c("bcid","info","ID","cle")
  if(nrow(rng)==0)
  { nin=smd
  nin=t(nin)
  } else{
    
    nrng$info=gsub(",","",nrng$info)
    nrng<-as.matrix(nrng)
    
    if(as.numeric(nrng[1,2])>10000)
    {nrng=as.data.frame(nrng)
    nrng$ID=seq.int(nrow(nrng))
    nin=sqldf('SELECT distinct info FROM nrng where ID in(2,4,5,8) ',drv='SQLite')
    } 
    else{
      nrng=as.data.frame(nrng)
      nrng$ID=seq.int(nrow(nrng))
      nin <-  sqldf('SELECT distinct info FROM nrng where ID in(1,3,4,7) ',drv='SQLite')
    }
    nin=as.matrix(nin)
    nin=rbind(tickers,nin)
    nin=as.data.frame(nin)
  }
  
  
  
  return(nin) }


rtd <- function(symbol){
  
  
  
  library(httr)
  library(XML)
  tickers <-toupper(symbol) 

  tryCatch({
    
    if((as.character(mest[1,10])==tickers)&&(nrow(mest)==tickers))
    {
      print("using prev data")
      
    }else
    {
  
      request_body <- list(grant_type='refresh_token',refresh_token='RoNhJtVpsSiRNL7sEpUL8aVfCd0d7t8hM7OXJt4ppXR37IfN+m+8+4SpdXxOVzlmnCOEiNf99Q1GilTq0GtU+rmtR9n2QVBsYhRAqyfcglncwnN30ivDkJYVw7RSv8iHGZXF5f1KpkPxLv9fM4dP0iwXrO65t1ZIU7Nb+zITWWrraq4m40CXBKbjntYDyOmBj4OzS7nPiwTwJlNKngoMDCnrKYaznybgktyLH4QYvDMQ0GBdt3QRjsUWw2GmQVLBZxlr5964VXDN68tm1pXvSAxFdcQzfZiDOOMiXZ48BlWwysqShoI5jcNDvXCNbF71sHjiAvrBc4tHVnr5/hdJ2CRHp0j/zjBKUSkBrARAp1OTLT+cEsmDyDhwDuV8jkHb6SsRAGXGURM8yp7PldVwcTjDCBPqv5sEDXu7YhgHfmXGQHh8sKGoPuNpUMr100MQuG4LYrgoVi/JHHvlouwXabKv5z6JLjSw/Qxs3by+0xfkfruKUhfd2cz2JscaPX5tsfROExf2ag0MRaDMg71BvJpBx9Vq+VSy9sXO2IepyM3H56YCWpVZiXINWUiMuSQxqEjF3JZjs01aJpEksTFh7nEBzal6lpwebeeZF4CYQJpZcy1S4gZE6VpCkBt510ndJLcozYvC7dSBUq9cqltyeoSdVrQFqqoi408M+nDhi9F/JMHyocUO49w9hRqJN+QRSnHJllqEaltlnYYp/qdd+RfrejKhGFHapuvU75l/0dsPif9pcsgYmU1P4kZXS212nwlfKXubGrBCDp1c32WiUUjnGkAh8j+gmgLy2NRzGVwn/OnbToHJOQUebzMLXrMt/X3QvNSNflyCpuC23SBnTXvS8jSMGTnFfd+ZfpWYE0TmdSpNMUci2PQ1wpN4vUdt48FhsZCqqXg=212FD3x19z9sWBHDJACbC00B75E'  ,
                           client_id='YNBNVMCWBVRP10MFDTONAK7XHLPUHOCH@AMER.OAUTHAP',
                           redirect_uri='htttp://127.0.0.1')
      
      user_token <- httr::content(httr::POST('https://api.tdameritrade.com/v1/oauth2/token',
                                             body=request_body,
                                             encode='form'))
      
      
      
      auth_header <- httr::add_headers('Authorization'=
                                         paste('Bearer',user_token$access_token))
      
      base <- "https://api.tdameritrade.com/v1/marketdata/"
 
      
      
      post="/pricehistory"
      
      #Pasting them together to make the API call. The result should look like this if you view it:"https://api.intrinio.com/prices?ticker=AAPL"
      call1 <- paste(base, tickers,post, sep="")
      
  
      daf <- httr::content(
        httr::GET( call1 ,query=list(periodType='day',frequencyType='minute',frequency='15',needExtendedHoursData='false', endDate='1787027540000'), auth_header))
      

      tryCatch({
        df <- data.frame(matrix(unlist(daf), ncol=6, byrow=T),stringsAsFactors=FALSE)
        
        
      },
      error=function(e){
        
      })
      
      
      
      
     
      
      df<- df[-nrow(df),]

      
      vv=as.numeric(df$X6)
      
      
      
      
      
      time <- vv
      timezone <- 10800000
      options(digits.secs=3)
      vv= .POSIXct((time+timezone)/1000, tz="US/Eastern")-3*3600+60
      df=cbind(df,vv)
      df=as.data.frame(df)
      
      
      colnames(df)=c("Open","High","Low","close","volume","uda","Date")
      
      
      df$symbol=symbol
      
      df=as.data.frame(df)
      
  
      tst <-  sqldf('SELECT distinct symbol,Date,Open,High,Low,close,volume FROM df WHERE close<>"0" order by Date desc',drv='SQLite')
      
      tst$Date=tst$Date-9.50*3600
      cal=tst
      cal <-  sqldf('SELECT  symbol,Date,max(Open) as Open,max(High) as High,max(Low) as Low,max(close) as close,max(volume)  as volume FROM cal group by symbol,Date order by Date desc',drv='SQLite')
      
      
      mbuy= sqldf('SELECT distinct max(Date) as Date from cal ',drv='SQLite')
      
      cal$diff=difftime(as.POSIXct(strptime(mbuy$Date,  "%Y-%m-%d %H:%M:%S")), as.POSIXct(strptime(cal$Date,  "%Y-%m-%d %H:%M:%S")), units='hours')
      
      cal$diff=as.numeric(cal$diff)
      cal$diff=round(cal$diff,2)
      cal= sqldf('SELECT distinct symbol,Date, Open,High, Low, close,volume  from cal',drv='SQLite')
      
      
    

      mest<<-cal
      

      write.table(mest, "mest.csv", col.names=FALSE,row.names=FALSE,sep=",",append=TRUE)
     
    
    }
      
      
  
  },
  error=function(e)
    
    
  {}
    

  )
  return(cal)
  
}

sma20c <- function(clse,sm20) {
  position <- ifelse((((clse-sm20)*100)/clse)>0.2,1,0)
  
}

ii20c <- function(sm50,sm200) {
  position <- ifelse((((sm50-sm200)*100)/clse)>0.2,1,0)
  
}

ema20c <- function(clse,em20) {
  position <- ifelse((((clse-em20)*100)/clse)>0.2,1,0)
  
  
  
}

vma20c <- function(clse,vm20) {

  position <- ifelse((((clse-vm20)*100)/clse)>0.2,0,1)
}

sr20c <- function(clse,sr20) {
  
  position <- ifelse((((clse-sr20)*100)/clse)>0,1,0)

}



myStrat <<- function(clse,sm20) {
  position <- ifelse(clse>sm20,1,0)
  
}

server <- function(input,output, session){
  storeWarn<- getOption("warn")
  options(warn = -1) 
  
  
  rv <- reactiveValues(i = 0)
  
  jv <- reactiveValues(i = 0)
  
  hv <- reactiveValues(i = 0)
  
  kv <- reactiveValues(i = 0)
  
  observeEvent(input$run, {
    rv$i <- 0
    observe({
      isolate({
        rv$i <- rv$i + 1
        
        
      })
      
      if (isolate(rv$i) < maxIter){
        invalidateLater(30000, session)
      }
    })
  })
  
  observeEvent(input$run1, {
    jv$i <- 0
    observe({
      isolate({
        jv$i <- jv$i + 1
        
        
        
      })
      
      if (isolate(jv$i) < maxIter1){
        invalidateLater(2000, session)
      }
    })
  })
  
  
  observeEvent(input$run, {
    kv$i <- 0
    observe({
      isolate({
        kv$i <- kv$i + 1
        
        
        
      })
      
      if (isolate(kv$i) < maxIter2){
        invalidateLater(2000, session)
      }
    })
  })
  
  
  
  observeEvent(input$run2, {
    hv$i <- 0
    observe({
      isolate({
        hv$i <- hv$i + 1
        
        print(hv$i)
        
      })
      
      if (isolate(hv$i) < maxIter3){
        invalidateLater(2000, session)
      }
    })
  })
  
  gen = c('MACD', 'SMA20', 'EVWMA20')
  
  len = c('RESISTANCE', 'TREND')
  token <- drop_auth()
  saveRDS(token, "droptoken.rds")
  # Upload droptoken to your server
  # ******** WARNING ********
  # Losing this file will give anyone 
  # complete control of your Dropbox account
  # You can then revoke the rdrop2 app from your
  # dropbox account and start over.
  # ******** WARNING ********
  # read it back with readRDS
  token <- readRDS("droptoken.rds")
  # Then pass the token to each drop_ function
  drop_acc(dtoken = token)
  
  
  output$allInputs1 <- renderUI({
    
    mc=as.data.frame(as.character(input$custom1))
    
    colnames(mc)=c("symbol")
    
    
    yoursym1=read.table(file="mcsy.csv",header=FALSE,sep=",") 
    yoursym1=as.matrix(yoursym1)
    yoursym1[is.na(yoursym1)] <- 0
    
    yoursym=as.data.frame(yoursym1[1,])
    colnames(yoursym)=c("symbol")
    
    mcsy<<-yoursym
    
    yoursym=rbind(mc,yoursym)
    mcsy<<-yoursym
    
    print(yoursym)
    write.table(t(yoursym), "mcsy.csv", col.names=FALSE,row.names=FALSE,sep=",",append=FALSE)
    
    
    tes1=t(as.character(yoursym$symbol))
    print(tes1)
    newInput1 <- selectInput("stock1", "Stocks:",tes1)
    # Append new input to list of existing inputs
    newInput1
    
    
  })
  
  
  output$allInputs <- renderUI({
    # Get value of button, which represents number of times pressed (i.e. number of inputs added)
    # Get value of button, which represents number of times pressed (i.e. number of inputs added)
    mc=as.data.frame(as.character(input$custom))
    
    colnames(mc)=c("symbol")
    
    
    yoursym1=read.table(file="mcsy.csv",header=FALSE,sep=",") 
    yoursym1=as.matrix(yoursym1)
    yoursym1[is.na(yoursym1)] <- 0
    
    yoursym=as.data.frame(yoursym1[1,])
    colnames(yoursym)=c("symbol")
    
    mcsy<<-yoursym
    
    yoursym=rbind(mc,yoursym)
    mcsy<<-yoursym
    
    #  print(yoursym)
    write.table(t(yoursym), "mcsy.csv", col.names=FALSE,row.names=FALSE,sep=",",append=FALSE)
    
    
    tes1=t(as.character(yoursym$symbol))
    #  print(tes1)
    newInput1 <- selectInput("stock", "Stocks:",tes1)
    # Append new input to list of existing inputs
    newInput1
    
    
    
  })
  
  
  
  
  pollData5 <- reactivePoll(2000, session,
                            # This function returns the time that the logfile was last
                            # modified
                            
                            
                            checkFunc = function() {
                              
                              
                              
                              if (file.exists("accinfo_tot.csv"))
                                file.info("accinfo_tot.csv")$mtime[1]
                              else
                                ""
                            },
                            # This function returns the content of the logfile
                            valueFunc = function() {
                            
                              
                              tstacc<-read.table(file="accinfo_tot.csv",header=TRUE,sep=",") 
                              
                              print(input$run1)
                              
                              return(tstacc)  
                              
                              
                            }
  )
  
  pollData6 <- reactivePoll(2000, session,
                            # This function returns the time that the logfile was last
                            # modified
                            
                            
                            checkFunc = function() {
                              
                              
                              
                              if (file.exists("accinfo_tot1.csv"))
                                file.info("accinfo_tot1.csv")$mtime[1]
                              else
                                ""
                            },
                            # This function returns the content of the logfile
                            valueFunc = function() {
                              tstacc<-read.table(file="accinfo_tot1.csv",header=TRUE,sep=",") 
                              
                        
                              return(tstacc)  
                              
                              
                              
                            }
  )
  
  
  option6 <- reactivePoll(55000, session,
                            # This function returns the time that the logfile was last
                            # modified
                            
                            
                            checkFunc = function() {
                              
                              
                              
                              if (file.exists("option.csv"))
                                file.info("option.csv")$mtime[1]
                              else
                                ""
                            },
                            # This function returns the content of the logfile
                            valueFunc = function() {
                              singles_file<-read.table(file="option.csv",header=TRUE,sep=",") 
                         
                              print(input$run3)
                             
                              return(singles_file)  
                            }
  )
  
  
  
  
  pollData2 <- reactivePoll(10, session,
                            # This function returns the time that the logfile was last
                            # modified
                            checkFunc = function() {
                              
                              
                              
                              
                              
                              if (file.exists("mest.csv"))
                                file.info("mest.csv")$mtime[1]
                              else
                                ""
                            },
                            # This function returns the content of the logfile
                            valueFunc = function() {
                              
                              singles_file <-  readLines("mest.csv",warn=FALSE)
                              
                              
                              tst=as.matrix(singles_file)
                              
                              
                              tst <- as.data.frame(do.call('rbind', strsplit((tst),',',fixed=TRUE)))
                              
                              tst=as.data.frame(tst,quote=FALSE)
                              
                              tst <-  sqldf('SELECT distinct  V1 , V2  ,V3 ,  V4,V5,V6,V7 FROM tst',drv='SQLite')
                              
                              colnames(tst)=c("symbol","Date","Open","High","Low","close","volume")
                              
                              
                              tst=as.data.frame(tst,quote=FALSE)
                              df <-  sqldf('SELECT  *  FROM tst ',drv='SQLite')
                              test=as.data.frame(sapply(df, function(x) gsub("\"", "", x)))
                              
                              test=as.matrix(test)
                              
                              cal=as.data.frame(test)
                              
                              
                              write.table(cal, "mest.csv", col.names=FALSE,row.names=FALSE,sep=",",append=FALSE)
                              
                              
                              return(cal)  
                              
                            }
  )
  
  
  
  
  dat2 <- reactive({
    
    
    tryCatch({
      mest= pollData2() 
      
      
    },
    error=function(e){
      
    })
    
    
    
    tryCatch({ 
      
      
      mest <-  sqldf('SELECT distinct * FROM mest  order by Date desc ',drv='SQLite')
      #   print("it is me")
      
      
      mest<<- as.data.frame(mest)
      
      
      
      return(mest)
      
      
      
    },
    error=function(e){
      
      
    }      )  })
  
  dat5 <- reactive({
    
    
    tryCatch({
      accinfo= pollData5() 
      
      
    },
    error=function(e){
      
    })
    
    
    
    
    
    return(accinfo)
    
  })
  dat6 <- reactive({
    
    
    tryCatch({
      accinfo= pollData6() 
      
      
    },
    error=function(e){
      
    })
    
    
    
    
    
    return(accinfo)
    
  })
  
  
  
  opti <- reactive({
    
    
    tryCatch({
      accinfo= option6()
      
      
    },
    error=function(e){
      
    })
    
 
    return(accinfo)
    
  })
  
  output$view2 <- renderTable({
    vg=dat5()

    vg
  })
  
  
  output$view3 <- renderTable({
    
    vg=dat6()
  
    vg
  })
  autoInvalidate <- reactiveTimer(30000, session)
  
  output$interactivem <- renderDygraph({
    
    if(rv$i > 0)   {
      
      
      allsym=(as.data.frame(input$stock1))
      
      allsym=as.data.frame(as.matrix(input$stock1))
      
      
      colnames(allsym)=c("symbol")
      
      symbol=as.character(allsym[1,1])
      
      yoursym2=symbol
      
      print(input$somevalue2)
      
      
      {
        
       
        
        rest= sqldf('SELECT distinct * FROM mest where symbol not in("NSE:XXX")  ',drv='SQLite')
        
        test=as.data.frame(rest)
        
        if(Ninf<(1))
        {
          Ninf<<-as.numeric(input$Ninf)
          
          
          
        }
        
        Ninf<<-as.numeric(Ninf)-1
        #   print(Ninf)
        
        
        
        if(input$somevalue2=="TRUE")
        {
          maxIter <<-3000
          mi=1
        
          for (mi in 1:7) {
            autoInvalidate()
            
        print(mi)
          if(mi==7)
          {
            mest=eval_with_timeout({rtd(symbol) }, timeout = 10)
            
 
          
          mest$ID=-seq.int(nrow(mest))
          
          mest=sqldf('SELECT distinct a.* FROM mest a join allsym b on TRIM(a.symbol)=TRIM(b.symbol) order by Date desc ',drv='SQLite')
          
          mest<<-as.data.frame(mest)
      
          }
            
          }
          
        }else {
          maxIter <<-0
          Ninf<<-as.numeric(input$Ninf)
        }
        
         
        dly3=-Ninf
        dly2=-2400
        dly3<- as.data.frame(dly3)
        colnames(dly3)= "dly"
        
        
        dly2<- as.data.frame(dly2)
        colnames(dly2)= "dly"
        
        
        
        test$dl=-seq.int(nrow(test))
        
    
        
        test <-  sqldf('SELECT a.* FROM test a   join dly2 b on a.dl>=b.dly  join dly3 c on a.dl<=c.dly order by Date desc',drv='SQLite')
        
        test=as.data.frame(test)
        
        test=sqldf('SELECT distinct * FROM test  order by Date asc ',drv='SQLite')
        

        
        
        mals=as.matrix(test$close)
        
        mals=as.numeric(mals[,1]) 
        
        
        nx=-(length(mals):1)
        
        
        vcols=as.matrix(test$volume)
        
        vcols=as.numeric(vcols[,1]) 
        vcols=vcols[-nx]
        
        mvol=mean(vcols)
        
        nx=-(length(mals):1)
        
        #    par(mar = c(5,6,4,5))
        
        
        
        
        #  barplot(vcols[-nx],col="light green")
        #  abline(h=mvol,col="pink")
        
        
        bals=mals[-nx]
        
        y <- c(bals)
        nx=-(length(bals):1)
        
        #  par(new = T)
        #   par(mar = c(5,5,3,5))
        #   plot(nx,bals[-nx],type="l", main= symbol,
        #        xlab="delay", ylab="Current Val", cex=1.5, col="red",  lwd=2)
        
        
        tals=bals
        #     bals=rals
        
        
      }
      
      
      
      
      Lines <-  sqldf('SELECT  Date as Date ,open as Open , high as High, low as Low, close as Close,  volume FROM test order by Date desc',drv='SQLite')
      
  
      Lines$Open=gsub("\\.", ",", Lines$Open)
      
      
      Lines$High=gsub("\\.", ",", Lines$High)
      
      
      Lines$Low=gsub("\\.", ",", Lines$Low)
      
      
      Lines$Close=gsub("\\.", ",", Lines$Close)
      
      
      df=sapply(Lines, function(x) gsub("\"", "", x))
      
      df1=as.data.frame(df,quote=FALSE)
      
      
      connectionString <- paste0(df1$Date,";",df1$Open,";",df1$High,";",df1$Low,";",df1$Close,";",df1$volume)
      
      names= paste0("Date;Open;High;Low;Close;Volume")
      
      
      names=as.data.frame(names)
      connectionString=as.data.frame(connectionString)
      
      colnames(connectionString)=c("names")
      
      dd=rbind(names,connectionString)
      
      
      connectionString <- lapply(dd, as.character)
      
      df=sapply(connectionString, function(x) gsub("\"", "", x))
      
      
      
      
      
      conn <- textConnection(df)
      stock=as.xts(read.zoo(conn, sep=';', tz="US/Eastern", dec=",", header=TRUE,
                            format='%Y-%m-%d %H:%M:%S', index.column=1))
      
      stock1= to.minutes(stock)
      
      
      
      symbol=as.character(allsym[1,1])
      
      close(conn)
      
      
      
      
      stock2=as.matrix(stock1)
      stock2=stock1[,1:4]
      
      stock2=as.data.frame(stock2)
      
      colnames(stock2) <- c("Open", "High", "Low", "Close")
      
      stock3=as.matrix(stock2)
      
      stock3=stock3[,1:4]
      
      
      
      stock3=as.xts(stock3,tz="EST")
      
      
      
      
      {
        
        
        {
          {  
            
            
            best<-sqldf('SELECT distinct * FROM test where close>0  order by Date asc ',drv='SQLite')
            
            
            uals=as.matrix(best$close)
            
            uals=as.numeric(uals[,1]) 
            
            
            nx=-(1:(nrow(best)))
            
            
            bals=uals[-nx]
            
            
            tryCatch({
              
              
              
              
              macd = MACD(bals, nFast=12, nSlow=26,nSig=9,maType=SMA, percent = TRUE)
              
              macd=as.data.frame(macd)
              macd[is.na(macd)] <- 0
              
              
              signal <- ifelse(macd$macd >macd$signal, 1, 0)
              signal=as.data.frame(signal)
              signal[is.na(signal)] <- 0
              
              colnames(signal)=c("signal")
              
              
              SIGNAL1=as.numeric(as.matrix(signal$signal))
              SIGNAL1[is.na(SIGNAL1)]=0
              
              nx=-(1:length(SIGNAL1))
              
              
              SSIGNAL1=SIGNAL1[-nx]
              
            },
            error=function(e){
              
              
            })
            
            
            tryCatch({
              
              
              
              
              macd = MACD(bals, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = TRUE)
              
              macd=as.data.frame(macd)
              macd[is.na(macd)] <- 0
              
              
              signal <- ifelse(macd$macd >macd$signal, 1, 0)
              signal=as.data.frame(signal)
              signal[is.na(signal)] <- 0
              
              colnames(signal)=c("signal")
              
              
              SIGNAL1=as.numeric(as.matrix(signal$signal))
              SIGNAL1[is.na(SIGNAL1)]=0
              
              nx=-(1:length(SIGNAL1))
              
              
              ESIGNAL1=SIGNAL1[-nx]
              
            },
            error=function(e){
              
              
            })
            
            
            
            tryCatch({
              
              
              HL=cbind(stock$High,stock$Low)
              
              
              
              aroon= aroon(HL,n = 25)
              aroon=as.data.frame(aroon)
              aroon[is.na(aroon)] <- 0
              aroon<-as.data.frame(aroon)
              
              
              arsignal <- ifelse(aroon$aroonUp > aroon$aroonDn, 1, 0)
              
              arsignal=as.data.frame(arsignal)
              arsignal[is.na(arsignal)] <- 0
              colnames(arsignal)=c("arsignal")
              
              ARSIGNAL1=as.numeric(as.matrix(arsignal$arsignal))
              ARSIGNAL1[is.na(ARSIGNAL1)]=0
              
              nx=-(1:length(ARSIGNAL1))
              
              
              ARSIGNAL1=ARSIGNAL1[-nx]
              
            },
            error=function(e){
              
              
            })
            
            
            
            tryCatch({
              
              
              HLC=cbind(stock$High,stock$Low,stock$Close)
              
              
              
              adx= ADX(HLC,n = 14, maType="EMA", wilder=TRUE)
              adx=as.data.frame(adx)
              adx[is.na(adx)] <- 0
              
              
              adsignal <- ifelse(adx$DIp > adx$DIn, 1, 0)
              
              adsignal=as.data.frame(adsignal)
              adsignal[is.na(adsignal)] <- 0
              colnames(adsignal)=c("adsignal")
              
              ADSIGNAL1=as.numeric(as.matrix(adsignal$adsignal))
              ADSIGNAL1[is.na(ADSIGNAL1)]=0
              
              nx=-(1:length(ADSIGNAL1))
              
              
              ADSIGNAL1=ADSIGNAL1[-nx]
              
              
            },
            error=function(e){
              
              
            })
            
            tryCatch({
              
              
              
              smi=SMI(HLC,n=13,slow=5,fast=20,signal=5,ma.type="EMA")
              
              
              smi=as.data.frame(smi)
              smi[is.na(smi)] <- 0
              
              
              
              smisignal <- ifelse(smi$SMI > smi$signal, 1, 0)
              
              smisignal=as.data.frame(smisignal)
              
              
              
              smisignal[is.na(smisignal)] <- 0
              colnames(smisignal)=c("smisignal")
              
              SMISIGNAL1=as.numeric(as.matrix(smisignal$smisignal))
              SMISIGNAL1[is.na(SMISIGNAL1)]=0
         
              nx=-(1:length(SMISIGNAL1))
              
              
              SMISIGNAL1=SMISIGNAL1[-nx]
              
              
            },
            error=function(e){
              
              
            })
            
            tryCatch({
              gelsma20=EMA(bals,200)
              gelsma20=as.data.frame(gelsma20)
              
              
              GELSMA20=as.numeric(as.matrix(gelsma20$gelsma20))
              
              
              
              GELSMA20[is.na(GELSMA20)]=bals[1:sum(is.na(GELSMA20))]
              
              GELSMA20[is.na(GELSMA20)]=bals[sum(is.na(GELSMA20))+1]
              
              
              nx=-(1:length(GELSMA20))
              
              
              GELSMA20=GELSMA20[-nx]
              
              
              
              
            },
            error=function(e){
              
              
            })
            
            
            tryCatch({
              gelema20=EMA(bals,100)
              gelema20=as.data.frame(gelema20)
              
              
              GELEMA20=as.numeric(as.matrix(gelema20$gelema20))
              
              
              
              GELEMA20[is.na(GELEMA20)]=bals[1:sum(is.na(GELEMA20))]
              
              GELEMA20[is.na(GELEMA20)]=bals[sum(is.na(GELEMA20))+1]
              
              
              nx=-(1:length(GELEMA20))
              
              
              GELEMA20=GELEMA20[-nx]
              
              
              
              
            },
            error=function(e){
              
              
            })
            
            
            tryCatch({  
              HL=cbind(stock$High,stock$Low)
              HL$me=1
              HL=cbind(HL[,1],HL[,2])
              sar=SAR(HL, accel = c(0.02, 0.2))
              sar=as.numeric(sar[,1]) 
              
              
              nx=-(1:(nrow(best)))
              
              
              sar=sar[-nx]
              
              
              GELSRA20=sar
              
              
              
              GELSRA20=GELSRA20[-nx]
              
              GELSRA20[1:10]=bals[1:10]
              
            },
            error=function(e){
              
              
            }) 
            
            tryCatch({
              
              vols=as.matrix(test$volume)
              
              vols=as.numeric(vols[,1]) 
              
              
              gelvma20=VWMA(bals,vols,200)
              gelvma20=as.data.frame(gelvma20)
              
              
              GELVMA20=as.numeric(as.matrix(gelvma20$gelvma20))
              
              
              
              GELVMA20[is.na(GELVMA20)]=bals[1:sum(is.na(GELVMA20))]
              
              GELVMA20[is.na(GELVMA20)]=bals[sum(is.na(GELVMA20))+1]
              
              
              nx=-(1:length(GELVMA20))
              
              
              GELVMA20=GELVMA20[-nx]
              
              
              
              
            },
            error=function(e){
              
              
            })
            
            
            tryCatch({
              
              
              
              rsi=RSI(bals,n = 20, maType = "EMA", wilder = TRUE)
              
              
              
              
              srsi=stoch(rsi)
              
              
              
              srsi=as.data.frame(srsi)
              srsi[is.na(srsi)] <- 0
              
              
              
              srsignal <- ifelse(srsi$fastD > srsi$slowD, 1, 0)
              
              srsignal=as.data.frame(srsignal)
              
              
              srsignal[is.na(srsignal)] <- 0
              colnames(srsignal)=c("srsignal")
              
              SRSIGNAL1=as.numeric(as.matrix(srsignal$srsignal))
              SRSIGNAL1[is.na(SRSIGNAL1)]=0
              
              nx=-(1:length(SRSIGNAL1))
              
              
              SRSIGNAL1=SRSIGNAL1[-nx]
              
            },
            error=function(e){
              
              
            }) 
            
            tryCatch({
              
              
              rsi=RSI(bals,n = 5, maType = "EMA", wilder = TRUE)
              
              
              
              
              rsi=as.data.frame(rsi)
              
              
              mrsi=RSI(bals,n = 20, maType = "EMA", wilder = TRUE)
              mrsi=as.data.frame(mrsi)
              
              
              
              rsi=as.numeric(as.matrix(rsi$rsi))
              rsi[is.na(rsi)]=0
              
              
              mrsi=as.numeric(as.matrix(mrsi$mrsi))
              mrsi[1:20]=rsi[1:20]
              
              nx=-(length(rsi):1)
              
              
              rsignal <- ifelse(rsi> mrsi, 1, 0)
              rsignal=as.data.frame(rsignal)
              rsignal[is.na(rsignal)] <- 0
              colnames(rsignal)=c("rsignal")
              
              RSIGNAL1=as.numeric(as.matrix(rsignal$rsignal))
              RSIGNAL1[is.na(RSIGNAL1)]=0
              
              nx=-(1:length(RSIGNAL1))
              
              
              RSIGNAL1=RSIGNAL1[-nx]
              
              
              
            },
            error=function(e){
              
              
            }) 
            
          }
          
          #   myReturnsc[1: myReturnsc, ] <- 1
          
          
          
          dt=test[,2]
          cals=as.matrix(test$close)
          
          cals=as.numeric(cals[,1]) 
          curr<-cals[length(cals)]
          vx=-(1:length(cals))
          bact=cbind("symbol",cals[-vx],GELSMA20[-nx],GELEMA20[-nx],GELVMA20[-nx],GELSRA20[-nx],1,1,1)          
          bact=as.data.frame(bact)
          
          
          
          bact=cbind(dt,bact)
          
          colnames(bact)=c("V1","V2","V3","V5","V6","V7","V8","V9","V10","V11") 
          
          
          bact3=sqldf('SELECT a.* FROM bact  a order by a.V1 ',drv='SQLite')
          
          bact3[is.na(bact3)] <- 0
          
          
          
          bact3=as.data.frame(bact3)
          
          
          cek=sqldf('SELECT max(V1) as Date FROM bact3 ',drv='SQLite')
          dm1<-as.POSIXct(strptime(cek$Date, "%Y-%m-%d %H:%M:%S"))
          
          
          bacte=sqldf('SELECT distinct * FROM bact3 ',drv='SQLite')
          
          
          
          
          df1=(as.data.frame(bacte))
          
          
          
          connectionString <- paste0(df1$V1,";",df1$V2,";",df1$V3,";",df1$V5,";",df1$V6,";",df1$V7,";",df1$V8,";",df1$V9,";",df1$V10,";",df1$V11)
          
          names= paste0("Date;Symbol;Close;sm20;em20;vm20;sr20;smistrd;vstrd;rvstrd")
          
          
          names=as.data.frame(names)
          connectionString=as.data.frame(connectionString)
          
          colnames(connectionString)=c("names")
          
          dd=rbind(names,connectionString)
          
          
          connectionString <- lapply(dd, as.character)
          
          df=sapply(connectionString, function(x) gsub("\"", "", x))
          
          
          
          
          conn <- textConnection(df)
          
          
          
          
          stock <- as.xts(read.zoo(conn, sep=';', tz="US/Eastern", dec=",", header=TRUE,
                                   format='%Y-%m-%d %H:%M:%S', index.column=1))
          
          stk1=as.matrix(stock)
          stk1=as.data.frame(stock)
          
          a <- as.character(stk1$Close)
          a <- as.numeric(a)
          
          sm20 =as.character(stk1$sm20)
          sm20=as.numeric(sm20)
          
          em20 =as.character(stk1$em20)
          em20=as.numeric(em20)
          
          vm20 =as.character(stk1$vm20)
          vm20=as.numeric(vm20)
          
          
          clse =as.character(stk1$Close)
          clse=as.numeric(clse)
          
          sr20 =as.character(stk1$sr20)
          sr20=as.numeric(sr20)
          
          
          
          i1 <-  ESIGNAL1
          
      
            i2 <-  SSIGNAL1
            
            i3  <-  ADSIGNAL1
            
            i4 <-  SMISIGNAL1
            
            i5 <-  RSIGNAL1
            i6 <- sma20c(clse,sm20)
            i7 <-   ema20c(clse,em20)
            
            
            i81 <- vma20c(clse,vm20)
            i9 <- ARSIGNAL1
            
            i8 <-  sr20c(clse,sr20)
           
          i6 <- sma20c(clse,sm20)
   
            
          em200=EMA(bals,100)
          em200=as.data.frame(em200)
          
          
          em200=as.numeric(as.matrix(em200$em200))
          
          
          
          em200[is.na(em200)]=bals[1:sum(is.na(em200))]
          
          em200[is.na(em200)]=bals[sum(is.na(em200))+1]
          
         
          nx=-(1:length(em200))
          
          
          em200=em200[-nx]
 
          em50=EMA(bals,50)
          em50=as.data.frame(em50)
          
          
          em50=as.numeric(as.matrix(em50$em50))
          
          
          
          em50[is.na(em50)]=bals[1:sum(is.na(em50))]
          
          em50[is.na(em50)]=bals[sum(is.na(em50))+1]
          
          
          nx=-(1:length(em50))
          
          
          em50=em50[-nx]
    
          ii10 <- sma20c(bals,em200)
       
          ii11 <- sma20c(bals,em50)
          
    
          
          best<-sqldf('SELECT distinct * FROM test  order by Date asc ',drv='SQLite')
          
          uals=as.matrix(best$close)
          
          uals=as.numeric(uals[,1]) 
          
          
          nx=-(length(uals):1)
          
          
          gals=uals[-nx]
          
          x <- c(gals)
          
          lv <- peaks(x, 1)
          
          
          mx=-which(lv == TRUE) 
          
          nx=length(gals):1
          
          
          y= c(gals[nx]) 
          
          
          
          my <- findValleys(y)
          
          
          
          
          lz= -(length(y)-my)
          
          mz=lz
          
          nx=-(length(bals):1)
          
          
          
          hz=as.data.frame(cbind(mz,bals[-mz]))
          
          hz <-  sqldf('SELECT mz FROM hz order by V2 DESC',drv='SQLite')
          
          
          hz=as.matrix(hz$mz)
          
          hz=as.numeric(hz[,1]) 
          
          kz1=(hz[round(length(hz)/2):length(hz)])
          
          kz1=as.data.frame(kz1)
          
          kz2=(hz[1:round(length(hz)/2)])
          
          kz2=as.data.frame(kz2)
          
          test$gf=-seq.int(nrow(test):1)
          
          
          minu<-sqldf('SELECT distinct gf as ID,min(close) FROM test where gf in (select kz1 from kz1) order by Date asc ',drv='SQLite')
          
          maxu<-sqldf('SELECT distinct gf as ID ,max(close) FROM test where gf in (select kz1 from kz1) order by Date asc ',drv='SQLite')
          
          
          minu=sqldf('SELECT distinct close FROM test where gf in (select ID from minu)  ',drv='SQLite')
          maxu=sqldf('SELECT distinct close FROM test where gf in (select ID from maxu)  ',drv='SQLite')
          
          
          
          
          minhu<-(as.character(minu[1,1]))
          
          maxhu<-(as.character(maxu[1,1]))
          
          
          if(input$somevalue4=="TRUE") 
          {
            {
              itr=10
              vstacc<<-read.table(file="vstcmnn.csv",header=TRUE,sep=",") 
              
              
              
              
              for(tj in 1:itr){
                tryCatch({
                  if(tj==10)
                  {
                    updateSwitchInput(session = session,
                                      inputId = "somevalue4",
                                      value = FALSE)
                    
                    mstacc=as.data.frame(vstacc)
                    
                    
                    
                    jsym=as.data.frame(allsym)
                    
                    print(jsym)
                    
                    
                    lastm=sqldf('SELECT distinct model  from mstacc order by tper desc   ',drv='SQLite')
                    
                    
                    lastm=lastm[1,]
                    
                    lastm=as.data.frame(lastm)
                    lastm=sqldf('SELECT *  from lastm ',drv='SQLite')
                    
                    testr[10]=as.character(lastm[1,1])
                    
                    cmnn=sqldf('SELECT a.* from mstacc  a join jsym b on a.symbol=b.symbol ',drv='SQLite')
                    write.table(cmnn, "accinfo_tot.csv", col.names=TRUE,row.names=FALSE,sep=",",append=FALSE)
                    dt=Sys.Date()
                    
                    whole=cbind(dt,cmnn)
                    
                   
                    write.table(whole, "dcmnn.csv", col.names=FALSE,row.names=FALSE,sep=",",append=TRUE)
                    cmn=read.table(file="dcmnn.csv",header=FALSE,sep=",") 
                    
                    dfq= sqldf('SELECT V2,max(V1) as dt FROM cmn  group by V2  ',drv='SQLite')
                    common <-  sqldf('SELECT distinct a.* FROM cmn a join dfq  b on  a.V1=b.dt  and a.V2=b.V2',drv='SQLite')
                    write.table(common, "dcmnn.csv",col.names=FALSE,row.names=FALSE,sep=",",append=FALSE)
                    
                  }
                  print(tj)
                  kj=eval(parse(text=testr[tj]))
                  mj=testr[tj]
                  {     
                    kj<- as.matrix(kj)
                    
                    
                    
                    for(i in 1:nrow(kj)){
                      
                      
                      if (as.numeric(kj[i,1])<(2))
                        
                      {
                        
                        
                        kj[i,1]=0
                      } else{
                        
                        kj[i,1]=1
                      }}
                    kj<- as.matrix(kj)
                    
                    ii10=as.matrix(ii10)
                    ii11=as.matrix(ii11)
                    
                    kj=kj+ii10
                    for(i in 1:nrow(kj)){
                      
                      
                      if (as.numeric(kj[i,1])<(2))
                        
                      {
                        
                        
                        kj[i,1]=0
                      } else{
                        
                        kj[i,1]=1
                      }}
                    kj<- as.matrix(kj)
                    kj=kj+ii11
                    
                    for(i in 1:nrow(kj)){
                      
                      
                      if (as.numeric(kj[i,1])<(2))
                        
                      {
                        
                        
                        kj[i,1]=0
                      } else{
                        
                        kj[i,1]=1
                      }}
                    kj<- as.matrix(kj)
                    
                    
                    
                    
                    
                    x=input$NOTRD
                    if(x==1)
                    {     
                      
                      i11=i12+kj
                    }
                    if(x==2)
                    {
                      i11=i12
                    }
                    if(x==3)
                    {
                      i11=kj
                    }
                    
                    
                    kj<- as.matrix(i11)
                    nd= input$Soff
                    
                    kj1=as.data.frame(cbind(kj,nd))
                    
                    
                    colnames(kj1)=c("kj","nd")
                    
                    x <- kj1
                    
                    i11=apply(x, 1, cave,  c1 = "kj", c2 ="nd")
                    
                    i11<- as.matrix(i11)
                    
                    ind=i11
                    
                    # corre<-cbind(ind1,ind2,ind3,ind4,ind5,ind6,ind7,ind8,ind9,kjlt)
                    
                    #  verri<<-corre
                    
                    
                    bmkReturns <-  (diff(a)*100)/a[-length(a)]
                    bmkReturns=cbind(stk1[2:nrow(stk1),0],bmkReturns)
                    bmkReturns1=as.xts(as.data.frame(bmkReturns))
                    myReturnsc <- as.matrix(1*ind[2:length(ind)])
                    myReturnsc=cbind(stk1[2:nrow(stk1),0],myReturnsc)
                    
                    myReturnsc=as.xts(as.data.frame(myReturnsc))
                    
                    names(bmkReturns1) <- 'stock'
                    
                    names(myReturnsc) <- 'Me'
                    
                    bmkReturns2=cumsum(bmkReturns1)
                    
                    
                    myReturns2c=(myReturnsc)
                    lungDeaths= as.xts(cbind(bmkReturns2, myReturns2c),tz="US/Eastern")
                    
                    
                    .index(lungDeaths)[1:nrow(lungDeaths)] <-(.index(lungDeaths)[1:nrow(lungDeaths)] + 5*3600)
                    
                    addy=as.numeric(as.character(test$close))
                    
                    
                    addy=addy[2:(length(addy))]
                    
                    haddy=as.numeric(as.character(test$High))
                    
                    
                    haddy=haddy[2:(length(haddy))]
                    
                    
                    laddy=as.numeric(as.character(test$Low))
                    
                    
                    laddy=laddy[2:(length(laddy))]
                    
                    oaddy=as.numeric(as.character(test$Open))
                    
                    
                    oaddy=oaddy[2:(length(oaddy))]
                    
                    
                    
                    lungDeaths5<-as.xts(cbind(lungDeaths,addy,haddy,laddy,oaddy ),tz="US/Eastern")
                    lungDeaths5[1,2]=0
       
                    
                    
                    kk<-lungDeaths5
                    updn <- c(kk$Me)
                    
                    updn3=rbind(updn[nrow(updn),])
                    
                    updn3$Me=0
                    
                    updn=rbind(updn,updn3)
                    
                    ix1 <- which(updn == 0)
                    
                    updn1 <- c(diff(ix1))
                    
                    ix <- which(updn1 != 1)
                    BID=as.data.frame(as.matrix(c(ix1[ix])))
                    ix0 <- which(updn1 == 1)
                    MBID=as.data.frame(as.matrix(c(ix1[ix])))
                    
                    if(max(MBID)==max(BID))
                    {
                      
                    } else     
                    {
                      BID=  rbind(BID,max(MBID))
                    }
                    
                    
                    
                    updn2 <- c(kk$Me)
                    
                    updn3=rbind(updn2[nrow(updn2),])
                    
                    updn3$Me=1
                    
                    
                    
                    updn2=rbind(updn2,updn3)
                    
                    updn3=rbind(updn2[nrow(updn2),])
                    
                    updn3$Me=0
                    
                    
                    
                    updn2=rbind(updn2,updn3)
                    
                    ix11 <- which(updn2 == 1)
                    
                    updn2 <- c(diff(ix11))
                    ix10 <- which(updn2 != 1)
                    SID=as.data.frame(as.matrix(c(ix11[ix10])))
                    
                    
                    fpnt<-data.frame(date=index(lungDeaths5), coredata(lungDeaths5))
                    
                    fpnt=as.data.frame(fpnt)
                    
                    
                    fpnt$ID=seq.int(nrow(fpnt))-1
                    
                    
                    
                    
                    cfpnts=sqldf('SELECT a.date,a.stock,a.ID, "BUY" as decision,a.addy from fpnt a join BID b  on a.ID=b.v1 order by a.ID asc ',drv='SQLite')
                    
                    
                    bfpnts=(cfpnts)
                    
                    #print(bfpnts)
                    bfpnts=as.data.frame(bfpnts)
                    
                    sfpnts=sqldf('SELECT a.date,a.stock,a.ID, "SELL" as decision,a.addy from fpnt a join SID b  on a.ID=b.v1 order by a.ID asc',drv='SQLite')
                    #print(sfpnts)
                    
                    
                    q=0
                    
                    if(nrow(bfpnts)>nrow(sfpnts))
                    {
                      
                      gfpnts=sqldf('SELECT a.date,a.stock,a.ID, "SELL" as decision,a.addy from fpnt a order by a.ID asc',drv='SQLite')
                      
                      gfpnts=gfpnts[nrow(gfpnts),]
                      
                      gfpnts$date <-as.POSIXct(strptime(gfpnts$date, "%Y-%m-%d %H:%M:%S"))+0.5*60
                      
                      
                      sfpnts=rbind(sfpnts,gfpnts)
                      
                      q=1
                    }
                    
                    
                    
                    
                    sfpnts=as.data.frame(sfpnts)
                    
                    
                    
                    bfpnts$ID1=seq.int(nrow(bfpnts))
                    
                    sfpnts$ID1=seq.int(nrow(sfpnts))
                    
                    
                    
                    if(input$Soff1==1)
                    {
                      bfpnts$date <-as.POSIXct(strptime(bfpnts$date,  "%Y-%m-%d %H:%M:%S"))+4.5*3600
                      
                      sfpnts$date <-as.POSIXct(strptime(sfpnts$date,  "%Y-%m-%d %H:%M:%S"))+4.5*3600
                      fpnt$date <-as.POSIXct(strptime(fpnt$date,  "%Y-%m-%d %H:%M:%S"))+4.5*3600
                    }else{
                      
                      
                      bfpnts$date <-as.POSIXct(strptime(bfpnts$date,  "%Y-%m-%d %H:%M:%S"))+0.5*3600
                      
                      sfpnts$date <-as.POSIXct(strptime(sfpnts$date,  "%Y-%m-%d %H:%M:%S"))+0.5*3600
                      fpnt$date <-as.POSIXct(strptime(fpnt$date,  "%Y-%m-%d %H:%M:%S"))+0.5*3600
                      
                    }
                    
                    
                    
                    orbfpnts=bfpnts
                    orsfpnts=sfpnts
                    
                    print(orbfpnts)
                    print(orsfpnts)
                    
                    
                    
                    tryCatch({
                      
                      
                      
                      remo=sqldf('SELECT * from orbfpnts  order by ID asc',drv='SQLite')
                      
                      print(remo)
                      
                    },
                    error=function(e){
                      
                      
                    })
                    bfpnts=orbfpnts
                    
                    sfpnts=orsfpnts
                    
                    
                    
                    
                    twopr=sqldf('SELECT a.addy as st,a.ID as bu, b.ID as su from bfpnts a join sfpnts b  on  a.ID1=b.ID1 order by a.ID asc',drv='SQLite')
                    twoprf=sqldf('SELECT a.*,b.*,((a.addy-b.st)*100/b.st) as diff from fpnt a join twopr b  on  a.ID>=b.bu and a.ID<=b.su order by a.ID asc',drv='SQLite')
                    
                    
                    bsu=sqldf('SELECT distinct  bu,su from twoprf ',drv='SQLite')
                    
                    
                    sm=1
                    smaaa=1
                    for(i in 1: nrow(bsu))
                    {
                      bu=bsu[i,1]
                      
                      su=bsu[i,2]
                      bu=as.data.frame(bu)
                      su=as.data.frame(su)
                      
                      dsu=sqldf('SELECT distinct a.* from twoprf a join bu b on a.bu=b.bu join su c on a.su=c.su ',drv='SQLite')
                      add=dsu$addy
                      if(length(add)>1)
                      {
                        
                        smadd=add
                        smadd[1]=add[1]
                        smadd[2:length(smadd)]=add[1:(length(add)-1)]
                      }
                      else{
                        smadd=add
                        smadd= smadd-0.0002*smadd
                      }
                      
                      signal <- ifelse(add >smadd, 1, 0)
                      sm=add*signal
                      
                      
                      smaaa<-c(smaaa,sm)
                      
                    }
                    
                    smaaa=smaaa[2:length(smaaa)]
                    
                    twoprf$addy=smaaa
                    twoprf$addy=   as.numeric(twoprf$add)
                    twoprf=as.data.frame(twoprf)
                    twoprf1=sqldf('SELECT * from twoprf where addy=0',drv='SQLite')
                    
                    ssid=sqldf('SELECT su,min(ID) as  mID from twoprf1 group by su ',drv='SQLite')
                    pbid=sqldf('SELECT su,max(ID) as  mID from twoprf1 group by su ',drv='SQLite')
                    
                    
                    sfpnts=sqldf('SELECT a.date,a.stock,a.ID, a.decision,a.addy from sfpnts a where ID not in (SELECT su from ssid) ',drv='SQLite')
                    
                    
                    cpsts=sqldf('SELECT a.date,a.stock,a.ID, "SELL" as decision,a.addy from fpnt a join ssid b  on a.ID=b.mID order by a.ID asc',drv='SQLite')
                    bbpsts=sqldf('SELECT a.date,a.stock,a.ID, "SELL" as decision,a.addy from fpnt a join pbid b  on a.ID=b.su order by a.ID asc',drv='SQLite')
                    
                    
                    sscpsts=cpsts
                    
                    
                    bbbfpnts=sqldf('SELECT distinct a.date,a.stock,a.ID, "BUY" as decision,a.addy from fpnt a join sscpsts b  on a.ID=b.ID  order by a.ID asc',drv='SQLite')
                    
                    
                    
                    sssfpnts=sqldf('SELECT distinct a.date,a.stock,a.ID, "SELL" as decision,a.addy from fpnt a join bbpsts c on a.ID=c.ID   order by a.ID asc',drv='SQLite')
                    
                    
                    samed=sqldf('SELECT distinct a.ID from bbbfpnts a join sssfpnts c on a.ID=c.ID   order by a.ID asc',drv='SQLite')
                    
                    bbbfpnts=sqldf('SELECT distinct * from bbbfpnts a where ID  not in (SELECT ID from samed) ',drv='SQLite')
                    
                    
                    sssfpnts=sqldf('SELECT distinct * from sssfpnts a where ID  not in (SELECT ID from samed) ',drv='SQLite')
                    
                    bbbfpnts$ID1=seq.int(nrow(bbbfpnts))
                    sssfpnts$ID1=seq.int(nrow(sssfpnts))
                    
                    
                    sfpnts=rbind(sfpnts,cpsts)
                    
                    sfpnts=sqldf('SELECT * from sfpnts order by ID asc   ',drv='SQLite')
                    sfpnts$ID1=seq.int(nrow(sfpnts)) 
                    
                    
                    
                    
                    tryCatch({
                      
                      twopr=sqldf('SELECT a.addy as st,a.ID as bu, b.ID as su from bbbfpnts a join sssfpnts b  on  a.ID1=b.ID1 order by a.ID asc',drv='SQLite')
                      
                      twoprf=sqldf('SELECT * from fpnt a join twopr b  on  a.ID>=b.bu and a.ID<=b.su order by a.ID asc',drv='SQLite')
                      
                      
                      twoprf=as.data.frame(twoprf)
                      
                      
                      
                      trwq=sqldf('SELECT distinct  bu,st from twoprf ',drv='SQLite')
                      
                      trwq=sqldf('SELECT distinct  b.*,a.haddy as st1 from twoprf a join trwq b on a.ID=b.bu  ',drv='SQLite')
                      
                      twoprf=sqldf('SELECT distinct  a.*, ((a.haddy-b.st1)*100/b.st1) as diff from twoprf a join trwq b on a.bu=b.bu  ',drv='SQLite')
                      
                      
                      bsu=sqldf('SELECT distinct  bu,su from twoprf ',drv='SQLite')
                      
                      
                      twop=sqldf('SELECT a.*, a.ID-a.bu as length from twoprf a where diff>0.25 and round(((a.addy-a.oaddy)*100/a.oaddy),2) >0 and round(((a.addy-a.haddy)*100/a.addy),2) < -(0.05) and round(((a.oaddy-a.laddy)*100/a.oaddy),2) >(0.05)  order by ID asc',drv='SQLite')
                      
                      
                      b4le=sqldf('SELECT su,min(ID) as bu from twop  group by su',drv='SQLite')
                      
                      twoprf=sqldf('SELECT a.* from twoprf a join b4le b on a.ID>=b.bu and a.ID<=b.su  order by ID asc',drv='SQLite')
                      
                      
                      bbfpnts=sqldf('SELECT a.date,a.stock,a.ID,"BUY" as decision,a.addy from twoprf a join b4le b on a.ID=b.bu order by ID asc',drv='SQLite')
                      
                      ssfpnts=sqldf('SELECT a.date,a.stock,a.ID,"SELL" as decision,a.addy from twoprf a join b4le b on a.ID=b.su order by ID asc',drv='SQLite')
                      
                      
                      bbfpnts$ID1=seq.int(nrow(bbfpnts))
                      
                      ssfpnts$ID1=seq.int(nrow(ssfpnts))
                      
                      twopr=sqldf('SELECT a.addy as st,a.ID as bu, b.ID as su from bbfpnts a join ssfpnts b  on  a.ID1=b.ID1 order by a.ID asc',drv='SQLite')
                      twoprf=sqldf('SELECT a.*,b.*,((a.addy-b.st)*100/b.st) as diff from fpnt a join twopr b  on  a.ID>=b.bu and a.ID<=b.su order by a.ID asc',drv='SQLite')
                      
                      
                      bsu=sqldf('SELECT distinct  bu,su from twoprf ',drv='SQLite')
                      
                      
                      sm=1
                      smaaa=1
                      for(i in 1: nrow(bsu))
                      {
                        bu=bsu[i,1]
                        
                        su=bsu[i,2]
                        bu=as.data.frame(bu)
                        su=as.data.frame(su)
                        
                        dsu=sqldf('SELECT distinct a.* from twoprf a join bu b on a.bu=b.bu join su c on a.su=c.su ',drv='SQLite')
                        add=dsu$addy
                        
                        print(dsu)
                        if(length(add)>2)
                        {
                          
                          smadd=EMA(add,2)
                          smadd[is.na(smadd)]=add[1:sum(is.na(smadd))]
                          
                          smadd[1]=add[1]
                          smadd[2]=add[2]
                          smadd= smadd-0.0005*smadd
                        }
                        else{
                          smadd=add
                          smadd= smadd-0.0005*smadd
                        }
                        
                        signal <- ifelse(add >smadd, 1, 0)
                        sm=add*signal
                        
                        
                        smaaa<-c(smaaa,sm)
                        
                      }
                      
                      smaaa=smaaa[2:length(smaaa)]
                      
                      twoprf$addy=smaaa
                      twoprf$addy=   as.numeric(twoprf$add)
                      twoprf=as.data.frame(twoprf)
                      twoprf1=sqldf('SELECT * from twoprf where addy=0',drv='SQLite')
                      
                      ssid=sqldf('SELECT su,min(ID) as  mID from twoprf1 group by su ',drv='SQLite')
                      pbid=sqldf('SELECT su,max(ID) as  mID from twoprf1 group by su ',drv='SQLite')
                      
                      
                      
                      ssfpnts=sqldf('SELECT a.date,a.stock,a.ID, a.decision,a.addy from ssfpnts a where ID not in (SELECT su from ssid) ',drv='SQLite')
                      ssfpnts$date <-as.POSIXct(strptime(ssfpnts$date,  "%Y-%m-%d %H:%M:%S"))+0.5*60
                      
                      
                      cpsts=sqldf('SELECT a.date,a.stock,a.ID, "SELL" as decision,a.addy from fpnt a join ssid b  on a.ID=b.mID order by a.ID asc',drv='SQLite')
                      cpsts$date <-as.POSIXct(strptime(cpsts$date,  "%Y-%m-%d %H:%M:%S"))
                      ssfpnts=rbind(ssfpnts,cpsts)
                      
                      ssfpnts=sqldf('SELECT * from ssfpnts order by ID asc',drv='SQLite')
                      
                      
                      ssfpnts$ID1=seq.int(nrow(ssfpnts))
                      
                      
                      
                      bfpnts=rbind(bfpnts,bbfpnts)
                      sfpnts=rbind(sfpnts,ssfpnts)
                      
                      
                      
                    },
                    error=function(e){
                      
                      bfpnts=bfpnts
                      sfpnts=sfpnts
                      
                    }) 
                    
                    
                    
                    
                    
                    bfpnts=sqldf('SELECT * from bfpnts order by ID asc',drv='SQLite')
                    
                    sfpnts=sqldf('SELECT * from sfpnts order by ID asc',drv='SQLite')
                    
                    
                    bfpnts$ID1=seq.int(nrow(bfpnts))
                    
                    sfpnts$ID1=seq.int(nrow(sfpnts))
                    
                    pcih= sssfpnts
                    
                    
                    for(i in 1:5){
                      
                      bbbfpnts=ssfpnts
                      
                      
                      tryCatch({
                        
                        
                        bbbfpnts=sqldf('SELECT distinct  * from bbbfpnts where ID not in (select ID from orsfpnts) order by ID ',drv='SQLite')
                        
                        mbts=sqldf('SELECT   min(ID) as mID from  bbbfpnts  ',drv='SQLite')
                        
                        pcih=sqldf('SELECT distinct  a.* from pcih a join mbts b on a.ID>b.mID  ',drv='SQLite')
                        
                        
                        bbbfpnts=sqldf('SELECT a.date,a.stock,a.ID,"BUY" as decision,a.addy from bbbfpnts a order by ID asc',drv='SQLite')
                        
                        pcih1=sqldf('SELECT b.ID, min(a.ID) as mID from pcih a join bbbfpnts b  on  a.ID>b.ID group by b.ID order by a.ID asc',drv='SQLite')
                        
                        sssfpnts=sqldf('SELECT a.* from pcih a join pcih1 b  on  a.ID=b.mID  order by a.ID asc',drv='SQLite')
                        
                        
                        
                        
                        bbbfpnts$ID1=seq.int(nrow(bbbfpnts))
                        
                        sssfpnts$ID1=seq.int(nrow(sssfpnts))
                        
                        
                        twopr=sqldf('SELECT a.addy as st,a.ID as bu, b.ID as su from bbbfpnts a join sssfpnts b  on  a.ID1=b.ID1 order by a.ID asc',drv='SQLite')
                        
                        twoprf=sqldf('SELECT * from fpnt a join twopr b  on  a.ID>=b.bu and a.ID<=b.su order by a.ID asc',drv='SQLite')
                        
                        
                        
                        twoprf=as.data.frame(twoprf)
                        
                        twoprf=sqldf('SELECT distinct * from twoprf ',drv='SQLite')
                        
                        trwq=sqldf('SELECT distinct  bu,st from twoprf ',drv='SQLite')
                        
                        trwq=sqldf('SELECT distinct  b.*,a.haddy as st1 from twoprf a join trwq b on a.ID=b.bu  ',drv='SQLite')
                        
                        
                        twoprf=sqldf('SELECT distinct  a.*, ((a.haddy-b.st1)*100/b.st1) as diff from twoprf a join trwq b on a.bu=b.bu  ',drv='SQLite')
                        
                        
                        bsu=sqldf('SELECT distinct  bu,su from twoprf ',drv='SQLite')
                        
                        
                        twop=sqldf('SELECT a.*, a.ID-a.bu as length from twoprf a where diff>0.25  and round(((a.addy-a.oaddy)*100/a.oaddy),2) >0 and round(((a.addy-a.haddy)*100/a.addy),2) < -(0.05) and round(((a.oaddy-a.laddy)*100/a.oaddy),2) >(0.05)  order by ID asc',drv='SQLite')
                        
                        
                        
                        b4le=sqldf('SELECT su,min(ID) as bu from twop  group by su',drv='SQLite')
                        
                        twoprf=sqldf('SELECT a.* from twoprf a join b4le b on a.ID>=b.bu and a.ID<=b.su  order by ID asc',drv='SQLite')
                        
                        
                        bbfpnts=sqldf('SELECT a.date,a.stock,a.ID,"BUY" as decision,a.addy from twoprf a join b4le b on a.ID=b.bu order by ID asc',drv='SQLite')
                        
                        ssfpnts=sqldf('SELECT a.date,a.stock,a.ID,"SELL" as decision,a.addy from twoprf a join b4le b on a.ID=b.su order by ID asc',drv='SQLite')
                        
                        
                        bbfpnts$ID1=seq.int(nrow(bbfpnts))
                        
                        ssfpnts$ID1=seq.int(nrow(ssfpnts))
                        
                        twopr=sqldf('SELECT a.addy as st,a.ID as bu, b.ID as su from bbfpnts a join ssfpnts b  on  a.ID1=b.ID1 order by a.ID asc',drv='SQLite')
                        twoprf=sqldf('SELECT a.*,b.*,((a.addy-b.st)*100/b.st) as diff from fpnt a join twopr b  on  a.ID>=b.bu and a.ID<=b.su order by a.ID asc',drv='SQLite')
                        
                        
                        bsu=sqldf('SELECT distinct  bu,su from twoprf ',drv='SQLite')
                        
                        
                        sm=1
                        smaaa=1
                        for(i in 1: nrow(bsu))
                        {
                          bu=bsu[i,1]
                          
                          su=bsu[i,2]
                          bu=as.data.frame(bu)
                          su=as.data.frame(su)
                          
                          dsu=sqldf('SELECT distinct a.* from twoprf a join bu b on a.bu=b.bu join su c on a.su=c.su ',drv='SQLite')
                          add=dsu$addy
                          if(length(add)>2)
                          {
                            
                            smadd=EMA(add,2)
                            smadd[is.na(smadd)]=add[1:sum(is.na(smadd))]
                            
                            smadd[1]=add[1]
                            smadd[2]=add[2]
                            smadd= smadd-0.0005*smadd
                          }
                          else{
                            smadd=add
                            smadd= smadd-0.0005*smadd
                          }
                          
                          signal <- ifelse(add >smadd, 1, 0)
                          sm=add*signal
                          
                          
                          smaaa<-c(smaaa,sm)
                          
                        }
                        
                        smaaa=smaaa[2:length(smaaa)]
                        
                        twoprf$addy=smaaa
                        twoprf$addy=   as.numeric(twoprf$add)
                        twoprf=as.data.frame(twoprf)
                        twoprf1=sqldf('SELECT * from twoprf where addy=0',drv='SQLite')
                        
                        ssid=sqldf('SELECT su,min(ID) as  mID from twoprf1 group by su ',drv='SQLite')
                        pbid=sqldf('SELECT su,max(ID) as  mID from twoprf1 group by su ',drv='SQLite')
                        
                        
                        ssfpnts=sqldf('SELECT a.date,a.stock,a.ID, a.decision,a.addy from ssfpnts a where ID not in (SELECT su from ssid) ',drv='SQLite')
                        ssfpnts$date <-as.POSIXct(strptime(ssfpnts$date,  "%Y-%m-%d %H:%M:%S"))+0.5*60
                        
                        
                        cpsts=sqldf('SELECT a.date,a.stock,a.ID, "SELL" as decision,a.addy from fpnt a join ssid b  on a.ID=b.mID order by a.ID asc',drv='SQLite')
                        cpsts$date <-as.POSIXct(strptime(cpsts$date,  "%Y-%m-%d %H:%M:%S"))
                        
                        
                        ssfpnts=rbind(ssfpnts,cpsts)
                        
                        ssfpnts=sqldf('SELECT * from ssfpnts order by ID asc',drv='SQLite')
                        
                        
                        ssfpnts$ID1=seq.int(nrow(ssfpnts))
                        
                        
                        bfpnts=rbind(bfpnts,bbfpnts)
                        sfpnts=rbind(sfpnts,ssfpnts)
                        
                        ssfpnts=sqldf('SELECT * from ssfpnts  ',drv='SQLite')
                        
                        
                        
                        
                      },
                      error=function(e){
                        
                        bfpnts=bfpnts
                        sfpnts=sfpnts
                        
                      })
                      
                      
                      bfpnts=sqldf('SELECT * from bfpnts order by ID asc',drv='SQLite')
                      
                      sfpnts=sqldf('SELECT * from sfpnts order by ID asc',drv='SQLite')
                      
                      
                      bfpnts$ID1=seq.int(nrow(bfpnts))
                      
                      sfpnts$ID1=seq.int(nrow(sfpnts))
                      
                    }
                    
                    
                    sfpnts1=sqldf('SELECT distinct  * from sfpnts where ID  in (select ID from orsfpnts) order by ID ',drv='SQLite')
                    
                    sfpnts1$date=as.character(sfpnts1$date)
                    sfpnts1=sqldf('SELECT distinct  * from sfpnts1 where  date like "%:30%" order by ID ',drv='SQLite')
                    sfpnts1$date <-as.POSIXct(strptime(sfpnts1$date,  "%Y-%m-%d %H:%M:%S"))
                    
                    sfpnts2=sqldf('SELECT distinct  * from orsfpnts where ID  in (select ID from sfpnts1) order by ID ',drv='SQLite')
                    
                    sfpnts=sqldf('SELECT distinct  * from sfpnts where ID not  in (select ID from sfpnts2) order by ID ',drv='SQLite')
                    
                    
                    sfpnts=rbind(sfpnts,sfpnts2)
                    
                    
                    sfpnts=sqldf('SELECT * from sfpnts order by ID asc',drv='SQLite')
                    
                    
                    sfpnts$ID1=seq.int(nrow(sfpnts))
                    
                    
                    
                    tryCatch({
                      bfpnts=sqldf('SELECT * from bfpnts  where ID not in (SELECT ID from remo ) ',drv='SQLite')
                      
                    },
                    error=function(e){
                      
                      bfpnts=bfpnts
                      
                    })
                    
                    
                    
                    sfpnts=sqldf('SELECT b.* from bfpnts a join sfpnts b  on a.ID1=b.ID1 ',drv='SQLite')
                    
                    
                    llp<-nrow(bfpnts)
                    
                    thp=sqldf('SELECT a.addy,b.addy,((b.addy-a.addy))*100/a.addy as value from bfpnts a join sfpnts b  on a.ID1=b.ID1 ',drv='SQLite')
                    
                    
                    totp=sqldf('SELECT a.ID1,((b.addy-a.addy))*100/a.addy as value from bfpnts a join sfpnts b  on a.ID1=b.ID1 ',drv='SQLite')
                    
                    
                    
                    ii=table(((totp$value))>=0)
                    
                    jj=as.data.frame(ii)  
                    
                    
                    
                    fjj=sqldf('SELECT Freq from jj where Var1="TRUE" ',drv='SQLite')
                    
                    
                    totp<- sum(as.numeric(as.character(totp$value)))
                    
                    
                    lbls <- as.numeric(llp)
                    notrd<-as.numeric(100)
                    
                    
                    pct <- as.numeric(totp)
                    
                    
                    invested=notrd*sum(as.numeric(as.character(bfpnts$addy)))
                    
                    currentval=notrd*sum(as.numeric(as.character(sfpnts$addy)))
                    
                    profit=currentval-invested
                    
                    ppr=(fjj)*100/lbls
                    
                    
                    
                    
                    tstacc=cbind(symbol,lbls,notrd,pct,curr,invested,currentval,profit,fjj,ppr,mj)
                    
                    
                    
                    
                    colnames(tstacc)=c("symbol","TTRD","NOTRD","PLP","Price","InAmount","Netvalue","PLoss","GdTrades","GtrPerc","model")
                    #vstacc<<-tstacc
                    
                    
                    tstacc=sqldf('SELECT distinct A.model,A.plp,A.price,A.TTRD,A.GdTrades,A.GtrPerc from tstacc   A ',drv='SQLite')
                    
                    
                    bpnts <-as.POSIXct(strptime(bfpnts$date,  "%Y-%m-%d %H:%M:%S"))+0*3600
                    bpnts=c(bpnts)
                    spnts <-as.POSIXct(strptime(sfpnts$date, "%Y-%m-%d %H:%M:%S"))+0*3600
                    spnts=c(spnts)
                    
                    bcfpnts=cbind(yoursym2,bfpnts)
                    
                    scfpnts=cbind(yoursym2,sfpnts)
                    
                    
                    bcfpnts=cbind(bcfpnts,tstacc)
                    
                    scfpnts=cbind(scfpnts,tstacc)
                    
                    colnames(bcfpnts)=c("symbol","Date","stock","ID","decision","bval","ID1","model","perc","curr","Ttrd","Gdtrd","Gtrdpr")
                    
                    
                    colnames(scfpnts)=c("symbol","Date","stock","ID","decision","sval","ID1","model","perc","curr","Ttrd","Gdtrd","Gtrdpr")
                    
                    buy<-as.data.frame(bcfpnts)
                    
                    
                    sell<-as.data.frame(scfpnts)
                    
            
                    buy$ID=seq.int(nrow(buy))
                    
                    sell$ID=seq.int(nrow(sell))
                    
                    
                    buy$Date=as.character(buy$Date)
                    sell$Date=as.character(sell$Date)
                    
                    common <-  sqldf('SELECT distinct a.symbol,a.model,a.date as date2,b.date as date1,a.Ttrd as T,a.bval,b.sval,a.curr,round((b.sval-a.bval)*100/a.bval,2) as cper,round(a.perc,2) as tper ,a.Gdtrd as S,round(a.Gtrdpr,2) as Gpr FROM buy a join sell b on a.symbol=b.symbol and a.ID=b.ID order by a.perc   desc ',drv='SQLite')
                    
                    common <-  sqldf('SELECT distinct * FROM common   order by date2 desc ',drv='SQLite')
                    vstacc<-rbind(vstacc,common)
                    
                    
                   vstacc=sqldf('SELECT * FROM  vstacc  where (Gpr*tper) in (SELECT max(Gpr*tper) as Gpr FROM  vstacc where symbol<>"PSXP" ) ',drv='SQLite')
                  print(vstacc)

                   vstacc<<-vstacc
                  }
                  
                  }
                  
                  
                  
                  ,
                  error=function(e){
                    tj<<-tj+1
                    
                  })}
            }
          }
          
          else
          {
            
            
            vstacc<<-read.table(file="vstcmnn.csv",header=TRUE,sep=",") 
            cmnn<-read.table(file="dcmnn.csv",header=FALSE,sep=",") 
            
            
         #   allsym="TSLA"
            
            jsym=as.data.frame(allsym)
            colnames(jsym)=c("symbol")
            jsym$symbol=toupper(jsym$symbol)
         
            
            cmnn=sqldf('SELECT a.* from cmnn  a join jsym b on a.V2=b.symbol ',drv='SQLite')
         
            if(nrow(cmnn)>1)
            {
              mj=as.character(cmnn$V3)
            }
            else
            {
              mj=as.character(input$Model)
              
            }
            
            {
              tryCatch({
                
                
                
                
                
                kj= eval(parse(text=mj))
                
                
                
                {     
                  kj<- as.matrix(kj)
                  
                  
                  
                  for(i in 1:nrow(kj)){
                    
                    
                    if (as.numeric(kj[i,1])<(2))
                      
                    {
                      
                      
                      kj[i,1]=0
                    } else{
                      
                      kj[i,1]=1
                    }}
                  kj<- as.matrix(kj)
                  
                 ii10=as.matrix(ii10)
                 ii11=as.matrix(ii11)

                 kj=kj+ii10
                  for(i in 1:nrow(kj)){


                    if (as.numeric(kj[i,1])<(2))

                    {


                      kj[i,1]=0
                    } else{

                      kj[i,1]=1
                    }}
                  kj<- as.matrix(kj)
                  kj=ii10+ii11

                  for(i in 1:nrow(kj)){


                    if (as.numeric(kj[i,1])<(2))

                    {


                      kj[i,1]=0
                    } else{

                      kj[i,1]=1
                    }}
                  kj<- as.matrix(kj)

                  bb=as.numeric(input$Ngap)
                  i12=SMA(i10,bb)
                  
                  i12[is.na(i12)]=0
                  
                  i12<- as.matrix(i12)
                  print(nrow(i12))
                  
                  
                  
                  x=input$NOTRD
                  if(x==1)
                  {     
                    
                    i11=i12+kj
                  }
                  if(x==2)
                  {
                    i11=i12
                  }
                  if(x==3)
                  {
                    i11=kj
                  }
                
                  
                  kj<- as.matrix(i11)
                  nd= input$Soff
                  
                  kj1=as.data.frame(cbind(kj,nd))
                  
                  
                  colnames(kj1)=c("kj","nd")
                  
                  x <- kj1
                  
                  i11=apply(x, 1, cave,  c1 = "kj", c2 ="nd")
                  
                  i11<- as.matrix(i11)
                  
                  
                  
                  
                  
                  ind=i11
                  # corre<-cbind(ind1,ind2,ind3,ind4,ind5,ind6,ind7,ind8,ind9,kjlt)
                  
                  #  verri<<-corre
                  
           
                  bmkReturns <-  (diff(a)*100)/a[-length(a)]
                  bmkReturns=cbind(stk1[2:nrow(stk1),0],bmkReturns)
                  bmkReturns1=as.xts(as.data.frame(bmkReturns))
                  myReturnsc <- as.matrix(1*ind[2:length(ind)])
                  myReturnsc=cbind(stk1[2:nrow(stk1),0],myReturnsc)
                  
                  myReturnsc=as.xts(as.data.frame(myReturnsc))
                  
                  names(bmkReturns1) <- 'stock'
                  
                  names(myReturnsc) <- 'Me'
                  
                  bmkReturns2=cumsum(bmkReturns1)
                  
                  
                  myReturns2c=(myReturnsc)
                  lungDeaths= as.xts(cbind(bmkReturns2, myReturns2c),tz="US/Eastern")
                  
                  
                  .index(lungDeaths)[1:nrow(lungDeaths)] <-(.index(lungDeaths)[1:nrow(lungDeaths)] + 5*3600)
                  
                  addy=as.numeric(as.character(test$close))
                  
                  
                  addy=addy[2:(length(addy))]
                  
                  haddy=as.numeric(as.character(test$High))
                  
                  
                  haddy=haddy[2:(length(haddy))]
                  
                  
                  laddy=as.numeric(as.character(test$Low))
                  
                  
                  laddy=laddy[2:(length(laddy))]
                  
                  oaddy=as.numeric(as.character(test$Open))
                  
                  
                  oaddy=oaddy[2:(length(oaddy))]
                  
                  
                  
                  lungDeaths5<-as.xts(cbind(lungDeaths,addy,haddy,laddy,oaddy ),tz="US/Eastern")
                  lungDeaths5[1,2]=0
                  
                  kk<-lungDeaths5
                  updn <- c(kk$Me)
                  
                  updn3=rbind(updn[nrow(updn),])
                  
                  updn3$Me=0
                  
                  updn=rbind(updn,updn3)
                  
                  ix1 <- which(updn == 0)
                  
                  updn1 <- c(diff(ix1))
                  
                  ix <- which(updn1 != 1)
                  BID=as.data.frame(as.matrix(c(ix1[ix])))
                  ix0 <- which(updn1 == 1)
                  MBID=as.data.frame(as.matrix(c(ix1[ix])))
                  
                  if(max(MBID)==max(BID))
                  {
                    
                  } else     
                  {
                    BID=  rbind(BID,max(MBID))
                  }
                  
                  
                  
                  updn2 <- c(kk$Me)
                  
                  updn3=rbind(updn2[nrow(updn2),])
                  
                  updn3$Me=1
                  
                  
                  
                  updn2=rbind(updn2,updn3)
                  
                  updn3=rbind(updn2[nrow(updn2),])
                  
                  updn3$Me=0
                  
                  
                  
                  updn2=rbind(updn2,updn3)
                  
                  ix11 <- which(updn2 == 1)
                  
                  updn2 <- c(diff(ix11))
                  ix10 <- which(updn2 != 1)
                  SID=as.data.frame(as.matrix(c(ix11[ix10])))
                  
                  
                  fpnt<-data.frame(date=index(lungDeaths5), coredata(lungDeaths5))
                  
                  fpnt=as.data.frame(fpnt)
                  
                  
                  fpnt$ID=seq.int(nrow(fpnt))-1
                  fpnt$date <-as.POSIXct(strptime(fpnt$date,  "%Y-%m-%d %H:%M:%S"))+4.5*3600
                  
                  fpntSS<<-fpnt
                  
                  
                  best<-sqldf('SELECT distinct * FROM fpnt where addy>0  order by Date asc ',drv='SQLite')
                  
                  uals=as.matrix(best$addy)
                  
                  uals=as.numeric(uals[,1]) 
                  
                  
                  nx=-(1:(nrow(best)))
                  
                  
                  src=uals[-nx]
                  
                  
                  uals=as.matrix(best$laddy)
                  
                  uals=as.numeric(uals[,1]) 
                  
                  
                  nx=-(1:(nrow(best)))
                  
                  
                  Low=uals[-nx]
                  
                  
                  uals=as.matrix(best$haddy)
                  
                  uals=as.numeric(uals[,1]) 
                  
                  
                  nx=-(1:(nrow(best)))
                  
                  
                  High=uals[-nx]
                  
                  
                  
                  
                  
                  
                  #Hull Lengths
                  length1 = 14 
                  length2 = 50
                  length3 = 100
                  
                  #StdDev Multiplier
                  mult =0.5
                  
                  
                  
                  hullma14 = WMA(2*WMA(src, length1/2)-WMA(src, length1), round(sqrt(length1)))
                  hullma50 = WMA(2*WMA(src, length2/2)-WMA(src, length2), round(sqrt(length2)))
                  hullma100 = WMA(2*WMA(src, length3/2)-WMA(src, length3), round(sqrt(length3)))
                  
                  
                  dev =mult*rollapply(src, width = 50, FUN = sd, na.rm = TRUE)
                  
                  
                  upper = hullma50[(length(hullma50)-(length(dev)-1)):length(hullma50)] + dev
                  
                  lower =  hullma50[(length(hullma50)-(length(dev)-1)):length(hullma50)] - dev
                  
                  
                  
                  # plot(hullma14,type='l', col="green", title="Hullma-14")
                  # par(new = T)
                  # plot(hullma50,type='l', col="blue", title="Hullma-50")
                  # par(new = T)
                  # plot(hullma100, type='l', col="orange", title="Hullma-100")
                  # par(new = T)
                  # plot(upper,  type='l', col="blue", title="Hullma50 Upper")
                  # par(new = T)
                  #  plot(lower,  type='l', col="blue", title="Hullma50 Lower")
                  
                  
                  
                  
                  cup= ifelse(crossover(hullma14, hullma100)=="UP",1,0)
                  cup[is.na(cup)] <- 0
                  
                  buy1 = ifelse(((hullma50 >= hullma100)  ),1,2)
                  buy1 = ifelse(((buy1 == cup)  ),1,0)
                  
                  cup1= ifelse(crossover(hullma14, hullma50)=="UP",1,0)
                  cup1[is.na(cup1)] <- 0
                  
                  
                  buy2 = ifelse(((hullma100 >= hullma50)  ),1,2)
                  buy2 = ifelse(((buy2 == cup1)  ),1,0)
                  
                  buy=  buy1+buy2
                  
                  
                  
                  cup= ifelse(crossover(hullma14, hullma100)=="DN",1,0)
                  cup[is.na(cup)] <- 0
                  
                  
                  sell1 = ifelse(((hullma50 >= hullma100)  ),1,2)
                  sell1 = ifelse(((sell1 == cup)  ),1,0)
                  
                  cup1= ifelse(crossover(hullma14, hullma50)=="DN",1,0)
                  cup1[is.na(cup1)] <- 0
                  
                  
                  sell2 = ifelse(((hullma100 >= hullma50)  ),1,2)
                  sell2 = ifelse(((sell2 == cup1)  ),1,0)
                  
                  sell=  sell1+sell2   
                  buy[is.na(buy)] <- 0
                  sell[is.na(sell)] <- 0
         
                  bfpnts=   cbind(best,buy)
                  sfpnts=   cbind(best,sell)
                  
                  
                  bfpnts=sqldf('SELECT distinct a.date,a.stock,a.ID, "BUY" as decision,a.addy from bfpnts a where buy=1  order by a.ID asc',drv='SQLite')
                  
                  sfpnts=sqldf('SELECT distinct a.date,a.stock,a.ID, "SELL" as decision,a.addy from sfpnts a where  sell=1 order by a.ID asc',drv='SQLite')
               
                  if(bfpnts[1,1]<sfpnts[1,1])
                  {
                    bfpnts$ID1=seq.int(nrow(bfpnts))
                    
                    sfpnts$ID1=seq.int(nrow(sfpnts))
                  }else
                  {
                    bfpnts$ID1=seq.int(nrow(bfpnts))
                    
                    sfpnts$ID1=seq.int(nrow(sfpnts))-1
                    
                    sfpnts=sqldf('SELECT * from sfpnts a where  ID1<>0 order by a.ID1 asc',drv='SQLite')
                    
                  }
                
            
                
                if(nrow(bfpnts)>nrow(sfpnts))
                {
                  
                  gfpnts=sqldf('SELECT a.date,a.stock,a.ID, "SELL" as decision,a.addy from fpnt a order by a.ID asc',drv='SQLite')
                  
                  gfpnts=gfpnts[nrow(gfpnts),]
                  
                  gfpnts$date <-as.POSIXct(strptime(gfpnts$date, "%Y-%m-%d %H:%M:%S"))
                  
                  gfpnts$ID1=seq.int(nrow(gfpnts))
                  sfpnts=rbind(sfpnts,gfpnts)
                  sfpnts$ID1=seq.int(nrow(sfpnts))
                  q=1
                }
                  
              
                  
                  
                  orbfpnts=bfpnts
                  orsfpnts=sfpnts
                  
                  bbbfpnts=bfpnts
                  sssfpnts=sfpnts
                  
                  
                  tryCatch({
                    
                    twopr=sqldf('SELECT a.addy as st,a.ID as bu, b.ID as su from bbbfpnts a join sssfpnts b  on  a.ID1=b.ID1 order by a.ID asc',drv='SQLite')
                    
                    twoprf=sqldf('SELECT * from fpnt a join twopr b  on  a.ID>=b.bu and a.ID<=b.su order by a.ID asc',drv='SQLite')
                    
                    
                    twoprf=as.data.frame(twoprf)
                    
                    
                    bsu=sqldf('SELECT distinct  bu,su from twoprf ',drv='SQLite')
                    print(twoprf)
                    print(bsu)
                    sm=1
                    smaaa=1
                    for(i in 1: nrow(bsu))
                    { 
                      bu=bsu[i,1]
                      
                      su=bsu[i,2]
                      bu=as.data.frame(bu)
                      su=as.data.frame(su)
                      
                      dsu=sqldf('SELECT distinct a.* from twoprf a join bu b on a.bu=b.bu join su c on a.su=c.su ',drv='SQLite')
                      print(dsu)
                      add=dsu$addy
                      smadd=dsu$laddy
                      smadd=smadd[1]
                      
                      
                      xadd=dsu$addy
                      xsmad=dsu$laddy
                      xsoad=dsu$oaddy
                      xsmadl=dsu$laddy
                      xsmadll <- ifelse(xadd <=xsoad, xsmadl, smadd)
                      
                      signal <- ifelse(xsmadll >=smadd, 1, 0)
                      
                      signal1=signal
                      print(signal1)
                      if(nrow(dsu)>2)
                      {
                        tryCatch({
                          for(j in 3: nrow(dsu))
                          { 
                            
                            xsmadd=     xsmad[(j-2):j]
                            xaddd=     xadd[(j-2):j]
                            xsoadd=     xsoad[(j-2):j]
                            
                            
                            xsmadd1=xsmadd[length(xsmadd)]
                            xsmadd2=xsmadd[(length(xsmadd))-1]
                            xsmadd3=xsmadd[(length(xsmadd))-2]
                            
                            xol=(xsmadd1+xsmadd2+xsmadd3)/3 -0.002*xsmadd1
                            
                            if(xol>xsmadd1)
                            {
                              
                              signal1[j]=0
                              print("CC1")
                              cc
                           
                            }else {
                              signal1[j]=1
                            }
                            
                            xco1=xaddd[length(xaddd)]-xsoadd[length(xsoadd)]
                            xco2=xaddd[(length(xaddd))-1]-xsoadd[(length(xsoadd))-1]
                            xco3=xaddd[(length(xaddd))-2]-xsoadd[(length(xsoadd))-2]
                            
                            
                            if(xco1<0)
                            {
                              if(xco2>0)
                              {
                                if(xco3>0)
                                {
                                  xco=xco2+xco3
                                  
                                  
                                  if(xco<(-(xco1)))
                                  {
                                    
                                    signal1[j]=0
                                    print("CC")
                                    cc
                                   
                                  }else {
                                    signal1[j]=1
                                  }
                                }
                              }
                              
                            }
                            print(xsmadd1)
                            print(xsmadd2)
                            print(xsmadd3)
                            if(((xsmadd3>xsmadd2)&&(xsmadd2>xsmadd1)&&(xsmadd3>xsmadd1)&&(xco1<0)&&(xco2<0)&&(xco3<0)))
                            {
                              
                              signal1[j]=0
                              cc
                            }else {
                              signal1[j]=1
                            }
                            
                            
                            
                            
                          }  
                        },
                        error=function(e){
                          
                          
                        })
                        
                        
                        signal2=signal+signal1
                        print(signal2)
                        
                        for(r in 1:length(signal2)){
                          
                          
                          if (as.numeric(signal2[r])>(1))
                            
                          {
                            
                            
                            signal2[r]=1
                          } else{
                            
                            signal2[r]=0
                          }}
                        
                      }
                      else{
                        signal2=signal
                      }
                      
                      sm=add*signal2
                      
                      print(sm)
                      smaaa<-c(smaaa,sm)
                      print(smaaa)
                    }
                    
                    print(signal2)
                    
                    smaaa=smaaa[2:length(smaaa)]
                    
                    twoprf$addy=smaaa
                    
                    print(twoprf)
                    twoprf$addy=   as.numeric(twoprf$add)
                    twoprf=as.data.frame(twoprf)
                    twoprf1=sqldf('SELECT * from twoprf where addy=0',drv='SQLite')
                    
                    print(twoprf1)
                    
                    ssid=sqldf('SELECT su,min(ID) as  mID from twoprf1 group by su ',drv='SQLite')
                    pbid=sqldf('SELECT su,max(ID) as  mID from twoprf1 group by su ',drv='SQLite')
                    
                    
                    print(ssid)
                    print(pbid)
                    ssfpnts=sqldf('SELECT a.date,a.stock,a.ID, a.decision,a.addy from sssfpnts a where ID not in (SELECT su from ssid) ',drv='SQLite')
                    
                    print(ssfpnts)
                    cpsts=sqldf('SELECT a.date,a.stock,a.ID, "SELL" as decision,a.addy from fpnt a join ssid b  on a.ID=b.mID order by a.ID asc',drv='SQLite')
                    cpsts$date <-as.POSIXct(strptime(cpsts$date,  "%Y-%m-%d %H:%M:%S"))+0.5*60
                    ssfpnts=rbind(ssfpnts,cpsts)
                    print("ssfpnts")
                    print(ssfpnts)
                    ssfpnts=sqldf('SELECT * from ssfpnts order by ID asc',drv='SQLite')
                    
                    
                    ssfpnts$ID1=seq.int(nrow(ssfpnts))
    
                    bfpnts=bbbfpnts
                    sfpnts=ssfpnts
                    
                    print(bfpnts)
                    print(sfpnts)
                    
                  },
                  error=function(e){
                    
                    bfpnts=bfpnts
                    sfpnts=sfpnts
                    
                  }) 
                  
      
                  
                  bfpnts=sqldf('SELECT * from bfpnts order by ID asc',drv='SQLite')
                  
                  sfpnts=sqldf('SELECT * from sfpnts order by ID asc',drv='SQLite')
                  
                  
                  bfpnts$ID1=seq.int(nrow(bfpnts))
                  
                  sfpnts$ID1=seq.int(nrow(sfpnts))
                  
                  pcih= sssfpnts
                  
                    
                  bfpnts$date <-as.POSIXct(strptime(bfpnts$date,  "%Y-%m-%d %H:%M:%S"))-0.5*60
                  
                  
                  sfpnts=sqldf('SELECT b.* from bfpnts a join sfpnts b  on a.ID1=b.ID1 ',drv='SQLite')
                  
                    
                  llp<-nrow(bfpnts)
                  
                  thp=sqldf('SELECT a.addy,b.addy,((b.addy-a.addy))*100/a.addy as value from bfpnts a join sfpnts b  on a.ID1=b.ID1 ',drv='SQLite')
                  
                  
                  totp=sqldf('SELECT a.ID1,((b.addy-a.addy))*100/a.addy as value from bfpnts a join sfpnts b  on a.ID1=b.ID1 ',drv='SQLite')
                  
                  
               
                  ii=table(((totp$value))>=0)
                  
                  jj=as.data.frame(ii)  
                  
                  
                  
                  fjj=sqldf('SELECT Freq from jj where Var1="TRUE" ',drv='SQLite')
          
                  totp<- sum(as.numeric(as.character(totp$value)))
                  
                  
                  lbls <- as.numeric(llp)
                  notrd<-as.numeric(100)
                  
                  
                  pct <- as.numeric(totp)
                  
                  
                  invested=notrd*sum(as.numeric(as.character(bfpnts$addy)))
                  
                  currentval=notrd*sum(as.numeric(as.character(sfpnts$addy)))
                  
                  profit=currentval-invested
                  
              
                  
                  if(nrow(fjj)==0){
                
                    tstacc=cbind(symbol,lbls,notrd,pct,curr,invested,currentval,profit,mj)
                    colnames(tstacc)=c("symbol","TTRD","NOTRD","plp","price","InAmount","Netvalue","PLoss","model")
                    tstacc=as.data.frame(tstacc)
                 
                    tstacc=sqldf('SELECT distinct A.model,A.plp,A.price,A.TTRD , 0 as GdTrades,0 as GtrPerc from tstacc   A ',drv='SQLite')
               
                  }else{
                    ppr=(fjj)*100/lbls
                    tstacc=cbind(symbol,lbls,notrd,pct,curr,invested,currentval,profit,fjj,ppr,mj)
                    colnames(tstacc)=c("symbol","TTRD","NOTRD","PLP","Price","InAmount","Netvalue","PLoss","GdTrades","GtrPerc","model")
                    tstacc=sqldf('SELECT distinct A.model,A.plp,A.price,A.TTRD,A.GdTrades,A.GtrPerc from tstacc   A ',drv='SQLite')
                    
                  }
                  
                  #vstacc<<-tstacc
               
                  
          
                  bpnts <-as.POSIXct(strptime(bfpnts$date,  "%Y-%m-%d %H:%M:%S"))
                  bpnts=c(bpnts)
           
                  spnts <-as.POSIXct(strptime(sfpnts$date, "%Y-%m-%d %H:%M:%S"))
                  spnts=c(spnts)
                  
                  tryCatch({
                    
                  nf<-(fpnt[nrow(fpnt),1])+5*3600
                
                  tts=bpnts
                  # write.table(bpnts, "/reports/temp/buy.csv", col.names=FALSE,sep=",",append=FALSE)
                  #write.table(symbol2, "/reports/temp/buydr.csv", col.names=FALSE,sep=",",append=TRUE)
                  
                  LTS=tts[length(tts)]
                 
                  if(gts1==LTS)
                  {
                    print("no change")
                  } else
                  {
                    x=symbol
                    toaddrs=input$Ngap1
                
                  
                 
                    y=strptime(LTS[length(LTS)], "%Y-%m-%d %H:%M:%S")
                    y= as.character(y)
                
              
                  }
                  gts1<<-(tts[length(tts)])

        
            
                  if(q==0)
                  {
                  
                    shts<-spnts
                    LTS1=shts[length(shts)]


                    delay=difftime(nf,LTS1[length(LTS1)],units = "mins")

                    dl=as.numeric(delay, units="mins")

              

                    if((gsts1==LTS1[length(LTS1)]))
                    {

                      print("not changed")
                    } else
                    {
                      x=symbol
                      toaddrs=input$Ngap1
               
                      y=strptime(LTS1[length(LTS1)], "%Y-%m-%d %H:%M:%S")
                      y= as.character(y)
                    
                    }



                    gsts1<<-(shts[length(shts)])

                  }


                  
                  },
           error=function(e){
             
             
                  })    
                  
                  
                  bcfpnts=cbind(yoursym2,bfpnts)
           
                  scfpnts=cbind(yoursym2,sfpnts)
               
                  
                  bcfpnts=cbind(bcfpnts,tstacc)
                  
                  scfpnts=cbind(scfpnts,tstacc)
                  
             
                  colnames(bcfpnts)=c("symbol","Date","stock","ID","decision","bval","ID1","model","perc","curr","Ttrd","Gdtrd","Gtrdpr")
                  
                  
                  colnames(scfpnts)=c("symbol","Date","stock","ID","decision","sval","ID1","model","perc","curr","Ttrd","Gdtrd","Gtrdpr")
                  
                  buy<-as.data.frame(bcfpnts)
                  
                  
                  sell<-as.data.frame(scfpnts)
                  
              
                  buy$ID=seq.int(nrow(buy))
                  
                  sell$ID=seq.int(nrow(sell))
                  
                  
                  buy$Date=as.character(buy$Date)
                  sell$Date=as.character(sell$Date)
                  
                  common <-  sqldf('SELECT distinct a.symbol,a.model,a.date as date2,b.date as date1,a.Ttrd as T,a.bval,b.sval,a.curr,round((b.sval-a.bval)*100/a.bval,2) as cper,round(a.perc,2) as tper ,a.Gdtrd as S,round(a.Gtrdpr,2) as Gpr FROM buy a join sell b on a.symbol=b.symbol and a.ID=b.ID order by a.perc   desc ',drv='SQLite')
                  
                  common <-  sqldf('SELECT distinct * FROM common   order by date2 desc ',drv='SQLite')
                  mstacc=as.data.frame(common)
          
                        
                  dt1 <-as.POSIXct(strptime(mstacc$date2,  "%Y-%m-%d %H:%M:%S"))
                  dt2 <-as.POSIXct(strptime(mstacc$date1,  "%Y-%m-%d %H:%M:%S"))
                  
                  dt=dt2-dt1
                  
                  mstacc$len=as.numeric(dt)
                  
                print(mstacc)
          
                  # mstacc=mstacc[1:10,]
                  write.table(mstacc, "accinfo_tot.csv", col.names=TRUE,row.names=FALSE,sep=",",append=FALSE)
                  
                }
                }
                
                
                
                ,
                error=function(e){
                  
                  
                })}
          }
          
          
          
          Names1 <- character(length(spnts))
          for(i in seq_along(bpnts)){
            Names1[i] <- paste0("spnts[", i, "]") 
          }
          
          
          nms1 <- character(length(spnts))
          for(i in seq_along(bpnts)){
            nms1[i] <- paste0("sts[", i, "]") 
          }
          
          nms111=Names1
          
          Names <- character(length(bpnts))
          for(i in seq_along(bpnts)){
            Names[i] <- paste0("bpnts[", i, "]") 
          }
          
          nms <- character(length(bpnts))
          for(i in seq_along(bpnts)){
            nms[i] <- paste0("bts[", i, "]") 
          }
          
     
          
          nms112=Names
          bpnts=(bpnts+1*3600)
          spnts=(spnts+1*3600)
          cek=sqldf('SELECT max(Date) as Date FROM mest ',drv='SQLite')
          dm1<-as.POSIXct(strptime(cek$Date, "%Y-%m-%d %H:%M:%S"),tz="EST")+10.5*3600
          
          .index(stock3)[1:nrow(stock3)] <-(.index(stock3)[1:nrow(stock3)] )+10.5*3600
          
          
          

          if(input$Soff1==1)
          {
          my_code<-paste("dygraph(stock3)%>%dyCandlestick()%>%dyRoller(rollPeriod = 0)%>%")
          y='"y"'
          
          dfg<-paste("dyShading(from=",nms112,",to=",nms111,",color = ",'"#FFE6E6"',")",collapse = " %>% ")
          wtst<- paste("dyShading(from=",minu,",to=",maxu,",axis =",y, ",","color = ",'"#CCEBD6"')
          
          etst=paste("%>% dyRangeSelector(height = 20,dateWindow =c((dm1+(-4*3600)), (dm1+2*3600)))")
          tmz=paste("%>% dyOptions(useDataTimezone = TRUE)")
          my_code<-paste(my_code,wtst,")","%>%",dfg,etst,tmz)
          
          eval(parse(text = my_code))
          }else
          {   my_code<-paste("dygraph(stock3)%>%dyCandlestick()%>%dyRoller(rollPeriod = 0)%>%")
          y='"y"'
          
          dfg<-paste("dyShading(from=",nms112,",to=",nms111,",color = ",'"#FFE6E6"',")",collapse = " %>% ")
          wtst<- paste("dyShading(from=",minu,",to=",maxu,",axis =",y, ",","color = ",'"#CCEBD6"')
          
          etst=paste("%>% dyRangeSelector(height = 20,dateWindow =c((dm1+(-6*3600)), (dm1-4.5*3600)))")
          tmz=paste("%>% dyOptions(useDataTimezone = TRUE)")
          my_code<-paste(my_code,wtst,")","%>%",dfg,etst,tmz)
          
          eval(parse(text = my_code))
          }
          
          
        }
        
        
      }
      
    }
    
    
    
    
  })
  
  output$iOptionm <- renderTable({
    vg=opti()
   
      allsym=as.data.frame(as.matrix(input$stock1))
      
      colnames(allsym)=c("symbol")
      
      symbol=as.character(allsym[1,1])
      
      yoursym2=symbol
      
      
      
      syss=toupper(yoursym2)
      
      print(syss)
      request_body <- list(grant_type='refresh_token',refresh_token='7Yz6yFRKxTRs3Ow/sGz9k9SElxLRPHnYOK5f/LYwLRMsfSVb2f55MkBkrIVHFsxAtSxgwjS0U/SBtMzADA2m95Ha7EWUIC4Fckr0GBpQJ89Y7Gc3tcC0RqOQFQ6klIIEav+fgLUcjgKsB2/wcuZgf5AzJMWktVXzC2juce2tmThuyAO7q3Yci/F9YuqOuHIatlSXg8caqRm8RvCukBUaZqN5s+Z51Jh/IsiH/VAkn2ByGY81TN6Q5IM/hUH3/nl4+dxiTZhYkO9JRungENdIiUnk3DnKlCX7RsCZRscETEJFXUV6GyRXVikCznw7Tl3sVp7ttlOCGH5Q0uNHrHqhxiUYwS9QXZ5tjmcUw2H1We0uLcVYekzv/+efI5BfKq6vc8oT+nr44odPhnY/bbX+jpXU+IPnaj9KmqgYcPSnAMuDzeNSvIeVRh1O+Bz100MQuG4LYrgoVi/JHHvlqWdsPrWwCSsP3WHX8flcqbuxFyHkXhHNkiiB4T9iFVOzZgQvgLbgfYr/nQ95Z8gzwUIHTRz+lBGdSNwsIFTQllzlV+Mqp+sFUpWX8UMCGg8pSoP3XnbSL2cxaAI46gzvK5WmD9rkZJckP8pWjLiaoqr6sXa0a298ROyzK6wnBrBv5Fowa8vxNSIXNOpx0B/BJyn6bcVS/4uU78y0TVT0SeL3bRcmG23uu/t4NgcWyaDE3f5ar528KxcORTVKuNvwWEcBRDVkTTgtjNHLy/HukLAwK5N62jBDI645ZFx94f0te01pGVb0L0vqvCcM2Kg1gRx3iFDPr5RL8byb1pb+ZzyDKxuTfmuJ7GCqOoH+KTmfvJOa6KIJgFqPDZWPAj0uc5GfU7mgTlLok1wcA0WhUD9HKfBWy3Omh0T5oX73uHB1LuAFj/6TjwpdC5c=212FD3x19z9sWBHDJACbC00B75E'
                           ,client_id='OJISG1MIOXZXHU1FTH2DCIXO5YHZOQFV@AMER.OAUTHAP',
                           redirect_uri='http://localhost:8080')
      
      user_token <- httr::content(httr::POST('https://api.tdameritrade.com/v1/oauth2/token',
                                             body=request_body,
                                             encode='form'))
      
      
      
      auth_header <- httr::add_headers('Authorization'=
                                         paste('Bearer',user_token$access_token))
      
      
      base1 <- "https://api.tdameritrade.com/v1/marketdata/chains"
      daf1 <- httr::content(
        httr::GET(base1 ,query=list(symbol=syss,strikeCount='10',range='NTM'), auth_header))
      
      df1 <- data.frame(matrix(unlist(daf1), ncol=1, byrow=T),stringsAsFactors=FALSE)
      
      
      df1<- df1[-(1:11),]
      
      tryCatch({
        conv <- data.frame(matrix(df1, ncol=44, byrow=T),stringsAsFactors=FALSE)
        
        
        
      },
      error=function(e){
        
      })
      
      
  
      conv$X8=as.numeric(conv$X8)
    
      conv$X17=as.numeric(conv$X17)
      conv$X39=as.numeric(conv$X39)

      
      tst1<-  sqldf('SELECT distinct * FROM conv WHERE X3 like "%2020%" ',drv='SQLite')
      
      
      
      write.table(tst1, "option.csv", col.names=TRUE,row.names=FALSE,sep=",",append=FALSE)
      
      vg=as.data.frame(vg)
      
      print(vg)
      
        tst<-  sqldf('SELECT distinct a.X2 as symbol,b.X39 as per1,a.X39 as per2,b.X8 as mark1,a.X8 as mark2, b.X17 as tv1,a.X17 as tv2,(a.X8-b.X8)*100/b.X8 as pdiff , a.X17-b.X17 as vdiff  FROM tst1 a join  vg b on   a.X2=b.X2 order by pdiff desc ',drv='SQLite')
      tst<-  sqldf('SELECT distinct *  FROM tst where pdiff<>0 and vdiff<>0  order by pdiff desc ',drv='SQLite')
      tst
  })
  
  
  
  output$interactiveh <- renderDygraph({
    
    
    if(kv$i > 0) {
      
      
      # Convert the input$stock1 to a data frame directly
      allsym <- as.data.frame(input$stock1)
      
      # Alternatively, if input$stock1 needs to be converted to a matrix first
      # allsym <- as.data.frame(as.matrix(input$stock1))
      
      # Set the column name of allsym to "symbol"
      colnames(allsym) <- "symbol"
      
      # Extract the first element as a character
      symbol <- as.character(allsym[1, 1])
      
      # Assign the extracted symbol to yoursym2
      yoursym2 <- symbol

        # Execute SQL query to select distinct rows from 'hest' excluding a specific symbol
        # and ordering the result by date in descending order
        rest <- sqldf('SELECT DISTINCT * FROM hest WHERE symbol NOT IN ("NSE:XXX") ORDER BY Date DESC', drv = 'SQLite')
        
        # Convert the SQL query result to a data frame (though sqldf returns a data frame, ensuring clarity of intent)
        test <- as.data.frame(rest)
        
        # Print a static message
        print("THIS IS AI")
        
        # Check if Ninf is less than 1, and if so, update it with a new value from input
        if(Ninf < 1) {
          Ninf <<- as.numeric(input$Ninf)
        }
        
        # Decrease Ninf by 1 and convert to numeric if it's not already
        Ninf <<- as.numeric(Ninf) - 1
        
        # Conditionally set maxIter2 based on the input value
        if(input$somevalue2 == "TRUE") {
          maxIter2 <<- 3000
        } else {
          maxIter2 <<- 0
          Ninf <<- as.numeric(input$Ninf)
        }
        
        # Initialize dly3 and dly2 with negative values of Ninf and a constant, respectively
        dly3 <- -Ninf
        dly2 <- -5000
        
        # Convert dly3 and dly2 to data frames and set their column names to 'dly'
        dly3 <- as.data.frame(dly3)
        colnames(dly3) <- "dly"
        
        dly2 <- as.data.frame(dly2)
        colnames(dly2) <- "dly"
        
        # Add a descending sequence column to 'test' data frame
        test$dl <- -seq.int(nrow(test))
        
        # Perform a SQL join operation on 'test', 'dly2', and 'dly3', ordering by Date in descending order
        # This query joins 'test' with 'dly2' and 'dly3' based on the 'dl' and 'dly' conditions
        test <- sqldf('SELECT a.* FROM test a JOIN dly2 b ON a.dl >= b.dly JOIN dly3 c ON a.dl <= c.dly ORDER BY Date DESC', drv = 'SQLite')
        
        # Perform a simple SQL query to order 'test' by Date in descending order
        # This step might be redundant if the previous step already orders 'test' as needed
        test <- sqldf('SELECT a.* FROM test a ORDER BY Date DESC', drv = 'SQLite')
        
        # Convert the result back to a data frame, though sqldf already returns a data frame
        test <- as.data.frame(test)
        
        # Retrieve distinct records from 'test' and order by Date in ascending order
        test <- sqldf('SELECT DISTINCT * FROM test ORDER BY Date ASC', drv = 'SQLite')
        
        # Convert 'close' column to numeric
        mals <- as.numeric(test$close)
        
        # Generate a sequence of negative numbers based on the length of 'mals'
        nx <- -(length(mals):1)
        
        # Convert 'volume' column to numeric and remove elements based on 'nx'
        vcols <- as.numeric(test$volume)
        vcols <- vcols[-nx]
        
        # Calculate the mean volume
        mvol <- mean(vcols)
        
        # Use 'mals' and adjust it based on 'nx'
        bals <- mals[-nx]
        
        # Prepare 'y' and 'nx' for potential plotting
        y <- bals
        nx <- -(length(bals):1)
        
        # Placeholder for potentially adjusted 'bals' values
        tals <- bals
        
        # Attempt to calculate a 20-period simple moving average (SMA) of 'rals'
        # Note: 'rals' is not defined in the provided snippet, potentially an oversight or external variable
        tryCatch({
          gelsma20 <- SMA(rals, 5) # Assuming 'rals' should be available and SMA() is from an external library, e.g., TTR
          gelsma20 <- as.data.frame(gelsma20)
          GELSMA20 <- as.numeric(gelsma20$gelsma20)
          # Replace NA values in GELSMA20 with the first 'sum(is.na(GELSMA20))' values of 'rals'
          GELSMA20[is.na(GELSMA20)] <- rals[1:sum(is.na(GELSMA20))]
        }, error = function(e) {
          # Error handling logic here
        })
        
        
        # Querying structured data from 'test' and ordering by date
        Lines <- sqldf('SELECT Date, open AS Open, high AS High, low AS Low, close AS Close, volume FROM test ORDER BY Date DESC', drv = 'SQLite')
        
        # Replace periods with commas for the numeric data
        Lines$Open <- gsub("\\.", ",", Lines$Open)
        Lines$High <- gsub("\\.", ",", Lines$High)
        Lines$Low <- gsub("\\.", ",", Lines$Low)
        Lines$Close <- gsub("\\.", ",", Lines$Close)
        
        # Removing quotes from all columns, if any
        Lines <- data.frame(sapply(Lines, function(x) gsub("\"", "", x)), stringsAsFactors = FALSE)
        
        # Creating a semicolon-separated string for each row
        connectionString <- with(Lines, paste(Date, Open, High, Low, Close, volume, sep = ";"))
        
        # Creating a single-column data frame from 'connectionString'
        connectionString <- data.frame(connectionString, stringsAsFactors = FALSE)
        names(connectionString) <- "Data"
        
        # Prepending column names as the first row in the data frame
        connectionString <- rbind("Date;Open;High;Low;Close;Volume", connectionString)
        
        # Opening a text connection to the 'connectionString' data frame for input to read.zoo
        conn <- textConnection(as.character(connectionString$Data))
        
        # Reading the data as a zoo object with specified formats and converting to xts for time series manipulation
        stock <- as.xts(read.zoo(conn, sep = ";", tz = "US/Eastern", dec = ",", header = TRUE, format = "%Y-%m-%d", index.column = 1))
        
        # Cleanup: Closing the text connection
        close(conn)
        
        # Preparing a subset of the 'stock' data for further manipulation or analysis
        stock2 <- as.data.frame(stock[, 1:4])
        colnames(stock2) <- c("Open", "High", "Low", "Close")
        
        # Convert 'stock2' to an xts object, focusing on the first four columns only
        stock3 <- as.xts(as.matrix(stock2[, 1:4]), order.by = index(stock), tz = "EST")
        
        # 'stock', 'stock1', 'stock2', and 'stock3' are now prepared for your analysis.
        # Note: 'stock1' was not modified from 'stock'; thus, it's omitted for redundancy.
      {
        # myRe #  myReturnsc <- as.matrix(mkReturnsc*(ind1[2:length(ind1)])*(rstrd[2:length(rstrd)])*(vstrd[2:length(vstrd)]))
        
        
        # myReturnsc <- as.matrix(mkReturnsc*(ind1[2:length(ind1)])*(trd[2:length(trd)])*(rstrd[2:length(rstrd)])*(adstrd[2:length(adstrd)])*(smistrd[2:length(smistrd)]))
        {  
          
          
          # Selecting distinct rows from 'test' where 'close' is greater than 0
          # and ordering the results by 'Date' in ascending order
          best <- sqldf("SELECT DISTINCT * FROM test WHERE close > 0 ORDER BY Date ASC", drv = "SQLite")
          
          # Converting the 'close' column to a numeric vector
          uals <- as.numeric(as.matrix(best$close)[, 1])
          
          # Generating a sequence of negative integers based on the number of rows in 'best'
          nx <- -(1:nrow(best))
          
          # Excluding the indices in 'nx' from 'uals'
          bals <- uals[-nx]
          
          {    
            # MACD calculation with SMA
            tryCatch({
              macd <- MACD(bals, nFast = 12, nSlow = 26, nSig = 9, maType = SMA, percent = TRUE)
              macd <- as.data.frame(macd)
              macd[is.na(macd)] <- 0
              
              signal <- as.data.frame(ifelse(macd$macd > macd$signal, 1, 0))
              colnames(signal) <- "signal"
              signal[is.na(signal)] <- 0
              
              SIGNAL1 <- as.numeric(signal$signal)
              SIGNAL1[is.na(SIGNAL1)] <- 0
              
              nx <- -(1:length(SIGNAL1))
              SSIGNAL1 <- SIGNAL1[-nx]
            }, error = function(e) {})
            
            
            # MACD calculation with EMA
            tryCatch({
              macd <- MACD(bals, nFast = 12, nSlow = 26, nSig = 9, maType = EMA, percent = TRUE)
              macd <- as.data.frame(macd)
              macd[is.na(macd)] <- 0
              
              signal <- as.data.frame(ifelse(macd$macd > macd$signal, 1, 0))
              colnames(signal) <- "signal"
              signal[is.na(signal)] <- 0
              
              SIGNAL1 <- as.numeric(signal$signal)
              SIGNAL1[is.na(SIGNAL1)] <- 0
              
              nx <- -(1:length(SIGNAL1))
              ESIGNAL1 <- SIGNAL1[-nx]
            }, error = function(e) {})
            
            
            
            
            # Aroon indicator
            tryCatch({
              HL=cbind(stock$High,stock$Low)
              HL$me=1
              HL=cbind(HL[,1],HL[,2])
              
              
              aroon= aroon(HL,n = 25)
              aroon=as.data.frame(aroon)
              aroon[is.na(aroon)] <- 0
              aroon<-as.data.frame(aroon)
              
              
              arsignal <- ifelse(aroon$aroonUp > aroon$aroonDn, 1, 0)
              
              arsignal=as.data.frame(arsignal)
              arsignal[is.na(arsignal)] <- 0
              colnames(arsignal)=c("arsignal")
              
              ARSIGNAL1=as.numeric(as.matrix(arsignal$arsignal))
              ARSIGNAL1[is.na(ARSIGNAL1)]=0
              
              nx=-(1:length(ARSIGNAL1))
              
              
              ARSIGNAL1=ARSIGNAL1[-nx]
            }, error = function(e) {})
            
            
        
            # ADX indicator
            tryCatch({
              
              HLC=cbind(stock$High,stock$Low,stock$Close)
              HLC$me=1
              HLC=cbind(HLC[,1],HLC[,2],HLC[,3])
              adx= ADX(HLC,n = 14, maType="EMA", wilder=TRUE)
              adx=as.data.frame(adx)
              adx[is.na(adx)] <- 0
              
              
              adsignal <- ifelse(adx$DIp > adx$DIn, 1, 0)
              
              adsignal=as.data.frame(adsignal)
              adsignal[is.na(adsignal)] <- 0
              colnames(adsignal)=c("adsignal")
              
              ADSIGNAL1=as.numeric(as.matrix(adsignal$adsignal))
              ADSIGNAL1[is.na(ADSIGNAL1)]=0
              
              nx=-(1:length(ADSIGNAL1))
              
              
              ADSIGNAL1=ADSIGNAL1[-nx]
            }, error = function(e) {})
            
            
            
            # SMI indicator
            tryCatch({
              smi <- SMI(HLC, n = 13, slow = 5, fast = 20, signal = 5, ma.type = "EMA")
              smi <- as.data.frame(smi)
              smi[is.na(smi)] <- 0
              
              smisignal <- as.data.frame(ifelse(smi$SMI > smi$signal, 1, 0))
              colnames(smisignal) <- "smisignal"
              smisignal[is.na(smisignal)] <- 0
              
              SMISIGNAL1 <- as.numeric(smisignal$smisignal)
              SMISIGNAL1[is.na(SMISIGNAL1)] <- 0
              
              nx <- -(1:length(SMISIGNAL1))
              SMISIGNAL1 <- SMISIGNAL1[-nx]
            }, error = function(e) {})
            
            
            
            # SMA and EMA calculations for 'bals'
            tryCatch({
              gelsma20 <- SMA(bals, 20)
              gelsma20 <- as.data.frame(gelsma20)
              GELSMA20 <- as.numeric(gelsma20$gelsma20)
              GELSMA20[is.na(GELSMA20)] <- bals[1:sum(is.na(GELSMA20))]
              nx <- -(1:length(GELSMA20))
              GELSMA20 <- GELSMA20[-nx]
            }, error = function(e) {})
            
            
            
            tryCatch({
              gelema20 <- EMA(bals, 20)
              gelema20 <- as.data.frame(gelema20)
              GELEMA20 <- as.numeric(gelema20$gelema20)
              GELEMA20[is.na(GELEMA20)] <- bals[1:sum(is.na(GELEMA20))]
              nx <- -(1:length(GELEMA20))
              GELEMA20 <- GELEMA20[-nx]
            }, error = function(e) {})
            
            
            
            # Volume weighted moving average
            tryCatch({
              vols <- as.numeric(as.matrix(test$volume)[, 1])
              gelvma20 <- EVWMA(bals, vols, 20)
              gelvma20 <- as.data.frame(gelvma20)
              GELVMA20 <- as.numeric(gelvma20$gelvma20)
              GELVMA20[is.na(GELVMA20)] <- bals[1:sum(is.na(GELVMA20))]
              nx <- -(1:length(GELVMA20))
              GELVMA20 <- GELVMA20[-nx]
            }, error = function(e) {})
            
            
            # SAR indicator
            tryCatch({
              HL=cbind(stock$High,stock$Low)
              HL$me=1
              HL=cbind(HL[,1],HL[,2])
              sar=SAR(HL, accel = c(0.02, 0.2))
              sar=as.numeric(sar[,1]) 
              
              
              nx=-(1:(nrow(best)))
              
              
              sar=sar[-nx]
              
              
              GELSRA20=sar
              
              
              
              GELSRA20=GELSRA20[-nx]
              
              GELSRA20[1:10]=bals[1:10]
            }, error = function(e) {})
            
            
            
            
            # RSI and Stochastic RSI
            tryCatch({
              rsi <- RSI(bals, n = 20, maType = "EMA", wilder = TRUE)
              srsi <- stoch(rsi)
              srsi <- as.data.frame(srsi)
              srsi[is.na(srsi)] <- 0
              
              srsignal <- as.data.frame(ifelse(srsi$fastD > srsi$slowD, 1, 0))
              colnames(srsignal) <- "srsignal"
              srsignal[is.na(srsignal)] <- 0
              
              SRSIGNAL1 <- as.numeric(srsignal$srsignal)
              SRSIGNAL1[is.na(SRSIGNAL1)] <- 0
              
              nx <- -(1:length(SRSIGNAL1))
              SRSIGNAL1 <- SRSIGNAL1[-nx]
            }, error = function(e) {})
            
            tryCatch({
              # Calculate RSI with a short window
              rsi <- RSI(bals, n = 5, maType = "EMA", wilder = TRUE)
              rsi <- as.data.frame(rsi)
              
              # Calculate RSI with a longer window
              mrsi <- RSI(bals, n = 20, maType = "EMA", wilder = TRUE)
              mrsi <- as.data.frame(mrsi)
              
              # Convert RSI data to numeric and replace NA values with 0
              rsi <- as.numeric(as.matrix(rsi$rsi))
              rsi[is.na(rsi)] <- 0
              
              # Convert mRSI data to numeric and standardize initial values
              mrsi <- as.numeric(as.matrix(mrsi$mrsi))
              mrsi[1:20] <- rsi[1:20]
              
              # Generate a negative sequence based on the length of rsi
              nx <- -(length(rsi):1)
              
              # Calculate the RSI signal
              rsignal <- ifelse(rsi > mrsi, 1, 0)
              rsignal <- as.data.frame(rsignal)
              rsignal[is.na(rsignal)] <- 0
              colnames(rsignal) <- c("rsignal")
              
              RSIGNAL1 <- as.numeric(as.matrix(rsignal$rsignal))
              RSIGNAL1[is.na(RSIGNAL1)] <- 0
              
              nx <- -(1:length(RSIGNAL1))
              RSIGNAL1 <- RSIGNAL1[-nx]
            },
            error = function(e) {
              # Error handling code here
            })
            
          }
          
          #   myReturnsc[1: myReturnsc, ] <- 1
          
          
          
          # Extracting the second column from 'test' data frame
          dt <- test[, 2]
          
          # Converting the 'close' column of 'test' to a matrix and then to a numeric vector
          cals <- as.matrix(test$close)
          cals <- as.numeric(cals[, 1])
          
          # Getting the last value from 'cals'
          curr <- cals[length(cals)]
          
          # Generating a negative sequence based on the length of 'cals'
          vx <- -(1:length(cals))
          
          # Binding data together and converting to a data frame
          bact <- cbind("symbol", cals[-vx], GELSMA20[-nx], GELEMA20[-nx], GELVMA20[-nx], GELSRA20[-nx], 1, 1, 1)
          bact <- as.data.frame(bact)
          
          # Adding the 'dt' column to 'bact'
          bact <- cbind(dt, bact)
          
          # Setting column names
          colnames(bact) <- c("V1", "V2", "V3", "V5", "V6", "V7", "V8", "V9", "V10", "V11")
          
          # Ordering 'bact' by 'V1' using SQL query and replacing NA values with 0
          bact3 <- sqldf('SELECT a.* FROM bact a ORDER BY a.V1', drv = 'SQLite')
          bact3[is.na(bact3)] <- 0
          
          # Converting 'bact3' back to a data frame
          bact3 <- as.data.frame(bact3)
          
          # Getting the maximum date from 'bact3'
          cek <- sqldf('SELECT max(V1) as Date FROM bact3', drv = 'SQLite')
          dm1 <- as.POSIXct(strptime(cek$Date, "%Y-%m-%d %H:%M:%S"))
          
          # Selecting distinct rows from 'bact3'
          bacte <- sqldf('SELECT DISTINCT * FROM bact3', drv = 'SQLite')
          
          # Converting 'bacte' to a data frame
          df1 <- as.data.frame(bacte)
          
          # Concatenating columns from 'df1' to create a connection string
          connectionString <- paste0(df1$V1, ";", df1$V2, ";", df1$V3, ";", df1$V5, ";",
                                     df1$V6, ";", df1$V7, ";", df1$V8, ";", df1$V9, ";",
                                     df1$V10, ";", df1$V11)
          
          # Creating column names string
          names <- "Date;Symbol;Close;sm20;em20;vm20;sr20;smistrd;vstrd;rvstrd"
          
          # Converting 'names' and 'connectionString' to data frames and setting column names
          
          names=as.data.frame(names)
          connectionString <- as.data.frame(connectionString)
          colnames(connectionString) <- c("names")
          
          # Combining 'names' and 'connectionString' rows
          dd <- rbind(names, connectionString)
          
          # Converting all elements in 'dd' to character to ensure consistent data type
          connectionString <- lapply(dd, as.character)
          
          # Removing potential double quotes from the connection string
          df <- sapply(connectionString, function(x) gsub("\"", "", x))
          
          # Creating a text connection object from 'df'
          conn <- textConnection(df)
          
          # Reading the data as a zoo object and converting to 'xts', specifying the format and time zone
          stock <- as.xts(read.zoo(conn, sep = ';', tz = "US/Eastern", dec = ",", header = TRUE,
                                   format = '%Y-%m-%d', index.column = 1))
          # Convert 'stock' to a data frame
          stk1=as.matrix(stock)
          stk1=as.data.frame(stock)
          
          # Convert various columns to numeric
          a <- as.numeric(as.character(stk1$Close))
          sm20 <- as.numeric(as.character(stk1$sm20))
          em20 <- as.numeric(as.character(stk1$em20))
          vm20 <- as.numeric(as.character(stk1$vm20))
          sr20 <- as.numeric(as.character(stk1$sr20))
          clse <- as.numeric(as.character(stk1$Close))
          
          # Placeholder assignments for signal calculations
          # Note: The actual calculations or functions for these signals are not provided
          i1 <- ESIGNAL1
          i2 <- SSIGNAL1
          i3 <- ADSIGNAL1
          i4 <- SMISIGNAL1
          i5 <- RSIGNAL1
          
          # Example custom functions applied to data (functions not defined in snippet)
          i6 <- sma20c(clse, sm20)
          i7 <- ema20c(clse, em20)
          i81 <- vma20c(clse, vm20)
          i9 <- ARSIGNAL1
          i8 <- sr20c(clse, sr20)
          
          # Calculating EMA for 50 periods and handling NA values
          em200=EMA(bals,50)
          em200=as.data.frame(em200)
          em200=as.numeric(as.matrix(em200$em200))
          em200[is.na(em200)] <- bals[1:sum(is.na(em200))]
          em200[is.na(em200)] <- bals[sum(is.na(em200)) + 1]
          
          # Removing initial values based on 'nx'
          nx <- -(1:length(em200))
          em200 <- em200[-nx]
          
          # Calculating EMA for 20 periods and handling NA values
          em50=EMA(bals,20)
          em50=as.data.frame(em50)
          em50=as.numeric(as.matrix(em50$em50))
          em50[is.na(em50)] <- bals[1:sum(is.na(em50))]
          em50[is.na(em50)] <- bals[sum(is.na(em50)) + 1]
          
          # Removing initial values based on 'nx'
          nx <- -(1:length(em50))
          em50 <- em50[-nx]
          
          # Example custom function applied to EMA data (function not defined in snippet)
          i10 <- sma20c(em50, em200)
          
          # Execute SQL query to select distinct records from 'test' and order by Date
          best <- sqldf('SELECT distinct * FROM test ORDER BY Date asc', drv = 'SQLite')
          
          # Convert the 'close' column to a numeric matrix, then to a numeric vector
          uals <- as.matrix(best$close)
          uals <- as.numeric(uals[, 1])
          
          # Generate a negative sequence based on the length of 'uals'
          nx <- -(length(uals):1)
          
          # Apply the negative sequence to 'uals'
          gals <- uals[-nx]
          x <- c(gals)
          
          # Identify peaks within the data
          lv <- peaks(x, 1)
          
          # Identify indices of the peaks
          mx <- -which(lv == TRUE)
          
          # Reverse the sequence for 'gals'
          nx <- length(gals):1
          y <- c(gals[nx])
          
          # Find valleys within the data
          my <- findValleys(y)
          
          # Calculate the position of valleys
          lz <- -(length(y) - my)
          mz <- lz
          
          # Apply a negative sequence based on the length of 'bals'
          nx <- -(length(bals):1)
          
          # Combine 'mz' with the adjusted 'bals' and convert to a data frame
          hz <- as.data.frame(cbind(mz, bals[-mz]))
          
          # Select 'mz' from 'hz' ordered by the second column descending
          hz <- sqldf('SELECT mz FROM hz ORDER BY V2 DESC', drv = 'SQLite')
          
          # Convert 'hz$mz' to a numeric matrix, then to a numeric vector
          hz <- as.matrix(hz$mz)
          hz <- as.numeric(hz[, 1])
          
          # Split 'hz' into two parts and convert them into data frames
          kz1=(hz[round(length(hz)/2):length(hz)])
          
          kz1=as.data.frame(kz1)
          
          kz2=(hz[1:round(length(hz)/2)])
          
          kz2=as.data.frame(kz2)
          # Add a descending sequence column to 'test'
          test$gf <- -seq.int(nrow(test):1)
          # Select the minimum 'close' value for specified 'gf' values from 'kz1'
          minu <- sqldf('SELECT distinct gf as ID, min(close) FROM test WHERE gf in (SELECT kz1 FROM kz1) ORDER BY Date asc', drv = 'SQLite')
          
          # Select the maximum 'close' value for specified 'gf' values from 'kz1'
          maxu <- sqldf('SELECT distinct gf as ID, max(close) FROM test WHERE gf in (SELECT kz1 FROM kz1) ORDER BY Date asc', drv = 'SQLite')
          
          # Filter 'test' to get distinct 'close' values where 'gf' matches 'ID' in 'minu'
          minu <- sqldf('SELECT distinct close FROM test WHERE gf in (SELECT ID FROM minu)', drv = 'SQLite')
          
          # Filter 'test' to get distinct 'close' values where 'gf' matches 'ID' in 'maxu'
          maxu <- sqldf('SELECT distinct close FROM test WHERE gf in (SELECT ID FROM maxu)', drv = 'SQLite')
          
          # Convert the first 'close' value from 'minu' and 'maxu' to character
          minhu <- as.character(minu[1, 1])
          maxhu <- as.character(maxu[1, 1])
        
          
          if(input$somevalue3=="TRUE") 
          {
            {
              itr=511
              vstacc<<-read.table(file="vstcmnn.csv",header=TRUE,sep=",") 
              
            
              
              
              for(tj in 1:itr){
                tryCatch({
                  if(tj==511)
                  {
                    
                    updateSwitchInput(session = session,
                                      inputId = "somevalue3",
                                      value = FALSE)
                    
                    
                    mstacc=as.data.frame(vstacc)
                    
                    
                    
                    jsym=as.data.frame(allsym)
                    
                    print(jsym)
                    
                    
                    lastm=sqldf('SELECT distinct model  from mstacc order by tper desc   ',drv='SQLite')
                    
                    
                    lastm=lastm[1,]
                    
                    lastm=as.data.frame(lastm)
                    lastm=sqldf('SELECT *  from lastm ',drv='SQLite')
                    
                    testr[511]=as.character(lastm[1,1])
                    
                    cmnn=sqldf('SELECT distinct a.* from mstacc  a join jsym b on a.symbol=b.symbol ',drv='SQLite')
              
                        dt1= as.POSIXct(strptime(paste0(as.numeric(substring(cmnn$date2,1,4)),"-",as.numeric(substring(cmnn$date2,5,6)),"-",as.numeric(substring(cmnn$date2,7,8))), "%Y-%m-%d"),tz="EST")
                       dt2= as.POSIXct(strptime(paste0(as.numeric(substring(cmnn$date1,1,4)),"-",as.numeric(substring(cmnn$date1,5,6)),"-",as.numeric(substring(cmnn$date1,7,8))), "%Y-%m-%d"),tz="EST")
                       
                       dt=dt2-dt1
                      
                    cmnn$len=as.numeric(dt)
                 
                   write.table(cmnn, "accinfo_tot1.csv", col.names=TRUE,row.names=FALSE,sep=",",append=FALSE)
                    
                    
                  }
                  print(tj)
                  kj=eval(parse(text=testr[tj]))
                  mj=testr[tj]
                  kj= eval(parse(text=mj))
                  
                  {     
                    kj<- as.matrix(kj)
                    
                    
                    
                    for(i in 1:nrow(kj)){
                      
                      
                      if (as.numeric(kj[i,1])<(input$Noind))
                        
                      {
                        
                        
                        kj[i,1]=0
                      } else{
                        
                        kj[i,1]=1
                      }}
                    
                    
                    bb=as.numeric(input$Ngap)
                    i12=SMA(i10,bb)
                    
                    i12[is.na(i12)]=0
                    
                    i12<- as.matrix(i12)
                    print(nrow(i12))
                    
                    
                    x=input$NOTRD
                    if(x==1)
                    {     
                      i11=i12+kj
                    }
                    if(x==2)
                    {
                      i11=i12
                    }
                    if(x==3)
                    {
                      i11=kj
                    }
                    
                    
                    for(i in 1:nrow(i11)){
                      
                      
                      if (as.numeric(i11[i,1])<(input$Soff))
                        
                      {
                        
                        
                        i11[i,1]=0
                      } else{
                        
                        i11[i,1]=1
                      }}
                    
                    i11<- as.matrix(i11)
                    
                    
                    
                    
                    ind=i11
                    # corre<-cbind(ind1,ind2,ind3,ind4,ind5,ind6,ind7,ind8,ind9,kjlt)
                    
                    #  verri<<-corre
                    
                    
                    bmkReturns <-  (diff(a)*100)/a[-length(a)]
                    bmkReturns=cbind(stk1[2:nrow(stk1),0],bmkReturns)
                    bmkReturns1=as.xts(as.data.frame(bmkReturns))
                    myReturnsc <- as.matrix(1*ind[2:length(ind)])
                    myReturnsc=cbind(stk1[2:nrow(stk1),0],myReturnsc)
                    
                    myReturnsc=as.xts(as.data.frame(myReturnsc))
                    
                    names(bmkReturns1) <- 'stock'
                    
                    names(myReturnsc) <- 'Me'
                    
                    bmkReturns2=cumsum(bmkReturns1)
                    
                    
                    myReturns2c=(myReturnsc)
                    lungDeaths= as.xts(cbind(bmkReturns2, myReturns2c),tz="US/Eastern")
                    
                    
                    .index(lungDeaths)[1:nrow(lungDeaths)] <-(.index(lungDeaths)[1:nrow(lungDeaths)] + 5*3600)
                    
                    addy=as.numeric(as.character(test$close))
                    
                    
                    addy=addy[2:(length(addy))]
                    
                    
                    
                    lungDeaths5<-as.xts(cbind(lungDeaths,addy ),tz="US/Eastern")
                    lungDeaths5[1,2]=0
                    
                    kk<-lungDeaths5
                    
                    updn <- c(kk$Me)
                    
                    updn3=rbind(updn[nrow(updn),])
                    
                    updn3$Me=0
                    
                    updn=rbind(updn,updn3)
                    
                    ix1 <- which(updn == 0)
                    
                    updn1 <- c(diff(ix1))
                    
                    ix <- which(updn1 != 1)
                    BID=as.data.frame(as.matrix(c(ix1[ix])))
                    ix0 <- which(updn1 == 1)
                    MBID=as.data.frame(as.matrix(c(ix1[ix])))
                    
                    if(max(MBID)==max(BID))
                    {
                      
                    } else     
                    {
                      BID=  rbind(BID,max(MBID))
                    }
                    
                    
                    
                    updn2 <- c(kk$Me)
                    
                    updn3=rbind(updn2[nrow(updn2),])
                    
                    updn3$Me=1
                    
                    
                    
                    updn2=rbind(updn2,updn3)
                    
                    updn3=rbind(updn2[nrow(updn2),])
                    
                    updn3$Me=0
                    
                    
                    
                    updn2=rbind(updn2,updn3)
                    
                    ix11 <- which(updn2 == 1)
                    
                    updn2 <- c(diff(ix11))
                    ix10 <- which(updn2 != 1)
                    SID=as.data.frame(as.matrix(c(ix11[ix10])))
                    
                    
                    fpnt<-data.frame(date=index(lungDeaths5), coredata(lungDeaths5))
                    
                    fpnt=as.data.frame(fpnt)
                    
                    
                    fpnt$ID=seq.int(nrow(fpnt))-1
                    
                    
                    cfpnts=sqldf('SELECT a.date,a.stock,a.ID, "BUY" as decision,a.addy from fpnt a join BID b  on a.ID=b.v1 order by a.ID asc ',drv='SQLite')
                    
                    
                    bfpnts=(cfpnts)
                    
                    #print(bfpnts)
                    bfpnts=as.data.frame(bfpnts)
                    
                    sfpnts=sqldf('SELECT a.date,a.stock,a.ID, "SELL" as decision,a.addy from fpnt a join SID b  on a.ID=b.v1 order by a.ID asc',drv='SQLite')
                    #print(sfpnts)
                    
                    q=0
                    
                    if(nrow(bfpnts)>nrow(sfpnts))
                    {
                      
                      gfpnts=sqldf('SELECT a.date,a.stock,a.ID, "SELL" as decision,a.addy from fpnt a order by a.ID asc',drv='SQLite')
                      
                      gfpnts=gfpnts[nrow(gfpnts),]
                      
                      gfpnts$date <-as.POSIXct(strptime(gfpnts$date, "%Y-%m-%d %H:%M:%S"))+10*3600
                      
                      
                      sfpnts=rbind(sfpnts,gfpnts)
                      
                      q=1
                    }
                    
                    
                    
                    
                    sfpnts=as.data.frame(sfpnts)
                    
                    
                    bfpnts$ID1=seq.int(nrow(bfpnts))
                    
                    sfpnts$ID1=seq.int(nrow(sfpnts))
                    
                    
                    
                    twopr=sqldf('SELECT a.addy as st,a.ID as bu, b.ID as su from bfpnts a join sfpnts b  on  a.ID1=b.ID1 order by a.ID asc',drv='SQLite')
                    
                    twoprf=sqldf('SELECT a.*,b.*,((a.addy-b.st)*100/b.st) as diff from fpnt a join twopr b  on  a.ID>=b.bu and a.ID<=b.su order by a.ID asc',drv='SQLite')
                    
                    
                    bsu=sqldf('SELECT distinct  bu,su from twoprf ',drv='SQLite')
                    
                    
                    sm=1
                    smaaa=1
                    for(i in 1: nrow(bsu))
                    {
                      bu=bsu[i,1]
                      
                      su=bsu[i,2]
                      bu=as.data.frame(bu)
                      su=as.data.frame(su)
                      
                      dsu=sqldf('SELECT distinct a.* from twoprf a join bu b on a.bu=b.bu join su c on a.su=c.su ',drv='SQLite')
                      add=dsu$addy
                      if(length(add)>5)
                      {
                        smadd=SMA(add,5)
                        smadd[is.na(smadd)]=add[1:sum(is.na(smadd))]
                        smadd= smadd-0.15*smadd
                      }
                      else{
                        smadd=add
                        smadd= smadd-0.15*smadd
                      }
                      
                      signal <- ifelse(add >smadd, 1, 0)
                      sm=add*signal
                      
                      
                      smaaa<-c(smaaa,sm)
                      
                    }
                    
                    smaaa=smaaa[2:length(smaaa)]
                    
                    twoprf$addy=smaaa
                    twoprf$addy=   as.numeric(twoprf$add)
                    twoprf=as.data.frame(twoprf)
                    twoprf1=sqldf('SELECT * from twoprf where addy=0',drv='SQLite')
                    
                    ssid=sqldf('SELECT su,min(ID) as  mID from twoprf1 group by su ',drv='SQLite')
                    
                    sfpnts=sqldf('SELECT a.date,a.stock,a.ID, a.decision,a.addy from sfpnts a where ID not in (SELECT su from ssid) ',drv='SQLite')
                    
               
                    cpsts=sqldf('SELECT a.date,a.stock,a.ID, "SELL" as decision,a.addy from fpnt a join ssid b  on a.ID=b.mID order by a.ID asc',drv='SQLite')
             
                    
                    
                    sfpnts=rbind(sfpnts,cpsts)
                    
                    sfpnts=sqldf('SELECT * from sfpnts order by ID asc   ',drv='SQLite')
                    sfpnts$ID1=seq.int(nrow(sfpnts))
                    
                    bfpnts=sqldf('SELECT * from bfpnts   ',drv='SQLite')
                    
                    
                    
                    sfpnts=sqldf('SELECT b.* from bfpnts a join sfpnts b  on a.ID1=b.ID1 ',drv='SQLite')
                    
                    llp<-nrow(bfpnts)
                    
                    
                    thp=sqldf('SELECT a.addy,b.addy,((b.addy-a.addy))*100/a.addy as value from bfpnts a join sfpnts b  on a.ID1=b.ID1 ',drv='SQLite')
                    
                    
                    totp=sqldf('SELECT a.ID1,((b.addy-a.addy))*100/a.addy as value from bfpnts a join sfpnts b  on a.ID1=b.ID1 ',drv='SQLite')
                    
                    
                    
                    ii=table(((totp$value))>0)
                    
                    jj=as.data.frame(ii)  
                    
                    
                    
                    fjj=sqldf('SELECT Freq from jj where Var1="TRUE" ',drv='SQLite')
                    
                    
                    totp<- sum(as.numeric(as.character(totp$value)))
                    
                    
                    lbls <- as.numeric(llp)
                    notrd<-as.numeric(100)
                    
                    
                    pct <- as.numeric(totp)
                    
                    
                    invested=notrd*sum(as.numeric(as.character(bfpnts$addy)))
                    
                    currentval=notrd*sum(as.numeric(as.character(sfpnts$addy)))
                    
                    profit=currentval-invested
                    
                    ppr=(fjj)*100/lbls
                    
                    
                    
                    
                    tstacc=cbind(symbol,lbls,notrd,pct,curr,invested,currentval,profit,fjj,ppr,mj)
                    
                    
                    
                    
                    colnames(tstacc)=c("symbol","TTRD","NOTRD","PLP","Price","InAmount","Netvalue","PLoss","GdTrades","GtrPerc","model")
                    #vstacc<<-tstacc
                    
                    
                    tstacc=sqldf('SELECT distinct A.model,A.plp,A.price,A.TTRD,A.GdTrades,A.GtrPerc from tstacc   A ',drv='SQLite')
                    
                    
                    bpnts <-as.POSIXct(strptime(bfpnts$date,  "%Y-%m-%d %H:%M:%S"))+0*3600
                    bpnts=c(bpnts)
                    spnts <-as.POSIXct(strptime(sfpnts$date, "%Y-%m-%d %H:%M:%S"))+0*3600
                    spnts=c(spnts)
                    
                    bcfpnts=cbind(yoursym2,bfpnts)
                    
                    scfpnts=cbind(yoursym2,sfpnts)
                    
                    
                    bcfpnts=cbind(bcfpnts,tstacc)
                    
                    scfpnts=cbind(scfpnts,tstacc)
                    
                    colnames(bcfpnts)=c("symbol","Date","stock","ID","decision","bval","ID1","model","perc","curr","Ttrd","Gdtrd","Gtrdpr")
                    
                    
                    colnames(scfpnts)=c("symbol","Date","stock","ID","decision","sval","ID1","model","perc","curr","Ttrd","Gdtrd","Gtrdpr")
                    
                    buy<-as.data.frame(bcfpnts)
                    
                    
                    sell<-as.data.frame(scfpnts)
                    
                    
                    buy$Date <-as.POSIXct(strptime(buy$Date, "%Y-%m-%d"),tz="EST")
                    
                    
                    
                    sell$Date <-as.POSIXct(strptime(sell$Date, "%Y-%m-%d"),tz="EST")
                    
                    
                    
                    
                    buy$ID=seq.int(nrow(buy))
                    
                    sell$ID=seq.int(nrow(sell))
                    buy$Date=gsub("\\-", "", buy$Date)
                    sell$Date=gsub("\\-", "", sell$Date)
                    
                    buy$Date=as.numeric(as.character(buy$Date))
                    sell$Date=as.numeric(as.character(sell$Date))
                    
                    common <-  sqldf('SELECT distinct a.symbol,a.model,a.date as date2,b.date as date1,a.Ttrd as T,a.bval,b.sval,a.curr,round((b.sval-a.bval)*100/a.bval,2) as cper,round(a.perc,2) as tper ,a.Gdtrd as S,round(a.Gtrdpr,2) as Gpr FROM buy a join sell b on a.symbol=b.symbol and a.ID=b.ID order by a.perc   desc ',drv='SQLite')
                    common <-  sqldf('SELECT distinct * FROM common   order by date2 desc ',drv='SQLite')
                  
                    vstacc<-rbind(vstacc,common)
              
               
                    totl=sqldf('SELECT distinct Gpr*tper as totl FROM  vstacc where symbol<>"PSXP" ',drv='SQLite')
                    
                    totl=sqldf('SELECT max(totl) as totl FROM  totl  ',drv='SQLite')
                    
                    vstacc <-  sqldf('SELECT distinct a.* FROM vstacc a join totl b on a.Gpr*a.tper=b.totl',drv='SQLite')
                    
                  print(vstacc)
                    vstacc<<-vstacc
                    
                  }}
                  
                  
                  
                  ,
                  error=function(e){
                    tj<<-tj+1
                    
                  })}
            }
          }
          
          else
          {
        
            # Reading CSV files into data frames
            vstacc <<- read.table(file = "vstcmnn.csv", header = TRUE, sep = ",")
            cmnn <- read.table(file = "cmnn.csv", header = FALSE, sep = ",")
            
            # Creating a data frame 'jsym' from an existing data frame 'allsym'
            jsym <- as.data.frame(allsym)
            
            # Converting the 'symbol' column in 'jsym' to uppercase
            jsym$symbol <- toupper(jsym$symbol)
            
            # Performing an SQL join between 'cmnn' and 'jsym' on the 'symbol' column
            cmnn <- sqldf('SELECT a.* FROM cmnn a JOIN jsym b ON a.V2 = b.symbol', drv = 'SQLite')
            
            # Conditional assignment based on the number of rows in 'cmnn'
            if (nrow(cmnn) > 1) {
              mj <- as.character(cmnn$V3)
            } else {
              mj <- as.character(input$Model)
            }
            
         {
              {
            
                # Evaluating the string 'mj' as R code
                kj <- eval(parse(text = mj))
                
                {     
                  # Convert 'kj' to a matrix
                  kj <- as.matrix(kj)
                  
                  # Iterate over the rows of the matrix 'kj'
                  for (i in 1:nrow(kj)) {
                    # Check if the value in the first column is less than 'input$Noind'
                    if (as.numeric(kj[i, 1]) < input$Noind) {
                      # Update the value to 0 if the condition is met
                      kj[i, 1] <- 0
                    } else {
                      # Update the value to 1 otherwise
                      kj[i, 1] <- 1
                    }
                  }
                  
                  # Setup based on input
                  bb <- as.numeric(input$Ngap)
                  i12 <- SMA(i10, bb)
                  i12[is.na(i12)] <- 0
                  i12 <- as.matrix(i12)
                  
                  # Debug prints to compare row counts
                  print(nrow(i12))
                  print(nrow(kj))
                  
                  # Logic to modify i12 based on input$NOTRD value
                  x <- input$NOTRD
                  if (x == 1) {
                    i11 <- i12 + kj
                  } else if (x == 2) {
                    i11 <- i12
                  } else if (x == 3) {
                    i11 <- kj
                  }
                  
                  # Conditional modification of i11 based on input$Soff
                  for (i in 1:nrow(i11)) {
                    if (as.numeric(i11[i, 1]) < input$Soff) {
                      i11[i, 1] <- 0
                    } else {
                      i11[i, 1] <- 1
                    }
                  }
                  i11 <- as.matrix(i11)
                  
                  # Assigning modified matrix to 'ind'
                  ind <- i11
                  
                  # Calculate benchmark returns
                  bmkReturns <- (diff(a) * 100) / a[-length(a)]
                  bmkReturns <- cbind(stk1[2:nrow(stk1), 0], bmkReturns)
                  bmkReturns1 <- as.xts(as.data.frame(bmkReturns))
                  
                  # Calculate modified returns based on 'ind'
                  myReturnsc <- as.matrix(1 * ind[2:length(ind)])
                  myReturnsc <- cbind(stk1[2:nrow(stk1), 0], myReturnsc)
                  myReturnsc <- as.xts(as.data.frame(myReturnsc))
                  
                  # Naming the columns for clarity
                  names(bmkReturns1) <- 'stock'
                  names(myReturnsc) <- 'Me'
                  
                  # Calculate cumulative returns
                  bmkReturns2 <- cumsum(bmkReturns1)
                  
                  # Prepare 'myReturns2c' for merging
                  myReturns2c <- (myReturnsc)
                  
                  # Combine benchmark and my returns, adjusting for time zone
                  lungDeaths <- as.xts(cbind(bmkReturns2, myReturns2c), tz = "US/Eastern")
                  
                  # Adjust the index of 'lungDeaths' by adding 5 hours to each timestamp
                  .index(lungDeaths)[1:nrow(lungDeaths)] <- (.index(lungDeaths)[1:nrow(lungDeaths)] + 5 * 3600)
                  
                  # Prepare 'addy' variable for merging
                  addy <- as.numeric(as.character(test$close))
                  addy <- addy[2:(length(addy))]
                  
                  # Combine 'lungDeaths' with 'addy', adjusting for time zone
                  lungDeaths5 <- as.xts(cbind(lungDeaths, addy), tz = "US/Eastern")
                  lungDeaths5[1, 2] <- 0
                  
                  # Prepare data for up/down pattern identification
                  kk <- lungDeaths5
                  updn <- c(kk$Me)
                  updn3 <- rbind(updn[nrow(updn),])
                  updn3$Me <- 0
                  updn <- rbind(updn, updn3)
                  
                  # Identify indices where 'updn' changes from 0
                  ix1 <- which(updn == 0)
                  updn1 <- c(diff(ix1))
                  ix <- which(updn1 != 1)
                  BID <- as.data.frame(as.matrix(c(ix1[ix])))
                  ix0 <- which(updn1 == 1)
                  MBID <- as.data.frame(as.matrix(c(ix1[ix])))
                  
                  # Compare 'MBID' and 'BID' for consistency
                  if (max(MBID) != max(BID)) {
                    BID <- rbind(BID, max(MBID))
                  }
                  
                  # Prepare 'updn2' for additional pattern identification
                  updn2 <- c(kk$Me)
                  updn3 <- rbind(updn2[nrow(updn2),])
                  updn3$Me <- 1
                  updn2 <- rbind(updn2, updn3)
                  updn3 <- rbind(updn2[nrow(updn2),])
                  updn3$Me <- 0
                  updn2 <- rbind(updn2, updn3)
                  
                  # Identify indices where 'updn2' changes from 1
                  ix11 <- which(updn2 == 1)
                  updn2 <- c(diff(ix11))
                  ix10 <- which(updn2 != 1)
                  SID <- as.data.frame(as.matrix(c(ix11[ix10])))
                  
                  # Create a dataframe 'fpnt' with 'lungDeaths5' data and an ID column
                  fpnt <- data.frame(date = index(lungDeaths5), coredata(lungDeaths5))
                  fpnt$ID <- seq.int(nrow(fpnt)) - 1
                  
                  # Selecting buy points
                  cfpnts <- sqldf('SELECT a.date, a.stock, a.ID, "BUY" as decision, a.addy 
                 FROM fpnt a 
                 JOIN BID b ON a.ID = b.v1 
                 ORDER BY a.ID ASC', drv = 'SQLite')
                  
                  bfpnts <- as.data.frame(cfpnts)
                  
                  # Selecting initial sell points
                  sfpnts <- sqldf('SELECT a.date, a.stock, a.ID, "SELL" as decision, a.addy 
                 FROM fpnt a 
                 JOIN SID b ON a.ID = b.v1 
                 ORDER BY a.ID ASC', drv = 'SQLite')
                  
                  # Logic to add a sell point if there are more buy points than sell points
                  q <- 0
                  if (nrow(bfpnts) > nrow(sfpnts)) {
                    gfpnts <- sqldf('SELECT a.date, a.stock, a.ID, "SELL" as decision, a.addy 
                   FROM fpnt a 
                   ORDER BY a.ID ASC', drv = 'SQLite')
                    gfpnts <- gfpnts[nrow(gfpnts),]
                    gfpnts$date <- as.POSIXct(strptime(gfpnts$date, "%Y-%m-%d %H:%M:%S")) + 10*3600
                    sfpnts <- rbind(sfpnts, gfpnts)
                    q <- 1
                  }
                  
                  sfpnts <- as.data.frame(sfpnts)
                  
                  # Adding sequential ID for pairing
                  bfpnts$ID1 <- seq.int(nrow(bfpnts))
                  sfpnts$ID1 <- seq.int(nrow(sfpnts))
                  
                  # Pairing buy and sell points
                  twopr <- sqldf('SELECT a.addy as st, a.ID as bu, b.ID as su 
                FROM bfpnts a 
                JOIN sfpnts b ON a.ID1 = b.ID1 
                ORDER BY a.ID ASC', drv = 'SQLite')
                  
                  # Calculating performance between paired buy and sell points
                  twoprf <- sqldf('SELECT a.*, b.*, ((a.addy - b.st) * 100 / b.st) as diff 
                 FROM fpnt a 
                 JOIN twopr b ON a.ID >= b.bu AND a.ID <= b.su 
                 ORDER BY a.ID ASC', drv = 'SQLite')
                  
                  # Distinct buy-sell pairs
                  bsu <- sqldf('SELECT distinct bu, su FROM twoprf', drv = 'SQLite')
                  
                  # Analysis over each buy-sell pair
                  smaaa <- 1
                  for (i in 1:nrow(bsu)) {
                    bu=bsu[i,1]
                    
                    su=bsu[i,2]
                    bu=as.data.frame(bu)
                    su=as.data.frame(su)
                    dsu <- sqldf('SELECT distinct a.* 
                FROM twoprf a 
                JOIN bu b ON a.bu = b.bu 
                JOIN su c ON a.su = c.su', drv = 'SQLite')
                    add <- dsu$addy
                    if (length(add) > 5) {
                      smadd <- SMA(add, 5)
                      smadd[is.na(smadd)] <- add[1:sum(is.na(smadd))]
                      smadd <- smadd - 0.15 * smadd
                    } else {
                      smadd <- add - 0.15 * add
                    }
                    
                    signal <- ifelse(add > smadd, 1, 0)
                    sm <- add * signal
                    smaaa <- c(smaaa, sm)
                  }
                  smaaa <- smaaa[2:length(smaaa)]
                  
                  # Updating 'twoprf' with the new 'addy' values
                  twoprf$addy <- smaaa
                  twoprf$addy <- as.numeric(twoprf$add)
                  twoprf <- as.data.frame(twoprf)
                  
                  # Filtering out zero 'addy' rows
                  twoprf1 <- sqldf('SELECT * FROM twoprf WHERE addy = 0', drv = 'SQLite')
                  ssid <- sqldf('SELECT su, min(ID) as mID FROM twoprf1 GROUP BY su', drv = 'SQLite')
                  
                  # Filtering 'sfpnts' to exclude those adjusted by 'ssid'
                  sfpnts <- sqldf('SELECT a.date, a.stock, a.ID, a.decision, a.addy 
                 FROM sfpnts a 
                 WHERE ID NOT IN (SELECT su FROM ssid)', drv = 'SQLite')
                  
                  # Print adjusted sell points
               #   print(sfpnts)
                  
                  # Selecting adjusted sell points based on 'ssid'
                  cpsts <- sqldf('SELECT a.date, a.stock, a.ID, "SELL" as decision, a.addy 
                FROM fpnt a 
                JOIN ssid b ON a.ID = b.mID 
                ORDER BY a.ID ASC', drv = 'SQLite')
                  
                  # Print final sell points
                #  print(cpsts)
                  
                  # Combine buy points and sell points, then order them
                  sfpnts <- rbind(sfpnts, cpsts)
                  sfpnts <- sqldf('SELECT * FROM sfpnts ORDER BY ID ASC', drv = 'SQLite')
                  sfpnts$ID1 <- seq.int(nrow(sfpnts))
                  
                  # Refresh buy points
                  bfpnts <- sqldf('SELECT * FROM bfpnts', drv = 'SQLite')
                  
                  # Filter sell points to match with buy points
                  sfpnts <- sqldf('SELECT b.* FROM bfpnts a JOIN sfpnts b ON a.ID1 = b.ID1', drv = 'SQLite')
                  
                  # Calculate total hold points
                  llp <- nrow(bfpnts)
                  
                  # Calculate trade values
                  thp <- sqldf('SELECT a.addy, b.addy, ((b.addy - a.addy)) * 100 / a.addy AS value 
              FROM bfpnts a JOIN sfpnts b ON a.ID1 = b.ID1', drv = 'SQLite')
                  
                  # Aggregate total percentage
                  totp <- sqldf('SELECT a.ID1, ((b.addy - a.addy)) * 100 / a.addy AS value 
               FROM bfpnts a JOIN sfpnts b ON a.ID1 = b.ID1', drv = 'SQLite')
                  
                  # Determine win rate
                  ii <- table(totp$value >= 0)
                  jj <- as.data.frame(ii)
                  fjj <- sqldf('SELECT Freq FROM jj WHERE Var1 = "TRUE"', drv = 'SQLite')
                  
                  # Sum total percentage
                  totp <- sum(as.numeric(as.character(totp$value)))
                  
                  # Calculate trading metrics
                  lbls <- as.numeric(llp)
                  notrd <- as.numeric(100)
                  pct <- as.numeric(totp)
                  invested <- notrd * sum(as.numeric(as.character(bfpnts$addy)))
                  currentval <- notrd * sum(as.numeric(as.character(sfpnts$addy)))
                  profit <- currentval - invested
                  ppr <- (fjj) * 100 / lbls
                  
                  # Combine all metrics into a single data frame
                  tstacc <- cbind(symbol, lbls, notrd, pct, curr, invested, currentval, profit, fjj, ppr, mj)
                  colnames(tstacc) <- c("symbol", "TTRD", "NOTRD", "PLP", "Price", "InAmount", "Netvalue", "PLoss", "GdTrades", "GtrPerc", "model")
                  
                  # Distinct model statistics
                  tstacc <- sqldf('SELECT DISTINCT A.model, A.plp, A.price, A.TTRD, A.GdTrades, A.GtrPerc FROM tstacc A', drv = 'SQLite')
                  
                  # Adjust dates for buy and sell points
                  bpnts <- as.POSIXct(strptime(bfpnts$date, "%Y-%m-%d %H:%M:%S")) + 0 * 3600
                  bpnts <- c(bpnts)
                  spnts <- as.POSIXct(strptime(sfpnts$date, "%Y-%m-%d %H:%M:%S")) + 0 * 3600
                  spnts <- c(spnts)
                  
                  # Combine buy and sell points with trade statistics
                  bcfpnts=cbind(yoursym2,bfpnts)
                  
                  scfpnts=cbind(yoursym2,sfpnts)
                  
                  
                  bcfpnts=cbind(bcfpnts,tstacc)
                  
                  scfpnts=cbind(scfpnts,tstacc)
                  # Set column names
                  colnames(bcfpnts) <- c("symbol", "Date", "stock", "ID", "decision", "bval", "ID1", "model", "perc", "curr", "Ttrd", "Gdtrd", "Gtrdpr")
                  colnames(scfpnts) <- c("symbol", "Date", "stock", "ID", "decision", "sval", "ID1", "model", "perc", "curr", "Ttrd", "Gdtrd", "Gtrdpr")
                  
                  # Convert to data frames
                  buy <- as.data.frame(bcfpnts)
                  sell <- as.data.frame(scfpnts)
                  
                  # Display buy and sell points
                print(buy)
                print(sell)
                  
                  
                  # Resetting ID columns to ensure sequential ordering
                  buy$ID <- seq.int(nrow(buy))
                  sell$ID <- seq.int(nrow(sell))
                  
                  # Removing hyphens from dates for consistency
                  buy$Date <- gsub("\\-", "", buy$Date)
                  sell$Date <- gsub("\\-", "", sell$Date)
                  
                  # Merging 'buy' and 'sell' data frames on symbol and ID to calculate profit percentages and other metrics
                  common <- sqldf('SELECT distinct a.symbol, a.model, a.Date AS date2, b.Date AS date1, 
                        a.Ttrd AS T, a.bval, b.sval, a.curr, 
                        ROUND((b.sval - a.bval) * 100 / a.bval, 2) AS cper, 
                        ROUND(a.perc, 2) AS tper, a.Gdtrd AS S, 
                        ROUND(a.Gtrdpr, 2) AS Gpr 
                 FROM buy a 
                 JOIN sell b ON a.symbol = b.symbol AND a.ID = b.ID 
                 ORDER BY a.perc DESC', drv = 'SQLite')
                  
                  # Ordering the merged data by date in descending order to prioritize recent transactions
                  common <- sqldf('SELECT distinct * FROM common ORDER BY date2 DESC', drv = 'SQLite')
                  
                  # Converting the final SQL result into a data frame
                  mstacc <- as.data.frame(common)
                  
                  # Writing the merged and calculated data to a CSV file
                  write.table(mstacc, "accinfo_tot1.csv", col.names = TRUE, row.names = FALSE, sep = ",", append = FALSE)
                  
                }}
                
                
                
                }
          }
          
          
          # Convert 'bfpnts$date' and 'sfpnts$date' to POSIXct date-time objects without timezone adjustment
          bpnts <- as.POSIXct(strptime(bfpnts$date, "%Y-%m-%d %H:%M:%S"))
          spnts <- as.POSIXct(strptime(sfpnts$date, "%Y-%m-%d %H:%M:%S"))
          
          # Optional: Print buy and sell points date vectors
          # print(bpnts)
          # print(spnts)
          
          # Assign buy points dates to 'bts' for further manipulation or inspection
          bts <- bpnts
          
          # Extract the last buy transaction date
          LTS <- bts[length(bts)]
          
        
          
          
          # Initializing character vectors to hold names
          Names1 <- character(length(spnts))
          for (i in seq_along(bpnts)) {
            Names1[i] <- paste0("spnts[", i, "]") 
          }
          
          Names <- character(length(bpnts))
          for (i in seq_along(bpnts)) {
            Names[i] <- paste0("bpnts[", i, "]") 
          }
          
          # Assignments for further operations
          nms111 = Names1
          nms112 = Names
          
          # Current time formatted and converted to POSIXct
          ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          df1 <- as.POSIXct(ts)
          
          # Assignments for dynamic plot construction
          dm1 = df1
          yoursym1 = "fb"
          .index(stock3)[1:nrow(stock3)] <- (.index(stock3)[1:nrow(stock3)]) + 5 * 3600
          
          # Building the dygraph command string
          my_code <- "dygraph(stock3) %>% dyCandlestick() %>% dyRoller(rollPeriod = 0) %>%"
          y = '"y"'
          
          # Dynamic shading parts based on Names1 and Names2
          dfg <- paste("dyShading(from = ", nms111, ", to = ", nms112, ", color = ", '"#FFE6E6"', ")", collapse = " %>% ")
          wtst <- paste("dyShading(from = ", minhu, ", to = ", maxhu, ", axis = ", y, ", ", "color = ", '"#CCEBD6"')
          
          # Range selector and timezone options
          etst = paste("%>% dyRangeSelector(height = 20, dateWindow = c((dm1 - 300 * 24 * 3600), (dm1 + 6 * 24 * 3600)))")
          tmz = paste("%>% dyOptions(useDataTimezone = TRUE)")
          
          # Final dynamic code construction
          my_code <- paste(my_code, wtst, ") ", "%>% ", dfg, etst, tmz)
          
          # Due to environment constraints, the following line is for reference and cannot be executed here:
          # eval(parse(text = my_code))
          
          eval(parse(text = my_code))
          
          
          
        }
        
      }
      
    }
    
    
  })
  
  
  
  
  
  
  
  output$plotwrm<-renderPlotly(
    {  
      
      if(hv$i > 0) {
        
        
        if(input$somevalue2=="TRUE")
        {
          maxIter3 <<-3000
          hv$i <<-10
          
          
          allsym=(as.data.frame(input$stock1))
          
          allsym=as.data.frame(as.matrix(input$stock1))
          
          
          colnames(allsym)=c("symbol")
          
          symbol=as.character(allsym[1,1])
          
          
          rest= sqldf('SELECT distinct * FROM mest where symbol not in("NSE:XXX") order by Date desc ',drv='SQLite')
          test=as.data.frame(rest)
          
          dly2<- as.data.frame(input$delaydmm)
          dly2=as.matrix(dly2)
          dnth<<-dnth-1
          dly3=-dnth
          dly2=dly2[1,1]
          dly3<- as.data.frame(dly3)
          colnames(dly3)= "dly"
          
          
          
          dly2<- as.data.frame(dly2)
          colnames(dly2)= "dly"
          
          lnth=-as.numeric(as.character(dly2))
          
          dnth=-as.numeric(as.character(dly3))
          
          
          
          if(dnth<2)
          {
            dnth<<-dnth+1
          }
          
          
          test$dl=-seq.int(nrow(test))
          
          
          test <-  sqldf('SELECT a.* FROM test a   join dly2 b on a.dl>=b.dly  join dly3 c on a.dl<=c.dly order by Date desc',drv='SQLite')
          
          test=as.data.frame(test)
          
          
          test=sqldf('SELECT distinct * FROM test  order by Date asc ',drv='SQLite')
          
          
          Lines <-  sqldf('SELECT  Date as Date ,open as Open , high as High, low as Low, close as Close,  volume FROM test order by Date desc',drv='SQLite')
          
          
          
          Lines$Open=gsub("\\.", ",", Lines$Open)
          
          
          Lines$High=gsub("\\.", ",", Lines$High)
          
          
          Lines$Low=gsub("\\.", ",", Lines$Low)
          
          
          Lines$Close=gsub("\\.", ",", Lines$Close)
          
          
          df=sapply(Lines, function(x) gsub("\"", "", x))
          
          df1=as.data.frame(df,quote=FALSE)
          
          
          connectionString <- paste0(df1$Date,";",df1$Open,";",df1$High,";",df1$Low,";",df1$Close,";",df1$volume)
          
          names= paste0("Date;Open;High;Low;Close;Volume")
          
          
          names=as.data.frame(names)
          connectionString=as.data.frame(connectionString)
          
          colnames(connectionString)=c("names")
          
          dd=rbind(names,connectionString)
          
          
          connectionString <- lapply(dd, as.character)
          
          df=sapply(connectionString, function(x) gsub("\"", "", x))
          
          
          
          
          
          conn <- textConnection(df)
          stock=as.xts(read.zoo(conn, sep=';', tz="US/Eastern", dec=",", header=TRUE,
                                format='%Y-%m-%d %H:%M:%S', index.column=1))
          
          stock1= to.minutes(stock)
          
          
          
          close(conn)
          
          
          stock2=as.matrix(stock1)
          stock2=stock1[,1:4]
          
          stock2=as.data.frame(stock2)
          
          colnames(stock2) <- c("Open", "High", "Low", "Close")
          
          stock3=as.matrix(stock2)
          
          stock3=stock3[,1:4]
          
          
          
          stock3=as.xts(stock3,tz="EST")
          
          
          
          .index(stock3)[1:nrow(stock3)] <-(.index(stock3)[1:nrow(stock3)])+5*3600
          
          mals=as.matrix(test$close)
          
          mals=as.numeric(mals[,1]) 
          
          
          
          vcols=as.matrix(test$volume)
          
          vcols=as.numeric(vcols[,1]) 
          
          
          
          mals=mals[1:length(mals)]
          nx=-(1:length(mals))
          
          bals=mals[-nx]
          
          nx=-(length(mals):1)
          
          x <- nx
          
          nx=-(1:length(mals))
          y <- bals[-nx]
          v <- vcols[-nx]
          {
            
            library(extRemes)
            library(xts)
            
            ams=stock3
            
            
            maxams= period.apply(ams$Close, endpoints(ams$Close, "hours"), max)
            maxams=as.matrix(as.data.frame(maxams))
            
            minams <- period.apply(ams$Close, endpoints(ams$Close, "hours"), min)
            minams=as.matrix(as.data.frame(minams))
            xt=1:nrow(maxams)
            i=1 
            j=1
            for(i in i:nrow(maxams))
            { 
              for(j in j:nrow(stock3))
              {
                if(maxams[i]==mals[j])
                {
                  xt[i]=j
                  
                  break
                }
                else
                {
                  
                }
                
              }
            }
            
            
            
            yt=1:nrow(minams)
            i=1 
            j=1
            for(i in i:nrow(minams))
            { 
              for(j in j:nrow(stock3))
              {
                if(minams[i]==mals[j])
                {
                  yt[i]=j
                  
                  break
                }
                else
                {
                  
                }
                
              }
            }
            
            
            
            pks11=xt
            pksl2=yt
            
            x1=(length(mals)-(pks11[length(pks11)]))-(length(mals)-(pks11[length(pks11)-1])) 
            y1= bals[pks11[length(pks11)]]- bals[pks11[length(pks11)-1]]
            m1=y1/x1
            print("resistance")
            print(m1)
            zy1=bals[pks11[length(pks11)]]-m1*(length(mals)-(pks11[length(pks11)])) 
            
            x2=(length(mals)-(pksl2[length(pksl2)]))-(length(mals)-(pksl2[length(pksl2)-1])) 
            y2= bals[pksl2[length(pksl2)]]- bals[pksl2[length(pksl2)-1]]
            m2=y2/x2
            print("support")
            print(m2)
            zy2=bals[pksl2[length(pksl2)]]-m1*(length(mals)-(pksl2[length(pksl2)])) 
            zx=pks11[length(pks11)]
            zy=pksl2[length(pksl2)]
            
            updateSliderInput(session, "delaymm", value = c(max(bals),max(bals)),
                              min = floor(min(bals)), max =max(bals), step = 1)
            
            
            df <- data.frame(Date=index(stock3),coredata(stock3))
            
            df$ID=(length(mals)-seq.int(nrow(df)))+1
            df$Volume=v
            
            mvl=mean(df$Volume)
            
            
            # create Bollinger Bands
            bbands <- BBands(df[,c("High","Low","Close")])
            
            # join and subset data
            df <- subset(cbind(df, data.frame(bbands[,1:3])))
            # colors column for increasing and decreasing
            for (i in 1:length(df[,1])) {
              if (df$Close[i] >= df$Open[i]) {
                df$direction[i] = 'Increasing'
              } else {
                df$direction[i] = 'Decreasing'
              }
            }
            
            dly2<- as.data.frame(input$delaymm)
            dly2=as.matrix(dly2)
            dly3=dly2[2,1]
            dly2=dly2[1,1]
            
  
            i <- list(line = list(color = I('green')))
            d <- list(line = list(color = I('red')))
            
            # plot candlestick chart
            p <- df %>%
              plot_ly(x = ~Date, type="candlestick",
                      open = ~df$Open, close = ~df$Close,
                      high = ~df$High, low = ~df$Low, name = "Stock Chart",
                      increasing = i, decreasing = d) %>%
              add_lines(x = ~df$Date, y = ~df$up , name = "B Bands",
                        line = list(color = 'brown', width = 1),
                        legendgroup = "Bollinger Bands",
                        hoverinfo = "none", inherit = F) %>%
              add_lines(x = ~df$Date, y = ~df$dn, name = "B Bands",
                        line = list(color = 'brown', width = 1),
                        legendgroup = "Bollinger Bands", inherit = F,
                        showlegend = FALSE, hoverinfo = "none") %>%
              add_lines(x = ~df$Date, y = ~df$mavg, name = "Mv Avg",
                        line = list(color = 'pink', width =1),
                        hoverinfo = "none", inherit = F) %>%
              add_lines(x = df$Date, y = df$Close,line = list(color = 'black', yaxis = 'y1',width = 0.75))   %>%  
              add_lines(x = ~df[(pks11),1],  y = ~ bals[pks11],    type = 'scatter',  mode = 'markers',yaxis = 'y1' ,color = I('red')    ,name = "chnlres"        )%>%   
              add_lines(  x = ~df[(pksl2),1],y = ~ bals[pksl2],  type = 'scatter',mode = 'markers',yaxis = 'y1' ,color = I('green') ,name = "chnlsup"           )%>%     
              add_segments(x =  ~df[(pks11),1], xend = ~ df[nrow(df),1], y =  ~ bals[pks11], yend =  ~ bals[pks11],name = "resistance" ,yaxis = 'y1' ,color = I('blue'))%>%    
              add_segments(x =  ~df[(pksl2),1], xend = ~df[nrow(df),1], y =  ~ bals[pksl2], yend =  ~ bals[pksl2],name =  "support" ,yaxis = 'y1' ,color = I('orange'))%>%    
              add_segments(x =  ~df[zx,1], xend = ~df[nrow(df),1], y =  ~  bals[zx], yend =  ~ zy1,name = "extrares" ,yaxis = 'y1' ,color = I('red'))%>%    
              add_segments(x =  ~df[zy,1], xend = ~df[nrow(df),1], y =  ~  bals[zy], yend =  ~ zy2,name =  "extrassup" ,yaxis = 'y1' ,color = I('green'))%>%  
              layout(yaxis = list(title = "Price",range=c(dly2,dly3)))
            
            # plot volume bar chart
            pp <- df %>%
              plot_ly(x=~df$Date, y=~df$Volume, type='bar', name = "Volume",hoverinfo =  "text" , color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
              add_lines(x = df$Date, y = mvl,name = "AVolume",line = list(color = 'pink', yaxis = 'y1',width = 2))   %>%  
              
              layout(yaxis = list(title = "AVolume"))
            
            
            
            p <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
                         shareX = TRUE, titleY = TRUE)
            
            
            p  
            
            
            
            
            
            
            
          }
          
          
        }else 
        {
          maxIter3 <<-0
          
          
          
          
          
          allsym=(as.data.frame(input$stock1))
          
          allsym=as.data.frame(as.matrix(input$stock1))
          
          
          colnames(allsym)=c("symbol")
          
          symbol=as.character(allsym[1,1])
          
          
          
          
          
          rest= sqldf('SELECT distinct * FROM mest where symbol not in("NSE:XXX") order by Date desc ',drv='SQLite')
          
          test=as.data.frame(rest)
          
          dly2<- as.data.frame(input$delaydmm)
          dly2=as.matrix(dly2)
          k=1:10
          l=1:10
          m=1:10
          n=1:10
          brk=1:10
          
          sup=1:10
          res=1:10
          resls=1:10
          supls=1:10
          mm1=1:10
          mm2=1:10
          
          m1=1:10
          m11=1:10
          m2=1:10
          m22=1:10
          zy1=1:10
          zy2=1:10
          zy11=1:10
          zy22=1:10
          mxb=1:10
          zx111=1:10
          zy122=1:10
          zx=1:10
          zy=1:10
          dly3=dly2[2,1]
          
          dly2=dly2[1,1]
          
          
          dly3<- as.data.frame(dly3)
          colnames(dly3)= "dly"
          
          
          
          dly2<- as.data.frame(dly2)
          colnames(dly2)= "dly"
          
          dnth=-as.numeric(as.character(dly3))
          dnth<<-dnth
          
          dly3=dly3-11
          test$dl=-seq.int(nrow(test))
          for(ch in 1:10)
            
          { 
            
            dly3=dly3+1
            test <- rest
            test$dl=-seq.int(nrow(test))
            test <-  sqldf('SELECT a.* FROM test a   join dly2 b on a.dl>=b.dly  join dly3 c on a.dl<=c.dly order by Date desc',drv='SQLite')
            
            test=as.data.frame(test)
            
            
            test=sqldf('SELECT distinct * FROM test  order by Date asc ',drv='SQLite')
            
            
            
            Lines <-  sqldf('SELECT  Date as Date ,open as Open , high as High, low as Low, close as Close,  volume FROM test order by Date desc',drv='SQLite')
            
            
            
            Lines$Open=gsub("\\.", ",", Lines$Open)
            
            
            Lines$High=gsub("\\.", ",", Lines$High)
            
            
            Lines$Low=gsub("\\.", ",", Lines$Low)
            
            
            Lines$Close=gsub("\\.", ",", Lines$Close)
            
            
            df=sapply(Lines, function(x) gsub("\"", "", x))
            
            df1=as.data.frame(df,quote=FALSE)
            
            
            connectionString <- paste0(df1$Date,";",df1$Open,";",df1$High,";",df1$Low,";",df1$Close,";",df1$volume)
            
            names= paste0("Date;Open;High;Low;Close;Volume")
            
            
            names=as.data.frame(names)
            connectionString=as.data.frame(connectionString)
            
            colnames(connectionString)=c("names")
            
            dd=rbind(names,connectionString)
            
            
            connectionString <- lapply(dd, as.character)
            
            df=sapply(connectionString, function(x) gsub("\"", "", x))
            
            
            
            
            
            
            conn <- textConnection(df)
            stock=as.xts(read.zoo(conn, sep=';', tz="US/Eastern", dec=",", header=TRUE,
                                  format='%Y-%m-%d %H:%M:%S', index.column=1))
            
            stock1= to.minutes(stock)
            
            
            close(conn)
            
            
            
            
            stock2=as.matrix(stock1)
            stock2=stock1[,1:4]
            
            stock2=as.data.frame(stock2)
            
            colnames(stock2) <- c("Open", "High", "Low", "Close")
            
            stock3=as.matrix(stock2)
            
            stock3=stock3[,1:4]
            
            
            stock3=as.xts(stock3,tz="EST")
            
            
            
            .index(stock3)[1:nrow(stock3)] <-(.index(stock3)[1:nrow(stock3)])+5*3600
            
            
            mals=as.matrix(test$close)
            mals=as.matrix(test$close)
            
            mals=as.numeric(mals[,1]) 
            
            hals=as.matrix(test$High)
            
            hals=as.numeric(hals[,1]) 
            
            lals=as.matrix(test$Low)
            
            lals=as.numeric(lals[,1]) 
            
            vcols=as.matrix(test$volume)
            
            vcols=as.numeric(vcols[,1]) 
            
            
            
            mals=mals[1:length(mals)]
            nx=-(1:length(mals))
            
            bals=mals[-nx]
            
            nx=-(length(mals):1)
            
            x <- nx
            
            nx=-(1:length(mals))
            y <- bals[-nx]
            v <- vcols[-nx]
            
            
            
            
            
            library(extRemes)
            library(xts)
            
            ams=stock3
            
            
            maxams= period.apply(ams$High, endpoints(ams$High, "hours"), max)
            maxams=as.matrix(as.data.frame(maxams))
            
            minams <- period.apply(ams$Low, endpoints(ams$Low, "hours"), min)
            minams=as.matrix(as.data.frame(minams))
            xt=1:nrow(maxams)
            i=1 
            j=1
            for(i in i:nrow(maxams))
            { 
              for(j in j:nrow(stock3))
              {
                if(maxams[i]==hals[j])
                {
                  xt[i]=j
                  
                  break
                }
                else
                {
                  
                }
                
              }
            }
            
            
            
            yt=1:nrow(minams)
            i=1 
            j=1
            for(i in i:nrow(minams))
            { 
              for(j in j:nrow(stock3))
              {
                if(minams[i]==lals[j])
                {
                  yt[i]=j
                  
                  break
                }
                else
                {
                  
                }
                
              }
            }
            
            
            
            pks11=xt
            pksl2=yt
            
            dpks11=xt
            dpksl2=yt
     
            x1=(length(hals)-(pks11[length(pks11)]))-(length(hals)-(pks11[length(pks11)-1])) 
            y1= hals[pks11[length(pks11)]]- hals[pks11[length(pks11)-1]]
            m11[ch]=y1/x1
            
            
            
            x2=(length(lals)-(pksl2[length(pksl2)]))-(length(lals)-(pksl2[length(pksl2)-1])) 
            y2= lals[pksl2[length(pksl2)]]- lals[pksl2[length(pksl2)-1]]
            m22[ch]=y2/x2
            
            
            x1=(length(hals)-(pks11[length(pks11)-1]))-(length(hals)-(pks11[length(pks11)-2])) 
            y1= hals[pks11[length(pks11)-1]]- hals[pks11[length(pks11)-2]]
            mm1[ch]=y1/x1
            
            
            x2=(length(lals)-(pksl2[length(pksl2)-1]))-(length(lals)-(pksl2[length(pksl2)-2])) 
            y2= lals[pksl2[length(pksl2)-1]]- lals[pksl2[length(pksl2)-2]]
            mm2[ch]=y2/x2
            
            xx1=(length(hals)-(dpks11[length(dpks11)]))-(length(hals)-(dpks11[length(dpks11)-1])) 
            yy1= hals[dpks11[length(dpks11)]]- hals[dpks11[length(dpks11)-1]]
            m1[ch]=yy1/xx1
            
            xx2=(length(lals)-(dpksl2[length(dpksl2)]))-(length(lals)-(dpksl2[length(dpksl2)-1])) 
            yy2= lals[dpksl2[length(dpksl2)]]- lals[dpksl2[length(dpksl2)-1]]
            m2[ch]=yy2/xx2
            
            
            k[ch]=m1[ch]
            
            l[ch]=m2[ch]
            
            m[ch]=m11[ch]
            
            n[ch]=m22[ch]
            
            
            
            
            zy1[ch]=hals[pks11[length(pks11)]]-m1[ch]*(length(hals)-(pks11[length(pks11)])) 
            
            
            
            zy11[ch]=hals[pks11[length(pks11)-1]]-m11[ch]*(length(hals)-(pks11[length(pks11)-1])) 
            
            
            zy22[ch]=lals[pksl2[length(pksl2)-1]]-m22[ch]*(length(lals)-(pksl2[length(pksl2)-1])) 
            
            
            
            zy2[ch]=lals[pksl2[length(pksl2)]]-m2[ch]*(length(lals)-(pksl2[length(pksl2)])) 
            zx[ch]=pks11[length(pks11)]
            zy[ch]=pksl2[length(pksl2)]
            
            zx111[ch]=pks11[length(pks11)-1]
            zy122[ch]=pksl2[length(pksl2)-1]
            mxb[ch]=bals[length(bals)]
            res[ch]=hals[pks11[length(pks11)]]
            sup[ch]=lals[pksl2[length(pksl2)]]
            resls[ch]=hals[pks11[length(pks11)-1]]
            supls[ch]=lals[pksl2[length(pksl2)-1]]
          }
          
          m22[is.na(m22)]=0
          
          
          ckk=1:10
          ckk[1:10]=0
          ukk=1:10
          ukk[1:10]=0
          rkk=1:10
          rkk[1:10]=0
          skk=1:10
          skk[1:10]=0
          for(ch in 2:10)
          { 
            print(ch)
            
            
            if((m11[ch]>0)&&(m22[ch]>0))
            {
              print("downtrending-last")
              
              if(((mxb[ch]-mxb[ch-1])*100)/(mxb[ch-1])>3)
              { 
                if((zy1[ch]>=zy11[ch])&&(zy1[ch]>=zy2[ch]))
                { ckk[ch]<-mxb[ch]
                print(ckk)
                
                
                print("Down channel breakout")
                }
                
              }
            } else if((m11[ch]<0)&&(m22[ch]<0))
            {
              
              print("uptrending-last")
              
              if(((mxb[ch]-mxb[ch-1])*100)/(mxb[ch-1])>3)
              { 
                if((zy1[ch]>=zy11[ch])&&(zy1[ch]>=zy2[ch]))
                {
                  ukk[ch]<-mxb[ch]
                  print(ukk)
                  
                  print("Up channel breakout")
                  
                  
                }
                
              }
              
            }
            
            
            if((m1[ch]>0)&&(m2[ch]>0))
            {
              print("downtrending")
              
            } else if((m1[ch]<0)&&(m2[ch]<0))
            {
              print("uptrending")
            }   
            
            if(( mxb[ch]<zy11[ch])&&( mxb[ch]>sup[ch]))
            {
              if(resls[ch]<mxb[ch])
              {  if(((mxb[ch]-mxb[ch-1])*100)/(mxb[ch-1])>2)
              {
                print("In channel last-res brk")
                rkk[ch]<-mxb[ch]
                print(rkk)
              }
                
              }
              
              
              if(sup[ch]<mxb[ch])
              { 
                
                if(((mxb[ch]-mxb[ch-1])*100)/(mxb[ch-1])>2)
                {
                  
                  print("In channel-sup brk")
                  skk[ch]<-mxb[ch]
                  print(skk) 
                }
              }
              
            }
            
            
            
          }
          
          
          print("seperate")
          
          
       
          
          
          
          if((k[ch-1]==k[ch])&&(l[1]==l[ch]))
          {
            
            
          } else if((k[ch-1]==k[ch])&&(l[1]>l[ch]))
          {
            print("down-up reversal")
          }else if((k[ch-1]==k[ch])&&(l[1]<l[ch]))
          {
            print("up-down reversal")
          }else if((k[ch-1]<k[ch])&&(l[1]==l[ch]))
          {   
            print("down-up reversal")
          }else if((k[ch-1]>k[ch])&&(l[1]==l[ch]))
          {
            print("down-up reversal")
          }else if((k[ch-1]<k[ch])&&(l[1]<l[ch]))
          {
            print("up-down reversal")
          }else if((k[ch-1]>k[ch])&&(l[1]>l[ch]))
          {
            print("down-up reversal")
          }else if((k[ch-1]<k[ch])&&(l[1]>l[ch]))
          {
            print("squeeze")
          }
          
          
          df <- data.frame(Date=index(stock3),coredata(stock3))
          
          df$ID=(length(mals)-seq.int(nrow(df)))+1
          df$Volume=v
          vl<-as.matrix(df$Volume)
          vl=as.numeric(vl[length( vl)])
          mvl=mean(df$Volume)
          
          
          if(mvl<vl)
          {
            print("above Avol")
          }
          
          
          # create Bollinger Bands
          bbands <- BBands(df[,c("High","Low","Close")])
          
          # join and subset data
          df <- subset(cbind(df, data.frame(bbands[,1:3])))
          
          bbnd<-as.matrix(df)
          uc1=(as.numeric(bbnd[ nrow(bbnd),8]))-0.05*(as.numeric(bbnd[ nrow(bbnd),8]))
          uc2=(as.numeric(bbnd[ nrow(bbnd),10]))+0.05*(as.numeric(bbnd[ nrow(bbnd),10]))
          
    
          
          if((ups>1)&&(csym==symbol)&&(uc==uc1))
          {
       
            
          } else
          {
            updateSliderInput(session, "delaymm", value =c(uc1, uc2),
                              min = uc1-2, max =uc2+2)
            
          }
          uc<<-uc1
          csym<<-symbol
          ups<<-ups+1
          
          if(  as.numeric(bbnd[ nrow(bbnd),9])<mxb[ch])
          {
            print("above bband")
          }
          
          # colors column for increasing and decreasing
          for (i in 1:length(df[,1])) {
            if (df$Close[i] >= df$Open[i]) {
              df$direction[i] = 'Increasing'
            } else {
              df$direction[i] = 'Decreasing'
            }
          }
          
          dly2<- as.data.frame(input$delaymm)
          dly2=as.matrix(dly2)
          dly3=dly2[2,1]
          dly2=dly2[1,1]
          
          
          
          
          i <- list(line = list(color = I('green')))
          d <- list(line = list(color = I('red')))
          
          # plot candlestick chart
          p <- df %>%
            plot_ly(x = ~Date, type="candlestick",
                    open = ~df$Open, close = ~df$Close,
                    high = ~df$High, low = ~df$Low, name = "Stock Chart",
                    increasing = i, decreasing = d) %>%
            add_lines(x = ~df$Date, y = ~df$up , name = "B Bands",
                      line = list(color = 'brown', width = 1),
                      legendgroup = "Bollinger Bands"
            ) %>%
            add_lines(x = ~df$Date, y = ~df$dn, name = "B Bands",
                      line = list(color = 'brown', width = 1),
                      legendgroup = "Bollinger Bands"
            ) %>%
            add_lines(x = ~df$Date, y = ~df$mavg, name = "Mv Avg",
                      line = list(color = 'pink', width =1)
            ) %>%
            
            add_lines(x = df$Date, y = df$Close,line = list(color = 'black', yaxis = 'y1',width = 0.75))   %>%  
            add_lines(x = ~df[(pks11),1],  y = ~ hals[pks11],    type = 'scatter',  mode = 'markers',yaxis = 'y1' ,color = I('black')    ,name = "chnlres"        )%>%   
            add_lines(  x = ~df[(pksl2),1],y = ~ lals[pksl2],  type = 'scatter',mode = 'markers',yaxis = 'y1' ,color = I('brown') ,name = "chnlsup"           )%>%     
            add_lines(x = ~df[(dpks11),1],  y = ~ hals[dpks11],    type = 'scatter',  mode = 'markers',yaxis = 'y1' ,color = I('purple')    ,name = "chnlres"        )%>%   
            add_lines(  x = ~df[(dpksl2),1],y = ~ lals[dpksl2],  type = 'scatter',mode = 'markers',yaxis = 'y1' ,color = I('cyan') ,name = "chnlsup"           )%>%     
            
            add_segments(x =  ~df[1,1], xend = ~ df[nrow(df),1], y =  ~ resls[ch], yend =  ~ resls[ch],name = "resistance" ,yaxis = 'y1' ,color = I('blue'))%>%    
            add_segments(x =  df[1,1], xend = ~df[nrow(df),1], y =  ~  supls[ch], yend =  ~supls[ch],name =  "support" ,yaxis = 'y1' ,color = I('orange'))%>%    
            add_segments(x =  ~df[zx[ch],1], xend = ~df[nrow(df),1], y =  ~  hals[zx[ch]], yend =  ~ zy1[ch],name = "extrares" ,yaxis = 'y1' ,color = I('black'))%>%    
            add_segments(x =  ~df[zy[ch],1], xend = ~df[nrow(df),1], y =  ~  lals[zy[ch]], yend =  ~ zy2[ch],name =  "extrassup" ,yaxis = 'y1' ,color = I('brown'))%>%  
            add_segments(x =  ~df[zx111[ch],1], xend = ~df[nrow(df),1], y =  ~  hals[zx111[ch]], yend =  ~ zy11[ch],name = "extrares1" ,yaxis = 'y1' ,color = I('#7F7F7F'))%>%    
            add_segments(x =  ~df[zy122[ch],1], xend = ~df[nrow(df),1], y =  ~  lals[zy122[ch]], yend =  ~ zy22[ch],name =  "extrassup1" ,yaxis = 'y1' ,color = I('#17BECF'))%>%  
            add_segments(x =  ~df[1,1], xend = ~ df[nrow(df),1], y =  ~ res[ch], yend =  ~ res[ch],name = "resistance" ,yaxis = 'y1' ,color = I('red'))%>%    
            add_segments(x =  df[1,1], xend = ~df[nrow(df),1], y =  ~  sup[ch], yend =  ~sup[ch],name =  "support" ,yaxis = 'y1' ,color = I('green'))%>%    
            layout(yaxis = list(title = "Price",range=c(dly2,dly3)))
          
          # plot volume bar chart
          pp <- df %>%
            plot_ly(x=~df$Date, y=~df$Volume, type='bar', name = "Volume",hoverinfo =  "text" , color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
            add_lines(x = df$Date, y = mvl,name = "AVolume",line = list(color = 'pink', yaxis = 'y1',width = 2))   %>%  
            
            layout(yaxis = list(title = "AVolume"))
          
          
          
          p <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
                       shareX = TRUE, titleY = TRUE)
          
          
          p    
        
          
     
        }
        
        
        
        
      }}
  )
  
  
  
  
  
  output$plotwrh<-renderPlotly(
    {  
      
      {  
        
        if(hv$i > 0) {
          
          
          if(input$somevalue2=="TRUE")
          {
            maxIter3 <<-3000
            hv$i <<-10
            
            
            
            
            allsym=(as.data.frame(input$stock1))
            
            allsym=as.data.frame(as.matrix(input$stock1))
            
            
            colnames(allsym)=c("symbol")
            
            symbol=as.character(allsym[1,1])
            
            
            
            rest= sqldf('SELECT distinct * FROM hest where symbol not in("NSE:XXX") order by Date desc ',drv='SQLite')
            test=as.data.frame(rest)
            
            dly2<- as.data.frame(input$delaydmm)
            dly2=as.matrix(dly2)
            dnth<<-dnth-1
            dly3=-dnth
            dly2=dly2[1,1]
            dly3<- as.data.frame(dly3)
            colnames(dly3)= "dly"
            
            
            
            dly2<- as.data.frame(dly2)
            colnames(dly2)= "dly"
            
            lnth=-as.numeric(as.character(dly2))
            
            dnth=-as.numeric(as.character(dly3))
            

            if(dnth<2)
            {
              dnth<<-dnth+1
            }
            
            
            test$dl=-seq.int(nrow(test))
            
            
            
            test <-  sqldf('SELECT a.* FROM test a   join dly2 b on a.dl>=b.dly  join dly3 c on a.dl<=c.dly order by Date desc',drv='SQLite')
            
            test=as.data.frame(test)
            
            
            
            test=sqldf('SELECT distinct * FROM test  order by Date asc ',drv='SQLite')
            
            
            
            Lines <-  sqldf('SELECT  Date as Date ,open as Open , high as High, low as Low, close as Close,  volume FROM test order by Date desc',drv='SQLite')
            
            
            
            Lines$Open=gsub("\\.", ",", Lines$Open)
            
            
            Lines$High=gsub("\\.", ",", Lines$High)
            
            
            Lines$Low=gsub("\\.", ",", Lines$Low)
            
            
            Lines$Close=gsub("\\.", ",", Lines$Close)
            
            
            df=sapply(Lines, function(x) gsub("\"", "", x))
            
            df1=as.data.frame(df,quote=FALSE)
            
            
            connectionString <- paste0(df1$Date,";",df1$Open,";",df1$High,";",df1$Low,";",df1$Close,";",df1$volume)
            
            names= paste0("Date;Open;High;Low;Close;Volume")
            
            
            names=as.data.frame(names)
            connectionString=as.data.frame(connectionString)
            
            colnames(connectionString)=c("names")
            
            dd=rbind(names,connectionString)
            
            
            connectionString <- lapply(dd, as.character)
            
            df=sapply(connectionString, function(x) gsub("\"", "", x))
            
            
            
            
            
            conn <- textConnection(df)
            stock=as.xts(read.zoo(conn, sep=';', tz="US/Eastern", dec=",", header=TRUE,
                                  format='%Y-%m-%d', index.column=1))
            
            stock1= to.minutes(stock)
            
            
            
            close(conn)
            
            
            
            
            stock2=as.matrix(stock1)
            stock2=stock1[,1:4]
            
            stock2=as.data.frame(stock2)
            
            colnames(stock2) <- c("Open", "High", "Low", "Close")
            
            stock3=as.matrix(stock2)
            
            stock3=stock3[,1:4]
            
            
            
            stock3=as.xts(stock3,tz="EST")
            .index(stock3)[1:nrow(stock3)] <-(.index(stock3)[1:nrow(stock3)])+10.5*3600
            
            
            mals=as.matrix(test$close)
            mals=as.matrix(test$close)
            
            mals=as.numeric(mals[,1]) 
            
            hals=as.matrix(test$High)
            
            hals=as.numeric(hals[,1]) 
            
            lals=as.matrix(test$Low)
            
            lals=as.numeric(lals[,1]) 
            
            vcols=as.matrix(test$volume)
            
            vcols=as.numeric(vcols[,1]) 
            
            
            
            mals=mals[1:length(mals)]
            nx=-(1:length(mals))
            
            bals=mals[-nx]
            
            nx=-(length(mals):1)
            
            x <- nx
            
            nx=-(1:length(mals))
            y <- bals[-nx]
            v <- vcols[-nx]
            
            
            k=1:10
            l=1:10
            m=1:10
            n=1:10
            brk=1:10
            
            m1=1:10
            m11=1:10
            m2=1:10
            m22=1:10
            zy1=1:10
            zy2=1:10
            zy11=1:10
            zy22=1:10
            mxb=1:10
            zx111=1:10
            zy122=1:10
            zx=1:10
            zy=1:10
            sup=1:10
            res=1:10
            resls=1:10
            supls=1:10 
            
            {
              library(extRemes)
              library(xts)
              
              
              
              ams=stock3
              
              
              vv=to.weekly(stock3)
              bbb<-stock3
              vv=bbb$High
              
              ress=detectSupportResistance(vv, tolerance=1, nChunks=10, nPoints=10, plotChart=TRUE)
              
              maxams <- ams$High
              
              
              maxams=as.matrix(as.data.frame(maxams))
              
              minams <- ams$Low
              
              minams=as.matrix(as.data.frame(minams))
              xt=1:nrow(maxams)
              i=1 
              j=1
              for(i in i:nrow(maxams))
              { 
                for(j in j:nrow(stock3))
                {
                  if(maxams[i]==hals[j])
                  {
                    xt[i]=j
                    
                    break
                  }
                  else
                  {
                    
                  }
                  
                }
              }
              
              
              
              yt=1:nrow(minams)
              i=1 
              j=1
              for(i in i:nrow(minams))
              { 
                for(j in j:nrow(stock3))
                {
                  if(minams[i]==lals[j])
                  {
                    yt[i]=j
                    
                    break
                  }
                  else
                  {
                    
                  }
                  
                }
              }
              
              
              
              pks11=sort(c(ress$maximaAt,ress$minimaAt))
              
              vv=bbb$Low
              
              ress=detectSupportResistance(vv, tolerance=1, nChunks=10, nPoints=10, plotChart=TRUE)
              
              pksl2=sort(c(ress$maximaAt,ress$minimaAt))
              
              print(pks11)
              print(pksl2)
              dpks111= which(xt == max(pks11))
              dpksl22=which(yt == max(pksl2))
              
         
              
              dpks11=xt[((dpks111):length(xt))]
              dpksl2=yt[((dpksl22):length(yt))]
              
              print(dpks11)
              print(dpksl2)
              if(length(dpks11)<2)
              {  dpks11=xt[((dpks111-1):length(xt))]
              
              
              }
              
              if(length(dpksl2)<2)
              { 
                dpksl2=yt[((dpksl22-1):length(yt))]
                
              }
              
              
              
              
              x1=(length(hals)-(pks11[length(pks11)]))-(length(hals)-(pks11[length(pks11)-1])) 
              y1= hals[pks11[length(pks11)]]- hals[pks11[length(pks11)-1]]
              m1=y1/x1
              
              
              
              x2=(length(lals)-(pksl2[length(pksl2)]))-(length(lals)-(pksl2[length(pksl2)-1])) 
              y2= lals[pksl2[length(pksl2)]]- lals[pksl2[length(pksl2)-1]]
              m2=y2/x2
              
              
              x1=(length(hals)-(pks11[length(pks11)-1]))-(length(hals)-(pks11[length(pks11)-2])) 
              y1= hals[pks11[length(pks11)-1]]- hals[pks11[length(pks11)-2]]
              m11=y1/x1
              
              
              x2=(length(lals)-(pksl2[length(pksl2)-1]))-(length(lals)-(pksl2[length(pksl2)-2])) 
              y2= lals[pksl2[length(pksl2)-1]]- lals[pksl2[length(pksl2)-2]]
              m22=y2/x2
              
              
              
              
              k=m1
              
              l=m2
              
              m=m11
              
              n=m22
              
              
              
              
              zy1=hals[pks11[length(pks11)]]-m1*(length(hals)-(pks11[length(pks11)])) 
              
              
              
              zy11=hals[pks11[length(pks11)-1]]-m11*(length(hals)-(pks11[length(pks11)-1])) 
              
              
              zy22=lals[pksl2[length(pksl2)-1]]-m22*(length(lals)-(pksl2[length(pksl2)-1])) 
              
              
              
              zy2=lals[pksl2[length(pksl2)]]-m2*(length(lals)-(pksl2[length(pksl2)])) 
              zx=pks11[length(pks11)]
              zy=pksl2[length(pksl2)]
              
              zx111=pks11[length(pks11)-1]
              zy122=pksl2[length(pksl2)-1]
              mxb=bals[length(bals)]
              res=hals[pks11[length(pks11)]]
              sup=lals[pksl2[length(pksl2)]]
              resls=hals[pks11[length(pks11)-1]]
              supls=lals[pksl2[length(pksl2)-1]]
              
              print( hals[pks11])
              
              print(  lals[pksl2])
              
              print( hals[dpks11])
              
              print(  lals[dpksl2])
              
              ch11=c(hals[pks11])
              ch12=c(lals[pksl2])
              ch111=c(hals[dpks11])
              ch112=c(lals[dpksl2])
              
              
              
              if((((ch11[length(ch11)]==ch111[length(ch111)])||ch12[length(ch12)]==ch112[length(ch112)]))||((ch11[length(ch11)]==ch111[length(ch111)-1])||(ch12[length(ch12)]==ch112[length(ch112)-1])))
              {
                if(((((ch11[length(ch11)]-ch11[length(ch11)-1])*100)/ch11[length(ch11)-1])>(-0.5))&&((((ch12[length(ch12)]-ch12[length(ch12)-1])*100)/ch11[length(ch12)-1])>(-0.5)))
                {
                  if((length(ch111)<=2)&&(length(ch112)<=2))
                  {
                    print("down-up reversal")
                  }
                }
              }
              
              
              
              if(length(dpks11)>2)
              { md= min(dpks11)
              mx= max(dpks11)
              dpks11=c(md,mx)
              
              }
              
              
              if(length(dpksl2)>2)
              {
                md= min(dpksl2)
                mx= max(dpksl2)
                dpksl2=c(md,mx)
                
              }
              
              
              
              ch111=c(hals[dpks11])
              ch112=c(lals[dpksl2])
              if(((((ch11[length(ch11)]-ch11[length(ch11)-1])*100)/ch11[length(ch11)-1])>(-0.5))&&((((ch12[length(ch112)]-ch12[length(ch12)-1])*100)/ch12[length(ch12)-1])>(-0.5))&&((((ch111[length(ch111)]-ch111[length(ch111)-1])*100)/ch111[length(ch111)-1])>(-0.5))&&((((ch112[length(ch112)]-ch112[length(ch112)-1])*100)/ch112[length(ch112)-1])>(-0.5)))
              {
                print("down-up reversal")
              }
              
              
              
              df <- data.frame(Date=index(stock3),coredata(stock3))
              
              df$ID=(length(mals)-seq.int(nrow(df)))+1
              df$Volume=v
              vl<-as.matrix(df$Volume)
              vl=as.numeric(vl[length( vl)])
              mvl=mean(df$Volume)
              
              # create Bollinger Bands
              bbands <- BBands(df[,c("High","Low","Close")])
              
              # join and subset data
              df <- subset(cbind(df, data.frame(bbands[,1:3])))
              
              bbnd<-as.matrix(df)
              uc1=(as.numeric(bbnd[ nrow(bbnd),8]))-0.3*(as.numeric(bbnd[ nrow(bbnd),8]))
              uc2=(as.numeric(bbnd[ nrow(bbnd),10]))+0.1*(as.numeric(bbnd[ nrow(bbnd),10]))
              
              if((ups>1)&&(csym==symbol)&&(uc==uc1))
              {
                
                
              } else
              {
                updateSliderInput(session, "delaymm", value =c(uc1, uc2),
                                  min = uc1-10, max =uc2+10)
                
              }
              uc<<-uc1
              csym<<-symbol
              ups<<-ups+1
              
              
              # colors column for increasing and decreasing
              for (i in 1:length(df[,1])) {
                if (df$Close[i] >= df$Open[i]) {
                  df$direction[i] = 'Increasing'
                } else {
                  df$direction[i] = 'Decreasing'
                }
              }
              
              dly2<- as.data.frame(input$delaymm)
              dly2=as.matrix(dly2)
              dly3=dly2[2,1]
              dly2=dly2[1,1]
              
              
              
              
              i <- list(line = list(color = I('green')))
              d <- list(line = list(color = I('red')))
              
              # plot candlestick chart
              p <- df %>%
                plot_ly(x = ~Date, type="candlestick",
                        open = ~df$Open, close = ~df$Close,
                        high = ~df$High, low = ~df$Low, name = "Stock Chart",
                        increasing = i, decreasing = d) %>%
                
                add_lines(x = ~df$Date, y = ~df$up , name = "B Bands",
                          line = list(color = 'brown', width = 1),
                          legendgroup = "Bollinger Bands"
                ) %>%
                add_lines(x = ~df$Date, y = ~df$dn, name = "B Bands",
                          line = list(color = 'brown', width = 1),
                          legendgroup = "Bollinger Bands"
                ) %>%
                add_lines(x = ~df$Date, y = ~df$mavg, name = "Mv Avg",
                          line = list(color = 'pink', width =1)
                ) %>%
                
                add_lines(x = df$Date, y = df$Close,line = list(color = 'black', yaxis = 'y1',width = 0.75))   %>%  
                add_lines(x = ~df[(pks11),1],  y = ~ hals[pks11],    type = 'scatter',  mode = 'markers',yaxis = 'y1' ,color = I('black')    ,name = "chnlres"        )%>%   
                add_lines(  x = ~df[(pksl2),1],y = ~ lals[pksl2],  type = 'scatter',mode = 'markers',yaxis = 'y1' ,color = I('brown') ,name = "chnlsup"           )%>%     
                add_lines(x = ~df[(dpks11),1],  y = ~ hals[dpks11],    type = 'scatter',  mode = 'markers',yaxis = 'y1' ,color = I('purple')    ,name = "chnlres"        )%>%   
                add_lines(  x = ~df[(dpksl2),1],y = ~ lals[dpksl2],  type = 'scatter',mode = 'markers',yaxis = 'y1' ,color = I('cyan') ,name = "chnlsup"           )%>%     
                
            add_segments(x =  ~df[1,1], xend = ~ df[nrow(df),1], y =  ~ resls, yend =  ~ resls,name = "resistance" ,yaxis = 'y1' ,color = I('blue'))%>%    
                   add_segments(x =  df[1,1], xend = ~df[nrow(df),1], y =  ~  supls, yend =  ~supls,name =  "support" ,yaxis = 'y1' ,color = I('orange'))%>%    
                #add_segments(x =  ~df[zx[ch],1], xend = ~df[nrow(df),1], y =  ~  hals[zx[ch]], yend =  ~ zy1[ch],name = "extrares" ,yaxis = 'y1' ,color = I('black'))%>%    
                #   add_segments(x =  ~df[zy[ch],1], xend = ~df[nrow(df),1], y =  ~  lals[zy[ch]], yend =  ~ zy2[ch],name =  "extrassup" ,yaxis = 'y1' ,color = I('brown'))%>%  
                #        add_segments(x =  ~df[zx111[ch],1], xend = ~df[nrow(df),1], y =  ~  hals[zx111[ch]], yend =  ~ zy11[ch],name = "extrares1" ,yaxis = 'y1' ,color = I('#7F7F7F'))%>%    
                #       add_segments(x =  ~df[zy122[ch],1], xend = ~df[nrow(df),1], y =  ~  lals[zy122[ch]], yend =  ~ zy22[ch],name =  "extrassup1" ,yaxis = 'y1' ,color = I('#17BECF'))%>%  
                 add_segments(x =  ~df[1,1], xend = ~ df[nrow(df),1], y =  ~ res, yend =  ~ res,name = "resistance" ,yaxis = 'y1' ,color = I('red'))%>%    
                 add_segments(x =  df[1,1], xend = ~df[nrow(df),1], y =  ~  sup, yend =  ~sup,name =  "support" ,yaxis = 'y1' ,color = I('green'))%>%    
                layout(yaxis = list(title = "Price",range=c(dly2,dly3)))
              
              # plot volume bar chart
              pp <- df %>%
                plot_ly(x=~df$Date, y=~df$Volume, type='bar', name = "Volume",hoverinfo =  "text" , color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
                add_lines(x = df$Date, y = mvl,name = "AVolume",line = list(color = 'pink', yaxis = 'y1',width = 2))   %>%  
                
                layout(yaxis = list(title = "AVolume"))
              
              
              
              p <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
                           shareX = TRUE, titleY = TRUE)
              
              
              p  
              
            }
            
          }else 
          {
            maxIter3 <<-0
            
            
            allsym=(as.data.frame(input$stock1))
            
            allsym=as.data.frame(as.matrix(input$stock1))
            
            
            colnames(allsym)=c("symbol")
            
            symbol=as.character(allsym[1,1])
            
            
            
            
            
            rest= sqldf('SELECT distinct * FROM hest where symbol not in("NSE:XXX") order by Date desc ',drv='SQLite')
            
            test=as.data.frame(rest)
            
            test=test[1:150, ]

            dly2<- as.data.frame(input$delaydmm)
            dly2=as.matrix(dly2)
            k=1:10
            l=1:10
            m=1:10
            n=1:10
            brk=1:10
            mm1=1:10
            mm2=1:10
            m1=1:10
            m11=1:10
            m2=1:10
            m22=1:10
            zy1=1:10
            zy2=1:10
            zy11=1:10
            zy22=1:10
            mxb=1:10
            zx111=1:10
            zy122=1:10
            zx=1:10
            zy=1:10
            sup=1:10
            res=1:10
            resls=1:10
            supls=1:10
            dun=1:10
            dun[1:10]=0
            dly3=dly2[2,1]
            
            dly2=dly2[1,1]
            
            
            dly3<- as.data.frame(dly3)
            colnames(dly3)= "dly"
            
            
            
            dly2<- as.data.frame(dly2)
            colnames(dly2)= "dly"
            
            dnth=-as.numeric(as.character(dly3))
            dnth<<-dnth
            
            dly3=dly3-11
            
            dly2=dly3-149
            
            
            test$dl=-seq.int(nrow(test))
            for(ch in 1:10)
              
            { print(ch)
              
              dly3=dly3+1
              
              dly2=dly2+1
              test <- rest
              test$dl=-seq.int(nrow(test))
              
            
              test <-  sqldf('SELECT a.* FROM test a   join dly2 b on a.dl>=b.dly  join dly3 c on a.dl<=c.dly order by Date desc',drv='SQLite')
              
              test=as.data.frame(test)
              
              
              test=sqldf('SELECT distinct * FROM test  order by Date asc ',drv='SQLite')
 
              Lines <-  sqldf('SELECT  Date as Date ,open as Open , high as High, low as Low, close as Close,  volume FROM test order by Date desc',drv='SQLite')
              
              
              
              Lines$Open=gsub("\\.", ",", Lines$Open)
              
              
              Lines$High=gsub("\\.", ",", Lines$High)
              
              
              Lines$Low=gsub("\\.", ",", Lines$Low)
              
              
              Lines$Close=gsub("\\.", ",", Lines$Close)
              
              
              df=sapply(Lines, function(x) gsub("\"", "", x))
              
              df1=as.data.frame(df,quote=FALSE)
              
              
              connectionString <- paste0(df1$Date,";",df1$Open,";",df1$High,";",df1$Low,";",df1$Close,";",df1$volume)
              
              names= paste0("Date;Open;High;Low;Close;Volume")
              
              
              names=as.data.frame(names)
              connectionString=as.data.frame(connectionString)
              
              colnames(connectionString)=c("names")
              
              dd=rbind(names,connectionString)
              
              
              connectionString <- lapply(dd, as.character)
              
              df=sapply(connectionString, function(x) gsub("\"", "", x))
              
              
              
              
              
              conn <- textConnection(df)
              stock=as.xts(read.zoo(conn, sep=';', tz="US/Eastern", dec=",", header=TRUE,
                                    format='%Y-%m-%d', index.column=1))
              
              stock1= to.minutes(stock)
              
              
              
              close(conn)
              
              
              
              
              stock2=as.matrix(stock1)
              stock2=stock1[,1:4]
              
              stock2=as.data.frame(stock2)
              
              colnames(stock2) <- c("Open", "High", "Low", "Close")
              
              stock3=as.matrix(stock2)
              
              stock3=stock3[,1:4]
              
              
              
              stock3=as.xts(stock3,tz="EST")
              .index(stock3)[1:nrow(stock3)] <-(.index(stock3)[1:nrow(stock3)])+10.5*3600
              
              
              #     sds<<-stock3
              
              #    sdds<<-test
              mals=as.matrix(test$close)
              
              mals=as.numeric(mals[,1]) 
              
              hals=as.matrix(test$High)
              
              hals=as.numeric(hals[,1]) 
              
              lals=as.matrix(test$Low)
              
              lals=as.numeric(lals[,1]) 
              
              vcols=as.matrix(test$volume)
              
              vcols=as.numeric(vcols[,1]) 
              
              
              
              mals=mals[1:length(mals)]
              nx=-(1:length(mals))
              
              bals=mals[-nx]
              
              nx=-(length(mals):1)
              
              x <- nx
              
              nx=-(1:length(mals))
              y <- bals[-nx]
              v <- vcols[-nx]
              
              
              
              
              
              library(extRemes)
              library(xts)
              
              
              
              ams=stock3
              
          
              vv=to.monthly(stock3)
              
              bbb<-stock3
              vv=bbb$High
           
              ress=detectSupportResistance(vv, tolerance=1, nChunks=5, nPoints=5, plotChart=TRUE)
              
              maxams <- ams$High
             

              maxams=as.matrix(as.data.frame(maxams))
              
              minams <- ams$Low
              
              minams=as.matrix(as.data.frame(minams))
              xt=1:nrow(maxams)
              i=1 
              j=1
              for(i in i:nrow(maxams))
              { 
                for(j in j:nrow(stock3))
                {
                  if(maxams[i]==hals[j])
                  {
                    xt[i]=j
                    
                    break
                  }
                  else
                  {
                    
                  }
                  
                }
              }
              
              
              
              yt=1:nrow(minams)
              i=1 
              j=1
              for(i in i:nrow(minams))
              { 
                for(j in j:nrow(stock3))
                {
                  if(minams[i]==lals[j])
                  {
                    yt[i]=j
                    
                    break
                  }
                  else
                  {
                    
                  }
                  
                }
              }
              
              
              
              pks11=sort(c(ress$maximaAt,ress$minimaAt))
              
              vv=bbb$Low
              
              ress=detectSupportResistance(vv, tolerance=1, nChunks=5, nPoints=5, plotChart=TRUE)
              
              pksl2=sort(c(ress$maximaAt,ress$minimaAt))
       
          
              dpks111= which(xt == max(pks11))
              dpksl22=which(yt == max(pksl2))
              
              
              
              dpks11=xt[((dpks111):length(xt))]
              dpksl2=yt[((dpksl22):length(yt))]
          
              if(length(dpks11)<2)
              {  dpks11=xt[((dpks111-1):length(xt))]
            
              
              }
           
              if(length(dpksl2)<2)
              { 
              dpksl2=yt[((dpksl22-1):length(yt))]
              
              }
              
          
           
              x1=(length(hals)-(pks11[length(pks11)]))-(length(hals)-(pks11[length(pks11)-1])) 
              y1= hals[pks11[length(pks11)]]- hals[pks11[length(pks11)-1]]
              m11[ch]=y1/x1
              
              
              
              x2=(length(lals)-(pksl2[length(pksl2)]))-(length(lals)-(pksl2[length(pksl2)-1])) 
              y2= lals[pksl2[length(pksl2)]]- lals[pksl2[length(pksl2)-1]]
              m22[ch]=y2/x2
              
              
              x1=(length(hals)-(pks11[length(pks11)-1]))-(length(hals)-(pks11[length(pks11)-2])) 
              y1= hals[pks11[length(pks11)-1]]- hals[pks11[length(pks11)-2]]
              mm1[ch]=y1/x1
              
              
              x2=(length(lals)-(pksl2[length(pksl2)-1]))-(length(lals)-(pksl2[length(pksl2)-2])) 
              y2= lals[pksl2[length(pksl2)-1]]- lals[pksl2[length(pksl2)-2]]
              mm2[ch]=y2/x2
              
              xx1=(length(hals)-(dpks11[length(dpks11)]))-(length(hals)-(dpks11[length(dpks11)-1])) 
              yy1= hals[dpks11[length(dpks11)]]- hals[dpks11[length(dpks11)-1]]
              m1[ch]=yy1/xx1
              
              xx2=(length(lals)-(dpksl2[length(dpksl2)]))-(length(lals)-(dpksl2[length(dpksl2)-1])) 
              yy2= lals[dpksl2[length(dpksl2)]]- lals[dpksl2[length(dpksl2)-1]]
              m2[ch]=yy2/xx2
              
              
              k[ch]=m1[ch]
              
              l[ch]=m2[ch]
              
              m[ch]=m11[ch]
              
              n[ch]=m22[ch]
              
              
              
              
              zy1[ch]=hals[pks11[length(pks11)]]-m1[ch]*(length(hals)-(pks11[length(pks11)])) 
              
              
              
              zy11[ch]=hals[pks11[length(pks11)-1]]-m11[ch]*(length(hals)-(pks11[length(pks11)-1])) 
              
              
              zy22[ch]=lals[pksl2[length(pksl2)-1]]-m22[ch]*(length(lals)-(pksl2[length(pksl2)-1])) 
              
              
              
              zy2[ch]=lals[pksl2[length(pksl2)]]-m2[ch]*(length(lals)-(pksl2[length(pksl2)])) 
              zx[ch]=pks11[length(pks11)]
              zy[ch]=pksl2[length(pksl2)]
              
              zx111[ch]=pks11[length(pks11)-1]
              zy122[ch]=pksl2[length(pksl2)-1]
              mxb[ch]=bals[length(bals)]
              res[ch]=hals[pks11[length(pks11)]]
              sup[ch]=lals[pksl2[length(pksl2)]]
              resls[ch]=hals[pks11[length(pks11)-1]]
              supls[ch]=lals[pksl2[length(pksl2)-1]]
              
              
              
              ch11=c(hals[pks11])
              ch12=c(lals[pksl2])
              ch111=c(hals[dpks11])
              ch112=c(lals[dpksl2])
              
              print(ch11)
              print(ch12)
              print(ch111)
              print(ch112)
       
                if(((((ch11[length(ch11)]-ch11[length(ch11)-1])*100)/ch11[length(ch11)-1])>(0.5))&&((((ch12[length(ch12)]-ch12[length(ch12)-1])*100)/ch12[length(ch12)-1])>(0.5)))             

                {
                  
                
                  if((length(ch111)<=3)&&(length(ch112)<=3))
                  { print("hi")
                    print(((ch112[length(ch112)]-ch112[length(ch112)-1])*100)/ch112[length(ch112)-1])
                    print(((ch111[length(ch111)]-ch111[length(ch111)-1])*100)/ch111[length(ch111)-1])
                  if(((((ch111[length(ch111)]-ch111[length(ch111)-1])*100)/ch111[length(ch111)-1])>(-1.5))&&((((ch112[length(ch112)]-ch112[length(ch112)-1])*100)/ch112[length(ch112)-1])>(-1.5)))            
                    {
                    print("down-up reversal")
                    dun[ch]=1
                  }
                  }
                }
           
              if((length(ch111)>3)||(length(ch112)>3))
              {
                if(length(dpks11)>2)
                { md= min(dpks11)
                mx= max(dpks11)
                dpks11=c(md,mx)
                
                }
                
                
                if(length(dpksl2)>2)
                {
                  md= min(dpksl2)
                  mx= max(dpksl2)
                  dpksl2=c(md,mx)
                  
                }
                
                
                
                ch111=c(hals[dpks11])
                ch112=c(lals[dpksl2])
                print(((ch111[length(ch111)]-ch111[length(ch111)-1])*100)/ch111[length(ch111)-1])
         
                print(((ch112[length(ch112)]-ch112[length(ch112)-1])*100)/ch112[length(ch112)-1])
                if(((((ch111[length(ch111)]-ch111[length(ch111)-1])*100)/ch111[length(ch111)-1])>=(2))&&((((ch112[length(ch112)]-ch112[length(ch112)-1])*100)/ch112[length(ch112)-1])>=(2)))            
                {
                  print("down-up reversal")
                  dun[ch]=1
                }
              }
              
              
            }
            
            print(dun)

            
            
            print("seperate")
           
       
             df <- data.frame(Date=index(stock3),coredata(stock3))
            
            df1 <- data.frame(Date=index(vv),coredata(vv))
            
            
            df$ID=(length(mals)-seq.int(nrow(df)))+1
            df$Volume=v
            vl<-as.matrix(df$Volume)
            vl=as.numeric(vl[length( vl)])
            mvl=mean(df$Volume)
            
            
            if(mvl<vl)
            {
              print("above Avol")
            }
            
            
            # create Bollinger Bands
            bbands <- BBands(df[,c("High","Low","Close")])
            
            # join and subset data
            df <- subset(cbind(df, data.frame(bbands[,1:3])))
            
            bbnd<-as.matrix(df)
            uc1=(as.numeric(bbnd[ nrow(bbnd),8]))-0.1*(as.numeric(bbnd[ nrow(bbnd),8]))
            uc2=(as.numeric(bbnd[ nrow(bbnd),10]))+0.1*(as.numeric(bbnd[ nrow(bbnd),10]))
            
            if((ups>1)&&(csym==symbol)&&(uc==uc1))
            {
              
              
            } else
            {
              updateSliderInput(session, "delaymm", value =c(uc1, uc2),
                                min = uc1-10, max =uc2+10)
              
            }
            
            csym<<-symbol
            ups<<-ups+1
            
            uc<<-uc1
            
            
            if(  as.numeric(bbnd[ nrow(bbnd),9])<mxb[ch])
            {
              print("above bband")
            }
            
            # colors column for increasing and decreasing
            for (i in 1:length(df[,1])) {
              if (df$Close[i] >= df$Open[i]) {
                df$direction[i] = 'Increasing'
              } else {
                df$direction[i] = 'Decreasing'
              }
            }
            
            dly2<- as.data.frame(input$delaymm)
            dly2=as.matrix(dly2)
            dly3=dly2[2,1]
            dly2=dly2[1,1]
         
           
            i <- list(line = list(color = I('green')))
            d <- list(line = list(color = I('red')))
            
            # plot candlestick chart
            p1 <- df1 %>%
              plot_ly(x = ~Date, type="candlestick",
                      open = ~df1$stock3.Open, close = ~df1$stock3.Close,
                      high = ~df1$stock3.High, low = ~df1$stock3.Low, name = "Stock Chart",
                      increasing = i, decreasing = d) 
            
            p <- df %>%
              plot_ly(x = ~Date, type="candlestick",
                      open = ~df$Open, close = ~df$Close,
                      high = ~df$High, low = ~df$Low, name = "Stock Chart",
                      increasing = i, decreasing = d) %>%
              
              add_lines(x = ~df$Date, y = ~df$up , name = "B Bands",
                        line = list(color = 'brown', width = 1),
                        legendgroup = "Bollinger Bands"
              ) %>%
              add_lines(x = ~df$Date, y = ~df$dn, name = "B Bands",
                        line = list(color = 'brown', width = 1),
                        legendgroup = "Bollinger Bands"
              ) %>%
              add_lines(x = ~df$Date, y = ~df$mavg, name = "Mv Avg",
                        line = list(color = 'pink', width =1)
              ) %>%
              
              add_lines(x = df$Date, y = df$Close,line = list(color = 'black', yaxis = 'y1',width = 0.75))   %>%  
              add_lines(x = ~df[(pks11),1],  y = ~ hals[pks11],    type = 'scatter',  mode = 'markers',yaxis = 'y1' ,color = I('black')    ,name = "chnlres"        )%>%   
              add_lines(  x = ~df[(pksl2),1],y = ~ lals[pksl2],  type = 'scatter',mode = 'markers',yaxis = 'y1' ,color = I('brown') ,name = "chnlsup"           )%>%     
              add_lines(x = ~df[(dpks11),1],  y = ~ hals[dpks11],    type = 'scatter',  mode = 'markers',yaxis = 'y1' ,color = I('purple')    ,name = "chnlres"        )%>%   
              add_lines(  x = ~df[(dpksl2),1],y = ~ lals[dpksl2],  type = 'scatter',mode = 'markers',yaxis = 'y1' ,color = I('cyan') ,name = "chnlsup"           )%>%     
              
          # add_segments(x =  ~df[1,1], xend = ~ df[nrow(df),1], y =  ~ resls[ch], yend =  ~ resls[ch],name = "resistance" ,yaxis = 'y1' ,color = I('blue'))%>%    
       #    add_segments(x =  df[1,1], xend = ~df[nrow(df),1], y =  ~  supls[ch], yend =  ~supls[ch],name =  "support" ,yaxis = 'y1' ,color = I('orange'))%>%    
          #  add_segments(x =  ~df[zx[ch],1], xend = ~df[nrow(df),1], y =  ~  hals[zx[ch]], yend =  ~ zy1[ch],name = "extrares" ,yaxis = 'y1' ,color = I('black'))%>%    
          #  add_segments(x =  ~df[zy[ch],1], xend = ~df[nrow(df),1], y =  ~  lals[zy[ch]], yend =  ~ zy2[ch],name =  "extrassup" ,yaxis = 'y1' ,color = I('brown'))%>%  
         #  add_segments(x =  ~df[zx111[ch],1], xend = ~df[nrow(df),1], y =  ~  hals[zx111[ch]], yend =  ~ zy11[ch],name = "extrares1" ,yaxis = 'y1' ,color = I('#7F7F7F'))%>%    
        #    add_segments(x =  ~df[zy122[ch],1], xend = ~df[nrow(df),1], y =  ~  lals[zy122[ch]], yend =  ~ zy22[ch],name =  "extrassup1" ,yaxis = 'y1' ,color = I('#17BECF'))%>%  
      # add_segments(x =  ~df[1,1], xend = ~ df[nrow(df),1], y =  ~ res[ch], yend =  ~ res[ch],name = "resistance" ,yaxis = 'y1' ,color = I('red'))%>%    
      #    add_segments(x =  df[1,1], xend = ~df[nrow(df),1], y =  ~  sup[ch], yend =  ~sup[ch],name =  "support" ,yaxis = 'y1' ,color = I('green'))%>%    
              layout(yaxis = list(title = "Price",range=c(dly2,dly3)))
            
            # plot volume bar chart
            pp <- df %>%
              plot_ly(x=~df$Date, y=~df$Volume, type='bar', name = "Volume",hoverinfo =  "text" , color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
              add_lines(x = df$Date, y = mvl,name = "AVolume",line = list(color = 'pink', yaxis = 'y1',width = 2))   %>%  
              
              layout(yaxis = list(title = "AVolume"))
            
            
            
            p <- subplot(p, pp, heights = c(0.7,0.3), nrows=2,
                         shareX = TRUE, titleY = TRUE)
            
            
            p  
            
            
            
          }
          
          
          
        }}      
    }
  )
  
  
  output$plot1mshm<-renderPlotly(
    
    {   
      print(input$delayhm)
      
      
      
      dly2<- as.data.frame(input$delayhm)
      dly2=as.matrix(dly2)
      
      dly3=-dly2[2,1]
      dly2=-dly2[1,1]
      dly3<- as.data.frame(dly3)
      colnames(dly3)= "dly"
      
      
      
      dly2<- as.data.frame(dly2)
      colnames(dly2)= "dly"
      
      allsym=as.data.frame(as.matrix(input$stock1))
      print(allsym)
      
      colnames(allsym)=c("symbol")
      
      symbol=as.character(allsym[1,1])
      
      
      yoursym1<- symbol
      
      tryCatch({
        
        
        
        
        if((length(yoursym1) >=1)&&(yoursym1[1]!=0)){
          i=1
          
          
          # cool progress bar to see the % of completion
          n <- length(yoursym1)
          pb <- txtProgressBar(min = 0, max = n, style=3)
          
          i=1;
          
          dataEnv <- new.env()
          # Actual loop: 
          for(i in 1:((length(yoursym1))))
          {
            yoursym1[i]-> symbol
            tickers <- c(symbol)
            
            
            tryit <- try(getSymbols(tickers,src="yahoo", env=dataEnv))
            
            
            
            
            if(inherits(tryit, "try-error")){
              i <- i+1
              
            } else {
              # specify the "from "date to desired start date
              
              rm(symbol)
              
            }
            
            plistcl <- eapply(dataEnv, Cl)
            
            plistop <- eapply(dataEnv, Op)
            
            plistvo <- eapply(dataEnv, Vo)
            
            plistlo <- eapply(dataEnv, Lo)
            
            plisthi <- eapply(dataEnv, Hi)
            
            plistop[is.na(plistop)] <- 0
            
            plistcl[is.na(plistcl)] <- 0
            
            plisthi[is.na(plisthi)] <- 0
            
            plistlo[is.na(plistlo)] <- 0
            
            plistvo[is.na(plistvo)] <- 0
          }
          
          
          k= setTxtProgressBar(pb, i)
        }
        
        
        pframehi<- do.call(merge, plisthi)
        
        
        pframevo <- do.call(merge, plistvo)
        
        pframecl <- do.call(merge, plistcl)
        
        pframelo <- do.call(merge, plistlo)
        
        pframeop <- do.call(merge, plistop)
        
        pframe<-cbind(pframeop,pframehi,pframelo,pframecl,pframevo)
        
        OPENMAT=as.matrix( coredata(pframe))
        OPENMAT[is.na(OPENMAT)] <- 0
        
        DATE=as.Date(index(pframe))
        
        pframe<-  data.frame(yoursym1,DATE,OPENMAT)
        
        colnames(pframe)=c("symbol","Date","Open","High","Low","close","volume")
        pframe<- as.data.frame(pframe)
        
        
        test=as.data.frame(pframe)
        
        
        colnames(test)=c("symbol","Date","Open","High","Low","close","volume")
        
        
        cest<-test
        
      },
      error=function(e){
        
        
      }) 
      cmp=as.numeric(as.character(dly2))
      
      if(((cmp))>400)
      {
        
        if((length(yoursym1) >=1)&&(yoursym1[1]!=0)){
          i=1
          
          
          # cool progress bar to see the % of completion
          n <- length(yoursym1)
          pb <- txtProgressBar(min = 0, max = n, style=3)
          
          i=1;
          
          
          dataEnv <- new.env()
          # Actual loop: 
          for(i in 1:((length(yoursym1))))
          {
            yoursym1[i]-> symbol
            tickers <- c(symbol)
            
            tryit <- try(getSymbols(tickers,src="av", api.key="M4SOMSBTQLGVJB54",output.size="full",from=(Sys.Date()-365), env=dataEnv))
            
            
            
            
            if(inherits(tryit, "try-error")){
              i <- i+1
              
            } else {
              # specify the "from "date to desired start date
              
              rm(symbol)
              
            }
            
            plistcl <- eapply(dataEnv, Cl)
            
            plistop <- eapply(dataEnv, Op)
            
            plistvo <- eapply(dataEnv, Vo)
            
            plistlo <- eapply(dataEnv, Lo)
            
            plisthi <- eapply(dataEnv, Hi)
            
            plistop[is.na(plistop)] <- 0
            
            plistcl[is.na(plistcl)] <- 0
            
            plisthi[is.na(plisthi)] <- 0
            
            plistlo[is.na(plistlo)] <- 0
            
            plistvo[is.na(plistvo)] <- 0
          }
          
          
          k= setTxtProgressBar(pb, i)
        }
        
        
        pframehi<- do.call(merge, plisthi)
        
        
        pframevo <- do.call(merge, plistvo)
        
        pframecl <- do.call(merge, plistcl)
        
        pframelo <- do.call(merge, plistlo)
        
        pframeop <- do.call(merge, plistop)
        
        pframe<-cbind(pframeop,pframehi,pframelo,pframecl,pframevo)
        
        OPENMAT=as.matrix( coredata(pframe))
        OPENMAT[is.na(OPENMAT)] <- 0
        
        DATE=as.Date(index(pframe))
        
        pframe<-  data.frame(yoursym1,DATE,OPENMAT)
        
        colnames(pframe)=c("symbol","Date","Open","High","Low","close","volume")
        pframe<- as.data.frame(pframe)
        
        
        test=as.data.frame(pframe)
        colnames(test)=c("symbol","Date","Open","High","Low","close","volume")
        
      }
      test <-  sqldf('SELECT * FROM test order by Date desc',drv='SQLite')
      
      mest=test
      
      mest$ID=seq.int(nrow(mest))
      
     # test <-  sqldf('SELECT a.* FROM mest a   join dly2 b on a.id<=b.dly  join dly3 c on a.id>=c.dly order by Date desc',drv='SQLite')
      
      test=sqldf('SELECT  symbol,Date,Open,High,Low,close,volume FROM test where close>0  order by Date asc',drv='SQLite')
      
      rest= sqldf('SELECT distinct * FROM test where symbol not in("NSE:XXX") order by Date asc ',drv='SQLite')
      test=as.data.frame(rest)
      
      hest<<-test
      
      
      
      
      mals=as.matrix(test$close)
      
      mals=as.numeric(mals[,1]) 
      
      
      mals=mals[1:length(mals)]
      nx=-(1:length(mals))
      
      bals=mals[-nx]
      
      nx=-(length(mals):1)
      
      x <- nx
      
      nx=-(1:length(mals))
      y <- bals[-nx]
      my_code<-paste("plot_ly(x = ~x) %>%add_lines(y = ~y, name =",'"stock"',", line = list(shape = ",'"linear"',"),yaxis = 'y2',line = list(color = '#45171D'))   ")
      
      eval(parse(text = my_code))
      
      
      
    }) 
  
  
  
  output$plot1mstm<-renderPlot(
    { 
      if(jv$i > 0)       {
        
        
        ddm<<-ddm+1
        
        
        allsym=as.data.frame(as.matrix(input$stock1))
        
        
        colnames(allsym)=c("symbol")
        
        symbol=as.character(allsym[1,1])
        
        
        dly2=as.data.frame(input$delaydm)
        
        dly2=as.matrix(dly2)
        dly3=dly2[2,1]
        dly2=dly2[1,1]
        dlf<-as.numeric(as.character(dly2))
        dly3<- as.data.frame(dly3)
        colnames(dly3)= "dly"
        
        
        
        dly2<- as.data.frame(dly2)
        colnames(dly2)= "dly"
        
        print(dlf)
        if((symbol==l)||(dlf==dgf))
        {
          
          
        }else {
          
          l<<-symbol
          dgf<<-dlf
          print("using loaded data")
          mest=dat2()
          mest=as.data.frame(mest)
        }
        
        mest=sqldf('SELECT distinct a.* FROM mest a join allsym b on TRIM(a.symbol)=TRIM(b.symbol) order by Date desc ',drv='SQLite')
        
        
        if((nrow(mest)>10)&&(dlf==dgf)&&(ddm<3))
        {
          print(ddm)
          
          if(input$somevalue5=="TRUE")
          {
            maxIter1 <<-3000
            
            
          }else {
            maxIter1 <<-0
          }
          
          rest= sqldf('SELECT distinct * FROM mest where symbol not in("NSE:XXX") order by Date asc ',drv='SQLite')
          test=as.data.frame(rest)
          
          mals=as.matrix(test$close)
          
          
          mals=as.numeric(mals[,1]) 
          
          
          mals=mals[1:length(mals)]
          nx=-(1:length(mals))
          
          bals=mals[-nx]
          
          nx=-(length(mals):1)
          
          x <- nx
          
          nx=-(1:length(mals))
          y <- bals[-nx]
          # my_code<-paste("plot_ly(x = ~x) %>%add_lines(y = ~y, name =",'"stock"',", line = list(shape = ",'"linear"',"),yaxis = 'y2',line = list(color = '#45171D'))   ")
          
          # eval(parse(text = my_code))
          plot(x,y,type="l")
          
        }
        
        else
        {  print("using api data")
          ddm<<-1
          print(symbol)
          mest=eval_with_timeout({rtd(symbol) }, timeout = 10)
          
          
          
          mest$ID=-seq.int(nrow(mest))
          
          mest=sqldf('SELECT distinct a.* FROM mest a join allsym b on TRIM(a.symbol)=TRIM(b.symbol) order by Date desc ',drv='SQLite')
          
        #  mest <-  sqldf('SELECT a.* FROM mest a   join dly2 b on a.id>=b.dly  join dly3 c on a.id<=c.dly order by Date desc',drv='SQLite')
          
          mest<<-as.data.frame(mest)
          
          
          rest=mest
          
          
          rest= sqldf('SELECT distinct * FROM mest where symbol not in("NSE:XXX") order by Date asc ',drv='SQLite')
          test=as.data.frame(rest)
          
          mals=as.matrix(test$close)
          
          mals=as.numeric(mals[,1]) 
          
          
          mals=mals[1:length(mals)]
          nx=-(1:length(mals))
          
          bals=mals[-nx]
          
          nx=-(length(mals):1)
          
          x <- nx
          
          nx=-(1:length(mals))
          y <- bals[-nx]
          # my_code<-paste("plot_ly(x = ~x) %>%add_lines(y = ~y, name =",'"stock"',", line = list(shape = ",'"linear"',"),yaxis = 'y2',line = list(color = '#45171D'))   ")
          
          # eval(parse(text = my_code))
          plot(x,y,type="l")
        }
        
      }
    })  
  
  
  
  
  
}


