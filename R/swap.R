make.swap <- function(spot1, spot2, Cy3, Cy5, BgCy3, BgCy5, Id, header = FALSE, is.ifc = FALSE,envir,nr,nc){
  spot.ori <- read.spot(spot1,Cy3,Cy5,BgCy3,BgCy5,Id,header = header,is.ifc = is.ifc,envir=envir)
  spot.swap <- read.spot(spot2,Cy3,Cy5,BgCy3,BgCy5,Id,header = header,is.ifc = is.ifc,envir=envir)
  if(is.ifc){
    nr <- get("nr.global",envir=envir)
    nc <- get("nc.global",envir=envir)
    nmr <- get("nmr.global",envir=envir)
    nmc <- get("nmc.global",envir=envir)
  }
  ori.c <- bg.correct(spot.ori)
  swap.c <- bg.correct(spot.swap)
  assign("ori.c", ori.c, envir = envir)
  assign("swap.c", swap.c, envir = envir)
  spot1.data <- attr(ori.c,"spotData")
  spot2.data <- attr(swap.c,"spotData")
  
  A <- log2(spot1.data$Cy5*spot2.data$Cy5)-log2(spot1.data$Cy3*spot2.data$Cy3)
  index <- spot1.data$Cy5 == 0 | spot2.data$Cy5 == 0 | spot1.data$Cy3 == 0 | spot2.data$Cy3 == 0
  A[index] <- 0
  A.sd <- sd(A,na.rm=TRUE)
  A.mean <- mean(A,na.rm=TRUE)
  cond <- A <= A.mean - (A.sd*2) | A >= A.mean + (A.sd*2)
  assign("cond.index",cond,envir = envir)
  conf.factor <- length(A[!cond])/length(A)
  assign("conf",paste("Confidence Factor: ",round(conf.factor*100,digits=2),sep=""),envir=envir)
  disp.factor <- length(A[cond])/length(A)
  assign("disp",paste("Dispersion Factor: ",round(disp.factor*100,digits=2),sep=""),envir=envir)
  x <- spot1.data$Cy3[!cond]*spot2.data$Cy5[!cond]
  y <- spot1.data$Cy5[!cond]*spot2.data$Cy3[!cond]
  z <- spot1.data$BgCy3[!cond]*spot2.data$BgCy5[!cond]
  w <- spot1.data$BgCy5[!cond]*spot2.data$BgCy3[!cond]
  R.file1 <- log2(spot1.data$Cy5)-log2(spot1.data$Cy3)
  R.file2 <- log2(spot2.data$Cy3)-log2(spot2.data$Cy5)
  assign("R1",R.file1,envir = envir)
  assign("R2",R.file2,envir = envir)
  geometric.mean <-  function(x){sqrt(x)}
  Cy3.new <- sapply(x, geometric.mean)
  Cy5.new <- sapply(y, geometric.mean)
  BgCy3.new <- sapply(z, geometric.mean)
  BgCy5.new <- sapply(w, geometric.mean)
  SD <- list(Cy3 = as.numeric(Cy3.new),Cy5 = as.numeric(Cy5.new),BgCy3 = as.numeric(BgCy3.new),BgCy5 = as.numeric(BgCy5.new),Id = as.vector(spot1.data$Id[!cond]))
  assign("o.spot", new ("Spot", name = paste(attr(spot.ori, "name"),"vs",attr(spot.swap, "name"),sep=""), spotData = SD), envir = envir)
  assign("a.spot",get("o.spot",envir=envir), envir = envir)
  assign("spot.name",paste(attr(spot.ori, "name"),"vs",attr(spot.swap, "name"),sep=""), envir = envir)
  assign("name.project", paste(attr(spot.ori, "name"),"vs",attr(spot.swap, "name"),sep=""), envir = envir)
}

single.norm <- function(envir){
  norm.alg <- get("norm.alg",envir = envir)
  ori.c <- get("ori.c",envir = envir)
  swap.c <- get("swap.c",envir = envir)
  nr <- get("nr.global",envir=envir)
  nc <- get("nc.global",envir=envir)
  switch(norm.alg,
         {ori.n <- grid.norm(ori.c,nr,nc)
          swap.n <- grid.norm(swap.c,nr,nc)},
         {ori.n <- global.norm(ori.c)
          swap.n <- global.norm(swap.c)})
  ori.f <- filter.spot(ori.n)
  swap.f <- filter.spot(swap.n)
  unique.alg <- get("unique.alg",envir = envir)
  switch(unique.alg,
         {ori.u <-  meanUnique(ori.f)
          swap.u <- meanUnique(swap.f)},
         {ori.u <- alter.unique(ori.f)
          swap.u <- alter.unique(swap.f)},
         {ori.u <- spotUnique(ori.f)
          swap.u <- spotUnique(swap.f)})
  ori.dif <- Zscore(ori.u)
  swap.dif <- Zscore(swap.u)
  assign("ori.dif",ori.dif, envir = envir)
  assign("swap.dif",swap.dif, envir = envir)
  
}

get.Zscore <- function( spot, name, Zscore.min=NULL, Zscore.max=NULL, all=FALSE, envir) {
  idHash <- new.env(hash=TRUE)
  tmp <-  unlist(strsplit(name, "\\."))
  name <- paste(tmp[-length(tmp)],sep=".",collapse=".")
  ext <- tmp[length(tmp)]
  zscore.ori <- attr(get("ori.dif",envir = envir), "dataSets")
  zscore.swap <- attr(get("swap.dif", envir = envir), "dataSets")
  zscore <- attr(spot, "dataSets")
  if(is.null(Zscore.min) && is.null(Zscore.max))
    stop("At least one range value must be different than null")
  if(is.null(Zscore.min)){
    if(!is.numeric(Zscore.max))
      stop("Zscore.max must be numeric")
    if(Zscore.max < 0 )
      stop("Zscore.max must be positive integer")
    Up.index <- abs(zscore$Zscore) <= Zscore.max
  }else{
    if(is.null(Zscore.max)){
      if(!is.numeric(Zscore.min))
        stop("Zscore.max must be numeric")
      if(Zscore.min < 0 )
        stop("Zscore.max must be positive integer")
      Up.index <-  zscore$Zscore > Zscore.min
      Down.index <- zscore$Zscore < (-Zscore.min)
    }else{
      if(Zscore.min > Zscore.max)
        stop("Invalid ranges")
      Up.index <-  (zscore$Zscore > Zscore.min) & (zscore$Zscore < Zscore.max)
      Down.index <- (zscore$Zscore < (-Zscore.min)) & (zscore$Zscore > (-Zscore.max))
    }
  }
  n <- length(zscore.ori$Id)
  zscore.swap.length <- length(zscore.swap$Id)
  zscoreIdUp <- zscore$Id[Up.index]
  zscoreZscoreUp <- zscore$Zscore[Up.index]
  up.length <- length(zscoreIdUp)
  zscoreIdDown <- zscore$Id[Down.index]
  zscoreZscoreDown <- zscore$Zscore[Down.index]
  
  for(i in 1:n){
    assign(zscore.ori$Id[i], list(ZscoreUp ="null" ,ZscoreDown = "null", Zscore.ori = zscore.ori$Zscore[i] , Zscore.swap =0),envir=idHash)
  }
  
  for(i in 1:zscore.swap.length){
    if(exists( zscore.swap$Id[i], envir = idHash)){
      idData <- get( zscore.swap$Id[i], envir = idHash)
      assign(zscore.swap$Id[i], list(ZscoreUp = idData$ZscoreUp, ZscoreDown= idData$ZscoreDown, Zscore.ori = idData$Zscore.ori, Zscore.swap =zscore.swap$Zscore[i]),envir=idHash)
    }             
  }
  
  for(i in 1:up.length){                                
    if(exists( zscoreIdUp[i], envir = idHash)){
      idData <- get( zscoreIdUp[i], envir = idHash)
      assign(zscoreIdUp[i], list(ZscoreUp =zscoreZscoreUp[i], ZscoreDown = idData$ZscoreDown, Zscore.ori = idData$Zscore.ori, Zscore.swap = idData$Zscore.swap),envir=idHash)
    }             
    
  }
  down.length <- length(zscoreIdDown)
  for(i in 1:down.length){                                
    if(exists( zscoreIdDown[i], envir = idHash)){
      idData <- get( zscoreIdDown[i], envir = idHash)
      assign(zscoreIdDown[i], list(ZscoreUp = idData$ZscoreUp, ZscoreDown =zscoreZscoreDown[i], Zscore.ori = idData$Zscore.ori, Zscore.swap = idData$Zscore.swap),envir=idHash)
    }
  }
  
  get.finalZ  <- function(id,range){
    zScore <- get(id, envir = idHash)
    if(range == "up"){
      if( zScore$ZscoreUp !="null"){
        list(Id=id,Zscore=zScore$ZscoreUp, Zscore.ori=zScore$Zscore.ori,Zscore.swap=zScore$Zscore.swap)
      }
    }else{
      if( zScore$ZscoreDown !="null"){
        list(Id=id,Zscore=zScore$ZscoreDown, Zscore.ori=zScore$Zscore.ori,Zscore.swap=zScore$Zscore.swap)
      }
    } 
  }
  if(all){
    if(file.exists(paste(name,".",ext, sep="")))
       file.remove(paste(name,".",ext, sep=""))
    write.table("Id\tSwapped Zscore\tZscore File1\tZscore File2", paste(name,".",ext, sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep="\t",append=FALSE)
    for(i in 1:n){
      write.table(get.finalZ(zscore.ori$Id[i],"up"), paste(name,".",ext, sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep="\t",append=TRUE)
      write.table(get.finalZ(zscore.ori$Id[i],"down"), paste(name,".", ext, sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep="\t",append=TRUE)
    }
  }
  else{
    if(file.exists(paste(name,"_Up.", ext, sep="")))
      file.remove(paste(name,"_Up.", ext, sep=""))
    if(file.exists(paste(name,"_Down.", ext, sep="")))
      file.remove(paste(name,"_Down.", ext, sep=""))
    write.table("Id\tSwapped Zscore\tZscore File1\tZscore File2", paste(name,"_Up.",ext, sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep="\t",append=FALSE)
    for(i in 1:n){
      write.table(get.finalZ(zscore.ori$Id[i],"up"), paste(name,"_Up.", ext, sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep="\t",append=TRUE)
    }
    
    write.table("Id\tSwapped Zscore\tZscore File1\tZscore File2", paste(name,"_Down.",ext, sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep="\t",append=FALSE)
    for(i in 1:n){
      write.table(get.finalZ(zscore.ori$Id[i],"down"), paste(name,"_Down.", ext, sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE,sep="\t",append=TRUE)
    }
  }  
}

