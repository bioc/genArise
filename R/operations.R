require(locfit)
bg.correct <- function(mySpot){
  spot <- as.list(attr(mySpot, "spotData"))
  spot$Cy3 <- spot$Cy3 - spot$BgCy3
  spot$Cy5 <- spot$Cy5 - spot$BgCy5
  spotName <- attr(mySpot, "name")	
  SD <- list(Cy3 = spot$Cy3, Cy5 = spot$Cy5, BgCy3 = spot$BgCy3, BgCy5 = spot$BgCy5, Id = spot$Id)
  new("Spot", name = spotName, spotData= SD)
}

                                        # Filtering
# Intensity-based filtering of array elements

filter.spot <- function (mySpot){
  spot <- attr(mySpot, "spotData")
  condition <- (spot$Cy3 > 2 * sd(spot$BgCy3)) & (spot$Cy5 > 2 * sd(spot$BgCy5))
  spot$Cy3 <- spot$Cy3[condition]
  spot$Cy5 <- spot$Cy5[condition]
  spot$BgCy3 <- spot$BgCy3[condition]
  spot$BgCy5 <- spot$BgCy5[condition]
  spot$Id <- spot$Id[condition]
  cys <- (!is.na(spot$Cy3)) & (!is.na(spot$Cy5)) & (!is.na(spot$BgCy3)) & (!is.na(spot$BgCy5)) & (!is.na(spot$Id))
  spot$Cy3 <- spot$Cy3[cys]
  spot$Cy5 <- spot$Cy5[cys]
  spot$BgCy3 <- spot$BgCy3[cys]
  spot$BgCy5 <- spot$BgCy5[cys]
  spot$Id <- spot$Id[cys]
  newSpot <- list(Cy3 = spot$Cy3 , Cy5= spot$Cy5 , BgCy3 = spot$BgCy3 , BgCy5 = spot$BgCy5, Id = spot$Id)
  spotName <- attr(mySpot, "name")
  new("Spot", name = spotName, spotData= newSpot)
}

# Normalisation

# Normalisation by grid

grid.norm <- function(mySpot, nr, nc){
  spot <- attr(mySpot, "spotData")
  x <- spot$Cy3
  y <- spot$Cy5
  k <-(nc * nr)-1
  j <- 1
  i <- 1
  Cy3 <- Cy5 <- BgCy3 <- BgCy5<- Id <- list()
  bloque <- 1
  n <- length (x)
  while(j<(n+1)){
    a <- unlist(sapply(j : (j+k),function(x) spot$Cy3[x]))
    b <- unlist(sapply(j : (j+k),function(x) spot$Cy5[x]))
    c <- unlist(sapply(j : (j+k),function(x) spot$BgCy3[x]))
    d <- unlist(sapply(j : (j+k),function(x) spot$BgCy5[x]))
    e <- unlist(sapply(j : (j+k),function(x) spot$Id[x]))

    index.temp <- a > 0 &  b > 0 & (!is.na(a)) & (!is.na(b)) & (!is.na(c)) & (!is.na(d)) & (!is.na(e))
    a <- a[index.temp]
    b <- b[index.temp]
    c <- c[index.temp]
    d <- d[index.temp]
    e <- e[index.temp]
    
    if(length(a) > 0){
      R <- log(b,2) - log(a,2)
      I <- log(a,10) + log( b, 10)
      zero.index <- R == 0
      R <- R[!zero.index]
      I <- I[!zero.index]
      a <- a[!zero.index]
      b <- b[!zero.index]
      c <- c[!zero.index]
      d <- d[!zero.index]
      e <- e[!zero.index]

      tmp <- fitted(locfit(R~I,alpha = 0.1, deg = 1, cut  = 0.9 ))
      Cy5 <- c(Cy5, b * (1/2^tmp), recursive = TRUE)
      Cy3 <- c(Cy3, a, recursive = TRUE)
      BgCy3 <- c(BgCy3, c, recursive = TRUE)
      BgCy5 <- c(BgCy5, d, recursive = TRUE)
      Id <- c(Id, e, recursive = TRUE)
    }
    j <- j + k + 1
  }
  Cy3 <- as.integer(Cy3)
  Cy5 <- as.integer(Cy5)
  BgCy5 <- as.integer(BgCy5)
  BgCy3 <- as.integer(BgCy3)
  SD <- list(Cy3 = Cy3, Cy5 = Cy5, BgCy3 = BgCy3, BgCy5 = BgCy5, Id = Id)
  
  new ("Spot", name = attr(mySpot, "name"), spotData = SD)
}

global.norm <- function(mySpot){
  spot <- attr(mySpot, "spotData")
  Cy3 <- Cy5 <- BgCy3 <- BgCy5<- Id <- list()
  
  index.temp <- spot$Cy3 > 0 &  spot$Cy5 > 0
  a <- spot$Cy3[index.temp]
  b <- spot$Cy5[index.temp]
  c <- spot$BgCy3[index.temp]
  d <- spot$BgCy5[index.temp]
  e <- spot$Id[index.temp]

  if(length(a) > 0){
    
    R <- log(b,2) - log(a,2)
    I <- log(a,10) + log( b, 10)
    zero.index <- R == 0
    R <- R[!zero.index]
    I <- I[!zero.index]
    a <- a[!zero.index]
    b <- b[!zero.index]
    c <- c[!zero.index]
    d <- d[!zero.index]
    e <- e[!zero.index]
    
    tmp <- fitted(locfit(R~I,alpha = 0.1, deg = 1, cut  = 0.9 ))
    Cy5 <- b * (1/2^tmp)
    Cy3 <- a
    BgCy3 <- c
    BgCy5 <- d
    Id <- e
  }
  Cy3 <- as.integer(Cy3)
  Cy5 <- as.integer(Cy5)
  BgCy5 <- as.integer(BgCy5)
  BgCy3 <- as.integer(BgCy3)
  SD <- list(Cy3 = Cy3, Cy5 = Cy5, BgCy3 = BgCy3, BgCy5 = BgCy5, Id = Id)
  new ("Spot", name = attr(mySpot, "name"), spotData = SD)
}

# Duplicate Analysis

# Mean Replicate filtering 
meanUnique <- function(mySpot){
  spot <- attr(mySpot, "spotData")
  idHash <- new.env(hash=TRUE)
  n <- length(spot$Cy3)
  
  for(i in 1:n){
    if(exists( spot$Id[i], envir = idHash)){
      idData <- get( spot$Id[i], envir = idHash)
      assign(spot$Id[i], list(spotCy3 = c( idData$spotCy3, spot$Cy3[i],recursive=TRUE) ,
                              spotCy5 = c( idData$spotCy5, spot$Cy5[i],recursive=TRUE),spotBgCy3 = c( idData$spotBgCy3, spot$BgCy3[i],recursive=TRUE),
                              spotBgCy5 = c( idData$spotBgCy5, spot$BgCy5[i],recursive=TRUE)), envir = idHash)
    }
    
    else{
      assign(spot$Id[i], list(spotCy3 =  spot$Cy3[i] ,spotCy5 =  spot$Cy5[i],spotBgCy3 =  spot$BgCy3[i],
                              spotBgCy5 = spot$BgCy5[i]), envir = idHash)
    }
  }
  
  compara <- function(lista.cy3, lista.cy5){
    resultado <- TRUE
    R.list <- log2(lista.cy5)-log2(lista.cy3)
    n <- length(R.list)
    if(n > 1){
      for( i in 1:(length(R.list)-1)) {
        if((R.list[i] < 0 & R.list[i+1] > 0) | (R.list[i] > 0 & R.list[i+1] < 0)){
          return( FALSE )
        }
        
        if(abs(R.list[i]) > (abs(R.list[i+1]) * 1.2)){
          resultado <- FALSE
        }
      }
    }    
    resultado
  }
  ids <- ls(envir = idHash)
  replicates <- function( id ){
    mainList <- get(id, envir = idHash)
    if(compara(as.numeric(mainList$spotCy3), as.numeric(mainList$spotCy5)))
        list(Id = id, Cy3 = mean(mainList$spotCy3), Cy5 = mean(mainList$spotCy5), BgCy3 = mainList$spotBgCy3[length(mainList$spotBgCy3)], BgCy5 = mainList$spotBgCy5[length(mainList$spotBgCy3)])
  }
  restCy3 <- restCy5 <- restBgCy3 <- restBgCy5 <- restId <- list()
  for(i in 1:length(ids)){
    rest <- replicates(ids[i])
    restCy3 <- c(restCy3,rest$Cy3,recursive=TRUE)
    restCy5 <- c(restCy5,rest$Cy5,recursive=TRUE)
    restBgCy3 <- c(restBgCy3,rest$BgCy3,recursive=TRUE)
    restBgCy5 <- c(restBgCy5,rest$BgCy5,recursive=TRUE)
    restId <- c(restId,rest$Id,recursive=TRUE)
  }
  SD <- list(Cy3 = restCy3, Cy5 = restCy5, BgCy3 = restBgCy3, BgCy5 = restBgCy5, Id = restId)
  spotName <- attr(mySpot, "name")
  new("Spot", name = spotName, spotData = SD)
}

                                        # Non-extreme values replicate filtering

alter.unique <- function(mySpot){
  spot <- attr(mySpot, "spotData")
  myHash <- new.env(hash=TRUE)
  n <- length(spot$Cy3)
  
  for(i in 1:n){
    if(exists( spot$Id[i], envir = myHash)){
      idData <- get( spot$Id[i], envir = myHash)
      assign(spot$Id[i], list(spotCy3 = c( idData$spotCy3, spot$Cy3[i]) ,
                              spotCy5 = c( idData$spotCy5, spot$Cy5[i]),spotBgCy3 = c( idData$spotBgCy3, spot$BgCy3[i]),
                              spotBgCy5 = c( idData$spotBgCy5, spot$BgCy5[i])), envir = myHash)
    }
    
    else{
      assign(spot$Id[i], list(spotCy3 =  spot$Cy3[i] ,spotCy5 =  spot$Cy5[i],spotBgCy3 =  spot$BgCy3[i],
                              spotBgCy5 = spot$BgCy5[i]), envir = myHash)
    }
  }
  ids <- ls(envir = myHash)
  Cy3.list <- Cy5.list <- BgCy3.list <- BgCy5.list <- Id.list <- list()
  Cy3s <- Cy5s <- BgCy3s <- BgCy5s <- Ids <- list()
  
  replicates2 <- function(id){
    mainList <- get(id, envir = myHash)
    Cy3.list <- mainList$spotCy3
    Cy5.list <- mainList$spotCy5
    BgCy3.list <- mainList$spotBgCy3
    BgCy5.list <- mainList$spotBgCy5
    R.list <- log2(Cy5.list)-log2(Cy3.list)
    x.prueba <- R.list[R.list < 0]
    Cy3s <- Cy5s <- BgCy3s <- BgCy5s <- ids <- list()
    if(length(x.prueba) == 0 || length(x.prueba) == length(Cy3.list)){
      if(length(x.prueba) == 0){ # all positive
        bueno.index <- as.numeric(which(R.list == max(R.list)))
      }
      else{# all negative
        bueno.index <- as.numeric(which(R.list == min(R.list)))
      }
      Cy3s <- Cy3.list[bueno.index]
      Cy5s <- Cy5.list[bueno.index]
      BgCy3s <- BgCy3.list[bueno.index]
      BgCy5s <- BgCy5.list[bueno.index]
      ids <- id
    }
    list(Cy3 = Cy3s, Cy5 = Cy5s, BgCy3 = BgCy3s, BgCy5 = BgCy5s, Id = ids)
  }
  
  restCy3 <- restCy5 <- restBgCy3 <- restBgCy5 <- restId <- list()
  for(i in 1:length(ids)){
    rest <- replicates2(ids[i])
    restCy3 <- c(restCy3,rest$Cy3,recursive=TRUE)
    restCy5 <- c(restCy5,rest$Cy5,recursive=TRUE)
    restBgCy3 <- c(restBgCy3,rest$BgCy3,recursive=TRUE)
    restBgCy5 <- c(restBgCy5,rest$BgCy5,recursive=TRUE)
    restId <- c(restId,rest$Id,recursive=TRUE)
  }
  SD <- list(Cy3 = restCy3, Cy5 = restCy5, BgCy3 = restBgCy3, BgCy5 = restBgCy5, Id = restId)
  spotName <- attr(mySpot, "name")
  new("Spot", name = spotName, spotData = SD)
}

# Remove replicated observations, geometric mean
spotUnique <- function(mySpot){
  spot <- attr(mySpot, "spotData")
  idHash <- new.env(hash=TRUE)
  n <- length(spot$Cy3)
  
  for(i in 1:n){
    if(spot$Id[i] == ""){
      spot$Id[i] <- "NOGB_accession"
    }
    if(exists( spot$Id[i], envir = idHash)){
      idData <- get( spot$Id[i], envir = idHash)
      assign(spot$Id[i], list(spotCy3 = c( idData$spotCy3, spot$Cy3[i],recursive=TRUE) ,
                              spotCy5 = c( idData$spotCy5, spot$Cy5[i],recursive=TRUE),spotBgCy3 = c( idData$spotBgCy3, spot$BgCy3[i],recursive=TRUE),
                              spotBgCy5 = c( idData$spotBgCy5, spot$BgCy5[i],recursive=TRUE)), envir = idHash)
    }
    
    else{
      assign(spot$Id[i], list(spotCy3 =  spot$Cy3[i] ,spotCy5 =  spot$Cy5[i],spotBgCy3 =  spot$BgCy3[i],
                              spotBgCy5 = spot$BgCy5[i]), envir = idHash)
      
    }
  }
  
  geometric.mean <-  function(x){
    prod(x^(1/length(x)))
  }
  
  ids <- ls(envir = idHash)
  
  replicates3 <- function(id){
    mainList <- get(id, envir = idHash)
    Cy3.list <- mainList$spotCy3
    Cy5.list <- mainList$spotCy5
    BgCy3.list <- mainList$spotBgCy3
    BgCy5.list <- mainList$spotBgCy5
    Cy3 <- Cy5 <- BgCy3 <- BgCy5 <- Id <- list()
    tmp1 <- geometric.mean(Cy3.list)
    tmp2 <- geometric.mean(Cy5.list)
    Cy3 <- c(Cy3, tmp1, recursive=TRUE)
    Cy5 <- c(Cy5, tmp2, recursive=TRUE)
    BgCy3 <- c(BgCy3, BgCy3.list[1],recursive=TRUE)
    BgCy5 <- c(BgCy5, BgCy5.list[1],recursive=TRUE)
    Id <- c(Id, id, recursive=TRUE)
    list(Cy3 = Cy3, Cy5 = Cy5, BgCy3 = BgCy3, BgCy5 = BgCy5, Id = Id)
  }
  
  restCy3 <- restCy5 <- restBgCy3 <- restBgCy5 <- restId <- list()
  for(i in 1:length(ids)){
    rest <- replicates3(ids[i])
    restCy3 <- c(restCy3,rest$Cy3,recursive=TRUE)
    restCy5 <- c(restCy5,rest$Cy5,recursive=TRUE)
    restBgCy3 <- c(restBgCy3,rest$BgCy3,recursive=TRUE)
    restBgCy5 <- c(restBgCy5,rest$BgCy5,recursive=TRUE)
    restId <- c(restId,rest$Id,recursive=TRUE)
  }
  
  spotName <- attr(mySpot, "name")
  SD <- list(Cy3 = restCy3, Cy5 = restCy5, BgCy3 = restBgCy3, BgCy5 = restBgCy5, Id = restId)
  new("Spot", name = spotName, spotData = SD)
}
