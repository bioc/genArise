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

    index.temp <- a > 0 &  b > 0
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
	id.sort <- sort.list(spot$Id)
	i <- 1
	n <- length(spot$Cy3)
	Cy3.list <- Cy5.list <- BgCy3.list <- BgCy5.list <- id.list <- list()
        compara <- function(lista.cy3, lista.cy5){
          resultado <- FALSE
          lista.length <- length(lista.cy3)
          k <- 1
          while( k < lista.length){
            r.temp <- (log(lista.cy5[k]) - log(lista.cy3[k]))
            r.prima.temp <- (log(lista.cy5[k + 1]) - log(lista.cy3[k + 1]))
            if(abs(r.prima.temp) <= (abs(r.temp) *1.2)){
              resultado <- TRUE 
            }
            else{
              resultado <- FALSE
            }
            k <- k + 1
          }
          resultado
        }
        while( i < n) {
          if(spot$Id[id.sort[i]] == spot$Id[id.sort[(i+1)]]){
            cy3.tmp <- cy5.tmp <- id.tmp <- list()
            j <- i
            while(j <= n && spot$Id[id.sort[i]] == spot$Id[id.sort[j]]){
              cy3.tmp <- c(cy3.tmp, spot$Cy3[id.sort[j]], recursive = TRUE)
              cy5.tmp <- c(cy5.tmp, spot$Cy5[id.sort[j]], recursive = TRUE)
              id.tmp <- c(id.tmp, spot$Id[id.sort[j]], recursive = TRUE)
              j  <- j + 1
            }
            condicion <- compara(cy3.tmp, cy5.tmp)
            if(condicion == TRUE){			
              tmp1 <- mean(cy3.tmp, na.rm = TRUE)
              tmp2 <- mean(cy5.tmp, na.rm = TRUE)
              Cy3.list <- c(Cy3.list, tmp1, recursive = TRUE)
              Cy5.list <- c(Cy5.list, tmp2, recursive = TRUE)
              id.list <- c(id.list, spot$Id[id.sort[i]], recursive = TRUE)
              BgCy3.list <- c(BgCy3.list, spot$BgCy3[id.sort[i]], recursive = TRUE)
              BgCy5.list <- c(BgCy5.list, spot$BgCy5[id.sort[i]], recursive = TRUE)
            }
            i <- i + length(cy3.tmp) - 1
          }
          else{
            Cy3.list <- c(Cy3.list, spot$Cy3[id.sort[i]], recursive = TRUE)
            Cy5.list <- c(Cy5.list, spot$Cy5[id.sort[i]], recursive = TRUE)
            BgCy3.list <- c(BgCy3.list, spot$BgCy3[id.sort[i]], recursive = TRUE)
            BgCy5.list <- c(BgCy5.list, spot$BgCy5[id.sort[i]], recursive = TRUE)
            id.list <- c(id.list, spot$Id[id.sort[i]], recursive = TRUE)
          }
          i <- i + 1
	}
	spotName <- attr(mySpot, "name")
	SD <- list(Cy3 = Cy3.list, Cy5 = Cy5.list, BgCy3 = BgCy3.list, BgCy5 = BgCy5.list, Id = id.list)
	new("Spot", name = spotName, spotData = SD)
}

# Non-extreme values replicate filtering

alter.unique <- function(mySpot){
  spot <- attr(mySpot, "spotData")
  id.sort <- sort.list(spot$Id)
  i <- 1
  n <- length(spot$Cy3)
  
  Cy3.list <- Cy5.list <- BgCy3.list <- BgCy5.list <- id.list <- list()
  while(i < n){
    if(spot$Id[id.sort[i]] == spot$Id[id.sort[(i+1)]]){
      cy3.tmp <- cy5.tmp <- bgcy3.tmp <- bgcy5.tmp <- id.tmp <- list()
      j <- i
      while( j <= n && spot$Id[id.sort[i]] == spot$Id[id.sort[j]]){		
        cy3.tmp <- c(cy3.tmp, spot$Cy3[id.sort[j]], recursive = TRUE)
        cy5.tmp <- c(cy5.tmp, spot$Cy5[id.sort[j]], recursive = TRUE)
        bgcy3.tmp <- c(bgcy3.tmp, spot$BgCy3[id.sort[j]], recursive = TRUE)
        bgcy5.tmp <- c(bgcy5.tmp, spot$BgCy5[id.sort[j]], recursive = TRUE)
        id.tmp <- c(id.tmp, spot$Id[id.sort[j]], recursive = TRUE)
        j  <- j + 1
      }
      x.tempo <- log(cy5.tmp/cy3.tmp, 2) # obtenemos los ratio
      x.prueba <- x.tempo[x.tempo < 0] 
      if(length(x.prueba) == 0 || length(x.prueba) == length(cy3.tmp)){ # todos positivos o todos negativos
        if(length(x.prueba) == 0 ){ # todos son positivos
          bueno.index <- as.numeric(which(x.tempo == max(x.tempo)))
        }
        else{# todos negativos
          bueno.index <- as.numeric(which(x.tempo == min(x.tempo)))
        }
        tmp1 <- cy3.tmp[bueno.index]
        tmp2 <- cy5.tmp[bueno.index]
        tmp3 <- bgcy3.tmp[bueno.index]
        tmp4 <- bgcy5.tmp[bueno.index]
        tmp5 <- id.tmp[bueno.index]
        
        Cy3.list <- c(Cy3.list, tmp1, recursive = TRUE)
        Cy5.list <- c(Cy5.list, tmp2, recursive = TRUE)
        BgCy3.list <- c(BgCy3.list, tmp3, recursive = TRUE)
        BgCy5.list <- c(BgCy5.list, tmp4, recursive = TRUE)
        id.list <- c(id.list, tmp5, recursive = TRUE)
      }
      i <- i + length(cy3.tmp) - 1
    }
    else{
      Cy3.list <- c(Cy3.list, spot$Cy3[id.sort[i]], recursive = TRUE)
      Cy5.list <- c(Cy5.list, spot$Cy5[id.sort[i]], recursive = TRUE)
      BgCy3.list <- c(BgCy3.list, spot$BgCy3[id.sort[i]], recursive = TRUE)
      BgCy5.list <- c(BgCy5.list, spot$BgCy5[id.sort[i]], recursive = TRUE)
      id.list <- c(id.list, spot$Id[id.sort[i]], recursive = TRUE)
    }
    i <- i + 1
  }
  
  spotName <- attr(mySpot, "name")
  SD <- list(Cy3 = Cy3.list, Cy5 = Cy5.list, BgCy3 = BgCy3.list, BgCy5 = BgCy5.list, Id = id.list)
  new("Spot", name = spotName, spotData = SD)
}

# Remove replicated observations, geometric mean
spotUnique <- function(mySpot){
  spot <- attr(mySpot, "spotData")
  id.sort.index <-  rev(sort.list(spot$Id))
  i <-  1
  n <- length(spot$Cy3)
  Cy3.list <- Cy5.list <- BgCy3.list <- BgCy5.list <- id.list <- list()
  Cy3.lista <- spot$Cy3[id.sort.index]
  Cy5.lista <- spot$Cy5[id.sort.index]
  BgCy3.lista <- spot$BgCy3[id.sort.index]
  BgCy5.lista <- spot$BgCy5[id.sort.index]
  Id.lista <- spot$Id[id.sort.index]

  geometric.mean <-  function(x){
    prod(x^(1/length(x)))
  }
  
  while( i < n){
    if(Id.lista[i] == Id.lista[i+1]){
      cy3.tmp <- cy5.tmp <- id.tmp <- list()
      j <- i
      while(j<= n && (Id.lista[i]  == Id.lista[j])){
        cy3.tmp <-  c(cy3.tmp, Cy3.lista[j], recursive = TRUE)
        cy5.tmp <-  c(cy5.tmp, Cy5.lista[j], recursive = TRUE)
        j <-  j + 1
      }
      tmp1 <- geometric.mean(cy3.tmp)
      tmp2 <- geometric.mean(cy5.tmp)
      Cy3.list <- c(Cy3.list, tmp1, recursive = TRUE)
      Cy5.list <- c(Cy5.list, tmp2, recursive = TRUE)
      id.list <- c(id.list, Id.lista[i], recursive = TRUE)
      BgCy3.list <- c(BgCy3.list, BgCy3.lista[i], recursive = TRUE)
      BgCy5.list <- c(BgCy5.list, BgCy5.lista[i], recursive = TRUE)
      i <- i + length(cy3.tmp) - 1
    }else{ # Si no hay emptys hay que colocarlos en el archivo
      Cy3.list <- c(Cy3.list, Cy3.lista[i], recursive = TRUE)
      Cy5.list <- c(Cy5.list, Cy5.lista[i], recursive = TRUE)
      id.list <- c(id.list, Id.lista[i], recursive = TRUE)
      BgCy3.list <- c(BgCy3.list, BgCy3.lista[i], recursive = TRUE)
      BgCy5.list <- c(BgCy5.list, BgCy5.lista[i], recursive = TRUE)
    }
    i <- i + 1
  }
  spotName <- attr(mySpot, "name")
  SD <- list(Cy3 = Cy3.list, Cy5 = Cy5.list, BgCy3 = BgCy3.list, BgCy5 = BgCy5.list, Id = id.list)
  new("Spot", name = spotName, spotData = SD)
}
