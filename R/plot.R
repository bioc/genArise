# GENARISE 
# Graphics and statistics operations

#Simple graphics 

cys.plot <- function(mySpot, col = "green"){
	spot <- attr(mySpot, "spotData")
	par( bg = "black", col.axis = "white", col.lab = "white", fg = "white")
	plot(spot$Cy3, spot$Cy5,xlab = "Cy3", ylab= "Cy5",  pch = ".", col = col)
}

ma.plot <- function (mySpot, col = "green"){
	spot <- attr(mySpot, "spotData")
	cys.index <- (spot$Cy3 > 0) & (spot$Cy5 > 0) & (!is.na(spot$Cy3)) &(!is.na(spot$Cy5)) & is.finite(spot$Cy3) & is.finite(spot$Cy5)
	cy3 <- spot$Cy3[cys.index]
	cy5 <- spot$Cy5[cys.index]
	M <- log(cy5,2) - log(cy3, 2)
	A <- (log(cy3, 2) + log(cy5, 2))/2
	spot$Id <- spot$Id[cys.index]
	par( bg = "black", col.axis = "white",col.lab = "white", fg = "white")
	plot(A,M, xlab = "A", ylab = "M", pch = ".",col = col)
}

ri.plot <- function(mySpot, col = "green"){
	spot <- attr(mySpot, "spotData")
	cys.index <- (spot$Cy3 > 0) & (spot$Cy5 > 0) & (!is.na(spot$Cy3)) &(!is.na(spot$Cy5)) & is.finite(spot$Cy3) & is.finite(spot$Cy5)
	cy3 <- spot$Cy3[cys.index]
	cy5 <- spot$Cy5[cys.index]
	R <- log(cy5,2) - log(cy3, 2)
	I <- log(cy3, 10) + log(cy5, 10)
	par( bg = "black", col.axis = "white", col.lab = "white",fg = "white")
	plot(I,R, xlab = "I", ylab = "R", pch = ".",col = col)
}

Zscore.plot <-  function(dataSet.spot, Zscore.min = NULL, Zscore.max = NULL, all = TRUE, col = "green"){
  values <- attr(dataSet.spot, "dataSets")
  type <- attr(dataSet.spot, "type")
  Zscore <- values$Zscore
  if(type == "ri"){
    x.axis <- log(values$Cy3, 10) + log(values$Cy5, 10)    
  }else{
    x.axis <- (log(values$Cy3, 2) + log(values$Cy5, 2))/2
  }
  y.axis <- log(values$Cy5, 2)  - log(values$Cy3, 2)
  if(all){
    par(bg = "black", col.axis = "white", col.lab = "white", fg = "white")
    azul <- abs(Zscore) < 1
    verde <- (abs(Zscore) >= 1) & (abs(Zscore) < 1.5)
    rojo <- (abs(Zscore) >= 1.5) & (abs(Zscore) <= 2)
    naranja <- abs(Zscore) > 2
    if(type == "ri"){    
      plot(x.axis, y.axis, col = "black", pch =3, xlab = "I", ylab = "R")
    }else{
      plot(x.axis, y.axis, col = "black", pch =3, xlab = "A", ylab = "M")
    }
    points(x.axis[naranja], y.axis[naranja], col = "snow", pch =3)      
    points(x.axis[azul], y.axis[azul], col = "green", pch = 3)
    points(x.axis[verde], y.axis[verde], col = "blue",pch= 3)
    points(x.axis[rojo], y.axis[rojo], col = "cyan", pch = 3)
  }
  else{
    if(is.null(Zscore.min) && is.null(Zscore.max)){
      stop("At least one value must be non null")
    }
    if(!is.null(Zscore.max) && !is.null(Zscore.min) && Zscore.max < Zscore.min){
      stop("Invalid ranges")
    }
    par(bg = "black", col.axis = "white", col.lab = "white", fg = "white")
    if(is.null(Zscore.min)){
      verde <- abs(Zscore) < Zscore.max
    }else{
      if(is.null(Zscore.max)){
        verde <- abs(Zscore) > Zscore.min
      }else{
        verde <- (abs(Zscore) > Zscore.min) & (abs(Zscore) < Zscore.max)
      }
    }
    if(type == "ri"){
      plot(x.axis, y.axis, col = "black", pch =3, xlab = "I", ylab = "R")      
      points(x.axis[verde], y.axis[verde], col = col, pch = 3)
    }else{
      plot(x.axis, y.axis, col = "black", pch =3, xlab = "A", ylab = "M")      
      plot(x.axis[verde], y.axis[verde], col = col, pch = 3)
    }
  }
}
