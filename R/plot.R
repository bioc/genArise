# GENARISE 
# Graphics and statistics operations

# Plot the signals: Cy3 vs Cy5 (obtained from the Spot object)
cys.plot <- function(mySpot, col = "green"){
  spot <- attr(mySpot, "spotData")
  par( bg = "black", col.axis = "white", col.lab = "white", fg = "white")
  plot(log2(spot$Cy3), log2(spot$Cy5),xlab = "log2(Cy3)", ylab= "log2(Cy5)",  pch = ".", col = col)
  lines(log2(spot$Cy5),log2(spot$Cy5),col="turquoise")
}

# Plot mean log intensity versus intensity log-ratio: M vs A
ma.plot <- function (mySpot, col = "green"){
  spot <- attr(mySpot, "spotData")
  cys.index <- (spot$Cy3 > 0) & (spot$Cy5 > 0) & (!is.na(spot$Cy3)) &(!is.na(spot$Cy5)) & is.finite(spot$Cy3) & is.finite(spot$Cy5)
  cy3 <- spot$Cy3[cys.index]
  cy5 <- spot$Cy5[cys.index]
  bgcy3 <- spot$BgCy3[cys.index]
  bgcy5 <- spot$BgCy5[cys.index]
  M <- log(cy5,2) - log(cy3, 2)
  A <- (log(cy3, 2) + log(cy5, 2))/2
  spot$Id <- spot$Id[cys.index]
  par( bg = "black", col.axis = "white",col.lab = "white", fg = "white")
  plot(A,M, xlab = "A", ylab = "M", pch = ".",col = col)
  lines(lowess(M~A),col="turquoise",lwd=1)
}

# log2(Cy5/Cy3) ratio as a function of the log10(Cy5*Cy3) product intensities
ri.plot <- function(mySpot, col = "green"){
  spot <- attr(mySpot, "spotData")
  cys.index <- (spot$Cy3 > 0) & (spot$Cy5 > 0) & (!is.na(spot$Cy3)) &(!is.na(spot$Cy5)) & is.finite(spot$Cy3) & is.finite(spot$Cy5)
  cy3 <- spot$Cy3[cys.index]
  cy5 <- spot$Cy5[cys.index]
  R <- log(cy5,2) - log(cy3, 2)
  I <- log(cy3, 10) + log(cy5, 10)
  spot$Id <- spot$Id[cys.index]
  par( bg = "black", col.axis = "white", col.lab = "white",fg = "white")
  plot(I,R, xlab = "I", ylab = "R", pch = ".",col = col )
  lines(lowess(R~I),col="turquoise",lwd=1)
}

# In this plot , array elements are color-coded depending on wether they are less than
# 1 standard deviation from the mean (green), between 1 and 1.5 standard deviations (blue)
# between 1.5 and 2 standard deviations (turquoise), or more than 2 standard deviations (white)
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
    par(bg = "black", col.axis = "white", col.lab = "white",fg="white")
    # get the ranges for the color-code
    blue <- abs(Zscore) < 1
    green <- (abs(Zscore) >= 1) & (abs(Zscore) < 1.5)
    red <- (abs(Zscore) >= 1.5) & (abs(Zscore) <= 2)
    #yellow <- abs(Zscore) > 1.5
    orange <- abs(Zscore) > 2
    # the plot will be R vs I or M vs A
    if(type == "ri"){    
      plot(x.axis, y.axis, col = "black", pch =3, xlab = "I", ylab = "R")
    }else{
      plot(x.axis, y.axis, col = "black", pch =3, xlab = "A", ylab = "M")
    }
    # draw the points
    points(x.axis[orange], y.axis[orange], col = "snow", pch =3)      
    points(x.axis[blue], y.axis[blue], col = "green", pch = 3)
    points(x.axis[green], y.axis[green], col = "blue",pch= 3)
#    points(x.axis[yellow], y.axis[yellow], col="yellow", pch= 3)
    points(x.axis[red], y.axis[red], col = "turquoise", pch = 3)
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
      green <- abs(Zscore) < Zscore.max
    }else{
      if(is.null(Zscore.max)){
        green <- abs(Zscore) > Zscore.min
      }else{
        green <- (abs(Zscore) > Zscore.min) & (abs(Zscore) < Zscore.max)
      }
    }
    if(type == "ri"){
      plot(x.axis, y.axis, col = "black", pch =3, xlab = "I", ylab = "R")      
      points(x.axis[green], y.axis[green], col = col, pch = 3)
    }else{
      plot(x.axis, y.axis, col = "black", pch =3, xlab = "A", ylab = "M")
      points(x.axis[green], y.axis[green], col = col, pch = 3)
    }
  }
}
