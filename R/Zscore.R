Zscore <- function(spot.object, type = "ri", window.size = 50){
	spot.data <- attr(spot.object, "spotData")
        if(type == "ri"){
          x.axis <- log(spot.data$Cy3, 10) + log(spot.data$Cy5, 10)
        }else{
          if(type == "ma"){
            x.axis <- (log(spot.data$Cy3, 2) + log(spot.data$Cy5, 2))/2
          }else{
            stop("Type argument must be ri or ma")
          }
        }
        resultado <- list()
        y.axis <- log(spot.data$Cy5,2) - log(spot.data$Cy3,2)
        
	sort.index <- sort.list(x.axis)        
        y.axis <- y.axis[sort.index]
	x.axis <- x.axis[sort.index]
        
        spot.data$Cy3 <- spot.data$Cy3[sort.index]
        spot.data$Cy5 <- spot.data$Cy5[sort.index]
        spot.data$Id <- spot.data$Id[sort.index]

	n.datos <- length(y.axis)
	inicio.y.axis <- unlist(sapply(1:as.integer(window.size/2), function(x) y.axis[x]))
        tmp <-  unlist(sapply(1:window.size, function(x) y.axis[x]))
        media <- mean(tmp)
	std <- sd(tmp)
        inicio.Zscore <- (inicio.y.axis - media)/ std
	resultados <- inicio.Zscore
        
	final.y.axis  <- unlist(sapply((n.datos - as.integer(window.size/2)):n.datos, function(x) y.axis[x]))
        tmp <-  unlist(sapply((n.datos - window.size):window.size, function(x) y.axis[x]))
	media <- mean(tmp)
	std <- sd(tmp)
	final.Zscore <- (final.y.axis - media) / std
   
	for( i in 26:(n.datos-26)){
		y.axis.temp <- unlist(sapply((i-as.integer(window.size/2)):(i+as.integer(window.size/2)), function(x) y.axis[x]))
		media <- mean(y.axis.temp)
		std <- sd(y.axis.temp)
		resultados <- c(resultados, (y.axis[i]- media)/std, recursive = TRUE)
	}
	resultados <- c(resultados, final.Zscore, recursive = TRUE)

        new("DataSet", name = attr(spot.object, "name"),
            dataSets = list(Cy3 = spot.data$Cy3, Cy5 = spot.data$Cy5, Id = spot.data$Id, Zscore = resultados), type = type) 
      }
