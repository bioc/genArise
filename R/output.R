# GenArise

# Write the spot to a file in csv format

write.spot <- function( spot, fileName, quote=FALSE,sep="\t",col.names=FALSE, row.names=FALSE ) {
	spotData<-attr(spot,"spotData")
	write.table(spotData, fileName, quote = quote,sep = sep, col.names = col.names, row.names = row.names )
}


write.dataSet <- function(dataSet.spot, fileName, Zscore.min = NULL, Zscore.max = NULL, sep = "\t"){
  if(is.null(Zscore.min) && is.null(Zscore.max))
    stop("At least one range value must be different than null")
  data.values <- attr(dataSet.spot, "dataSets")
  if(is.null(Zscore.min)){
    if(!is.numeric(Zscore.max))
      stop("Zscore.max must be numeric")
    if(Zscore.max < 0 )
      stop("Zscore.max must be positive integer")
    index <- abs(data.values$Zscore) <= Zscore.max
    Center <- list(Cy3 = data.values$Cy3[index], Cy5 = data.values$Cy5[index],
                   Id = data.values$Id[index], Zscore = data.values$Zscore[index])
    write.table(Center, paste(fileName,"Center.csv", sep = "_"), quote = FALSE, sep = sep, col.names = FALSE, row.names = FALSE)
  }else{
    if(is.null(Zscore.max)){
      if(!is.numeric(Zscore.min))
        stop("Zscore.max must be numeric")
      if(Zscore.min < 0 )
        stop("Zscore.max must be positive integer")
      Up.index <-  data.values$Zscore > Zscore.min
      Down.index <- data.values$Zscore < (-Zscore.min)

      Up <- list(Cy3 = data.values$Cy3[Up.index], Cy5 = data.values$Cy5[Up.index],
                     Id = data.values$Id[Up.index], Zscore = data.values$Zscore[Up.index])
      Down <- list(Cy3 = data.values$Cy3[Down.index], Cy5 = data.values$Cy5[Down.index],
                     Id = data.values$Id[Down.index], Zscore = data.values$Zscore[Down.index])
      write.table(Up, paste(fileName,"Up.csv", sep = "_"), quote = FALSE, sep = sep, col.names = FALSE, row.names = FALSE)
      write.table(Down, paste(fileName,"Down.csv", sep = "_"), quote = FALSE, sep = sep, col.names = FALSE, row.names = FALSE)
    }else{
      if(Zscore.min > Zscore.max)
        stop("Invalid ranges")
      Up.index <-  (data.values$Zscore > Zscore.min) & (data.values$Zscore < Zscore.max)
      Down.index <- (data.values$Zscore < (-Zscore.min)) & (data.values$Zscore > (-Zscore.max))
          
      Up <- list(Cy3 = data.values$Cy3[Up.index], Cy5 = data.values$Cy5[Up.index],
                     Id = data.values$Id[Up.index], Zscore = data.values$Zscore[Up.index])
      Down <- list(Cy3 = data.values$Cy3[Down.index], Cy5 = data.values$Cy5[Down.index],
                     Id = data.values$Id[Down.index], Zscore = data.values$Zscore[Down.index])
      write.table(Up, paste(fileName,"Up.csv", sep = "_"), quote = FALSE, sep = sep, col.names = FALSE, row.names = FALSE)
      write.table(Down, paste(fileName,"Down.csv", sep = "_"), quote = FALSE, sep = sep, col.names = FALSE, row.names = FALSE)
    }
  }
}
