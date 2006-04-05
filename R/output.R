# GenArise

# Write the spot to a file in txt format

write.spot <- function( spot, fileName, quote=FALSE,sep="\t",col.names=FALSE, row.names=FALSE ) {
  spotData<-attr(spot,"spotData")
  spotData$Cy3 <- format(spotData$Cy3,justify="left",trim=TRUE)
  spotData$Cy5 <- format(spotData$Cy5,justify="left",trim=TRUE)
  spotData$BgCy3 <- format(spotData$BgCy3,justify="left",trim=TRUE)
  spotData$BgCy5 <- format(spotData$BgCy5,justify="left",trim=TRUE)
  if(is.null(spotData$Symdesc))
    write.table("Cy3\tCy5\tBgCy3\tBgCy5\tId", fileName, quote = quote,sep = sep, col.names = col.names, row.names = row.names )
  else
    write.table("Cy3\tCy5\tBgCy3\tBgCy5\tId\tSymbol", fileName, quote = quote,sep = sep, col.names = col.names, row.names = row.names )
  write.table(spotData, fileName, quote = quote,sep = sep, col.names = col.names, row.names = row.names, append=TRUE )
}

write.dataSet <- function(dataSet.spot, fileName, quote = FALSE, col.names=FALSE, row.names=FALSE, Zscore.min = NULL, Zscore.max = NULL, sep = "\t"){
  tmp <-  unlist(strsplit(fileName, "\\."))
  fileName <- paste(tmp[-length(tmp)],sep=".",collapse=".")
  ext <- tmp[length(tmp)]
  
  if(is.null(Zscore.min) && is.null(Zscore.max))
    stop("At least one range value must be different than null")
  data.values <- attr(dataSet.spot, "dataSets")
  if(is.null(Zscore.min)){
    if(!is.numeric(Zscore.max))
      stop("Zscore.max must be numeric")
    if(Zscore.max < 0 )
      stop("Zscore.max must be positive integer")
    index <- abs(data.values$Zscore) <= Zscore.max
    if(is.null(data.values$Symdesc)){
      Center <- list(Cy3 = data.values$Cy3[index], Cy5 = data.values$Cy5[index],
                     Id = data.values$Id[index], Zscore = data.values$Zscore[index])
      write.table("Cy3\tCy5\tId\tZscore", paste(fileName,"_Center.", ext, sep = "") , quote = quote,sep = sep, col.names = col.names, row.names = row.names )
    }
    else{
      Center <- list(Cy3 = data.values$Cy3[index], Cy5 = data.values$Cy5[index],
                     Id = data.values$Id[index], Symdesc = data.values$Symdesc[index], Zscore = data.values$Zscore[index])
      write.table("Cy3\tCy5\tId\tSymbol\tZscore", paste(fileName,"_Center.", ext, sep = "") , quote = quote,sep = sep, col.names = col.names, row.names = row.names )
    }
    write.table(Center, paste(fileName,"_Center.", ext, sep = ""), quote = quote, sep = sep, col.names = col.names, row.names = row.names, append = TRUE)
  }else{
    if(is.null(data.values$Symdesc)){
      write.table("Cy3\tCy5\tId\tZscore", paste(fileName,"_Up.", ext, sep = "") , quote = quote,sep = sep, col.names = col.names, row.names = row.names )
      write.table("Cy3\tCy5\tId\tZscore", paste(fileName,"_Down.", ext, sep = "") , quote = quote,sep = sep, col.names = col.names, row.names = row.names )
    }
    else{
      write.table("Cy3\tCy5\tId\tSymbol\tZscore", paste(fileName,"_Up.", ext, sep = "") , quote = quote,sep = sep, col.names = col.names, row.names = row.names )
      write.table("Cy3\tCy5\tId\tSymbol\tZscore", paste(fileName,"_Down.", ext, sep = "") , quote = quote,sep = sep, col.names = col.names, row.names = row.names )
    }
     if(is.null(Zscore.max)){
      if(!is.numeric(Zscore.min))
        stop("Zscore.max must be numeric")
      if(Zscore.min < 0 )
        stop("Zscore.max must be positive integer")
      Up.index <-  data.values$Zscore > Zscore.min
      Down.index <- data.values$Zscore < (-Zscore.min)
      if(is.null(data.values$Symdesc)){
        Up <- list(Cy3 = data.values$Cy3[Up.index], Cy5 = data.values$Cy5[Up.index],
                   Id = data.values$Id[Up.index], Zscore = data.values$Zscore[Up.index])
        Down <- list(Cy3 = data.values$Cy3[Down.index], Cy5 = data.values$Cy5[Down.index],
                     Id = data.values$Id[Down.index], Zscore = data.values$Zscore[Down.index])
      }else{
        Up <- list(Cy3 = data.values$Cy3[Up.index], Cy5 = data.values$Cy5[Up.index],
                   Id = data.values$Id[Up.index], Symdesc = data.values$Symdesc[Up.index], Zscore = data.values$Zscore[Up.index])
        Down <- list(Cy3 = data.values$Cy3[Down.index], Cy5 = data.values$Cy5[Down.index],
                     Id = data.values$Id[Down.index], Symdesc = data.values$Symdesc[Down.index], Zscore = data.values$Zscore[Down.index])
      }
      write.table(Up, paste(fileName,"_Up.", ext, sep = ""), quote = quote, sep = sep, col.names = col.names, row.names = row.names,append=TRUE)
     
      write.table(Down, paste(fileName,"_Down.", ext, sep = ""), quote = quote, sep = sep, col.names = col.names, row.names = row.names,append=TRUE)
    }else{
      if(Zscore.min > Zscore.max)
        stop("Invalid ranges")
      Up.index <-  (data.values$Zscore > Zscore.min) & (data.values$Zscore < Zscore.max)
      Down.index <- (data.values$Zscore < (-Zscore.min)) & (data.values$Zscore > (-Zscore.max))
      if(is.null(data.values$Symdesc)){
        Up <- list(Cy3 = data.values$Cy3[Up.index], Cy5 = data.values$Cy5[Up.index],
                   Id = data.values$Id[Up.index], Zscore = data.values$Zscore[Up.index])
        Down <- list(Cy3 = data.values$Cy3[Down.index], Cy5 = data.values$Cy5[Down.index],
                     Id = data.values$Id[Down.index], Zscore = data.values$Zscore[Down.index])
      }
      else{
        Up <- list(Cy3 = data.values$Cy3[Up.index], Cy5 = data.values$Cy5[Up.index],
                   Id = data.values$Id[Up.index], Symdesc = data.values$Symdesc[Up.index], Zscore = data.values$Zscore[Up.index])
        Down <- list(Cy3 = data.values$Cy3[Down.index], Cy5 = data.values$Cy5[Down.index],
                     Id = data.values$Id[Down.index], Symdesc = data.values$Symdesc[Down.index], Zscore = data.values$Zscore[Down.index])
      }
      write.table(Up, paste(fileName,"_Up.", ext, sep = ""), quote = quote, sep = sep, col.names = col.names, row.names = row.names,append=TRUE)
      write.table(Down, paste(fileName,"_Down.", ext, sep = ""), quote = quote, sep = sep, col.names = col.names, row.names = row.names, append=TRUE)
    }
  }
}

write.zscore <- function(dataSet.spot, fileName, sep = "\t"){
  data.values <- attr(dataSet.spot, "dataSets")
  type <- attr(dataSet.spot, "type")
  if(is.null(data.values$Symdesc))
    zscore <- list(Cy3 = data.values$Cy3, Cy5 = data.values$Cy5,
                   Id = data.values$Id, Zscore = data.values$Zscore)
  else
    zscore <- list(Cy3 = data.values$Cy3, Cy5 = data.values$Cy5,
                   Id = data.values$Id, Symdesc = data.values$Symdesc, Zscore = data.values$Zscore)
  unlink(fileName)
  object <- file(fileName,"w")
  cat("\t\t\t",type,"\n",file=fileName)
  write.table(zscore, fileName, quote = FALSE, sep = sep, col.names = FALSE, row.names = FALSE, append=TRUE)
  close(object)
}
