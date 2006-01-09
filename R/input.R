# GenArise

# Read all file, but only extract the interested columns and create a Spot object
# The numnber of columns are the IFC format 

read.spot <- function( file.name, cy3, cy5, bg.cy3, bg.cy5, ids, symdesc, header = FALSE, sep = "\t", is.ifc = FALSE,envir){
  if(missing(envir)){
    envir <- globalenv()
  }
  tmp <- unlist(strsplit(file.name, "\\/"))
  spot.name <- unlist(strsplit(tmp[length(tmp)], "\\."))[1]
  if(is.ifc){
    temp <- read.csv(file.name, header = FALSE, sep = "\t", comment.char = "", quote="")
    temp <- temp[-1,] # we delete the header
    row1 <- temp[,1]
    flag <- which(rev(row1) == "Mean value") # regresa un solo valor
    empty.row <- length(row1[row1==""])
    total.rows <-  length(temp[,1])
    temp <- temp[-((total.rows-(flag-1 + empty.row)):total.rows),]
    total.rows <- length(temp[,1])
    temp <- apply(temp, 2, as.vector)
    ans <- paste(max(as.integer(temp[,2]),na.rm=TRUE), max(as.integer(temp[,3]),na.rm=TRUE),
                 max(as.integer(temp[,4]),na.rm=TRUE), max(as.integer(temp[,5]),na.rm=TRUE), sep = " ")
#    print(paste("The dimensions are ", max(as.integer(temp[,2]),na.rm=TRUE), max(as.integer(temp[,3]),na.rm=TRUE), max(as.integer(temp[,4]),na.rm=TRUE), max(as.integer(temp[,5]),na.rm=TRUE), "Is it right? [y/n]",sep = "  "))
 #   resp <- readline()
  #  if(tolower(resp) == "n"| tolower(resp) == "no"){
   #   print("Enter the right dimensions? ")
    #  ans <- readline()
     #                                   }
    assign("nr.global",max(as.integer(temp[,4])),envir = envir)
    assign("nc.global",max(as.integer(temp[,5])),envir = envir)
    assign("nmr.global",max(as.integer(temp[,2])),envir = envir)
    assign("nmc.global",max(as.integer(temp[,3])),envir = envir)
    dimensiones <-  unlist(strsplit(ans, " "))
    dimensiones <- as.numeric(dimensiones)
    
    
    ubications <- function(mr,mc,r,c){
      lista <- seq(1:mr)
      lista <- sapply( lista, paste, seq(1:mc))
      lista <- as.vector(lista)
      lista <- sapply( lista, paste, seq(1:r))
      lista <- as.vector(lista)
      lista <- sapply(lista, paste, seq(1:c))
      lista <- as.vector(lista)
      lista
    }
    
    lista <-  ubications(dimensiones[1],dimensiones[2],dimensiones[3],dimensiones[4])
    final <- matrix(ncol = 5)
    conf <-  paste(temp[,2],temp[,3],temp[,4],temp[,5], sep = " ")
    index <- is.element(lista,conf)
    if(!missing(symdesc)){
      result.list <-  matrix(ncol = 6, nrow=length(lista))
      result.list[!index,6] <- "empty"
    }
    else
      result.list <-  matrix(ncol = 5, nrow=length(lista))
    result.list[!index,1:4] <- as.integer(0)
    result.list[!index,5] <- "empty"

    koala <- match(conf,lista)
    if(missing(symdesc)){
      result.list[koala,] <- c(temp[koala, cy3], temp[koala,cy5], temp[koala, bg.cy3], temp[koala, bg.cy5], temp[koala, ids])
    return(new ("Spot", name = spot.name,
                spotData = list(Cy3 = as.numeric(result.list[,1]), Cy5 = as.numeric(result.list[,2]),
                  BgCy3 = as.numeric(result.list[,3]), BgCy5 = as.numeric(result.list[,4]), Id = as.vector(result.list[,5]))))
    }
    else{
      result.list[koala,] <- c(temp[koala, cy3], temp[koala,cy5], temp[koala, bg.cy3], temp[koala, bg.cy5], temp[koala, ids],temp[koala, symdesc])
      return(new ("Spot", name = spot.name,
                  spotData = list(Cy3 = as.numeric(result.list[,1]), Cy5 = as.numeric(result.list[,2]),
                    BgCy3 = as.numeric(result.list[,3]), BgCy5 = as.numeric(result.list[,4]), Id = as.vector(result.list[,5]), Symdesc = as.vector(result.list[,6]))))
    }
  }
  else{
    spot <- read.csv( file.name, header = header, sep = sep)
    spot[,ids] <-  as.vector(spot[,ids])
    if(missing(symdesc))
      return(new ("Spot", name = spot.name ,spotData = list(Cy3=spot[,cy3],
                                              Cy5=spot[,cy5], BgCy3=spot[,bg.cy3], BgCy5=spot[,bg.cy5], Id = as.vector(spot[,ids]))))
    else{
      return(new ("Spot", name = spot.name ,spotData = list(Cy3=spot[,cy3],
                                              Cy5=spot[,cy5], BgCy3=spot[,bg.cy3], BgCy5=spot[,bg.cy5], Id = as.vector(spot[,ids]), Symdesc=as.vector(spot[,symdesc]))))
    }
  }
}

read.dataset <- function( file.name, cy3 = 1, cy5 = 2, ids = 3, symdesc = 4, zscore = 5, type = 6, header = FALSE, sep = "\t"){
  tmp <- unlist(strsplit(file.name, "\\/"))
  dataset.name <- unlist(strsplit(tmp[length(tmp)], "\\."))[1]
  dataset <- read.csv( file.name, header = header, sep = sep)
  type <- trim(as.character(dataset[1,4]))
  dataset <- dataset[-1,]
  if(missing(symdesc)){
    new ("DataSet", name = dataset.name ,dataSets = list(Cy3 = as.numeric(dataset[,cy3]),
                                         Cy5 = as.numeric(dataset[,cy5]), Id = as.vector(dataset[,ids]),
                                         Zscore = as.numeric(as.vector(dataset[,zscore]))), type= type)
}
  else{
      new ("DataSet", name = dataset.name ,dataSets = list(Cy3 = as.numeric(dataset[,cy3]),
                                         Cy5 = as.numeric(dataset[,cy5]), Id = as.vector(dataset[,ids]), Symdesc = as.vector(dataset[,symdesc]),
                                         Zscore = as.numeric(as.vector(dataset[,zscore]))), type= type)
    }
  
}
