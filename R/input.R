# GenArise

# Read all file, but only extract the interested columns and create a Spot object
# The numnber of columns are the IFC format 

read.spot <- function( file.name, cy3 = 1, cy5 = 2, bg.cy3 = 3, bg.cy5 = 4, ids = 5, header = FALSE, sep = "\t", is.ifc = FALSE){
  tmp <- unlist(strsplit(file.name, "\\/"))
  spot.name <- unlist(strsplit(tmp[length(tmp)], "\\."))[1]
  if(is.ifc){
    temp <- read.csv(file.name, header = FALSE, sep = "\t", comment.char = "")
    temp <- temp[-1,] # aqui estamos quitar los encabezados
    row1 <- temp[,1]
    flag <- which(rev(row1) == "Mean value") # regresa un solo valor
    empty.row <- length(row1[row1==""])
    total.rows <-  length(temp[,1])
    temp <- temp[-((total.rows-(flag-1 + empty.row)):total.rows),]
    total.rows <- length(temp[,1])
    temp <- apply(temp, 2, as.vector)
    return(new ("Spot", name = spot.name,
                spotData = list(Cy3 = as.numeric(temp[,11]), Cy5 = as.numeric(temp[,12]),
                  BgCy3 = as.numeric(temp[,13]), BgCy5 = as.numeric(temp[,14]), Id = as.vector(temp[,6]))))
  }
  else{
        spot <- read.csv( file.name, header = header, sep = sep)
        return(new ("Spot", name = spot.name ,spotData = list(Cy3=spot[,cy3],
                                                Cy5=spot[,cy5], BgCy3=spot[,bg.cy3], BgCy5=spot[,bg.cy5], Id = as.vector(spot[,ids]))))
      }
}
