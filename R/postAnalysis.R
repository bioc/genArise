get.values <- function(list.values, genes.values, up.down, min.val, max.val){
  if( !(up.down == "up" || up.down == "down"))
    stop("The value for the  up.down argument must be up or down")
  if(is.null(min.val) && is.null(max.val))
    stop("You must select a range")
  if(up.down == "up"){
    if(!is.null(max.val) && !is.null(min.val)){
      indices <- (list.values > min.val) & (list.values <= max.val)
    }else{
      if(is.null(max.val)){
        indices <- list.values >= min.val
      }else{
        indices <- list.values <= max.val
      }
    }      
  }else{
    if(!is.null(max.val) && !is.null(min.val)){
      indices <- (list.values < -min.val) & (list.values >= -max.val)
    }else{
      if(is.null(max.val)){
        indices <- list.values < -min.val
      }else{
        indices <- list.values >= -max.val
      }
    }      
  }
  toupper(genes.values[indices])
}

post.analysis <-  function(values, min.val, max.val, up.down, output){
   if(length(values) <= 1)
    stop("You must at least two files to perform the post-analysis")
  myHash <- new.env(hash = TRUE)
  for(i in 1 :length(values)){
    results.file <- read.table(values[i], sep ="\t", header = FALSE)[1,1]
    file.data <- read.csv(file.path(unlist( strsplit(values[i], "\\.prj"))[1],
                                      results.file, "zscore.txt"),sep = "\t", header = FALSE)
    file.data <- file.data[-1,]
    if(length(file.data[1,])==5)
      assign(values[i], get.values(as.vector(file.data[,5]), as.vector(file.data[,3]), up.down, min.val, max.val), envir = myHash)
    else
      assign(values[i], get.values(as.vector(file.data[,4]), as.vector(file.data[,3]), up.down, min.val, max.val), envir = myHash)
  }


  intersect.sets <-  function(hash,archivos, binario){
    archivos.int <- archivos[binario == 1]
    archivos.dif <- archivos[binario == 0]

    interseccion <- get(archivos.int[1], envir = hash)

    if(length(archivos.int) > 1){
      for(i in 2:length(archivos.int)){
        ids <-  get(archivos.int[i], envir = hash)
        interseccion <- intersect(interseccion, ids)
      }
    }
    if(length(archivos.dif) > 0){
      for(i in 1:length(archivos.dif)){
        ids <-  get(archivos.dif[i], envir = hash)
        interseccion <- setdiff(interseccion, ids)
      }
    }
    interseccion
  }
  
  nextNumber <- function(binario, pos1){
    while(pos1 > 0){
      
      if(binario[pos1] == 0){
        binario[pos1] <- 1
        break
      }else{
        binario[pos1] <- 0
        pos1 <- pos1 - 1
        nextNumber(binario, pos1)
      }
    }
    binario
  }
  
  binary.add <- function(lista, cantidad){
    for(i in 0:cantidad){
      nextNumber(lista, length(lista))
    }
  }

  final <- function(lista){
    
    contador.binario <- rep(0, times = length(lista))
    if(file.exists(output)){
      files.list <- list.files(output)
      for(i in 1:length(files.list))
        file.remove(paste(output,files.list[i],sep=.Platform$file.sep))
    }
    else
      dir.create(output)
    write.table(lista, file.path(output, "order.txt"), sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)
    cat("The order of the files is important for the right interpretation of the data.", file = file.path(output, "order.txt"),
        append = TRUE)

    for(i in 1:((2^length(lista))-1)){
      contador.binario <- binary.add(contador.binario,1)
      datos <- intersect.sets(myHash, lista, contador.binario)
      output.file <- file.path(output, paste(paste(contador.binario, collapse= ""), ".set", sep = ""))
      unlink(output.file)
      objeto <- file(output.file, "w")
      write.table(datos, output.file, col.names = FALSE, row.names = FALSE,quote = FALSE, sep = "\t")
      close(objeto)
    }
  }
  
  final(values)

 }
