require(methods) # In case R version is earlier than 1.7.0

setClass("Spot",
	  representation(name ="character", spotData ="list"), where = .GlobalEnv)

#prine(findClass("Spot"))
setValidity("Spot",function(object){

	spotName <- as.character(attr(object, "name"))
	spotData <- attr(object, "spotData")

	Cy3 <- spotData$Cy3
	Cy5 <- spotData$Cy5
	BgCy3 <- spotData$BgCy3
	BgCy5 <- spotData$BgCy5
	Id <- spotData$Id
	if(is.null(Cy3) || is.null(Cy5)) return("Element R or G missing")
	if(!is.numeric(Cy3) || !is.numeric(Cy5)) return("R or G contain non-numeric elements")
	if(length(dim(Cy3)) > 2) return("R and G have more than two dimensions")
})

setClass("DataSet", representation(name = "character", dataSets = "list", type = "character"), where = .GlobalEnv)

setValidity("DataSet", function(object){
  spot.name <- attr(object, "name")
  dataSets <- attr(object, "dataSets")
  type <- attr(object, "type")
  Cy3 <-  dataSets$Cy3
  Cy5 <-  dataSets$Cy5
  Id  <-  dataSets$Id
  Zscore <- dataSets$Zscore
  if(!(type == "ri" || type == "ma")) return ("Type argument must be ri or ma")
  if(is.null(Cy3) || is.null(Cy5) || is.null(Id) || is.null(Zscore)) return("Some element is missing")
  if(!is.numeric(Cy3) || !is.numeric(Cy5) || !is.numeric(Zscore)) return("There is a non-numeric element")
})
