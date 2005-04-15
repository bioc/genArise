# Extract columns of interest from complete data frames

#Intensity log-ratio
m.arise <- function (mySpot){
	spot <- as.list(attr(mySpot, "spotData"))
	cys.index <- (spot$Cy3 > 0) & (spot$Cy5 > 0) & (!is.na(spot$Cy3)) &(!is.na(spot$Cy5)) & is.finite(spot$Cy3) & is.finite(spot$Cy5)
	cy3 <- spot$Cy3[cys.index]
	cy5 <- spot$Cy5[cys.index]
	log(cy5,2) - log(cy3, 2)
}

#Mean log intensity
a.arise <- function(mySpot) {
	spot <- attr(mySpot, "spotData")
	cys.index <- (spot$Cy3 > 0) & (spot$Cy5 > 0) & (!is.na(spot$Cy3)) &(!is.na(spot$Cy5)) & is.finite(spot$Cy3) & is.finite(spot$Cy5)
	cy3 <- spot$Cy3[cys.index]
	cy5 <- spot$Cy5[cys.index]
	(log(cy3, 2) + log(cy5, 2))/2
}

#Get the Ratios
r.arise <- function(mySpot){
	spot <- as.list(attr(mySpot, "spotData"))
	log(spot$Cy5,2) - log(spot$Cy3, 2)
}

#Get the Intensities
i.arise <- function(mySpot){
	spot <- as.list(attr(mySpot, "spotData"))
	log(spot$Cy3, 10) + log(spot$Cy5, 10)
}

