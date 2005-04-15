genMerge <- function(gene.association, description, population.genes, study.genes, output.file = "GenMerge.txt"){

	gene.association.file <- read.csv(gene.association, header = FALSE, sep = "\t")
	des.file <- read.csv(description, header = FALSE, sep = "\t")
	population.file <- read.csv(population.genes, header = FALSE, sep = "\t")
	study.genes.file <- read.csv(study.genes, header = FALSE, sep = "\t")

	gene.association.file$V1 <- as.vector(gene.association.file$V1)
	gene.association.file$V2 <- as.vector(gene.association.file$V2)

	des.file$V1 <-  as.vector(des.file$V1)
	des.file$V2 <-  as.vector(des.file$V2)

	population.file$V1 <- as.vector(population.file$V1)
	total.no.detected.genes <- length(population.file$V1)
	GMRGgenomehash <- new.env(hash = TRUE)
	descriptionHash <- new.env(hash = TRUE)
	id.hash <- new.env(hash = TRUE)

	for(i in 1:total.no.detected.genes){
		assign(gene.association.file$V1[i], list( GO = gene.association.file$V2[i]), env = GMRGgenomehash)
	}

	for(i in 1:length(des.file$V1)){
		assign(des.file$V1[i], list( Description = des.file$V2[i]), env = descriptionHash)
	}

	key.gene.association <- ls(env = GMRGgenomehash)

	GMRGs.line.by.line <- list()

	for(i in 1:total.no.detected.genes){
		if(exists(population.file$V1[i], envir = GMRGgenomehash)){
			big.line <- get( population.file$V1[i], env = GMRGgenomehash)
			indivGMRGs <-  strsplit(big.line, ";")[[1]]
			GMRGs.line.by.line <- c(indivGMRGs, GMRGs.line.by.line, recursive = TRUE)
		}
	}

	tmp <- GMRGs.line.by.line[sort.list(GMRGs.line.by.line)]

	unique.list <- rle(tmp)

	output.hash <- new.env( hash = TRUE)

	for( i in 1:length(unique.list$values)){
		assign(unique.list$values[i], list(count = unique.list$lengths[i], frequence = unique.list$lengths[i]/total.no.detected.genes), env = output.hash)
	}

	study.genes.file$V1 <- as.vector(study.genes.file$V1)
	total.no.updown.genes <- length(study.genes.file$V1)
	updownGMRGs.line.by.line <- list()
	genes.not.found.in.ontology <- list()

	for(i in 1:total.no.updown.genes){
		if(exists(study.genes.file$V1[i], envir = GMRGgenomehash)){
		bigline <- get(study.genes.file$V1[i], env = GMRGgenomehash)
			updownGMRGs <-  strsplit(bigline, ";")[[1]]
			for(k in 1:length(updownGMRGs)){
				if(exists(updownGMRGs[k], envir = id.hash)){
					unique.list1 <- get( updownGMRGs[k], env = id.hash)
					unique.list <- paste(unique.list1, study.genes.file$V1[i], sep = " ")
					assign(updownGMRGs[k], unique.list,  env = id.hash)
				}
				else{
					assign(updownGMRGs[k], study.genes.file$V1[i], env = id.hash)
				}
			}
			updownGMRGs.line.by.line <- c(updownGMRGs, updownGMRGs.line.by.line, recursive = TRUE)
		}
		else{
			genes.not.found.in.ontology <- c(study.genes.file$V1[i], genes.not.found.in.ontology, recursive = TRUE)
		}
	}

	tmp <- updownGMRGs.line.by.line[sort.list(updownGMRGs.line.by.line)]

	unique.list <- rle(tmp)

	output2 <- list(uniqueUpDownGMRGIDs = unique.list$values , frequence = unique.list$lengths)

	BCr <- 0
	frequences <- list()
	count.list <- list()

	for(i in 1:length(output2$uniqueUpDownGMRGIDs)){
		if(exists(output2$uniqueUpDownGMRGIDs[i], output.hash)){
			tmp <- get(output2$uniqueUpDownGMRGIDs[i], env = output.hash)
			frequences <- c(frequences,tmp$frequence, recursive = TRUE)
			count.list <- c( count.list, tmp$count, recursive = TRUE)
		}
	}

BCr <- length(frequences[frequences > (1/total.no.detected.genes)])

hyper <- function(n,p,k,r){
	q <- (1-p)
	np <- floor(n*p + 0.5)
	nq <- floor(n*q + 0.5)
	log.n.choose.k <- lchoose(n,k)
	top <- k
	if(np < k){
		top <- np
	}
	lfoo <- lchoose(np, top) + lchoose(n*(1-p),k-top)
	sum <- 0
	i <- top
	while(i >= r){
		sum <- sum + exp(lfoo - log.n.choose.k)
		if(i > r){
			lfoo <- lfoo + log(i/(np-i+1)) + log((nq-k+i)/(k-i+1))
		}
		i <- i - 1
	}
	sum
}

p.value <- list()
bonferroni <- list()

for(s in 1:length(frequences)){
	if(output2$frequence[s] != 1){
		p.v <- hyper(total.no.detected.genes,frequences[s] ,total.no.updown.genes,output2$frequence[s])
		p.value <- c(p.value, p.v)
		p.value.corrected <- p.v * BCr
		if(p.value.corrected >= 1){
			bonferroni <- c( bonferroni, 1, recursive = TRUE)
		}
		else{
			bonferroni <- c( bonferroni, p.value.corrected, recursive = TRUE)
		}
	}
	else{
		p.value<- c(p.value, NA, recursive = TRUE)
		bonferroni <- c(bonferroni, NA, recursive = TRUE)
	}
}

description.list <- list()

for( k in 1:length(frequences)){
if(exists(output2$uniqueUpDownGMRGIDs[k], envir = descriptionHash)){
	description.list <- c(description.list, get(output2$uniqueUpDownGMRGIDs[k], env = descriptionHash), recursive = TRUE)
}
}
# write output file

listita <- list()
for(k in 1:length(output2$uniqueUpDownGMRGIDs)){
	listita <- c(listita, get(output2$uniqueUpDownGMRGIDs[k], env = id.hash), recursive = TRUE)
}

pop.fac <- paste(count.list, "/", total.no.detected.genes, sep = "")
std.fac <- paste(output2$frequence, "/", total.no.updown.genes, sep = "")

tablota <- list( output2$uniqueUpDownGMRGIDs, frequences, pop.fac, std.fac, p.value, bonferroni, description.list, listita)

#Crear archivo de salida:
unlink(output.file)

objeto <- file( output.file, "w")

cat( paste("genMerge; ", output.file, sep = "  "), file = output.file, append = TRUE, sep = " ")
cat("\n", paste("Gene Association File: ", gene.association, sep = " "),  paste("Description File: ", description, sep = " "), paste("Population File: ", population.genes, sep = " "), paste("Study File: ", study.genes, sep = " "), "\n", "\n", file = output.file, append = TRUE, sep = "\n")

cat("GMRG_Term", "Pop_freq", "Pop_frac", "Study_frac", "Raw_es", "e-score", "Description", "Contributing_genes", append = TRUE, file = output.file, sep = "\t")

cat("\n", append = TRUE, file = output.file, sep = "")

write.table(tablota, output.file, col.names = FALSE, row.names = FALSE, append = TRUE,quote = FALSE, sep = "\t")
cat("\n", append = TRUE, file = output.file, sep = "")

cat (paste("Total number of genes:", total.no.detected.genes, sep = " "), append = TRUE,file = output.file)
cat("\n", file = output.file, append = TRUE,sep = "")
cat (paste("Total number of Study genes:", total.no.updown.genes, sep = " "), append = TRUE,file = output.file)
cat("\n", append = TRUE,file = output.file, sep = "")
cat (paste("Total number of Study gene GMRG terms (pop non-singletons):", length(output2$uniqueUpDownGMRGIDs),"(", BCr, ")",  sep = " "), append = TRUE,file = output.file)
cat("\n", append = TRUE, file = output.file, sep = "")

cat(paste("Genes with GMRG information:", total.no.updown.genes - length(genes.not.found.in.ontology), sep = " "), file = output.file,  append = TRUE)
cat("\n", append = TRUE, file = output.file, sep = "")
cat(paste("Genes with no GMRG information:", length(genes.not.found.in.ontology), sep = " "), file = output.file,  append = TRUE)
cat("\n", append = TRUE, file = output.file, sep = "")
cat("These are", append = TRUE,file = output.file, sep = "")
cat("\n", append = TRUE, file = output.file, sep = "")
cat(genes.not.found.in.ontology, file = output.file, append = TRUE)
close(objeto)
}
