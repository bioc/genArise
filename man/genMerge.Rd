\name{genMerge}
\alias{genMerge}
\title{genMerge: Post-Genomic Analysis}
\description{
  After we finished our slice analysis we get a up-regulated and
  down-regulated set. This will be the set of study genes for genMege.
  Given this set, genMerge retrieves functional genomic data for each
  gene and provides statistical rank scores for over-representation of
  particular functions in the dataset.
}
\usage{
genMerge(gene.association, description, population.genes,
study.genes, output.file = "GenMerge.txt")
}
\arguments{
  \item{gene.association}{The gene-association file links gene names
    with a particular datum of information using a shorthand of
    gene-association IDS}
  \item{description}{The description file contains human-readable
    description of gene-association IDS}
  \item{population.genes}{Set of all genes detected on a array}
  \item{study.genes}{Set of genes may be those that are up-regulated or
    down-regulated or both of them.}
  \item{output.file}{The name of output file that includes all results
    obtained after this analisys.}
}
\note{This function is completly based on GeneMerge from Cristian
  I. Castillo-Davis and Daniel L. Hartl}
\references{
  Cristian I. Castillo-Davis
  Department of Statistics
  Harvard University
  \url{http://www.oeb.harvard.edu/hartl/lab/publications/GeneMerge}
}
\keyword{math}
