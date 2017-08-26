#' Create taxonomy-aggregated community data.frame
#'
#' @param comm community format data.frame (samples in rows, taxa in columns, rows and columns have informative names)
#' @param taxo a data frame with rows in same order as columns of \code{comm} and taxonomic information for each row provided in columns
#' @param rank Taxonomic rank to aggregate at (a column name in \code{taxo})
#' @return A community data.frame where columns are taxonomic groups
#' @export
taxocomm <- function(comm, taxo, rank) {
  comm.taxo <- aggregate(t(comm), by=list(taxonrank=taxo[,rank]), sum)
  rownames(comm.taxo) <- comm.taxo[,1]
  return(t(comm.taxo[,-1]))
}
