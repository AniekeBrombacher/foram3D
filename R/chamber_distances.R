#' Distance between all chambers of a foram
#'
#' This function creates a matrix with distances between every possible pair of chambers in R^3 based on the chamber centroid coordinates
#' @param X Vector with X-coordinates of all chambers
#' @param Y Vector with Y-coordinates of all chambers
#' @param Z Vector with Z-coordinates of all chambers
#' @keywords chamber
#' @keywords centroid
#' @usage chamber.distances(X,Y,Z)
#' @export


chamber.distances <- function(X, Y, Z)     
{
  C <- matrix(data=NA, ncol=length(X), nrow=length(X))
  
  for(i in 1:length(X))
  {
    for(j in 1:length(X))
    {
      C[i,j] <- cham.dist(c(X[i], Y[i], Z[i]), c(X[j], Y[j], Z[j]))   
    }
  }
  return(C)
}