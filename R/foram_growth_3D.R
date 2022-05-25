#' Three-dimensional foram growth
#'
#' This function calculates the distances between all chambers, angles between subsequent chambers, 
#' trochospirality of each chamber, the number of chambers in the final whorl at the time 
#' each chamber was built and coiling direction at every step in the ontogeny. It orders the data
#' by increasing chamber number first.
#' 
#' @param n chamber number
#' @param X Vector with X-coordinates of all chambers
#' @param Y Vector with Y-coordinates of all chambers
#' @param Z Vector with Z-coordinates of all chambers
#' @param proloculus Indicates whether the proloculus is present in the dataset. Defaults to TRUE
#' @keywords chamber
#' @keywords centroid
#' @keywords distance
#' @keywords angle
#' @keywords trochospirality
#' @keywords whorl
#' @usage foram.growth.3D(n, X, Y, Z, proloculus=TRUE)
#' @export

foram.growth.3D <- function(n, X, Y, Z, proloculus=TRUE)
{
  # First order data by increasing chamber number
  M <- data.frame(n, X, Y, Z)
  M1 <- M[order(M[,1]),]
  n <- M1[,1]
  X <- M1[,2]
  Y <- M1[,3]
  Z <- M1[,4]
  
  # Perform analyses on ordered data
  if (proloculus==FALSE)
  {
  A <- data.frame(n, X, Y, Z,
             chamber.angles(X,Y,Z), 
             trochospirality(X,Y,Z), 
             chambers.in.whorl(X,Y,Z)[,2],
             whorls(X, Y, Z),
             coiling.direction(X,Y,Z),
             check.chamber.order(X, Y, Z, proloculus=FALSE))
  }
  
  else
  {
    A <- data.frame(n, X, Y, Z,
                    chamber.angles(X,Y,Z), 
                    trochospirality(X,Y,Z), 
                    chambers.in.whorl(X,Y,Z)[,2], 
                    whorls(X, Y, Z),
                    coiling.direction(X,Y,Z),
                    check.chamber.order(X, Y, Z))
  }
 
  # Assign column names to all output
  names(A) <- c('chamber number', 'centroid X', 'centroid Y', 'centroid Z', 'chamber angle', 'trochospirality', 'chambers in final whorl', 'whorl number', 'Whorl complete?', 'coiling direction', 'chamber order')
  
  return(A)
  }