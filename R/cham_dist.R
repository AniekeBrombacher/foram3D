#' Distance between two chambers
#'
#' This function calculates the distance between two chambers in R^3 based on 
#' their centroid coordinates
#' @param P Centroid coordinates c(x,y,z) of chamber i
#' @param Q Centroid coordinates c(x,y,z) of chamber j
#' @keywords chamber
#' @keywords centroid
#' @usage cham.dist(P, Q)
#' @export
#' @examples
#' cham.dist(c(1,1,1), c(1,2,3)) = 2.24

cham.dist <- function(P, Q)             
{
  v <- c(P[1] - Q[1], P[2] - Q[2], P[3] - Q[3])
  return(sqrt(v[1]^2+v[2]^2+v[3]^2))  
}

