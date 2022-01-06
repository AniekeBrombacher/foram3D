#' Cross product
#'
#' Calculates a vector orthogonal to the plane spanned by three consequtive chamber centroids.
#' Growth direction determines the direction of the orthogonal vector (i.e. up or down) so
#' centroids are given in the direction of growth, with P being the oldest chamber of the three, 
#' Q the subsequent chamber and R the subsequent chamber after that. 
#' @param P xyz coordinates of chamber i
#' @param Q xyz coordinates of chamber i+1
#' @param R xyz coordinates of chamber i+2
#' @keywords chamber
#' @keywords centroid
#' @usage cross.prod(P, Q, R)
#' @export

cross.prod <- function(P, Q, R)        
{
  v <- c(Q[1] - P[1], Q[2] - P[2], Q[3] - P[3])
  w <- c(R[1] - Q[1], R[2] - Q[2], R[3] - Q[3])

  return(c(v[2]*w[3]-w[2]*v[3], -(v[1]*w[3]-v[3]*w[1]), v[1]*w[2]-v[2]*w[1]))  
}
