#' Angle between vectors
#'
#' This function determines the angle between two vectors v and w. 
#' 
#' @param v Vector in R3
#' @param w Vector in R3
#' @keywords vector
#' @keywords angle
#' @usage ang.vec(v,w)
#' @export

ang.vec <- function(v, w)          
{
  v <- as.vector(v)
  w <- as.vector(w)
  return(as.numeric((acos((v%*%w)/(sqrt(v[1]^2+v[2]^2+v[3]^2)*sqrt(w[1]^2+w[2]^2+w[3]^2)))/pi)*180))  
}