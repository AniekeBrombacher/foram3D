#' Coiling direction
#'
#' This function determines coiling direction at the time chamber i was built, relative to
#' the two previous chambers. 
#' 
#' @param X Vector with X-coordinates of all chambers
#' @param Y Vector with Y-coordinates of all chambers
#' @param Z Vector with Z-coordinates of all chambers
#' @keywords chamber
#' @keywords centroid
#' @keywords coiling
#' @usage coiling.direction(X,Y,Z)
#' @export

coiling.direction <- function(X, Y, Z)
{
  coiling <- rep(NA, length=length(X))  
  
  for(i in 3:length(X))
    {
      # Cross product between the vectors from chamber e
      axb <- cross.prod(c(X[i-2], Y[i-2], Z[i-2]), c(X[i-1], Y[i-1], Z[i-1]), c(X[i], Y[i], Z[i]))
      
      # angle between growth from proloculus to chamber i, and cross product
      ang <- ang.vec(c(X[i-1], Y[i-1], Z[i-1]) - c(X[1], Y[1], Z[1]), axb)
      
      # Now check if cross product and vector from proloculus to chamber i point in the 
      # same or opposite directions
      if(ang < 90)
      { 
        coiling[i] <- 'dextral'    # Cross product in same direction as direction of growth. 
                                   # Counter-clockwise growth.
      }
      else
      {
        coiling[i] <- 'sinistral'  # Cross product in opposite direction to direction of growth.
                                   # Clockwise growth.
      }
    }
  return(coiling)
}