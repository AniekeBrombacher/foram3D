#' Total number of whorls
#'
#' This function calculates the number of complete whorls plus any remaining chambers per foram. Returns as 
#' c(number of complete whorls, number of remaining chambers).
#' @param X Vector with X-coordinates of all chambers
#' @param Y Vector with Y-coordinates of all chambers
#' @param Z Vector with Z-coordinates of all chambers
#' @keywords chamber
#' @keywords centroid
#' @keywords whorl
#' @usage total.whorls(X,Y,Z)
#' @export

total.whorls <- function(X,Y,Z)         
{
  A <- chambers.in.whorl(X,Y,Z)
  a0 <- max(which(A[,1] == 1))
  a1 <- min(which(A[,1]==a0))
  if(length(X)-a1 > A[a1,2])              # If there is more than 1 whorl left, continue
  {
    a2 <- min(which(A[,1]==a1))
    if(length(X)-a2 > A[a2, 2])           # If there is more than 1 whorl left, continue
    {
      a3 <- min(which(A[,1]==a2))
      if(length(X)-a3 > A[a3,2])          # If there is more than 1 whorl left, continue
      {
        a4 <- min(which(A[,1]==a3))       # Planktonic foraminifera don't usually have 6 whorls or more
        rest <- length(X) - a4            # Stop here.
        totalwhorls <- c(5, rest)
        
      }
      else
      {
        rest <- length(X) - a3
        totalwhorls <- c(4, rest)
      }
      
    }
    else
    {
      rest <- length(X) - a2
      totalwhorls <- c(3, rest)
    }
  }
  else
  {
    rest <- length(X) - a1
    totalwhorls <- c(2, rest)
  }
  
  return(totalwhorls)
}