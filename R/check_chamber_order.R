#' Check if chambers are in the correct order
#'
#' This function checks if the chambers of a foram are in the correct order by analysing the 
#' angles between consecutive chambers. In species with three or more chambers in a whorl 
#' the angles between consecutive chambers are expected to be larger than 60 degrees. The 
#' exception is the angle between the first three chambers, which can be smaller than 60 degrees. In 
#' specimens with the proloculus known to be present the first angle is not taken into 
#' consideration. If the proloculus is not present due to for example dissolution, the first 
#' angle can be taken into account.
#' 
#' @param X Vector with X-coordinates of all chambers
#' @param Y Vector with Y-coordinates of all chambers
#' @param Z Vector with Z-coordinates of all chambers
#' @param proloculus Indicates whether the proloculus is present. Defaults to TRUE
#' @keywords chamber
#' @keywords whorl
#' @usage check.chamber.order(X, Y, Z, proloculus=TRUE)
#' @export

check.chamber.order <- function(X, Y, Z, proloculus=TRUE)
{
  A <- chamber.angles(X, Y, Z)

  # Proloculus not present: analyse all chamber angles
  if (proloculus==FALSE)
  {
    if(any( A[3:length(A)] < 60 ))
    {
      c <- rep('Check chamber order', length(A))
    }
    else
    {
      c <- rep('Correct', length(A))
    }
  }
  
  # Proloculus present: discard first angle
  else
  {
    if(any( A[4:length(A)] < 60 ))
    {
      c <- rep('Check chamber order', length(A))
    }
    else
    {
      c <- rep('Correct', length(A))
    }
  }

return(c)
}
