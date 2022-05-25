#' Whorl number
#'
#' Determines the whorl number of each chamber
#' @param X Vector with X-coordinates of all chamber centroids
#' @param Y Vector with Y-coordinates of all chamber centroids
#' @param Z Vector with Z-coordinates of all chamber centroids
#' @keywords chamber
#' @keywords centroid
#' @keywords whorl
#' @usage whorl(X,Y,Z)
#' @export

whorl <- function(X, Y, Z)
{
  wh <- vector(mode='numeric', length=length(X))
  for(i in 1:length(X))
  {
    # Whorl 1
    if(chambers.in.whorl(X, Y, Z)[i,1] == 1)
    {
      wh[i] = 1   # all chambers with the proloculus as closest non-neighbour chamber are in the first whorl
    }
  
    # Whorl 2
    w1 <- which(wh==1)     # all chambers in whorl 1
    w1 <- w1[-1]           # discard proloculus
    if(chambers.in.whorl(X, Y, Z)[i,1] %in% w1)
    {
     wh[i] = 2   # all chambers with the closest non-neighbouring chamber in whorl 1
    }
  
    # Whorl 3 and on
    for(j in 2:10)
    {
      # The closest chamber to chamber i is in whorl j. Therefore, chamber i is in whorl j+1
      if(chambers.in.whorl(X, Y, Z)[i,1] %in% which(wh==j))
      {
        wh[i] = j+1    # all chambers with the closest non-neighbouring chamber in whorl j
      }
    }
    
  }
  return(wh)
}
