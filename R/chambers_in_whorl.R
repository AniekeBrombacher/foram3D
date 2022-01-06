#' Chambers in the whorl
#'
#' This function calculates the number of chambers in the final whorl, at the time that chamber i was built. 
#' @param X Vector with X-coordinates of all chambers
#' @param Y Vector with Y-coordinates of all chambers
#' @param Z Vector with Z-coordinates of all chambers
#' @keywords chamber
#' @keywords centroid
#' @usage chambers.in.whorl(X,Y,Z)
#' @export


chambers.in.whorl <- function(X, Y, Z)  
{
  closestchamber <- vector(mode='numeric', length=length(X))
  chambersinwhorl <- vector(mode='numeric', length=length(X))
  
  
  closestchamber[1] <- 1        # The closest chamber to chamber 1 is chamber 1
  chambersinwhorl[1] <- 1       # At the time the proloculus is built, there is only one chamber in the whorl
  closestchamber[2] <- 1        # The closest chamber to chamber 2 is chamber 1
  chambersinwhorl[2] <- 2       # At the time the deuteroconch is built, there are two chambers in the whorl
  closestchamber[3] <- 1        # The closest chamber to chamber 3 is chamber 1
  chambersinwhorl[3] <- 3       # At the time the 3rd chamber is built, there are three chambers in the whorl
  
  A <- cbind(X,Y,Z)
  
  #Calculate distance between all chambers up until chamber i (i starts at chamber 4)
  for(i in 4:length(X))
  {
    B <- A[1:i,]                                   # Matrix with centroid coordinates of first i chambers
    C <- matrix(NA, nrow=i, ncol=i)
    C <- chamber.distances(B[,1], B[,2], B[,3])    # Distances between all chambers built before chamber i
    
    a1 <- which(C[,i]==Rfast::nth(C[,i], 2, descending=F))            # Closest chamber to chamber i
    a2 <- which(C[,i]==Rfast::nth(C[,i], 3, descending=F))            # Second closest chamber to chamber i
    
    if(a1 == i-1)     # ai == i-1 means that the closest chamber to chamber i is the chamber next to it, not below it
    {
      # If yes: discard chamber a1 and select chamber a2 as the closest chamber in the whorl below
      closestchamber[i] <- a2   
    }
    else
    {
      # If no: the closest chamber in the whorl below is chamber a1
      closestchamber[i] <- a1   
    }
    
    # Two options: the closest chamber is the proloculus (a=1, for juvenile chambers in first whorl) or not
    if(closestchamber[i]==1)
    {
      chambersinwhorl[i] <- i
    }
    # If not: substract number of the chamber in whorl below i from i
    else
    {
      chambersinwhorl[i] <- i - closestchamber[i]
    }
  }
  
  M <- cbind(closestchamber, chambersinwhorl)
  return(M)
}