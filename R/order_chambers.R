#' Put chambers in the correct order of growth
#'
#' This function puts the chambers in the order of growth. There are two options: either the full growth sequence
#' from proloculus to final chamber is recorded, or the first few chambers are missing due to for example
#' dissolution. 
#' 
#' Full growth sequence: the smallest chamber is the deuteroconch. The proloculus is the closest chamber to the deuteroconch.
#' The third chamber is the next closest chamber to chamber 2, the fourth chamber is the next closest to chamber 3, etc.
#' 
#' First few chambers missing: the smallest chamber is the first in the growth sequence. The next chamber is the closest to 
#' the first chamber in the sequence, the third in the sequence is the next closest to the second in the sequence, etc.
#' 
#' 
#' @param X Vector with X-coordinates of all chambers
#' @param Y Vector with Y-coordinates of all chambers
#' @param Z Vector with Z-coordinates of all chambers
#' @param V Vector with volumes of each chamber
#' @param proloculus Indicates whether the proloculus is present and determines which algorithm is applied. Defaults to TRUE
#' @keywords chamber
#' @keywords whorl
#' @usage order.chambers(X, Y, Z, V, proloculus=TRUE)
#' @export

order.chambers <- function(X, Y, Z, V, proloculus=TRUE)          
{
  # Algorithm for determining chamber order in case the proloculus is not recorded
  if(proloculus==FALSE)
  {
    A <- chamber.distances(X, Y, Z)
    r <- length(X)                                # r = number of chambers 
    no <- vector(mode='numeric', length=r)        # no = a vector for chamber numbers
    
    # Step 1: find the earliest recorded chamber (assumes that this is the smallest chamber)
    v0 <- min(V)                        # v0 = volume of the smallest chamber
    a <- which(V==v0)                   # a = position of smallest chamber
    no[a] <- 1                          # smallest chamber is assigned chamber 1
    
    # Steps 2 to r: find chamber i
    b <- c(which(no==1))                # b = row number of chamber 1. This row needs to be discarded from A in the first for-loop iteration, 
    for (i in 2:r)                      # which finds chamber 2.
    {
      Q <- A[-b,]                       # Q = matrix with chambers 1 to i-2 discarded (they've already been assigned)
      x <- which(no==i-1)               # x = row number of chamber i-1
   
      d <- min(Q[,x][Q[,x] >0])         # d = smallest non-zero distance between chamber i-1 and all unassigned chambers
      no[which(A[x,]==d)] <- i          # Assigns chamber i: chamber with shortest non-zero distance to chamber i-1
      
      b <- c(b, which(no==i-1))         # b = vector for row numbers of chambers 1 to i-1, which need to be discarded from A for iteration i+1
    }
    
    # Data output
    data <- data.frame(X, Y, Z, V, no)
    colnames(data) <- c('X centroid', 'Y centroid', 'Z centroid', 'Volume', 'Chamber number')
    data <- data[order(data[,5]),]
    return(data)
  }
  
  # Algorithm for determining chamber order when the proloculus is recorded
  else
  {
    A <- chamber.distances(X, Y, Z)
    r <- length(X)                                # r = number of chambers 
    no <- vector(mode='numeric', length=r)        # Creates a vector for chamber numbers
    
    # Step 0: find chamber 2: the smallest chamber
    v0 <- min(V)                        # V0 = volume of the smallest chamber
    a <- which(V==v0)                   # a = position of kth smallest chamber
    no[a] <- 2                          # smallest chamber is assigned chamber 2
    
    # Step 1: find the proloculus: this is the chamber closest to chamber 2
    d0 <- min(A[,a][A[,a] > 0])         # d0 = smallest non-zero distance of another chamber to chamber 1 
    no[which(A[, a]==d0)] <- 1          # Assigns chamber number 1: chamber with smallest non-zero distance to chamber 2
    
    # Steps 2 to r: find chamber i
    b <- c(which(no==1))                # b = row number of chamber 1. This row needs to be discarded from A in the first for-loop iteration, 
    for (i in 3:r)                      # which finds chamber 3.
    {
      Q <- A[-b,]                       # Q = matrix with chambers 1 to i-2 discarded (they've already been assigned)
      x <- which(no==i-1)               # x = row number of chamber i-1
      
      d <- min(Q[,x][Q[,x] > 0])        # d = smallest non-zero distance between chamber i-1 and all remaining chambers
      no[which(A[x,]==d)] <- i          # Assigns chamber i: chamber with shortest non-zero distance to chamber i-1
      
      b <- c(b, which(no==i-1))         # b = vector for row numbers of chambers 1 to i-1, which need to be discarded from A for iteration i+1
    }
    
    # Data output
    data <- data.frame(X, Y, Z, V, no)
    colnames(data) <- c('X centroid', 'Y centroid', 'Z centroid', 'Volume', 'Chamber number')
    data <- data[order(data[,5]),]
    return(data)
  }

  
}

