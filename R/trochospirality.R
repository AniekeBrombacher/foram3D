#' Trochospirality
#'
#' This function calculates the angle a chamber makes with the plane defined by the centroids of the previous three 
#' chambers. 
#' @param X Vector with X-coordinates of all chambers
#' @param Y Vector with Y-coordinates of all chambers
#' @param Z Vector with Z-coordinates of all chambers
#' @keywords chamber
#' @keywords centroid
#' @keywords trochospirality
#' @usage trochospirality(X,Y,Z)
#' @export

trochospirality <- function(X,Y,Z)  
{
  trocho <- rep(NA, length(X))
  
  for(i in 4:length(X))
  {
    P <- c(X[i], Y[i], Z[i])         # Chamber i for which we want to calculate trochospirality
    Q <- c(X[i-1], Y[i-1], Z[i-1])   # Chamber i-1
    R <- c(X[i-2], Y[i-2], Z[i-2])   # Chamber i-2
    S <- c(X[i-3], Y[i-3], Z[i-3])   # Chamber i-3
    
    w <- cross.prod(S,R,Q)  # calculate vector normal to plane defined by the centroids of chambers i-3, i-2 and i-1
    
    # D-component in equation Plane=Ax+By+Cz+D=0. 
    # The w's are the components of the normal vector w. 
    # Q is a point on the plane.
    D <- -w[1]*Q[1]-w[2]*Q[2]-w[3]*Q[3]  
    
    # Orthogonal distance of point P (chamber i) to plane determined by chambers i-1, i-2 and i-3
    d <- abs(w[1]*P[1]+w[2]*P[2]+w[3]*P[3]+D)/sqrt(w[1]^2+w[2]^2+w[3]^2)  
    # sin(alpha) = opposite / hypothenuse
    trocho[i] <- (asin(d/cham.dist(P, Q))/pi)*180  
  }
  
  return(trocho)
}