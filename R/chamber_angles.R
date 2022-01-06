#' Angle between chambers
#'
#' Calculates the angle each chamber makes with its previous two chambers.  
#' @param X x coordinates of all chamber centroids
#' @param Y y coordinates of all chamber centroids
#' @param Z z coordinates of all chamber centroids
#' @keywords chamber
#' @keywords centroid
#' @keywords angle
#' @usage chamber.angles(X, Y, Z)
#' @export

chamber.angles <- function(X, Y, Z)          
{
  ang <- rep(NA, length(X))
  
  for (i in 3:length(X))
       {
         P <- c(X[i-2], Y[i-2], Z[i-2])     # Coordinates of centroid chamber i-2
         Q <- c(X[i-1], Y[i-1], Z[i-1])     # Coordinates of centroid chamber i-1
         R <- c(X[i], Y[i], Z[i])           # Coordinates of centroid chamber i
         
         v <- c(P[1] - Q[1], P[2] - Q[2], P[3] - Q[3])   # Vector going from chamber i-1 to i-2
         w <- c(R[1] - Q[1], R[2] - Q[2], R[3] - Q[3])   # Vector going from chamber i-1 to i
         
         # angle made by chamber i to chambers i-1 and i-2
         ang[i] <- as.numeric((acos((v %*% w)/(cham.dist(P, Q)*cham.dist(Q, R)))/pi)*180)
  }
  return(ang)
}