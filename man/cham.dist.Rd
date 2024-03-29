% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cham_dist.R
\name{cham.dist}
\alias{cham.dist}
\title{Distance between two chambers}
\usage{
cham.dist(P, Q)
}
\arguments{
\item{P}{Centroid coordinates c(x,y,z) of chamber i}

\item{Q}{Centroid coordinates c(x,y,z) of chamber j}
}
\description{
This function calculates the distance between two chambers in R^3 based on
their centroid coordinates
}
\examples{
cham.dist(c(1,1,1), c(1,2,3)) = 2.24
}
\keyword{centroid}
\keyword{chamber}
