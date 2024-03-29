% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/order_chambers.R
\name{order.chambers}
\alias{order.chambers}
\title{Put chambers in the correct order of growth}
\usage{
order.chambers(X, Y, Z, V, proloculus=TRUE)
}
\arguments{
\item{X}{Vector with X-coordinates of all chambers}

\item{Y}{Vector with Y-coordinates of all chambers}

\item{Z}{Vector with Z-coordinates of all chambers}

\item{V}{Vector with volumes of each chamber}

\item{proloculus}{Indicates whether the proloculus is present and determines which algorithm is applied. Defaults to TRUE}
}
\description{
This function puts the chambers in the order of growth. There are two options: either the full growth sequence
from proloculus to final chamber is recorded, or the first few chambers are missing due to for example
dissolution.
}
\details{
Full growth sequence: the smallest chamber is the deuteroconch. The proloculus is the closest chamber to the deuteroconch.
The third chamber is the next closest chamber to chamber 2, the fourth chamber is the next closest to chamber 3, etc.

First few chambers missing: the smallest chamber is the first in the growth sequence. The next chamber is the closest to
the first chamber in the sequence, the third in the sequence is the next closest to the second in the sequence, etc.
}
\keyword{chamber}
\keyword{whorl}
