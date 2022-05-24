#' Example planktonic foraminifera centroid data 
#'
#' A Menardella specimen collected from ODP Site 925. 
#' Data includes chamber number and xyz-coordinates of each
#' chamber's centroid.
#'
#' @docType data
#'
#' @usage data(foram1)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @examples
#' data(foram1)
#' foram.growth3D(foram1$n, foram1$centroidX, foram1$centroidY, foram1$centroidZ)
"foram1"