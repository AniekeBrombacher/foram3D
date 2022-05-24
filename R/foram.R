#' Example planktonic foraminifera centroid data 
#'
#' A Menardella specimen collected from ODP Site 925. 
#' Data includes chamber number and xyz-coordinates of each
#' chamber's centroid.
#'
#' @docType data
#'
#' @usage data(foram2)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @examples
#' data(foram)
#' foram.growth.3D(foram$Chamber, foram$CentroidX, foram$CentroidY, foram$CentroidZ)
"foram"