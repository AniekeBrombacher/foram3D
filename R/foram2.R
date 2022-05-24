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
#' data(foram2)
#' foram.growth.3D(foram2$Chamber, foram2$CentroidX, foram2$CentroidY, foram2$CentroidZ)
"foram2"