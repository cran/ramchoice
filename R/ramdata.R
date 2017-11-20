################################################################################
#' @title ramdata: Simulated Data
#'
#' @description The file is a standard choice data containing 8000 observations. There are six alternatives in the
#'   grand set.
#'
#' See \code{\link{rAtte}} for estimation and inference using the data. \code{\link{sumData}}
#'   is a low-level function that computes summary statistics, and \code{\link{genMat}} generates
#'   constraint matrices subject to given preferences.
#'
#' @format
#' \describe{
#'   \item{menu}{Numeric matrix of 0s and 1s, the menus/choice problems, where 1 indicates an element in the menu and 0 otherwise.}
#'   \item{choice}{Numeric matrix of 0s and 1s, the choices, where 1 indicates an element being chosen.}
#' }
#'
#' @docType data
#' @name ramdata
#' @aliases menu choice
NULL
