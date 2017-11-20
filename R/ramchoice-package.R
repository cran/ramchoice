################################################################################
#' @title ramchoice: Estimation and Inference in Random Attention Models
#'
#' @description Information on socio-economic agents' preference (consumers, firms, organizations, voters, etc.)
#'   is important not only to understand the decision making process, but also for conducting
#'   welfare analysis and provide robust policy recommendations. On the other hand, it is widely documented
#'   in psychology, economics and other disciplines that decision makers do not pay full
#'   attention to all available choices, rendering standard revealed preference theory
#'   invalid.
#'
#' This package implements the estimation and inference procedures documented in
#'   \href{http://www-personal.umich.edu/~cattaneo/papers/Cattaneo-Ma-Masatlioglu-Suleymanov_2017_RAM.pdf}{Cattaneo, Ma, Masatlioglu and Suleymanov (2017)},
#'   which utilizes standard choice data to partially identify and estimate decision
#'   maker's preference. For statistical inference, different simulation-based critical values are provided.
#'
#'   The following functions are provided: \code{\link{rAtte}} (the main function),
#'   \code{\link{sumData}}, \code{\link{genMat}}. A simulated dataset
#'   \code{\link{ramdata}} is also included for illustration purpose.
#'
#' @references
#' M. D. Cattaneo, X. Ma, Y. Masatlioglu and E. Suleymanov (2017). \href{http://www-personal.umich.edu/~cattaneo/papers/Cattaneo-Ma-Masatlioglu-Suleymanov_2017_RAM.pdf}{A Random Attention Model}. Working Paper, University of Michigan.
#'
#' @author
#' Matias D. Cattaneo, University of Michigan. \email{cattaneo@umich.edu}.
#'
#' Xinwei Ma (maintainer), University of Michigan. \email{xinweima@umich.edu}
#'
#' Yusufcan Masatlioglu, University of Maryland. \email{yusufcan@umd.edu}
#'
#' Elchin Suleymanov, University of Michigan. \email{elchin@umich.edu}
#'
#' @importFrom stats quantile
#' @importFrom MASS mvrnorm
#'
#' @aliases ramchoice-package
"_PACKAGE"