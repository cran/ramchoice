################################################################################
#' @title ramchoice: Estimation and Inference in Random Attention Models
#'
#' @description Information about socio-economic agent's preference (consumer, firm, organization, voter, etc.)
#'   is important not only for understanding the decision making process, but also for conducting
#'   welfare analysis and providing robust policy recommendations. On the other hand, it is widely documented
#'   in psychology, economics and other disciplines that decision makers may not pay full
#'   attention to all available alternatives, rendering standard revealed preference theory
#'   invalid.
#'
#' This package implements the estimation and inference procedure documented in
#'   \href{http://arxiv.org/abs/1712.03448}{Cattaneo, Ma, Masatlioglu and Suleymanov (2019)},
#'   which utilizes standard choice data to partially identify decision
#'   maker's preference. For statistical inference, several simulation-based critical values are provided.
#'
#'   The following functions are provided: \code{\link{rAtte}} (the main function),
#'   \code{\link{sumData}}, \code{\link{genMat}}. A simulated dataset
#'   \code{\link{ramdata}} is also included for illustration purpose.
#'
#' @references
#' M. D. Cattaneo, X. Ma, Y. Masatlioglu and E. Suleymanov (2019). \href{http://arxiv.org/abs/1712.03448}{A Random Attention Model}. Working Paper, University of Michigan.
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
