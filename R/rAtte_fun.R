################################################################################
#' @title ramchoice Package: Generate Summary Statistics
#'
#' @description \code{sumData} generates summary statistics. Given a collection of
#'   menus/choice problems and corresponding choices, \code{sumData} calculates the
#'   number of occurrences of each menu/choice problem, as well as the estimated choice
#'   rule.
#'
#' This function is embedded in \code{\link{rAtte}}, hence not needed to be called
#'   separately.
#'
#' @param menu Numeric matrix of 0s and 1s, the collection of menus.
#' @param choice Numeric matrix of 0s and 1s, the collection of choices.
#'
#' @return
#' \item{sumMenu}{Summary of menus/choice problems, with repetitions collapsed.}
#' \item{sumProb}{Estimated choice rules as sample averages for different menus/choice problems.}
#' \item{sumN}{Effective sample size for each menu.}
#' \item{sumMsize}{Size of each menu/choice problem.}
#' \item{sumProbVec}{Estimated choice rule as sample averages,
#'   collapsed into a column vector.}
#' \item{Sigma}{Estimated variance-covariance matrix for the choice rule,
#'   scaled by relative sample sizes.}
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
#' @examples
#' # load data
#' data(ramdata)
#'
#' # summary statistics
#' summaryStats <- sumData(menu, choice)
#' nrow(summaryStats$sumMenu)
#' min(summaryStats$sumN)
#'
#' summaryStats$sumMenu[1, ]
#' summaryStats$sumProb[1, ]
#' summaryStats$sumN[1]
#'
#' @export
sumData <- function(menu, choice) {

  ################################################################################
  # Generate Summary Statistics
  ################################################################################

  sumMenu  <- matrix(NA, ncol=ncol(menu), nrow=nrow(menu)) # collection of distinct menus
  sumProb  <- matrix(NA, ncol=ncol(menu), nrow=nrow(menu)) # collection of estimated choice probabilities
  sumN     <- matrix(NA, ncol=1, nrow=nrow(menu)) # collection of effective sample sizes
  sumMsize <- matrix(NA, ncol=1, nrow=nrow(menu)) # collection of menu sizes

  j <- 1
  while (nrow(menu) > 0) {
    # add new line to sumMENU, for a new menu
    sumMenu[j, ] <- menu[1, ]

    # add new line to sumMSIZE for the size of the new menu
    sumMsize[j, ] <- sum(menu[1, ])

    # enumerate and detect the same menus
    if (nrow(menu) >= 2) {
      i_menu <- apply(menu, MARGIN=1, FUN=function(x) all(x == menu[1, ]))
    } else {
      i_menu <- TRUE
    }

    # determine the effective sample size
    sumN[j, ] <- sum(i_menu)

    # calculate choice probabilities
    sumProb[j, ] <- apply(choice[i_menu, , drop=FALSE], MARGIN=2, FUN=mean)

    # delete rows used
    menu <- menu[!i_menu, , drop=FALSE]
    choice <- choice[!i_menu, , drop=FALSE]

    j <- j + 1
  }

  j <- j - 1
  sumMenu  <- sumMenu[1:j, ]
  sumProb  <- sumProb[1:j, ]
  sumN     <- sumN[1:j, ]
  sumMsize <- sumMsize[1:j, ]


  sumProbVec  <- matrix(0, nrow=sum(sumMsize), ncol=1)
  Sigma       <- matrix(0, nrow=sum(sumMsize), ncol=sum(sumMsize))

  j = 0;
  for (i in 1:nrow(sumProb)) {
    temp <- sumProb[i, sumMenu[i, ] == 1]
    sumProbVec[(j+1):(j+sumMsize[i])] <- temp
    Sigma[(j+1):(j+sumMsize[i]), (j+1):(j+sumMsize[i])] <-
      (diag(temp) - tcrossprod(temp, temp)) / sumN[i] * sum(sumN)

    j <- j + sumMsize[i]
  }

  return (list(sumMenu=sumMenu, sumProb=sumProb, sumN=sumN, sumMsize=sumMsize,
               sumProbVec=sumProbVec, Sigma=Sigma))

}

################################################################################
#' @title ramchoice Package: Generate Matrices of Constraints
#'
#' @description \code{genMat} generates matrices of constraints, imposed by the monotonicity
#'   assumption and preferences specified as the null hypotheses.
#'
#' This function is embedded in \code{\link{rAtte}}, hence not needed to be called
#'   separately.
#'
#' @param sumMenu Numeric matrix, summary of menus, returned by \code{\link{sumData}}.
#' @param sumMsize Numeric matrix, summary of menu sizes, returned by \code{\link{sumData}}.
#' @param pref_list Numeric matrix, each row corresponds to a preference. For example \code{c(2, 3, 1)} means
#'   2 is preferred to 3 and to 1. When set to \code{NULL}, the default \code{c(1, 2, 3, ...)}
#'   will be used.
#' @param limDataCorr Boolean, whether assumes limited data (default is \code{TRUE}). When set to
#'   \code{FALSE}, will assume all menus/choice problems are observed, hence no correction.
#'
#' @return
#' \item{R}{Matrices of constraints, stacked vertically.}
#' \item{ConstN}{The number of constraints for each preference, used to extract from \code{R}
#'   individual matrices of constraints.}
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
#' @examples
#' # load data
#' data(ramdata)
#'
#' # summary statistics
#' summaryStats <- sumData(menu, choice)
#'
#' # constraints
#' constraints <- genMat(summaryStats$sumMenu,
#'   summaryStats$sumMsize, NULL)
#' constraints$ConstN
#' constraints$R[1:10, 1:10]
#'
#' @export
genMat <- function(sumMenu, sumMsize, pref_list = NULL, limDataCorr=TRUE) {

  # initializing preference, for the default
  if (length(as.vector(pref_list)) == 0) {
    pref_list <- matrix(1:ncol(sumMenu), nrow=1)
  }

  ################################################################################
  # Default
  ################################################################################

  if (nrow(sumMenu) <= 1) {
    return(list(R=matrix(NA, nrow=0, ncol=sum(sumMsize)), constN=0))
  }

  ################################################################################
  # Construction
  ################################################################################

  K <- ncol(sumMenu) # dimension of the problem

  R <- matrix(0, nrow=0, ncol=sum(sumMsize))

  ConstN <- rep(NA, nrow(pref_list))

  temp <- rep(0, sum(sumMsize))

  for (pref_index in 1:nrow(pref_list)) {
    R_temp <- matrix(0, nrow=0, ncol=sum(sumMsize))
    pref <- pref_list[pref_index, ] # current preference

    ### goal: construct constraints of the form
    # mu(a|S) - mu(a|T) <= 0
    ###

    # index of menus to be enumerated in the first layer, S
    index_menu_1 <- (1:length(sumMsize))[sumMsize > min(sumMsize)]
    for (i_menu_1 in index_menu_1) {
      menu_1 <- sumMenu[i_menu_1, ] # current menu, S

      # index of menus to be enumerated in the second layer / subsets of current menu, T
      if (limDataCorr) { # with limited data correction
        index_menu_2 <- (1:length(sumMsize))[apply(sumMenu, MARGIN=1, FUN=function(x) {all((menu_1 - x)>=0) & (sum(menu_1) > sum(x))})]
      } else { # without limited data correction, i.e. assume full observability
        index_menu_2 <- (1:length(sumMsize))[apply(sumMenu, MARGIN=1, FUN=function(x) {all((menu_1 - x)>=0) & (sum(menu_1) - sum(x) == 1)})]
      }
      for (i_menu_2 in index_menu_2) {
        menu_2 <- sumMenu[i_menu_2, ] # a subset, T

        # decide elements a < S-T in S and T
        i_element <- K # index for the lower contour element a
        while (menu_1[pref[i_element]] == menu_2[pref[i_element]]) { # if the index never enters S - T, so that remains in lower contour set
          if (menu_1[pref[i_element]] == 1) { # if in S (so also in T)
            pos_1 <- sum(sumMsize[1:i_menu_1]) - sumMsize[i_menu_1] + sum(menu_1 & ((1:K) <= pref[i_element]))
            pos_2 <- sum(sumMsize[1:i_menu_2]) - sumMsize[i_menu_2] + sum(menu_2 & ((1:K) <= pref[i_element]))
            temp <- temp * 0
            temp[pos_1] <- 1; temp[pos_2] <- -1
            R_temp <- rbind(R_temp, temp)
          }
          i_element <- i_element - 1
        }
      }
    }



    # combine them
    R <- rbind(R, R_temp)
    ConstN[pref_index] <- nrow(R_temp)
  }

  rownames(R) <- NULL
  return(list(R=R, ConstN=ConstN))
}
