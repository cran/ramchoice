% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/revealPrefModel.R
\name{revealPrefModel}
\alias{revealPrefModel}
\title{Model Falsification with Random Limited Attention}
\usage{
revealPrefModel(
  menu,
  prob,
  pref_list = NULL,
  RAM = TRUE,
  AOM = TRUE,
  limDataCorr = TRUE,
  attBinary = 1
)
}
\arguments{
\item{menu}{Numeric matrix of 0s and 1s, the collection of choice problems.}

\item{prob}{Numeric matrix, the collection of choice probabilities}

\item{pref_list}{Numeric matrix, each row corresponds to one preference. For example, \code{c(2, 3, 1)} means
2 is preferred to 3 and to 1. When set to \code{NULL}, the default, \code{c(1, 2, 3, ...)},
will be used.}

\item{RAM}{Boolean, whether the restrictions implied by the RAM of
\href{https://arxiv.org/abs/1712.03448}{Cattaneo et al. (2020)} should be incorporated, that is, their monotonic attention assumption (default is \code{TRUE}).}

\item{AOM}{Boolean, whether the restrictions implied by the AOM of
\href{https://arxiv.org/abs/2110.10650}{Cattaneo et al. (2024)} should be incorporated, that is, their attention overload assumption (default is \code{TRUE}).}

\item{limDataCorr}{Boolean, whether assuming limited data (default is \code{TRUE}). When set to
\code{FALSE}, will assume all choice problems are observed. This option only applies when \code{RAM} is set to \code{TRUE}.}

\item{attBinary}{Numeric, between 1/2 and 1 (default is \code{1}), whether additional restrictions (on the attention rule)
should be imposed for binary choice problems (i.e., attentive at binaries).}
}
\value{
\item{constraints}{Matrices of constraints, generated by \code{\link{genMat}}. \code{R}: a matrix containing all constraints. \code{ConstN}: number of constraints for each preference.}
\item{inequalities}{The moment inequalities. Positive numbers indicate that the RAM/AOM restrictions are rejected by the given choice probabilities. \code{R}: a vector containing all moment inequalities. \code{ConstN}: number of constraints for each preference.}
}
\description{
Given a collection of choice problems and corresponding
choice probabilities, \code{revealPrefModel} determines if they are compatible with
the Random Attention Model (RAM) of
\href{https://arxiv.org/abs/1712.03448}{Cattaneo, Ma, Masatlioglu, and Suleymanov (2020)}
and/or the Attention Overload Model (AOM) of
\href{https://arxiv.org/abs/2110.10650}{Cattaneo, Cheung, Ma, and Masatlioglu (2024)}.

See \code{\link{revealPref}} for revealed preference analysis with empirical choice data.
}
\examples{
# Logit attention with parameter 2
# True preference: 1 2 3 4 5 6
menu <- prob <- matrix(c(1, 1, 1, 1, 1, 1,
                         0, 1, 1, 1, 1, 1,
                         1, 0, 1, 1, 1, 1,
                         1, 1, 0, 1, 1, 1,
                         1, 1, 1, 0, 1, 1,
                         1, 1, 1, 1, 0, 1,
                         1, 1, 1, 1, 1, 0), ncol=6, byrow=TRUE)
for (i in 1:nrow(prob)) prob[i, menu[i, ]==1] <- logitAtte(sum(menu[i, ]), 2)$choiceProb

# List of preferences to be tested
pref_list <- matrix(c(1, 2, 3, 4, 5, 6,
                      2, 3, 4, 5, 6, 1), ncol=6, byrow=TRUE)
# RAM only
result1 <- revealPrefModel(menu = menu, prob = prob, pref_list = pref_list, RAM = TRUE, AOM = FALSE)
summary(result1)

# AOM only
result2 <- revealPrefModel(menu = menu, prob = prob, pref_list = pref_list, RAM = FALSE, AOM = TRUE)
summary(result2)

# Both RAM and AOM
result3 <- revealPrefModel(menu = menu, prob = prob, pref_list = pref_list, RAM = TRUE, AOM = TRUE)
summary(result3)

}
\references{
M. D. Cattaneo, X. Ma, Y. Masatlioglu, and E. Suleymanov (2020). \href{https://arxiv.org/abs/1712.03448}{A Random Attention Model}. \emph{Journal of Political Economy} 128(7): 2796-2836. \doi{10.1086/706861}

M. D. Cattaneo, P. Cheung, X. Ma, and Y. Masatlioglu (2024). \href{https://arxiv.org/abs/2110.10650}{Attention Overload}. Working paper.
}
\author{
Matias D. Cattaneo, Princeton University. \email{cattaneo@princeton.edu}.

Paul Cheung, University of Maryland. \email{hycheung@umd.edu}

Xinwei Ma (maintainer), University of California San Diego. \email{x1ma@ucsd.edu}

Yusufcan Masatlioglu, University of Maryland. \email{yusufcan@umd.edu}

Elchin Suleymanov, Purdue University. \email{esuleyma@purdue.edu}
}
