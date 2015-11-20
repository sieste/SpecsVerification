#' Calculate the ensemble-adjusted Quadratic Score (QS) for categorical forecasts
#'
#' @param ens a N*K matrix representing N time instances of categorical ensemble forecasts; ens[t,k] indicates the number of ensemble members that predict category k at time t
#' @param obs a N*K matrix, with obs[i,k] either 0 or 1, and all rowSums equal to one; y[t,k] = 1 if category k verifies at time t, otherwise y[t,k] = 0
#' @param R.new ensemble size for which the scores should be adjusted
#' @return numeric vector of length N with the ensemble-adjusted quadratic score values
#' @examples
#' # N=5 time instances, R=10 ensemble members, K=3 classes
#' ens <- t(rmultinom(n=5, size=10, prob=c(.3, .1, .6)))  
#' obs <- t(rmultinom(n=5, size=1,  prob=c(.3, .1, .6)))
#' EnsQs(ens, obs, R.new=Inf)
#' @seealso EnsBrier, EnsRps, EnsCrps
#' @references n/a
#' @export

EnsQs <- function(ens, obs, R.new=NA) {


  # calculate time length, and ensemble sizes at each time
  N <- nrow(ens)
  R <- rowSums(ens)


  # calculate unadjusted quadratic score
  q.score <- rowMeans((ens / R - obs) ^ 2)


  # check if ensemble-adjustment is requested by user
  R.new <- R.new[1]
  if (!is.na(R.new)) {
    if (R.new < 2) {
      q.score <- q.score + NA
    } else {
      adjustment <- rowMeans(-1 * (1/R - 1/R.new) * ens * (R - ens) / R / (R-1))
      # catch one-member ensembles, which cannot be adjusted
      adjustment[!is.finite(adjustment)] <- NA
      q.score <- q.score + adjustment
    }
  }

  return(q.score)
}


