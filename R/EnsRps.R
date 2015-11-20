#' Calculate the ensemble-adjusted Ranked Probability Score (RPS) for categorical forecasts
#'
#' @param ens a N*K matrix representing N time instances of categorical ensemble forecasts; ens[t,k] indicates the number of ensemble members that predict category k at time t
#' @param obs a N*K matrix, with obs[i,k] either 0 or 1, and all rowSums equal to one; y[t,k] = 1 if category k verifies at time t, otherwise y[t,k] = 0
#' @param R.new ensemble size for which the scores should be adjusted
#' @return numeric vector of length N with the ensemble-adjusted RPS values
#' @examples
#' # N=5 time instances, R=10 ensemble members, K=3 classes
#' ens <- t(rmultinom(n=5, size=10, prob=c(.3, .1, .6)))  
#' obs <- t(rmultinom(n=5, size=1,  prob=c(.3, .1, .6)))
#' EnsRps(ens, obs, R.new=Inf)
#' @seealso EnsBrier, EnsQs, EnsCrps
#' @references n/a
#' @export
EnsRps <- function(ens, obs, R.new) {

  # calculate time length, and ensemble sizes at each time
  N <- nrow(ens)
  R <- rowSums(ens)

  # accumulate ensemble members and observations
  ens.cum <- t(apply(ens, 1, cumsum))
  obs.cum <- t(apply(obs, 1, cumsum))

  # calculate unadjusted quadratic score
  rps <- rowMeans((ens.cum / R - obs.cum) ^ 2)


  # check if ensemble-adjustment is requested by user
  R.new <- R.new[1]
  if (!is.na(R.new)) {
    if (R.new < 2) {
      rps <- rps + NA
    } else {
      adjustment <- rowMeans(-1 * (1/R - 1/R.new) * ens.cum * (R - ens.cum) / R / (R-1))
      # catch one-member ensembles, which cannot be adjusted
      adjustment[!is.finite(adjustment)] <- NA
      rps <- rps + adjustment
    }
  }

  return(rps)
  
}

