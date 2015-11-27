#' Calculate the ensemble-adjusted Ranked Probability Score (RPS) for categorical forecasts
#'
#' @param ens a N*R matrix representing N time instances of categorical ensemble forecasts; ens[t,r] indicates the category that the r-th ensemble member predicts for time t
#' @param obs a vector of length N, with obs[t] is the category observed at time t
#' @param R.new ensemble size for which the scores should be adjusted
#' @return numeric vector of length N with the ensemble-adjusted RPS values
#' @examples
#' # N=5 time instances, R=10 ensemble members
#' ens <- matrix(c(1,3,2, 1,2,3), nrow=3, ncol=2)
#' obs <- c(1,3,2)
#' EnsRps(ens, obs, R.new=Inf)
#' @seealso EnsBrier, EnsQs, EnsCrps
#' @references n/a
#' @export
EnsRps <- function(ens, obs, R.new=NA) {

  ## calculate histogram at each time, the tabulate function automatically removes NAs and NaNs
  K <- max(c(ens,obs), na.rm=TRUE)
  ens.hist <- t(apply(ens, 1, tabulate, nbins=K))
  obs.hist <- t(sapply(obs, tabulate, nbins=K))

  ## calculate time length, and ensemble sizes at each time
  N <- nrow(ens.hist)
  R <- rowSums(ens.hist)

  # accumulate ensemble members and observations
  ens.cum <- t(apply(ens.hist, 1, cumsum))
  obs.cum <- t(apply(obs.hist, 1, cumsum))

  ## calculate unadjusted quadratic score, make use of row-first indexing when
  ## dividing the matrix ens.cum by the vector R
  rps <- rowSums((ens.cum / R - obs.cum) ^ 2)


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

