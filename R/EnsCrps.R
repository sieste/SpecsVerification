#' Calculate the ensemble-adjusted Continuous Ranked Probability Score (CRPS)
#'
#' @param ens a N*R representing N time instances of real-valued R-member ensemble forecasts
#' @param obs a numeric vector of length N with real-valued observations
#' @param R.new ensemble size for which the scores should be adjusted
#' @return numeric vector of length N with the ensemble-adjusted CRPS values
#' @examples
#' data(eurotempforecast)
#' mean(EnsCrps(ens, obs, R.new=Inf))
#' @seealso EnsBrier, EnsRps, DressCrps, GaussCrps, ScoreDiff, SkillScore
#' @references Ferro CAT, Richardson SR, Weigel AP (2008) On the effect of ensemble size on the discrete and continuous ranked probability scores. Meteorological Applications. http://dx.doi.org/10.1002/met.45
#' @export

EnsCrps <- function(ens, obs, R.new=NA) {

  # calculate sum of absolute differences (sad) between ensemble and
  # observations at each time
  sad.obs <- rowSums(abs(ens - obs), na.rm=TRUE)

  # calculate sum of absolute differences between ensemble members at each
  # time; NOTE: if we use function `dist` as below, we only get one half of the
  # total sum of absolute differences between ensemble members
  sad.ens.half <- apply(ens, 1, function(x) sum(dist(x), na.rm=TRUE))

  # calculate number of finite-valued ensemble members at each time
  R <- rowSums(is.finite(ens))

  # for R.new == NA (the default), no correction for ensemble size is performed
  if (length(R.new) == 1 & is.na(R.new[1])) {
    R.new <- R
  }

  # calculate ensemble-adjusted crps for each time
  if (is.na(R.new) || all(R == R.new)) {
    crps <- sad.obs / R - sad.ens.half / R^2
  } else {
    crps <- sad.obs / R - 1/(R*(R-1)) * (1 - 1/R.new) * sad.ens.half
  }

  # return the vector of crps
  return(crps)

}

