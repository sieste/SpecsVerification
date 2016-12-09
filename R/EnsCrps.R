#' Calculate the ensemble-adjusted Continuous Ranked Probability Score (CRPS)
#'
#' @param ens a N*R matrix representing N time instances of real-valued R-member ensemble forecasts
#' @param obs a numeric vector of length N with real-valued observations
#' @param R.new ensemble size for which the scores should be adjusted
#' @return numeric vector of length N with the ensemble-adjusted CRPS values
#' @examples
#' data(eurotempforecast)
#' mean(EnsCrps(ens, obs, R.new=Inf))
#' @seealso EnsBrier, EnsRps, DressCrps, GaussCrps, ScoreDiff, SkillScore
#' @references Ferro CAT, Richardson SR, Weigel AP (2008) On the effect of ensemble size on the discrete and continuous ranked probability scores. Meteorological Applications. http://dx.doi.org/10.1002/met.45
#' @export

EnsCrps = function(ens, obs, R.new=NA) {

  # calculate number of finite-valued ensemble members at each time
  R = rowSums(is.finite(ens))

  # calculate sum of absolute differences ("sad") between ensemble and
  # observations at each time
  sad.obs = rowSums(abs(ens - obs), na.rm=TRUE)

  # calculate sum of absolute differences between ensemble members at each
  # time; NOTE: if we use function `dist` as below, we only get one half of the
  # total sum of absolute differences between ensemble members; we only have to
  # calculate this if R.new > 1
  if (is.na(R.new) | R.new > 1) {
    sad.ens.half = apply(ens, 1, function(x) sum(dist(x), na.rm=TRUE))
  }

  
  # calculate the ensemble-adjusted crps 

  if (is.na(R.new)) { 

    # no adjustment, default case, we catch this first because we assume that
    # this is the most common usage of the function
    crps = sad.obs / R - sad.ens.half / R^2

  } else if (R.new > 1) { # user wants ensemble-adjustment 

    # calculate the ensemble adjusted crps; returns the fair crps if
    # R.new==Inf, returns the unadjusted crps as above for R.new==R; returns NA
    # whenever R==1 (which is desired)
    crps = sad.obs / R - sad.ens.half / (R*(R-1)) * (1 - 1/R.new)

  } else if (R.new == 1) {

    # probably a rare case; essentially the same as bove but we account for the
    # fact that (1-1/R.new)=0 so the entire sad.ens.half term vanishes; also:
    # for R.new == R == 1 the above would return NA, because one-member
    # ensembles can't be adjusted; but we have R == R.new, so no
    # ensemble-adjustment was required, so the unadjusted crps is returned,
    # which is sad.obs/R as well for R==1
    crps = sad.obs / R

  } else {

    # all remaining cases such as R.new <= 0
    crps = rep(NA_real_, length(obs))

  }

  # return the vector of crps
  return(crps)

}

