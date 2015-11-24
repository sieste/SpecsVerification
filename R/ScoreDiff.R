#' Calculate average score difference and assess uncertainty
#'
#' @param scores vector of verification scores
#' @param scores.ref vector of verification scores of the reference forecast, must be of the same length as `scores`
#' @param N.eff user-defined effective sample size to be used in hypothesis test and for confidence bounds; if NA, the length of `scores` is used; default: NA
#' @param p.ci vector of limits of the confidence interval; default: c(0.025, 0.975)
#' @param handle.na how should missing values in scores vectors be handled; possible values are 'na.fail' and 'use.pairwise.complete'; default: 'na.fail'
#' @return vector with mean score difference, estimated standard error of the mean, p-value of the diebold-mariano test, and the user-specified confidence interval
#' @examples
#' fcst <- rnorm(20)
#' fcst.ref <- rnorm(20)
#' obs <- rnorm(20)
#' ScoreDiff(SqErr(fcst, obs), SqErr(fcst.ref, obs))
#' @seealso n/a
#' @references n/a
#' @export
ScoreDiff <- function(scores, scores.ref, N.eff=NA, p.ci=c(0.025, 0.975), handle.na="na.fail") {


  ## sanity checks
  N.eff <- N.eff[1L]
  stopifnot(N.eff > 0 | is.na(N.eff))
  stopifnot(length(scores) == length(scores.ref))
  stopifnot(all(p.ci) > 0, all(p.ci) < 1)


  ## handle NA's
  if (handle.na == "na.fail") {
    if (any(is.na(c(scores, scores.ref)))) {
      stop("missing values")
    }
  } else if (handle.na == "use.pairwise.complete") {
    nna <- !is.na(scores) & !is.na(scores.ref)
    if (all(nna == FALSE)) {
      stop("there are no complete pairs of scores")
    }
    scores <- scores[nna]
    scores.ref <- scores.ref[nna]
  } else {
    stop("unknown 'handle.na' argument")
  }


  ## _after_ removing any missing values, deal with user-defined effective sample size
  if (is.na(N.eff)) {
    N.eff <- length(scores)
  }


  ## calculations

  # calculate loss-differentials
  d <- scores.ref - scores

  # calculate mean score difference between reference forecast and forecast
  d.bar <- mean(d)

  # calculate variance of loss-differentials
  var.d <- var(d)
  
  # calculate standard error of the mean loss-differential, use effective sample size
  d.bar.sd <- sqrt(var.d / N.eff)

  # calculate p-value using asymptotic test by Diebold-Mariano (1995), return
  # NA if variance is non-positive
  if (d.bar.sd > 0) {
    p.value <- pnorm(d.bar / d.bar.sd)
  } else {
    p.value <- NA
  }

  # calculate confidence interval of the mean
  ci <- qnorm(p=p.ci, mean=d.bar, sd=d.bar.sd)
  

  ## return vector including the score difference, error of the mean, p.value,
  # and confidence interval
  ret <- c(score.diff=d.bar, score.diff.sd=d.bar.sd, p.value, ci.L=ci[1], ci.U=ci[2])
  return(ret)

}

