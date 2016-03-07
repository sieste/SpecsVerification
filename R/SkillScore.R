#' Calculate a skill score and assess uncertainty
#'
#' @param scores vector of verification scores
#' @param scores.ref vector of verification scores of the reference forecast, must be of the same length as `scores`
#' @param N.eff user-defined effective sample size to be used to estimate the sampling uncertainty; if NA, the length of `scores` is used; default: NA
#' @param score.perf a numeric constant, indicating the value that the score would assign to the perfect forecast
#' @param handle.na how should missing values in scores vectors be handled; possible values are 'na.fail' and 'use.pairwise.complete'; default: 'na.fail'
#' @return vector with skill score and its estimated standard deviation
#' @examples
#' fcst <- rnorm(20)
#' fcst.ref <- rnorm(20)
#' obs <- rnorm(20)
#' SkillScore(SqErr(fcst, obs), SqErr(fcst.ref, obs))
#' @seealso ScoreDiff
#' @export
SkillScore <- function(scores, scores.ref, N.eff=NA, score.perf=0, handle.na="na.fail") {

  ## sanity checks
  N.eff <- N.eff[1L]
  score.perf <- score.perf[1L]
  stopifnot(N.eff > 0 | is.na(N.eff))
  stopifnot(length(scores) == length(scores.ref))
  stopifnot(is.finite(score.perf))


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

  # calculate mean scores, shift by score.perf
  score <- mean(scores) - score.perf
  score.ref <- mean(scores.ref) - score.perf

  # calculate skill score
  skillscore <- 1 - score / score.ref

  # calculate auxiliary quantities
  v.score <- var(scores)
  v.score.ref <- var(scores.ref)
  cov.score   <- cov(scores, scores.ref)

  # calculate skill score standard deviation by error propagation 
  sqrt.na <- function(z) {
    z[z<0] <- NA
    return(sqrt(z))
  }
  skillscore.sigma <- 
         1 / sqrt(N.eff) * sqrt.na( v.score / score.ref^2 + 
         v.score.ref * score^2 / score.ref^4 - 
         2 * cov.score * score / score.ref^3)

  # set skillscore.sigma to NA if not finite
  if (!is.finite(skillscore.sigma)) {
    skillscore.sigma <- NA 
  }

  #return
  c(skillscore=skillscore, skillscore.sd=skillscore.sigma)

}
