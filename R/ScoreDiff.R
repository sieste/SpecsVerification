ScoreDiff <- function(scores, scores.ref, N.eff=NA, confidence.bounds=c(0.025, 0.975)) {

  ## sanity checks

  # demand N.eff to be positive and have length 1 
  stopifnot(length(N.eff) == 1, N.eff > 0)

  # demand confidence.bounds to be of length 2 with both values between 0 and 1
  stopifnot(length(confidence.bounds) == 2)
  confidence.bounds <- sort(confidence.bounds)
  stopifnot(all(confidence.bounds > 0), all(confidence.bounds < 1))

  # demand scores vectors to be of equal length
  stopifnot(length(scores) == length(scores.ref))


  ## handle missing values

  # only consider pairwise complete scores
  nna <- is.finite(scores + scores.ref)
  scores <- scores[nna]
  scores.ref <- scores.ref[nna]


  ## sample size

  # calculate N as length of scores vector after removing missing values, or
  # use user-defined effective sample size 
  if (is.na(N.eff)) {
    N <- length(scores)
  } else {
    N <- N.eff
  }


  ## calculations

  # calculate loss-differentials
  d <- scores.ref - scores

  # calculate mean score difference between reference forecast and forecast
  d.bar <- mean(d)

  # calculate variance of loss-differentials
  var.d <- var(d)

  # calculate p-value using asymptotic test by Diebold-Mariano (1995), return
  # NA is N or variance are non-positive
  if (N > 0 & var.d > 0) {
    p.value <- pnorm(sqrt(N / var.d) * d.bar)
  } else {
    p.value <- NA
  }


  # calculate confidence interval of the mean
  ci <- qnorm(p=confidence.bounds, mean=d.bar, sd=sqrt(var.d / N))
  
  # return vector including the score difference, p.value, and confidence interval
  ret <- c(score.diff=d.bar, p.value=p.value, ci.L=cip[1], ci.U=ci[2])
  return(ret)

}

