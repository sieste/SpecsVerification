SkillScore <- function(scores, scores.ref, N.eff=NA, confidence.bounds=c(0.025, 0.975), score.perf=0) {

  ## sanity checks

  # demand score.perf to be length 1 and none of NA|NaN|Inf
  stopifnot(length(score.perf) == 1, is.finite(score.perf))

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

  # calculate mean scores
  score <- mean(scores)
  score.ref <- mean(scores.ref)

  # calculate skill score
  skillscore <- (score.ref - score) / (score.ref - score.perf)

  # calculate auxiliary quantities
  v.score <- var(scores)
  v.score.ref <- var(scores.ref)
  cov.score   <- cov(scores, scores.ref)

  # calculate error propagation standard deviation
  sqrt.na <- function(z) {
    z[z<0] <- NA
    return(sqrt(z))
  }
  skillscore.sigma <- 
         1 / sqrt(N) * sqrt.na( v.score / score.ref^2 + 
         v.score.ref * score^2 / score.ref^4 - 
         2 * cov.score * score / score.ref^3)

  # set skillscore.sigma to NA if not finite
  if (!is.finite(skill.score.sigma)) {
    skillscore.sigma <- NA 
  }

  #return
  c(skillscore=skillscore, skillscore.sigma=skillscore.sigma)

}
