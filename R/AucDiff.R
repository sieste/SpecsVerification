#' Calculate difference between areas under the ROC curve (AUC) between a forecast and a reference forecast for the same observation, and estimate the variance of the AUC difference
#'
#' @param fcst vector of forecasts
#' @param fcst.ref vector of reference forecasts
#' @param obs vector of binary observations (0 for non-occurrence, 1 for occurrence of the event)
#' @param handle.na how should missing values in forecasts and observations be handled; possible values are 'na.fail' and 'only.complete.triplets'; default: 'na.fail'
#' @return vector with AUC difference, and estimated standard deviation
#' @examples
#' fcst <- runif(20)
#' obs <- rbinom(n=20, size=1, prob=fcst)
#' fcst.ref <- rep(0.5, 20)
#' AucDiff(fcst, fcst.ref, obs)
#' @seealso Auc
#' @references DeLong et al (1988): Comparing the Areas under Two or More Correlated Receiver Operating Characteristic Curves: A Nonparametric Approach. Biometrics. http://dx.doi.org/10.2307/2531595
#' @export
AucDiff <- function(fcst, fcst.ref, obs, handle.na="na.fail") {

  ## sanity checks
  stopifnot(length(fcst) == length(obs))
  stopifnot(length(fcst.ref) == length(obs))


  ## handle NA's
  if (handle.na == "na.fail") {
    if (any(is.na(c(fcst, fcst.ref, obs)))) {
      stop("missing values")
    }
  } else if (handle.na == "only.complete.triplets") {
    nna <- !is.na(fcst) & !is.na(fcst.ref) & !is.na(obs)
    if (all(nna == FALSE)) {
      stop("there are no complete sets of forecasts and observations")
    }
    fcst <- fcst[nna]
    fcst.ref <- fcst.ref[nna]
    obs <- obs[nna]
  } else {
    stop("unknown 'handle.na' argument")
  }


  ## after removing any NA's, check if observations are either 0 or 1
  stopifnot(all(obs %in% c(0,1)))
  if (sum(obs) == length(obs) | sum(obs) == 0) {
    stop("need at least one event and one non-event")
  }


  ## calculate sets of forecasts with events (X) and forecasts with non-events (Y)
  X <- cbind(fcst[obs == 1], fcst.ref[obs == 1])
  Y <- cbind(fcst[obs == 0], fcst.ref[obs == 0])
  m <- nrow(X)
  n <- nrow(Y)  


  ## Delong's Psi function as matrix (Psi.mat[i, j] = Psi(X[i], Y[j]))
  psi.fun <- function(x, y) {
    return(1 * (x > y) + 0.5 * (x == y))
  }
  Psi <- outer(X[, 1], Y[, 1], psi.fun)
  Psi.ref <- outer(X[, 2], Y[, 2], psi.fun)

  
  ## AUC estimates
  theta <- mean(Psi)
  theta.ref <- mean(Psi.ref)


  ## AUC difference
  auc.diff <- theta - theta.ref


  ## Delong's S matrix and variance estimate
  V <- rowMeans(Psi)
  V.ref <- rowMeans(Psi.ref)
  W <- colMeans(Psi)
  W.ref <- colMeans(Psi.ref)

  v.11 <- sum((V - theta)^2) / (m - 1)
  v.22 <- sum((V.ref - theta.ref)^2) / (m - 1)
  v.12 <- sum((V - theta) * (V.ref - theta.ref)) / (m - 1)

  w.11 <- sum((W - theta)^2) / (n - 1)
  w.22 <- sum((W.ref - theta.ref)^2) / (n - 1)
  w.12 <- sum((W - theta) * (W.ref - theta.ref)) / (n - 1)

  var.aucdiff <- (v.11 + v.22 - 2 * v.12) / m +
                 (w.11 + w.22 - 2 * w.12) / n

  sd.aucdiff <- sqrt(var.aucdiff)


  ## return
  ret <- c(auc.diff = auc.diff, auc.diff.sd = sd.aucdiff)
  return(ret)
}

