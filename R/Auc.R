#' Calculate area under the ROC curve (AUC) for a forecast and its verifying binary observation, and estimate the variance of the AUC 
#'
#' @param fcst vector of forecasts
#' @param obs vector of binary observations (0 for non-occurrence, 1 for occurrence of the event)
#' @param handle.na how should missing values in forecasts and observations be handled; possible values are 'na.fail' and 'only.complete.pairs'; default: 'na.fail'
#' @return vector containing AUC and its estimated standard deviation
#' @examples
#' fcst <- runif(20)
#' obs <- rbinom(n=20, size=1, prob=fcst)
#' Auc(fcst, obs)
#' @seealso n/a
#' @references DeLong 1988
#' @export
Auc <- function(fcst, obs, handle.na="na.fail") {

  ## sanity checks
  stopifnot(length(fcst) == length(obs))


  ## handle NA's
  if (handle.na == "na.fail") {
    if (any(is.na(c(fcst, obs)))) {
      stop("missing values")
    }
  } else if (handle.na == "only.complete.pairs") {
    nna <- !is.na(fcst) & !is.na(obs)
    if (all(nna == FALSE)) {
      stop("there are no complete sets of forecasts and observations")
    }
    fcst <- fcst[nna]
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
  X <- fcst[obs == 1]
  Y <- fcst[obs == 0]
  m <- length(X)
  n <- length(Y)  


  ## Delong's Psi function as matrix (Psi.mat[i, j] = Psi(X[i], Y[j]))
  psi.fun <- function(x, y) {
    return(1 * (x > y) + 0.5 * (x == y))
  }
  Psi <- outer(X, Y, psi.fun)

  
  ## AUC estimates
  theta <- mean(Psi)


  ## Delong's S matrix and variance estimate
  V <- rowMeans(Psi)
  W <- colMeans(Psi)

  v <- sum((V - theta)^2) / (m - 1)
  w <- sum((W - theta)^2) / (n - 1)

  var.auc <- v / m + w / n

  sd.auc <- sqrt(var.auc)


  ## return
  ret <- c(auc = theta, auc.sd = sd.auc)
  return(ret)
}

