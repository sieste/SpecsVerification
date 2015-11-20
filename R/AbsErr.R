#' Calculate the absolute error between forecast and observation
#'
#' @param fcst a N-vector representing N time instances of real-valued forecasts
#' @param obs a N-vector representing N time instances of real-valued observations
#' @return numeric N-vector of absolute errors
#' @examples
#' fcst <- rnorm(20)
#' obs <- rnorm(20)
#' mean(AbsErr(fcst, obs))
#' @seealso n/a
#' @references n/a 
#' @export

AbsErr <- function(fcst, obs) {

  # calculate absolute difference between forecasts and observations
  abs <- abs(fcst - obs)

  # return 
  return(abs.err)

}

