\name{ReliabilityDiagram}
\alias{ReliabilityDiagram}
\title{Reliability diagram for probability forecasts}
\description{
  Calculate and plot the reliability diagram of a collection of probability forecasts and their corresponding binary observations.
}
\usage{
ReliabilityDiagram(probs, obs, bins, nboot, plot, plot.refin, cons.probs, attributes)
}
\arguments{
  \item{probs}{vector of length N. probs[k] has the predicted probability for the event obs[k]}
  \item{obs}{vector of length N. obs[k] = 1 if the event happened at instance k, obs[k] = 0 otherwise}
  \item{bins}{scalar or vector. Number of equidistant bins to discretize the forecast probabilities, or vector of user-defined breakpoints of the bins. Default: 10}
  \item{nboot}{Number of bootstrap resamples for estimating consistency bars. If nboot==0, no resampling is done and NAs are returned as consistency bars. Default: 500}
  \item{plot}{logical. Whether to plot the reliability diagram. Default: FALSE}
  \item{plot.refin}{logical. Whether to plot the small refinement histogram in lower right corner. Default: TRUE}
  \item{cons.probs}{vector of length 2. Lower and upper probability for the consistency bars. Default: c(0.025, 0.975)}
  \item{attributes}{logical. Whether to include the no-resolution and no-skill line. Default: FALSE}
}
\value{
A data frame of nbins rows with the following columns:

"p.avgs": In-bin averages of the forecast probabilities.\cr
"cond.probs": Observed frequency of event when p is in bin i.\cr
"cbar.lo": Lower limit consistency of consistency bar i.\cr
"cbar.hi": Upper limit consistency of consistency bar i.\cr
}
\examples{
  # Example:
  N <- 1000
  p <- rbeta(n=N, shape1=1, shape2=3)
  y <- rbinom(n=N, size=1, prob=p)
  rd <- ReliabilityDiagram(p, y, plot=TRUE)
  print(rd)
}
\references{
Jolliffe IT, Stephenson DB, eds. (2012): Forecast verification: A practitioner's guide in atmospheric science. John Wiley & Sons, 2012. ISBN: 978-0-470-66071-3

Broecker J, Smith LA (2007):Increasing the Reliability of Reliability Diagrams. Wea. Forecasting, 22, 651--661.  DOI: 10.1175/WAF993.1. 
}

