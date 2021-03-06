% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotRankhist.R
\name{PlotRankhist}
\alias{PlotRankhist}
\title{Plotting function for rank histograms}
\usage{
PlotRankhist(rank.hist, mode = "raw")
}
\arguments{
\item{rank.hist}{A vector or rank counts.}

\item{mode}{Either "raw" (default) or "prob.paper". Whether to draw the raw rank histogram, or the rank histogram on probability paper. See Details.}
}
\description{
Plots a rank histogram in different modes.
}
\details{
The plotting modes currently implemented are:

raw (the default): A simple bar plot of the counts provided by the `rank.hist` argument.

prob.paper: The individual counts given by `rank.hist` are transformed to their cumulative probabilities under the binomial distribution with parameters `N` and `1/K`, where `N=sum(rank.hist)` and `K=length(rank.hist)`. This transformation makes possible an assessment of the observed rank counts under the hypothesis of equally likely ranks. The y-axis on the left indicates the cumulative probabilities. The intervals on the right of the plot indicate central 90, 95, and 99 percent _simultaneous_ confidence intervals. That is, if all ranks were equally likely on average, approximately 90 percent of all rank histograms would be _completely_ contained in the 90 percent interval and approximately 10 percent of all rank histograms would have _at least_ one bar that falls outside this interval.
}
\examples{
data(eurotempforecast)
rank.hist <- Rankhist(ens, obs)
PlotRankhist(rank.hist, mode="prob.paper")
}
\seealso{
Rankhist, TestRankhist
}
