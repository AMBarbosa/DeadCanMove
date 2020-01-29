\name{hotspots.comparison}
\alias{hotspots.comparison}
\title{
Hotspots comparison
}
\description{
This is a wrapper for most of the functions in this package (one function
to rule them all). You'll probably only need to use this one, which in turn
calls each of the other functions and does all the calculations in one step.
}
\usage{
hotspots.comparison(dataset, sampl.columns, sampl.intervals, 
region.column, group.column, include.all.together = TRUE, 
confidence = 0.95, min.total.events = 80, min.hotspot.threshold = 2, 
comp.method = "Phi", plot = TRUE, sep.plots = FALSE, 
omit.baseline.interval = TRUE, ...)
}
\arguments{
  \item{dataset}{name of the matrix or dataframe to analyze}
  \item{sampl.columns}{index numbers of the columns containing the (daily)
  sampling data, e.g. 4:180}
  \item{sampl.intervals}{intervals at which to extract sampling data, e.g. 1:30;
  currently must be consecutive and start with 1}
  \item{region.column}{name or index number of the column containing the regions
  (road segments, sites) to classify as hotspots or non-hotspots}
  \item{group.column}{name or index number of the column containing the taxa or
  groups to analyse separately, e.g. 3 or "Family"; if NULL, all records will be
  used together}
  \item{include.all.together}{logical, whether to get subsampling matrices also
  for the complete data (including all groups combined)}
  \item{confidence}{confidence threshold to consider hotspots (see Malo et al.
  2004); defaults to 0.95}
  \item{min.total.events}{minimum total number of events (e.g. deaths) to
  calculate hotspots for a group}
  \item{min.hotspot.threshold}{minimum number of events for a region to be
  considered a hotspot}
  \item{comp.method}{the method with which to compare the hotspots obtained with
  increasing sampl.intervals with those of the baseline scenario; type
  \code{binary.comp.methods()} for available options}
  \item{plot}{logical, whether to plot the correlations between subsamples and
  baseline for each group (may cause function to fail if \code{sep.plots = FALSE}
  and figure margins are too large for the number of resulting plots)}
  \item{sep.plots}{logical, whether to present the plots in separate windows
  rather than all in the same window}
  \item{omit.baseline.interval}{logical, whether to omit the first column
  (correlation of baseline hotspots with themselves) from calculations and
  results}
  \item{\dots}{additional arguments to pass to the \code{\link{plot}} function}
}
\value{
A list with 9 elements:
\item{hotspots.list}{}
\item{N.events}{}
\item{HS.threshold}{}
\item{N.hotspots}{}
\item{events.in.HS}{}
\item{event.corrs}{}
\item{event.loss}{}
\item{event.gain}{}
\item{event.balance}{}
}
\references{
Malo, J.E., Suarez, F., Diez, A. (2004) Can we mitigate animal-vehicle accidents
using predictive models? J. Appl. Ecol. 41, 701-710
(doi: 10.1111/j.0021-8901.2004.00929.x)
}
\author{
A. Marcia Barbosa, J. Tiago Marques, Sara M. Santos
}
\seealso{
\code{\link{hotspots}}
}
\examples{
data(roadkills)

hc <- hotspots.comparison(dataset = roadkills, 
sampl.columns = 4:ncol(roadkills), sampl.intervals = 1:5, 
region.column = "segment", group.column = "taxon",
include.all.together = TRUE, confidence = 0.95, 
min.total.events = 80, min.hotspot.threshold = 2, 
comp.method = "Phi", plot = TRUE, sep.plots = FALSE,
omit.baseline.interval = TRUE, ylim = c(0, 1))

hc
}
