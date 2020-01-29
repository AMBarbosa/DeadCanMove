\name{repl.hs.comp}
\alias{repl.hs.comp}
\title{
Replicate hotspot comparison
}
\description{
This function calculates hotspot correlation, loss, gain or balance for the different replicates per sampling scheme and taxonomic group.
}
\usage{
repl.hs.comp(seqsubmats.hs, hs.baseline, method = "Phi", 
stats = TRUE, plot = TRUE, plot.mean = TRUE, ylim = NULL, 
horiz.line = NA)
}
\arguments{
  \item{seqsubmats.hs}{
hotspots for the seqsubmats
}
  \item{hs.baseline}{
hotspots for the baseline
}
  \item{method}{
binary comparison method to use. See \code{\link{binary.comparison}} for available options.
}
  \item{stats}{
logical, whether to calculate also the stats (mean, min, max, sd) of the replicate comparison for each group.
}
  \item{plot}{
logical, whether to plot the hotspot comparison values per replicate per group.
}
  \item{plot.mean}{
logical, whether to plot (with a white circle) the mean value of the replicates per group.
}
  \item{ylim}{
limits for the y axis. The default is NULL for automatic limits, but you may want to use ylim = c(0,1) for e.g. phi correlations to be directly comparable among plots.
}
  \item{horiz.line}{
optionally, a numeric value indicating the y axis value for a horizontal threshold line to be drawn.
}
}
\value{
This function returns a list.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{binary.comparison}}
}
\examples{
\dontrun{
replicate.corrs <- repl.hs.comp(seqsubmats.hs = seqsubmats.hs, 
hs.baseline = hs.baseline, method = "Phi")

replicate.gains <- repl.hs.comp(seqsubmats.hs = seqsubmats.hs, 
hs.baseline = hs.baseline, method = "gain")
}
}
