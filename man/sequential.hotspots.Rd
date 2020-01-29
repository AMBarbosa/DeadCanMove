\name{sequential.hotspots}
\alias{sequential.hotspots}
\title{
Calculate roadkill hotspots for a series of (sub)sampling datasets
}
\description{
This function applies \code{\link{hotspots}} sequencially to a given set of submatrices to identify the hotspot regions in each dataset, using an adaptation of the method of Malo et al. (2004).
}
\usage{
sequential.hotspots(dataset, submats, region.column, 
first.subsampl.col, confidence = 0.95)
}
\arguments{
  \item{dataset}{
name of the matrix or dataframe containing the complete data
}
  \item{submats}{
a list of the submatrices for which to calculate the hotspots (result of the 
\code{\link{sequential.submatrix}} function)
}
  \item{region.column}{
name or index number of the column containing the regions (road segments, sites) to classify as hotspots or non-hotspots
}
  \item{first.subsampl.col}{
index number of the first column containing subsampling data
}
  \item{confidence}{
confidence threshold to consider hotspots. The default is 0.95
}
}
\value{
A list of 2 elements:
\item{hotspots.thresholds}{A named integer vector}
\item{hotspots.maps}{A list of data frames, each showing the total number of events (deaths) per region and whether or not it was considered a hospot.}
}
\references{
Malo, J.E., Suarez, F., Diez, A. (2004) Can we mitigate animal-vehicle accidents using predictive models? J. Appl. Ecol. 41, 701-710 (doi: 10.1111/j.0021-8901.2004.00929.x)
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{hotspots}}
}
\examples{
data(roadkills)

submats <- sequential.submatrix(dataset = roadkills,
sampl.columns = 4:ncol(roadkills), window.sizes = 1, gap.sizes = 1:3,
group.column = "taxon", include.all.together = TRUE, 
remove.zeros = TRUE, keep.nonsampl.columns = TRUE, 
n.subsampl.columns = 85)

shs <- sequential.hotspots(dataset = roadkills, submats = submats,
region.column = "segment", first.subsampl.col = 4, confidence = 0.95)

shs
str(shs)
}
