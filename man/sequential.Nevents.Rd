\name{sequential.Nevents}
\alias{sequential.Nevents}
\title{
Sequential numbers of events
}
\description{
Applies function \code{\link{hotspots}} (with hotspots=FALSE) sequentially to a given set of submatrices
}
\usage{
sequential.Nevents(dataset, submats, region.column, 
first.subsampl.col, estimate = FALSE)
}
\arguments{
  \item{dataset}{
name of the matrix or dataframe containing the complete data
}
  \item{submats}{
a list of the submatrices for which to calculate the hotspots (result of the sequential.submatrix function)}
  \item{region.column}{
name or index number of the column containing the regions (road sectors, sites) to classify as hotspots or non-hotspots
}
  \item{first.subsampl.col}{
index number of the first column containing subsampling data
}
  \item{estimate}{
logical, whether to add estimates from package \pkg{carcass}
}
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{hotspots}}, \code{\link{sequential.hotspots}}
}
