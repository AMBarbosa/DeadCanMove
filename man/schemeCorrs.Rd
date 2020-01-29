\name{schemeCorrs}
\alias{schemeCorrs}
\title{
Scheme correlations
}
\description{
Get correlation between each sampling scheme and the corresponding baseline
}
\usage{
schemeCorrs(dataset, submats, submats.baseline, region.column, 
group.column, first.subsampl.col)
}
\arguments{
  \item{dataset}{
name of the matrix or dataframe containing the complete data
}
  \item{submats}{
a list of the submatrices for which to calculate the correlation (result of the \code{\link{sequential.submatrix}} function)
}
  \item{submats.baseline}{
a list of the submatrices corresponding to the baseline sampling scheme for each group
}
  \item{region.column}{
name or index number of the column containing the regions (road segments, sites) to classify as hotspots or non-hotspots
}
  \item{group.column}{
name or index number of the column containing the taxonomic groups
}
  \item{first.subsampl.col}{
index number of the first column containing subsampling data
}
}
\value{
This function returns a list of corrs.tables.
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{getBoxplots}}
}
