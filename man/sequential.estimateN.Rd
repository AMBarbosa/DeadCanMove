\name{sequential.estimateN}
\alias{sequential.estimateN}
\title{
Sequential estimate N
}
\description{
This function estimates the actual numbers of animal casualties given the observed numbers and a set of estimators, sequentially for all given submats. Requires package \pkg{carcass}.
}
\usage{
sequential.estimateN(submats, submats.N, first.subsampl.col, 
region.column, persist, effic, estimators = c("korner", "huso", 
"erickson", "etterson"), margin = 0.05, ...)
}
\arguments{
  \item{submats}{
result of the \code{\link{sequential.submatrix}} function.
}
  \item{submats.N}{
result of the \code{\link{sequential.Nevents}} function.
}
  \item{first.subsampl.col}{
index number of the first column containing the (sub)sampling data in \code{submats}
}
  \item{region.column}{
name or index number of the column containing the regions (road segments, sites) to classify as hotspots or non-hotspots
}
  \item{persist}{
named vector of persistence per group; group names must match those in the data
}
  \item{effic}{
named vector of detection efficiency per group; group names must match those in the data
}
  \item{estimators}{
character vector of the estimator(s) to use. The default is all estimators available.
}
  \item{margin}{
proportion of each estimator to subtract from and add to it in order to get p.lower and p.upper, respectively, when using function \code{estimateN} in package \pkg{carcass}
}
  \item{\dots}{
currently not in use
}
}
\value{
This function returns a list.
}
\author{
A. Marcia Barbosa, J. Tiago Marques, Sara Santos
}
\note{
This function currently works only for \code{submats} created with 'window.size' and 'gap.size', not with 'sampl.interval'. See \code{\link{submatrix}}, \code{\link{sequential.submatrix}}, and check that your names(submats) are something like "group.w1.g2.s1" and not "group.intv1".
}
\seealso{
function \code{estimateN} in package \pkg{carcass}
}
