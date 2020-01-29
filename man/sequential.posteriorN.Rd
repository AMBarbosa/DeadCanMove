\name{sequential.posteriorN}
\alias{sequential.posteriorN}
\title{Sequential posterior N}
\description{
Applies function \code{posteriorN} of package \pkg{carcass} sequentially to a given set of submatrices, to estimate the number of (roadkill mortality) events based on Bayes' theorem using up to four different estimators (Korner-Nievergelt et al. 2015)
}
\usage{
sequential.posteriorN(submats, submats.N, first.subsampl.col, 
region.column, persist, effic, estimators = c("korner", "huso", 
"erickson", "etterson"), ...)
}
\arguments{
  \item{submats}{
a list of submatrices resulting from function \code{\link{sequential.submatrix}}
}
  \item{submats.N}{
a list of submatrices resulting from function \code{\link{sequential.Nevents}}
}
  \item{first.subsampl.col}{
index number of the first column containing subsampling data
}
  \item{region.column}{
name or index number of the column containing the regions (road sectors, sites) to classify as hotspots or non-hotspots
}
  \item{persist}{
named numeric vector of persistence probability per group; names must match the names of groups in the data
}
  \item{effic}{
named numeric vector of detection efficiency per group; names must match the names of groups in the data
}
  \item{estimators}{
character vector specifying the estimators to calculate; the default is all available ones
}
  \item{\dots}{
additional arguments for function \code{posteriorN} in package \pkg{carcass}
}
}
\value{
This function returns a list
}
\references{
Korner-Nievergelt F, Behr O, Brinkmann R, Etterson MA, Huso MMP, Dalthorp D, Korner-Nievergelt P, Roth T & Niermann I (2015) Mortality estimation from carcass searches using the R-package carcass: a tutorial. Wildlife Biology 21: 30-43
}
\author{
A. Marcia Barbosa, J. Tiago Marques, Sara M. Santos
}
\seealso{
\code{\link{sequential.submatrix}}, \code{\link{sequential.Nevents}}
}
