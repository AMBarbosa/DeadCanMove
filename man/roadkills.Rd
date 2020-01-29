\name{roadkills}
\alias{roadkills}
\docType{data}
\title{
Imaginary roadkill data
}
\description{
An imaginary dataset of roadkill data for 5 "taxonomic" groups.
}
\usage{data(roadkills)}
\format{
  A data frame with 900 observations on the following variables:
  \describe{
    \item{\code{individ}}{an integer vector attributing an identifier to each  
    recorded individual}
    \item{\code{segment}}{a numeric vector identifying the road segment at which 
    each individual was recorded}
    \item{\code{group}}{a character vector indicating the "taxonomic" group to 
    which each individual belongs}
    \item{\code{day1}}{a numeric vector indicating whether the individual was 
    found (1) or not (0) on that sampling day (the same for all remaining days 
    in the data frame)}
  }
}
\details{
Each row corresponds to an individual recorded at a particular road stretch 
(\code{segment}), with a 1 if it was present and a 0 if it was not present at 
that segment on each of the sampling days. Individuals were not manually removed 
from the road, so each individual has value 1 in all days in which its body was 
detected on the road.
}
\source{
Freely modified from data collected by:
Santos S.M., Carvalho F., Mira A. (2011) How long do the dead survive on the 
road? Carcass persistence probability and implications for road-kill monitoring 
surveys. PLoS ONE 6(9), e25383 (doi:10.1371/journal.pone.0025383)
}
\examples{
data(roadkills)

roadkills[1:20, 1:10]

}
\keyword{datasets}
