\name{getBoxplots}
\alias{getBoxplots}
\title{
Get boxplots
}
\description{
Get boxplots
}
\usage{
getBoxplots(corrs.list, ...)
}
\arguments{
  \item{corrs.list}{
a list of corrs.tables given by function \code{\link{schemeCorrs}}
}
  \item{\dots}{
additional arguments for the \code{\link{boxplot}} function (e.g. ylim = c(0, 1), las = 2, main = as.character(bquote(corrs.list)))
}
}
\value{
Box plots
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{schemeCorrs}}}
