\name{sequential.submatrix}
\alias{sequential.submatrix}
\title{
Extract subsampling matrices for a series of subsampling schemes
}
\description{
This function applies \code{\link{submatrix}} sequencially to all specified
sampling schemes and taxonomic groups.
}
\usage{
sequential.submatrix(dataset, sampl.columns, sampl.intervals = NULL,
window.sizes = NULL, gap.sizes = NULL, start.columns = 1, 
all.combinations = TRUE, group.column = NULL, 
include.all.together = TRUE, remove.zeros = TRUE,
keep.nonsampl.columns = TRUE, n.subsampl.columns = NULL)
}
\arguments{
  \item{dataset}{
name of the matrix or dataframe to analyze
}
  \item{sampl.columns}{
numbers of the consecutive columns with the (daily) sampling data, e.g. 4:180
  }
  \item{sampl.intervals}{a vector of the intervals at which to extract sampling data, e.g. 5 to take one every five samples}
  \item{window.sizes}{
the size (in sampling time units, e.g. days) of the sampling periods
}
  \item{gap.sizes}{
the size of the gaps between sampling periods
}
  \item{start.columns}{
vector of start columns, see \code{\link{submatrix}}}
  \item{all.combinations}{
logical, whether to use all window x gap size combinations (the default, TRUE) or just the number corresponding to the length of window.sizes and gap sizes (in which case window.sizes and gap.sizes must have the same length)
}
  \item{group.column}{
name or index number of the column containing the taxa or groups to analyse separately, e.g. 3 or "Family"; if NULL, all records will be used together
}
  \item{include.all.together}{
logical, whether to get subsampling matrices also for the complete data (including all groups together)
}
  \item{remove.zeros}{
logical, whether to remove rows where all extracted samples have zero observations
}
  \item{keep.nonsampl.columns}{
logical, whether to include also the non-sampling columns of \code{dataset} in the resulting submatrices)
}
  \item{n.subsampl.columns}{
number of subsampl.columns
}
}
\value{
A list of submatrices
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{submatrix}}, \code{\link{subset}}
}
\examples{
data(roadkills)

submats1 <- sequential.submatrix(dataset = roadkills, 
sampl.columns = 4:ncol(roadkills), sampl.intervals = c(1, 3), 
group.column = "taxon", include.all.together = TRUE,
remove.zeros = TRUE, keep.nonsampl.columns = TRUE, 
n.subsampl.columns = 85)

names(submats1)
head(submats1[[1]])

submats2 <- sequential.submatrix(dataset = roadkills, 
sampl.columns = 4:ncol(roadkills), window.sizes = c(1,3,5), 
gap.sizes = 1:3, start.columns = 1, all.combinations = TRUE,
group.column = "taxon", include.all.together = TRUE, 
remove.zeros = TRUE, keep.nonsampl.columns = TRUE, 
n.subsampl.columns = 85)

names(submats2)
head(submats2[[1]])
}
\keyword{manip}
