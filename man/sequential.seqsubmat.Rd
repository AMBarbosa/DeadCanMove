\name{sequential.seqsubmat}
\alias{sequential.seqsubmat}
\title{
Sequential sequential.submatrix
}
\description{
Applies \code{\link{sequential.submatrix}} sequentially to a set of gap sizes, with window.size =1
}
\usage{
sequential.seqsubmat(dataset, sampl.columns, group.column, gap.sizes,
n.replicates.limit)
}
\arguments{
  \item{dataset}{
  name of the matrix or dataframe to analyze
}
  \item{sampl.columns}{
numbers of the consecutive columns with the (daily) sampling data, e.g. 4:180
}
  \item{group.column}{
name or index number of the column containing the taxa or groups to analyse separately, e.g. 3 or "Family"; if NULL, all records will be used together
}
  \item{gap.sizes}{
integer vector of the size(s) of the gaps between sampling days. Must be either 0 (for no gap) or a vector of positive integers.
}
  \item{n.replicates.limit}{
Maximum number of replicates allowed
}
}
\value{
This function returns a list of submatrices
}
\author{
A. Marcia Barbosa
}
\seealso{
\code{\link{sequential.submatrix}}
}
\examples{
data(roadkills)

seqsubmats <- sequential.seqsubmat(dataset = roadkills, 
sampl.columns = 4:ncol(roadkills), group.column = "taxon", gap.sizes = 1:4, 
n.replicates.limit = 7)

names(seqsubmats)

# seqsubmats for gap 0 (baseline scenario) must be obtained separately:
seqsubmats.baseline <- sequential.seqsubmat(dataset = roadkills, 
sampl.columns = 4:ncol(roadkills), group.column = 3, gap.sizes = 0, 
n.replicates.limit = 7)

names(seqsubmats.baseline)
}
\keyword{manip}
