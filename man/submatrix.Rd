\name{submatrix}
\alias{submatrix}
\title{
Extract a submatrix for a given taxomic group and/or sampling scheme
}
\description{
Given a baseline dataset, this function extracts a sub-dataset for a given taxomic group and/or sampling scheme, defined either by a sampling interval (periodicity) or by a window size (consecutive sampling days each time) and a gap size (gaps between sampling windows).
}
\usage{
submatrix(dataset, sampl.columns, sampl.interval = NULL, 
window.size = NULL, gap.size = NULL, start.column = 1, 
group.column = NULL, group.names = NULL, remove.zeros = TRUE, 
keep.nonsampl.columns = TRUE)
}
\arguments{
  \item{dataset}{
name of the matrix or dataframe to analyze
}
  \item{sampl.columns}{
index numbers of the (consecutive) columns containing the baseline (daily) sampling data, e.g. 3:180
  }
  \item{sampl.interval}{interval at which to extract sampling data, e.g. 5 (to take one every five samples)}
  \item{window.size}{
instead of sampl.interval (for one sampling every so many time units), size (in sampling time units, e.g. days) of each sampling window (e.g. 3 for 3 consecutive days sampling each time); must be complemented with \code{gap.size}, for the gap between sampling windows.
}
  \item{gap.size}{
size (in sampling time units, e.g. days) of the gaps between sampling windows -- e.g. 1 for 1 gap (non-sampled) day between sampling windows.
}
  \item{start.column}{
column of sampl.columns where to actually start the sampling (e.g. with a gap size of 1 between sampling windows, the start column can be either 1 or 2)
}
  \item{group.column}{
name or index number of the column containing the taxa or groups to analyse, e.g. 3 or "Family"
}
  \item{group.names}{
name(s) of the group(a) to extract, e.g. c("Mustelidae", "Procyonidae"); if NULL (the default), all groups in group.names are extracted
}
  \item{remove.zeros}{
logical indicating whether to remove rows where all extracted days have zero observations
}
  \item{keep.nonsampl.columns}{
logical indicating whether to keep the non-sampling columns in the extracted result
}
}
\value{
This function returns a subset of \code{dataset} containing the taxonomic groups and sampling columns resulting from the given sampling scheme.
}
\author{
A. Marcia Barbosa, J. Tiago Marques
}
\seealso{
\code{\link{sequential.submatrix}}, \code{\link{subset}}
}
\examples{
data(roadkills)

submat1 <- submatrix(dataset = roadkills, sampl.columns = 4:ncol(roadkills),
sampl.interval = 3, start.column = 1, group.column = "taxon", group.names = NULL)

head(submat1)

submat2 <- submatrix(dataset = roadkills, sampl.columns = 4:ncol(roadkills),
window.size = 5, gap.size = 2, start.column = 1, group.column = "taxon",
group.names = NULL)

head(submat2)

}
\keyword{manip}
