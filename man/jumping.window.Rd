\name{jumping.window}
\alias{jumping.window}
\title{
Jumping window
}
\description{
This function extracts a moving (a.k.a. running, rolling, sliding) window but with no overlap between windows and with the option for gaps between windows.
}
\usage{
jumping.window(sampl.columns, window.size, gap.size, 
start.column = 1, J = FALSE)
}
\arguments{
  \item{sampl.columns}{
index numbers of the consecutive columns with the sampling data (e.g. 3:180) from which to extract the jumping windows. Can also be any vector from which to extract a jumpting window.
}
  \item{window.size}{
size of each sampling window/season (consecutive sampling days each time)
}
  \item{gap.size}{
size of the gap between sampling windows. Can be zero or a positive integer.
}
  \item{start.column}{
column of sampl.columns where to actually start the sampling windows. The default is 1, but e.g. with a gap size of 1 between windows, the start column can be either 1 or 2.
}
  \item{J}{
  logical, whether to provide the results in the form of J for function \code{carcass::etterson}. Defaults to FALSE.
  }
}
\details{
This function is now integrated within \code{\link{submatrix}} to provide for additional sampling scheme options.
}
\value{
When J = FALSE (the default), this function returns a vector containing the elements of \code{sampl.columns} that are included in the extracted windows; elements falling within the gaps are left out.
}
\author{
A. Marcia Barbosa, J. Tiago Marques
}
\examples{
data(roadkills)

names(roadkills)

jumping.window(4:ncol(roadkills), window.size = 1, gap.size = 0)

jumping.window(4:ncol(roadkills), window.size = 1, gap.size = 1)

w3g5 <- jumping.window(4:ncol(roadkills), window.size = 3, 
gap.size = 5)

w3g5

}
