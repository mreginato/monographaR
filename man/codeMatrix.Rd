\name{codeMatrix}
\alias{codeMatrix}
\title{
Code character matrix
}
\description{
This is a wrapper function to {\link[monographaR:codeGap]{codeGap}}, which accepts a data.frame of characters values including ranges (1-10).
}
\usage{
codeMatrix(dat, n = NULL, max.states = NULL, poly.sep = "/", gap.size = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
data.frame
}
  \item{n}{
integer, see \code{\link[monographaR:codeGap]{codeGap}}
}
  \item{max.states}{
integer, see \code{\link[monographaR:codeGap]{codeGap}}
}
  \item{poly.sep}{
character, see \code{\link[monographaR:codeGap]{codeGap}}
}
  \item{gap.size}{
integer, see \code{\link[monographaR:codeGap]{codeGap}}
}
}
\value{
data.frame
}
\author{
Marcelo Reginato
}

\seealso{
\code{\link[monographaR:codeGap]{codeGap}}
}
\examples{

data(monographaR_examples)
monographaR_examples$dichoKey_q -> dat
rownames(dat) <- dat[,1]
dat[,-1] -> dat

codeMatrix(dat, max.states = 3) -> mat
head(mat)


}

