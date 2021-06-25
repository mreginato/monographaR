\name{dataKey}
\alias{dataKey}
\title{
Prepares a data.frame suitable for buildKey
}
\description{
This function takes a data.frame with polymorphic characters and prepares 
it to use in the function buildKey.
}
\usage{
dataKey(dat, poly.sep = "/", return.summary = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
data.frame
}
  \item{poly.sep}{
character
}
  \item{return.summary}{
boolean
}
}
\details{
This function takes a data.frame with polymorphic characters and prepares 
it to use in the function buildKey.
}
\value{
data.frame
}
\author{
Marcelo Reginato
}

\seealso{
\code{\link[monographaR:dichoKey]{dichoKey}}
}
\examples{

data(monographaR_examples)

monographaR_examples$dichoKey -> dat
colnames(dat)[1] <- "species"

### Prepare matrix (polymorphic)

dataKey(dat, poly.sep = "/") -> dat.k
dat.k$dat -> dat.p

}