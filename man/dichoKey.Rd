\name{dichoKey}
\alias{dichoKey}
\title{
Dichotomous key
}
\description{
This function generates a dichotomous key
}
\usage{
dichoKey(dat, cost = NULL, clean.characters = TRUE, cp = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
data.frame
}
  \item{cost}{
numeric
}
  \item{clean.characters}{
boolean
}
  \item{cp}{
numeric
}
}
\details{
This is a wrapper function based on the rpart function of the rpart package.
It modifies the output of rpart to a more "taxonomic" dichotomous key.
}
\value{
list
}
\author{
Marcelo Reginato
}
\seealso{
\code{\link[rpart:rpart]{rpart}}
\code{\link[monographaR:dataKey]{dataKey}}
}
\examples{

data(monographaR_examples)

monographaR_examples$dichoKey -> dat
colnames(dat)[1] <- "species"

### Prepare matrix (polymorphic)

dataKey(dat, poly.sep = "/") -> dat.k
dat.k$dat -> dat.p

### key

dichoKey(dat.p) -> key
length(key$unresolved)

# Export

#cat(key$key, file="Pleiochiton_key.txt")

### key with costs

dat.k$summary
((dat.k$summary)+1) -> dat.c
colnames(dat.p)
dat.c[2] <- max(dat.c)
dat.c[11] <- max(dat.c)

dichoKey(dat.p, dat.c, cp=0) -> key.c
length(key.c$unresolved)

# Export

#cat(key.c$key, file="Pleiochiton_key_costs.txt")



}