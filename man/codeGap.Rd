\name{codeGap}
\alias{codeGap}
\title{
Code gap
}
\description{
This function takes a numeric vector (or a data.frame with two columns including min and max values of a sample) and tries to find breaks in the distribution (gaps), if any gap is found it returns a coded character based on that. 
}
\usage{
codeGap(x, n = NULL, max.states = NULL, poly.sep = "/", gap.size = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
integer/numeric or a two column data.frame (min and max)
}
  \item{n}{
integer, desired number of states (if NULL the function will try to suggest a number)
}
  \item{max.states}{
integer, the maximum possible number of states
}
  \item{poly.sep}{
character, to indicate polymorphic states (if any)
}
  \item{gap.size}{
numeric, the number that should be considered as a "gap"
}
}
\details{
If n = NULL the function will try to find the best scenario of states (n) based on the number of polymophic samples in the resulting classification. In large data sets it will be a good idea to constrain the search using (e.g., max.states=10). This coding tries to replicate the coding traditionally used in taxonomy.
}
\value{
list, including: 
dat = data.frame including the original value and the coded value (state)
polymorphic = the number of polymorphic samples (if n=NULL, it returns for all tested scenarios)
dist = a histogram of the data distribution 
}
\author{
Marcelo Reginato
}
\examples{

c(NA, 1:5, 15:20, 25:42, 49:60, 68:90) -> x
data.frame(x,x=x+2) -> x2

codeGap(x, n=3, max.states = 5) -> code1
code1$dat
#plot(code1$dist)

codeGap(x, n=NULL, max.states = NULL) -> code1
code1$dat
#plot(code1$dist)

codeGap(x2, n=NULL, max.states =3 , gap.size=2) -> code1
code1$dat
unique(code1$dat$state)


}

