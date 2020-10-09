\name{examinedSpecimens}
\alias{examinedSpecimens}
\title{
Generates an examined specimens list
}
\description{
This function will generate a txt file with an examined specimens list. 
}
\usage{
examinedSpecimens(data, filename = "examined.txt")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data.frame
}
  \item{filename}{
character
}
}
\details{
It requires a data.frame with eight columns, ordered as: species, collector name, collector
number, herbarium acronym, herbarium number, country, state and municipality. 
}
\value{
Exports a txt file.
}
\author{
Marcelo Reginato
}

\examples{

## loading the example data

data(monographaR_examples)
monographaR_examples$examinedSpecimens -> data
head(data)

## running the function, it will print in the terminal the output. 
## To export a txt, place a name in the filename argument
## (i.e., filename = "myoutput.txt")

examinedSpecimens(data, filename = "")

}

