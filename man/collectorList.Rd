\name{collectorList}
\alias{collectorList}
\title{
Generates a collector list
}
\description{
This function will generate a txt file with a collector list for all species in data.
}
\usage{
collectorList(data = data, filename = "collector_list.txt", 
paragraphs = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data.frame
}
  \item{filename}{
character
}
  \item{paragraphs}{
logical
}
}
\details{
It requires a data.frame with five columns, ordered as species, collector name, collector
number, herbarium acronym and herbarium number. Herbarium columns are only used if 
any collector number is missing (NA). Thus, if there is no missing values in
collector number, then the herbarium columns might be empty.
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
monographaR_examples$collectorList -> data
head(data)

## running the function, it will print in the terminal the output. 
## To export a txt, place a ## name in the filename argument
## (i.e., filename = "myoutput.txt")

collectorList(data, filename = "", paragraphs = TRUE)

## or a second option

collectorList(data, filename = "", paragraphs = FALSE)


}

