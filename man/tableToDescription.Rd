\name{tableToDescription}
\alias{tableToDescription}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates species descriptions
}
\description{
This function will generate a txt file with species descriptions. 
}
\usage{
tableToDescription(data, filename = "species_descriptions.txt")
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
It requires a data.frame where the first three columns are the character description, putative 
complement and the character to use as separator (i.e., words that will remain constant across 
descriptions). The character description and/or the complement might be empty. The remaining 
columns are the species with their respective character states, where each row is a character. 
The function accepts any number of species and/or characters. 
}
\value{
Exports a txt file
}
\author{
Marcelo Reginato
}

\examples{

## loading the example data

data(monographaR_examples)
monographaR_examples$tableToDescription -> data
head(data)

## the first column is just an identifier for the characters, we need to 
## remove it before running the analysis

data[,-1] -> data

## running the function, it will print in the terminal the output. 
## To export a txt, place a name in the filename argument 
## (i.e., filename = "myoutput.txt")

tableToDescription(data, filename = "")

}

