\name{interactiveKey}
\alias{interactiveKey}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interactive key (shiny app)
}
\description{
Generates code to build an interactive key (shiny app)
}
\usage{
interactiveKey(dat = NULL, txt.labels = NULL, poly.sep = "/", 
state.sep = " = ", taxa.in.italics = TRUE, theme = "lumen")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
data.frame
}
  \item{txt.labels}{
data.frame 
}
  \item{poly.sep}{
character
}
  \item{state.sep}{
character
}
  \item{taxa.in.italics}{
logical
}
  \item{theme}{
character 
}
}
\details{
See vignette
}
\value{
It generates 6 files (app.R, server.R, ui.R, about.Rmd, Dat_characters.csv, and Dat_matrix.csv)
}
\author{
Marcelo Reginato
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[monographaR:interactiveKeyLabels]{interactiveKeyLabels}}
}
\examples{

## see vignette
