\name{interactiveKey}
\alias{interactiveKey}
\title{
Interactive key (shiny app)
}
\description{
Generates code to build an interactive key (shiny app)
}
\usage{
interactiveKey(dat = NULL, txt.labels = NULL, poly.sep = "/", 
taxa.in.italics = TRUE, theme = "lumen", about.first = FALSE)
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
  \item{taxa.in.italics}{
logical
}
  \item{theme}{
character 
}
  \item{about.first}{
logical, whether the tab "about" is the first or the last 
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

\seealso{
\code{\link[monographaR:interactiveKeyLabels]{interactiveKeyLabels}}
\code{\link[shinythemes:shinythemes]{themes}}
}
\examples{

## see vignette
data(monographaR_examples)
monographaR_examples$interactiveKey

}

