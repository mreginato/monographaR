\name{interactiveKeyLabels}
\alias{interactiveKeyLabels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Text labels for interactiveKey
}
\description{
It generates a data.frame with txt labels required to use the function interactiveKey
}
\usage{
interactiveKeyLabels(taxon = "species", language = "english")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{taxon}{
character, "species", "genus" or "family"
}
  \item{language}{
character, "english" or "portuguese"
}
}
\details{
It generates a data.frame with txt labels required to use the function interactiveKey.
You can choose what rank will be identified in the key (species, genus or family),
and the language (english and portuguese so far). You can further modify the fields
prior using it with the interactiveKey function.
}
\value{
data.frame
}
\author{
Marcelo Reginato
}

\seealso{
\code{\link[monographaR:interactiveKey]{interactiveKey}}
}
\examples{

### standard labels species in english

interactiveKeyLabels(taxon = "species", language = "english") -> labs.spp.eng
labs.spp.eng

### standard labels families in portuguese

interactiveKeyLabels(taxon = "family", language = "portuguese") -> labs.fam.por
labs.fam.por
