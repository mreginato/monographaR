\name{dichoKey.app}
\alias{dichoKey.app}
\title{
Dichotomous key (shiny app)
}
\description{
This is a shiny app that generates a dichotomous key
}
\usage{
dichoKey.app()
}
%- maybe also 'usage' for other objects documented here.
\details{
A shiny app that runs the function "dichoKey". It allows 
the user to easily subset the data (taxa and characters), 
as well as change the cost values of the characters.
}
\value{
NULL
}
\author{
Marcelo Reginato
}
\seealso{
\code{\link[rpart:rpart]{rpart}}
\code{\link[monographaR:dataKey]{dataKey}}
\code{\link[monographaR:dichoKey]{dichoKey}}
}
\examples{

if (interactive()) {

dichoKey.app()

}

}

