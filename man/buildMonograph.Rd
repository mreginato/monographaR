\name{buildMonograph}
\alias{buildMonograph}
\title{
Build and export a monograph skeleton (draft)
}
\description{
This function will generate a MS-Word or html file with a monograph skeleton (draft)
}
\usage{
buildMonograph(headings, tableToDescription.data, examinedSpecimens.data = NULL, 
collectorList.data = NULL, output = "Word", title = "Taxonomic treatment", 
open = TRUE)
}

\arguments{
  \item{headings}{
data.frame
}
  \item{tableToDescription.data}{
data.frame
}
  \item{examinedSpecimens.data}{
data.frame (optional)
}
  \item{collectorList.data}{
data.frame (optional)
}
  \item{output}{
"Word" or "html"
}
  \item{title}{
character
}
  \item{open}{
logical
}
}
\details{
This function wraps around the functions tableToDescription, 
examinedSpecimens and collectorList generating a monograph draft in 
MS-Word or html format. The resulting monograph skeleton will include the
taxonomic heading, the description, comments and examined specimens list 
for all species found in the input tables, and it will append the collector
list in the end of the file. It requires four tables as input. Three of them
are the same tables used for \code{\link[monographaR:tableToDescription]{tableToDescription}}, \code{\link[monographaR:collectorList]{collectorList}}, and 
\code{\link[monographaR:examinedSpecimens]{examinedSpecimens}} functions. The additional input table should have three 
columns: species, taxonomic heading and comments. The examinedSpecimens.data and
collectorList.data tables are optional. It uses functions of the \code{\link[rmarkdown:render]{rmarkdown}} 
package to export the output file. 
}
\value{
Exports a file (MS-Word or html).
}

\author{
Marcelo Reginato
}


\seealso{
\code{\link[rmarkdown:rmarkdown-package]{rmarkdown}}
}
\examples{

data(monographaR_examples)
monographaR_examples$taxonomic_headings -> taxonomic.headings
monographaR_examples$collectorList -> col.d
monographaR_examples$examinedSpecimens -> exam.d
monographaR_examples$tableToDescription -> desc.d
desc.d[,-1] -> desc.d

###	buildMonograph(headings=taxonomic.headings, 
###               collectorList.data = col.d, 
###               examinedSpecimens.data = exam.d, 
###               tableToDescription.data = desc.d, 
###               output = "Word", title="Monograph skeleton")


}

