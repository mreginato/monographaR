\name{phenoHist}
\alias{phenoHist}
\title{
Circular histograms of phenology
}
\description{
This wrapper function will generate circular histograms of phenology, using 
functions of the package circular. 
}
\usage{
phenoHist(data = data, mfrow = c(1, 1), shrink = 1.2, axis.cex = 
1.5, title.cex = 1.5, pdf = F, height=11, width=8.5, 
filename = "phenology.pdf", flower = "Flower", fruit = "Fruit", 
both = "Both", flower.col = NULL, flower.border = "black", 
fruit.col = "darkgray", fruit.border = "darkgray", mar=c(2,2,2,2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data.frame
}
  \item{mfrow}{
numeric, (nrow, ncol)
}
  \item{shrink}{
numeric
}
  \item{axis.cex}{
numeric
}
  \item{title.cex}{
numeric
}
  \item{pdf}{
logical
}
  \item{height}{
numeric
}
  \item{width}{
numeric
}
  \item{filename}{
character
}
  \item{flower}{
character (how is the flower indicated in data, if missing place "missing")
}
  \item{fruit}{
character (how is the fruit indicated in data, if missing place "missing")
}
  \item{both}{
character (how is the both indicated in data, if missing place "missing")
}
  \item{flower.col}{
character (color of flower bars)
}
  \item{flower.border}{
character (color of flower border bars)
}
  \item{fruit.col}{
character (color of fruit bars)
}
  \item{fruit.border}{
character (color of fruit border bars)
}
  \item{mar}{
numeric (plot margins, vector of 4 values)
}
}
\details{
It requires a data.frame with three columns, ordered as: species, month and 
phenology. The month column should be numeric (month number), while the 
phenology column must have these values: "Flower", "Fruit" and/or "Both". If 
any of these are missing is possible to indicate in the "flower", "fruit" and 
"both" arguments (both="missing"). The function will plot the bars 
indicating flower observations in white, and fruits in gray by default (is 
possible to change it with the "flower.col", "flower.border", "fruit.col" and 
"fruit.border" arguments). The size of the bar corresponds to number of 
observations. The arguments "shrink", "axis.cex" and "title.cex" control 
sizes, while the "mfrow" changes the number of histograms plotted at the same 
page (rows, columns).
}
\value{
Exports a pdf file.
}
\author{
Marcelo Reginato
}
\seealso{
\code{\link[circular:circular-package]{circular}}
}
\examples{

## loading the example data

data(monographaR_examples)
monographaR_examples$phenoHist -> data
head(data)

## running the function

phenoHist(data, mfrow=c(2,2), shrink=1.2, axis.cex=1.5, title.cex=1.5, 
pdf=FALSE)

## changing the color

phenoHist(data, mfrow=c(2,2), shrink=1.2, axis.cex=1.5, title.cex=1.5, 
pdf=FALSE, flower.col=rgb(0.2,1,0.2, 0.5), flower.border=rgb(0.2,1,0.2, 
0.5), fruit.col="darkgreen", fruit.border="black")

## plotting only flower (if "fruit" and/or "both" information are 
## missing for instance)

phenoHist(data, mfrow=c(2,2), shrink=1.2, axis.cex=1.5, title.cex=1.5, 
pdf=FALSE, fruit="missing", both="missing", flower.col="red", 
flower.border="darkgray")

}

