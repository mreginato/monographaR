\name{mapDiversity}
\alias{mapDiversity}
\title{
Diversity heatmap
}
\description{
This function will generate a diversity heatmap using presence/absence of 
species on grid cells.
}
\usage{
mapDiversity(data, resolution = 1, plot = T, plot.with.grid = T, 
col=rev(terrain.colors(55)), alpha=0.8, export = F, legend = T, 
filename = "diversity_map")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data.frame
}
  \item{resolution}{
numeric, size of the grid cells (degrees)
}
  \item{plot}{
logical
}
  \item{plot.with.grid}{
logical, whether or not to add a grid to the plot
}
  \item{col}{
character, a vector of colors
}
  \item{alpha}{
numerical, controls color transparency (0-1)
}
  \item{export}{
logical
}
  \item{legend}{
logical
}
  \item{filename}{
character
}
}
\details{
It requires a data.frame with three columns, ordered as: species, longitude 
and latitude. The function will plot and return a raster object. The 
resolution of the grid can be changed by the argument "resolution" (in 
degrees). It uses functions of the package raster.
}
\value{
A raster object.
}
\author{
Marcelo Reginato
}

\seealso{
\code{\link[raster:raster-package]{raster}}
}
\examples{

## loading the example data

data(monographaR_examples)
monographaR_examples$map_data -> data
head(data)

## running the function

mapDiversity(data , resolution=1, plot=TRUE, plot.with.grid=TRUE)

## Without the grid borders

mapDiversity(data , resolution=1, plot=TRUE, plot.with.grid=FALSE)

## Changing colors

mapDiversity(data , resolution=1, plot=TRUE, col=gray.colors(55))

## Changing transparency

mapDiversity(data , resolution=1, plot=TRUE, alpha=0.5)

## The function returns a raster object

mapDiversity(data , resolution=1, plot=TRUE, alpha=0.5, plot.with.grid=FALSE) -> my.div.raster
my.div.raster
# plot(my.div.raster)
# writeRaster(my.div.raster, "mydivraster.tif")

}

