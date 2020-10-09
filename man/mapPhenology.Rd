\name{mapPhenology}
\alias{mapPhenology}
\title{
Phenology heatmap
}
\description{
This function will generate phenology maps across time (month, week, etc..).
}
\usage{
mapPhenology(data, resolution = 1, time.range = c(1:12), label = "Month", 
binary = T, by_species = F, plot = T, col = rev(heat.colors(12)), 
alpha = 0.8, mfrow = c(4, 3), legend = T, pdf = F, height = 11, 
width = 8.5, filename = "mapPhenology.pdf")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data.frame
}
  \item{resolution}{
numeric (degrees)
}
  \item{time.range}{
numeric (vector of months, weeks, etc...)
}
  \item{label}{
character ("Month", "Week")
}
  \item{binary}{
logical
}
  \item{by_species}{
logical
}
  \item{plot}{
logical
}
  \item{col}{
character (vector of colors)
}
  \item{alpha}{
numeric (0-1)
}
  \item{mfrow}{
numeric
}
  \item{legend}{
logical
}
  \item{pdf}{
logical
}
  \item{height}{
numerical
}
  \item{width}{
numerical
}
  \item{filename}{
character
}
}
\details{
This wrapper function will generate heatmaps of phenology across a time range. 
The default is to produce 12 heatmaps plotted on a single plate. This can be 
changed with the argument time.range, where any numerical range can be provided 
(representing weeks for instance). The argument mfrow controls the plate layout. 
It requires a data.frame with four columns, ordered as: species, longitude, 
latitude and phenology. The phenology column should be numeric (i.e., the number 
of the month, week or day the specimen was collected with flower/fruit). It is 
possible to change the resolution of the resulting rasters. The function can 
produce presence/absence heatmaps (if binary = T) or abundance heatmaps 
(if binary = F). The abundance values are relative (divided by the maximum 
abundance observed across all rasters). The function returns a RasterStack that 
can be exported or used in customized plots. To export a pdf, set "pdf=TRUE". 
The function wraps around functions of the raster package.

}
\value{
RasterStack
}
\author{
Marcelo Reginato
}
\seealso{
\code{\link[raster:raster-package]{raster}}
}
\examples{

### load the example data

data(monographaR_examples)
monographaR_examples$mapPhenology -> data
head(data) ## check the first rows

### running the function

# mapPhenology(data, binary=FALSE, by_species=FALSE, legend=FALSE)

### changing the colors

# mapPhenology(data, binary=FALSE, by_species=FALSE, legend=FALSE, col=rev(terrain.colors(55)))

### exporting raster

# require(raster)
# mapPhenology(data, binary=FALSE, by_species=FALSE, legend=FALSE) -> myphenorasters
# plot(myphenorasters[[1]]) ## plot first month
# writeRaster(myphenorasters[[2]], "pheno_month2.asc") ## exporting 2nd month

### making an GIF animation

# require(animation)
# saveGIF(
#  {mapPhenology(data, binary=F, resolution=0.5, by_species=F, legend=F, mfrow=c(1,1))},
#  movie.name="phenology.gif", interval=0.5, ani.width=600, ani.height=600
# )



}

