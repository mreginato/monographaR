\name{mapBatch}
\alias{mapBatch}
\title{
Generates map in batch mode
}
\description{
This wrapper function will export maps for all species in data. 
}
\usage{
mapBatch(data, zoom = T, margin = 0.1, axes = T, shape = NULL,
export = "pdf", raster = NULL, RGB = NULL, points.col = "black", 
points.border = "gray50", points.cex = 1, shape.col = "white", 
shape.border = "black", raster.col = rev(gray.colors(65, start = 0, end = 1)), 
raster.legend = F, hillshade = F, width = 8, height = 8, 
image.resolution = 100, figure.number = T, title = T, box = T, 
add.minimap = F, minimap.shape = NULL, minimap.shape.col = "white", 
minimap.shape.border = "gray50", minimap.pos = "topleft", 
minimap.add.points = T, minimap.points.col = "black", 
minimap.points.border = "gray50", minimap.points.cex = 1, 
minimap.extent = NULL, minimap.rect.fill = NA, minimap.rect.border = NULL,
maxpixels = 1e+05, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data.frame
}
  \item{zoom}{
logical
}
  \item{margin}{
numeric
}
  \item{axes}{
logical
}
  \item{shape}{
a single or a list of spatial shape objects
}
  \item{export}{
"pdf", "jpeg" or "tiff"
}
  \item{raster}{
a raster object
}
  \item{RGB}{
a raster stack object (with three layers)
}
  \item{points.col}{
character
}
  \item{points.border}{
character
}
  \item{points.cex}{
numeric
}
  \item{shape.col}{
character
}
  \item{shape.border}{
character
}
  \item{raster.col}{
character (a vector of colors)
}
  \item{raster.legend}{
logical
}
  \item{hillshade}{
logical
}
  \item{width}{
numeric (in inches)
}
  \item{height}{
numeric (in inches)
}
  \item{image.resolution}{
numeric
}
  \item{figure.number}{
logical
}
  \item{title}{
logical
}
  \item{box}{
logical
}
  \item{add.minimap}{
logical
}
  \item{minimap.shape}{
a spatial shape object
}
  \item{minimap.shape.col}{
character (color)
}
  \item{minimap.shape.border}{
character (color)
}
  \item{minimap.pos}{
"topleft", "topright", "bottomleft" or "bottomright"
}
  \item{minimap.add.points}{
logical
}
  \item{minimap.points.col}{
character (color)
}
  \item{minimap.points.border}{
character (color)
}
  \item{minimap.points.cex}{
numeric
}
  \item{minimap.extent}{
numeric (x1, x2, y1, y2)
}
  \item{minimap.rect.fill}{
character (color)
}
  \item{minimap.rect.border}{
character (color)
}
  \item{maxpixels}{
numeric
}
  \item{\dots}{
additional arguments for plotting the extra shapes
}
}
\details{
The function has three output options: a single pdf with all maps (export = 
"pdf") or individual image files for each species (export = "tiff" or "jpeg"). 
It requires a data.frame with three columns, ordered as: species, longitude 
and latitude. If zoom = TRUE, the function will set the limits of the plot 
using the distribution of each species plus the margin (relative value). If 
zoom = FALSE, the function will use the distribution of the whole data to set 
the limits (all maps will have the same limits). Colors can be changed with 
the arguments points.col, shape.col, shape.border, while the size of the 
points can be changed with points.cex. A raster layer can be provided 
(elevation for instance), and the colors of the raster are controlled by 
raster.col. The user can provide a single or a list of shape files, 
otherwise the \code{\link[rnaturalearth:ne_countries]{rnaturalearth}} map is used.
}
\value{
Exports a pdf or image files.
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

# mapBatch(data , type="simple", zoom=T, margin=0.2, points.col="black",
# points.border="white", shape.col="gray90", points.cex=1.5, shape.border 
# = "gray90", export="pdf")

}