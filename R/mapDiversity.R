mapDiversity <-
function(data, resolution=1, plot=T, plot.with.grid=T, col=rev(terrain.colors(55)), alpha=0.8, export=F, legend=T, filename="diversity_map") {
  if (inherits(data, "data.frame") == FALSE) {
    stop("data must be a data.frame")
  }
  if (ncol(data) != 3) {
    stop("data must have 3 columns, see help(\"mapDiversity\")")
  }
  if (is.numeric(data[,2]) == F) {
    stop("longitude must be numeric, see help(\"mapDiversity\")")
  }
  if (is.numeric(data[,3]) == F) {
    stop("latitude must be numeric, see help(\"mapDiversity\")")
  }
  wrld_simpl = NULL
  message("Assuming the columns are ordered as: species, longitude and latitude")
  data -> geo
  colnames(geo) <- c("Species", "x", "y")
  coordinates(geo) = ~x+y
  r0 <- raster(resolution=resolution)
  r0[] <- NA
  crop(r0,  extent(geo)++(resolution*2)) -> r0
  data.frame(spp=data[,1], cells=cellFromXY(r0, data[,2:3])) -> cells
  unique(cells) -> cells
  table(cells$cells) -> t.cells
  as.numeric(t.cells) -> r0[as.numeric(names(t.cells))]
  if (export == T) {
    sub(".asc", "", filename) -> filename
    writeRaster(r0, filename=paste(filename,".asc", sep=""), overwrite=T)
    if (plot.with.grid == T) {
      rasterToPolygons(r0) -> grid
      vect(grid) -> grid
      writeVector(grid, filename=paste(filename,"_grid.shp", sep=""), filetype="ESRI Shapefile")
    }
  }
  if (plot == T) {
    ne_countries(type="countries", returnclass = "sv") -> wrld_simpl
    st_as_sf(wrld_simpl) -> wrld_simpl
    as_Spatial(wrld_simpl) -> wrld_simpl
    plot(geo, col=NA)
    plot(wrld_simpl, add=T)
    plot(r0, add=T, legend=legend, col=col, alpha=alpha)
    if (plot.with.grid == T) {
      rasterToPolygons(r0) -> grid
      plot(grid, add=T, border="gray20", lwd=0.5)
    } 
  }
  if (export == T) {
    cat("Diveristy map (raster) was saved in:")
    cat("\n", getwd())
  }
  return(r0)
}
