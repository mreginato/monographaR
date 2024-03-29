mapTable <-
function(data, type="grid", resolution=1, pres.abs=TRUE, write.output=FALSE, layer=NULL) {

if (inherits(data, "data.frame") == FALSE) {
  stop("data must be a data.frame")
}
if (ncol(data) != 3) {
  stop("data must have 3 columns, see help(\"mapTable\")")
}
if (is.numeric(data[,2]) == F) {
  stop("longitude must be numeric, see help(\"mapTable\")")
}
if (is.numeric(data[,3]) == F) {
  stop("latitude must be numeric, see help(\"mapTable\")")
}
message("Assuming the columns are ordered as: species, longitude and latitude")
data -> geo.data
colnames(geo.data) <- c("Species", "x", "y")
coordinates(geo.data) = ~x+y
wrld_simpl = NULL
  if (type == "grid") {
    r0 <- raster(resolution=resolution)
    r0[] <- NA
    data.frame(sp=data[,1], cells=cellFromXY(r0, data[,2:3])) -> cells
    unique(cells) -> cells
    unique(cells$cells) -> old.labs
    sort(old.labs) -> old.labs
    c(1:length(old.labs)) -> new.labs
    cells$cells -> cells$new.cells
    for (i in 1:length(old.labs)) {
      cells$new.cells[which(is.na(match(cells$cells, old.labs[i])) == F)] <- new.labs[i]
    }
    r0[cells$cells] <- cells$new.cells
    rasterToPolygons(r0) -> grid
    #data(wrld_simpl, envir = environment())
    ne_countries(type="countries", returnclass = "sv") -> wrld_simpl
    st_as_sf(wrld_simpl) -> wrld_simpl
    as_Spatial(wrld_simpl) -> wrld_simpl
    plot(geo.data, col=NA)
    plot(wrld_simpl, add=T)
    plot(grid, add=T)
    text(grid, grid@data$layer, cex=0.8)
    cells[,-2] -> cells
    colnames(cells)[2] <- "cells"
    as.matrix(table(cells)) -> cells
    if (pres.abs) {
      cells[cells >= 1] <- 1
    } else {
      cells[cells >= 1] <- "x"
      cells[cells == "0"] <- ""
    }
    as.data.frame.matrix(cells) -> cells
    list(grid=grid, table=cells) -> result
    if (write.output == T) {
      write.csv(cells, "mapTable.csv")
      vect(grid) -> grid
      writeVector(grid, filename="mapTable_grid.shp", filetype="ESRI Shapefile")
      cat("Presence/absence matrix and the grid (shapefile) were saved in:")
      cat("\n", getwd())
    }
  }
  if (type == "country") {
    #data(wrld_simpl, envir = environment())
    ne_countries(type="countries", returnclass = "sv") -> wrld_simpl
    st_as_sf(wrld_simpl) -> wrld_simpl
    as_Spatial(wrld_simpl) -> wrld_simpl
    suppressWarnings(proj4string(wrld_simpl)) -> proj4string(geo.data)
    data.frame(sp=data[,1], over(geo.data, wrld_simpl)$admin) -> countries
    unique(countries) -> countries
    as.character(countries[,2]) -> countries[,2]
    colnames(countries)[2] <- "Country"
    as.matrix(table(countries)) -> countries
    if (pres.abs) {
      countries[countries >= 1] <- 1
    } else {
      countries[countries >= 1] <- "x"
      countries[countries == "0"] <- ""
    }
    as.data.frame.matrix(countries) -> countries
    countries -> result
  }
  if (type == "user") {
    if (is.null(layer)) {
      stop("When type = \"user\" a layer (spatial polygons) should be provided")
    }
    suppressWarnings(proj4string(layer)) -> proj4string(geo.data)
    over(geo.data, layer) -> over0
    data.frame(sp=data[,1], over(geo.data, layer)) ->t0
    na.omit(t0) -> t0
    if (ncol(t0)-1 > 1) {
      vector("list", length=ncol(t0)-1) -> result
    }
    names(result) <- names(layer)
    t0[,-1] -> t1
    for (i in 1:ncol(t1)) {
      data.frame(t0[,1],t1[,i]) -> t2
      colnames(t2) <- c("species", names(layer)[i])
      table(t2) -> m0
      m0[m0 > 0] <- "x"
      m0[m0 == 0] <- ""
      m0 -> result[[i]]
    }
  }
return(result)
}
