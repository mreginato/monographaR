mapPhenology <-
function(data, resolution=1, time.range=c(1:12), label="Month", binary=T, by_species=F, plot=T, col=rev(heat.colors(12)), alpha=0.8, mfrow=c(4,3), legend=T, pdf=F, height=11, width=8.5, filename="mapPhenology.pdf") {
  if (class(data) != "data.frame") {
    stop("data must be a data.frame")
  }
  if (ncol(data) != 4) {
    stop("data must have 3 columns, see help(\"mapBatch\")")
  }
  if (is.numeric(data[,2]) == F) {
    stop("longitude must be numeric, see help(\"mapBatch\")")
  }
  if (is.numeric(data[,3]) == F) {
    stop("latitude must be numeric, see help(\"mapBatch\")")
  }
  wrld_simpl = NULL	
  message("Assuming the columns are ordered as: species, longitude, latitude, phenology")
  colnames(data) <- c("sp", "x", "y", "phenology")
  geo <- data
  coordinates(geo) <- ~x + y
  r0 <- raster(resolution=resolution)
  r0[] <- NA
  crop(r0,  extent(geo)++(resolution*2)) -> r0
  pheno.rasters <- vector("list", length=length(time.range))
  names(pheno.rasters) <- paste(label, as.character(time.range), sep="_")
  for (k in time.range) {
    r0 -> pheno0
    data[which(data$phenology == k),] -> d0
    cells.all <- data.frame(spp = d0[,1], cells = cellFromXY(r0, d0[,c(2,3)]))
    if (by_species) {
      unique(cells.all) -> cells.all
    }
    unique(cells.all$cells) -> cells.id
    for (i in 1:length(cells.id)) {
      cells.id[i] -> cell0
      d0[which(cells.all$cells == cell0),] -> d1
      nrow(d1) -> pheno0[cell0]
    }
    if (binary) {
      pheno0[which(pheno0[] > 0)] <- 1
    }
    pheno0 -> pheno.rasters[[k]]
  }
  if (binary == F) {
    lapply(pheno.rasters, FUN=function(x){max(x[], na.rm=T)}) -> max.n
    max(unlist(max.n)) -> max.n
    lapply(pheno.rasters, FUN=function(x)(x/max.n)) -> pheno.rasters
  }
  if (pdf == T) {
    pdf(filename, height=height, width=width)
  }
  if (plot) {
    data(wrld_simpl, envir = environment())
    par()$mfrow -> x
    par(mfrow=mfrow)
    for (i in 1:length(time.range)) {
      pheno.rasters[[i]] -> r0
      plot(geo, col=NA)
      plot(wrld_simpl, add=T)
      if (length(col) == 1) {
        plot(r0, add=T, legend=legend, col=col)
      } else {
        plot(r0, add=T, legend=legend, col=col, alpha=alpha)
      }
      title(names(pheno.rasters)[i])
    }
    par(mfrow=x)
  }
  if (pdf == T) {
    dev.off()
    cat("mapPhenology plate (pdf) was saved in:")
    cat("\n", getwd())
  }
  stack(pheno.rasters) -> pheno.rasters
  return(pheno.rasters)
}
