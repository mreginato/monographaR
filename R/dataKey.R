dataKey <-
function(dat, poly.sep="/", return.summary=T) {
  colnames(dat)[1] <- "species"
  as.matrix(dat) -> dat
  dat.s <- apply(dat, MARGIN=2, FUN=function(x)(grep(poly.sep, x, fixed=T)))
  round(unlist(lapply(dat.s, length))/nrow(dat),2) -> dat.s
  dat.s[-1] -> dat.s
  cat("Percentage of spp. with polymorphic characters per character:", fill=T)
  cat(paste(names(dat.s), "=", dat.s), sep="\n")
  
  matrix(ncol=ncol(dat), nrow=0) -> dat.n
  colnames(dat.n) <- colnames(dat)
  dat[,1] -> spp
  for (i in 1:length(spp)) {
    spp[i] -> sp0
    t(as.matrix(dat[match(sp0, dat[,1]),])) -> d0
    grep(poly.sep, d0) -> poly0
    if (length(poly0) > 0) {
      strsplit(d0, poly.sep) -> d1
      expand.grid(d1) -> dp
      colnames(dp) <- colnames(d0)
    } else {
      d0 -> dp
    }
    rbind(dat.n,dp) -> dat.n
  }
  data.frame(dat.n) -> dat.n
  if (return.summary) {
    list(dat=dat.n, summary=dat.s) -> dat.n
  } 
}
