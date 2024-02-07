codeMatrix <-
function(dat, n=NULL, max.states=NULL, poly.sep="/", gap.size=NULL) {
  
  if (!inherits(dat, "data.frame")) {
    stop("'dat' must be a data.frame")
  }
  ### split range
  vector("list", length=ncol(dat)) -> dat.l
  names(dat.l) <- colnames(dat)
  
  for (i in 1:ncol(dat)) {
    do.call(rbind, strsplit(dat[,i], "-")) -> dat.c
    mode(dat.c) <- 'numeric'
    data.frame(dat.c) -> dat.c
    rownames(dat.c) <- rownames(dat)
    dat.c -> dat.l[[i]]
  }
  
  ### get coding
  lapply(dat.l, codeGap, n, max.states, poly.sep, gap.size) -> out
  
  ### get matrix
  lapply(out, "[", 1) -> mats
  lapply(mats, FUN=function(x)(subset(x[[1]], select=3))) -> mats
  do.call(cbind, mats) -> mat
  colnames(mat) <- colnames(dat)
  return(mat)
}
