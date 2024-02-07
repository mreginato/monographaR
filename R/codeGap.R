codeGap <-
function(x, n=NULL, max.states=NULL, poly.sep="/", gap.size=NULL) {
  
  int=F
  if (is.null(n)) {out.l=T} else {out.l=F}
  max.states -> max.n
  if (inherits(x, c("integer", "numeric"))) {
    data.frame(min=x,max=x) -> x
    int=T
  } else {
    colnames(x) <- c("min", "max")
  }
  
  ### ckeck missing
  na=F
  which(complete.cases(x)) -> keep
  which(!complete.cases(x)) -> rem
  if (length(rem) > 0) {
    rownames(x) -> order.i
    x[rem,] -> removed
    x[keep,] -> x
    na=T
  }
  
  ### decimals
  max(sapply(as.numeric(as.matrix(x)[]), FUN=function(x) {
    if ((x %% 1) != 0) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  })) -> fact.n
  
  fact=10^fact.n
  
  ### gap size
  
  if (is.null(gap.size)) {
    1/fact -> gap.size
  }
  min.gap = gap.size
  
  ### find gaps
  
  apply(x, MARGIN=1, range, simplify = F) -> x.all
  lapply(x.all, FUN=function(x)(c((x[1]*fact):(x[2]*fact)))) -> x.all
  lapply(x.all, FUN=function(x)x/fact) -> x.all
  names(x.all) <- rownames(x)
  
  if (max(unlist(x.all)) <= min.gap) {
    stop("Your gap size is larger than the values in data")
  }
  
  data.frame(x=unlist(x.all),index=NA) -> dat.t
  dat.t[order(dat.t$x),] -> dat.t
  dat.t$index <- c(0, diff(dat.t$x))
  which(dat.t$index >= min.gap) -> gaps
  ### weights
  c(dat.t$index[gaps]) -> gap.w
  names(gap.w) <- gaps
  names(gaps) <- gaps
  sort(gap.w, decreasing = T) -> gap.w
  gaps -> gaps.b
  gap.w -> gap.w.b
  dat.t -> dat.t.b
  
  if (is.null(n)) {
    ### optimize n (min.poly)
    if (is.null(max.n)) { max.n = length(gaps)+1 }
    if (max.n > (length(gaps)+1)) { length(gaps)+1 -> max.n }
    Ns <- c(2:max.n)
    Ns.poly <- vector(length=length(Ns))
    names(Ns.poly) <- Ns
    
    for (k in 1:length(Ns)) {
      Ns[k] -> n0
      gaps.b -> gaps
      gap.w.b -> gap.w
      dat.t.b -> dat.t
      ### subset states
      gap.w[1:(n0-1)] -> gap.w
      gaps[match(names(gap.w), names(gaps))] -> gaps
      sort(gaps) -> gaps
      gap.w[names(gaps)] -> gap.w  
      ### find states
      c(min(dat.t$x), dat.t$x[gaps]) -> mins
      c(dat.t$x[gaps-1], max(dat.t$x)) -> maxs
      cbind(mins, maxs) -> states
      data.frame(states, gap.w=c(0,gap.w), code=paste(states[,1],states[,2],sep="-")) -> states
      vector("list", length=nrow(states)) -> states.r
      for (i in 1:nrow(states)) {
        c((states[i,1]*fact):(states[i,2]*fact))/fact -> s0
        round(s0, fact.n) -> states.r[[i]]
      }
      ### get coding
      dat.t$x -> dat.t$x.coded
      for (i in 1:nrow(dat.t)) {
        dat.t$x[i] -> x0
        round(x0, fact.n) -> x0
        which(unlist(lapply(lapply(lapply(states.r, match, x0), na.omit), length)) == 1) -> r0
        states[r0,4] -> dat.t$x.coded[i]
      }
      
      dat.t -> clust.out
      colnames(clust.out)[3] <- "state"
      
      ### re-code
      data.frame(x,state=NA) -> dat.t
      for (i in 1:length(x.all)) {
        x.all[[i]] -> x0
        clust.out$state[match(x0, clust.out$x)] -> s0
        paste(sort(unique(s0)), collapse = poly.sep) -> dat.t$state[i]
      }
      
      # summary
      length(grep(poly.sep, dat.t$state, fixed=T)) -> Ns.poly[k]
    }
    ### data set best n
    as.numeric(names(which.min(Ns.poly))[1]) -> max.n
    dat.t.b -> dat.t
    gaps.b -> gaps
    gap.w.b -> gap.w
    gap.w[1:(max.n-1)] -> gap.w
    gaps[match(names(gap.w), names(gaps))] -> gaps
    sort(gaps) -> gaps
    gap.w[names(gaps)] -> gap.w
    
  } else {
    ### subset states
    n -> max.n
    if (length(gaps) > max.n-1) {
      gap.w[1:(max.n-1)] -> gap.w
      gaps[match(names(gap.w), names(gaps))] -> gaps
      sort(gaps) -> gaps
      gap.w[names(gaps)] -> gap.w
    }
  }
  
  
  ### find states
  c(min(dat.t$x), dat.t$x[gaps]) -> mins
  c(dat.t$x[gaps-1], max(dat.t$x)) -> maxs
  cbind(mins, maxs) -> states
  data.frame(states, gap.w=c(0,gap.w), code=paste(states[,1],states[,2],sep="-")) -> states
  vector("list", length=nrow(states)) -> states.r
  for (i in 1:nrow(states)) {
    c((states[i,1]*fact):(states[i,2]*fact))/fact -> states.r[[i]]
  }
  
  ### get coding
  dat.t$x -> dat.t$x.coded
  for (i in 1:nrow(dat.t)) {
    dat.t$x[i] -> x0
    round(x0, fact.n) -> x0
    which(unlist(lapply(lapply(lapply(states.r, match, x0), na.omit), length)) == 1) -> r0
    states[r0,4] -> dat.t$x.coded[i]
  }
  
  dat.t -> clust.out
  colnames(clust.out)[3] <- "state"
  
  ### re-code
  
  data.frame(x,state=NA) -> dat.t
  for (i in 1:length(x.all)) {
    x.all[[i]] -> x0
    clust.out$state[match(x0, clust.out$x)] -> s0
    paste(sort(unique(s0)), collapse = poly.sep) -> dat.t$state[i]
  }
  
  ### return
  
  unlist(x.all) -> hist.d
  range(hist.d)*fact -> r0
  hist(hist.d, breaks=r0[2]-r0[1], plot = F) -> hist.out
  hist.out$xname <- "value"
  
  if (na) {
    #removed$state <- paste(states$code, collapse=poly.sep)
    removed$state <- NA
    rbind(dat.t,removed) -> dat.t
    dat.t[order.i,] -> dat.t
  }
  
  if (int) {
    dat.t[,-1] -> dat.t
    colnames(dat.t)[1] <- "value"
  }
  if (out.l) {
    data.frame(Ns.poly) -> poly.out
    colnames(poly.out) <- "Polymorphics"
    list(dat=dat.t, polymorphic=poly.out, dist=hist.out) -> out
  } else {
    list(dat=dat.t, polymorphic=length(grep(poly.sep, dat.t$state, fixed=T)), dist=hist.out) -> out
  }
  return(out)
}
