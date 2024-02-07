dichoKey <-
function (dat, cost=NULL, clean.characters=TRUE, cp=0) {
  ### run rpart
  colnames(dat)[1] <- "species"
  f <- as.formula(species ~ .)
  if (is.null(cost)) {
    fit <- rpart(f, data=dat,  x=T, control=rpart.control(minbucket = 1, minsplit = 1, cp=cp, xval=1000, maxsurrogate = 999, usesurrogate = 2, surrogatestyle = 1), method="class")
  } else {
    fit <- rpart(f, data=dat,  control=rpart.control(minbucket = 1, minsplit = 1, cp=cp, xval=1000, maxsurrogate = 999, usesurrogate = 2, surrogatestyle = 1), x=T, cost=cost, method="class")
  }
  fit -> x
  ### get key from rpart
  ### code adapted from print.rpart
  minlength = 0L
  spaces = 2L
  digits = getOption("digits")
  frame <- x$frame
  ylevel <- attr(x, "ylevels")
  nsmall=digits
  node <- as.numeric(row.names(frame))
  tfun <- (x$functions)$print
  yval <- if (!is.null(tfun)) {
    if (is.null(frame$yval2)) {
      tfun(frame$yval, ylevel, digits, nsmall)
    } else {
      tfun(frame$yval2, ylevel, digits, nsmall)
    } 
  } else { 
    format(signif(frame$yval, digits))
    }
  tree.depth <- utils::getFromNamespace("tree.depth", "rpart")
  depth <- tree.depth(node)
  term <- rep(" ", length(depth))
  term[frame$var == "<leaf>"] <- "*"
  z <- labels(x, digits = digits, minlength = minlength)
  n <- frame$n
  species <- sub(" \\([^\\(]*$", "", yval)
  species[which(term != "*")] <- ""
  species[-which(term != "*")] -> spp.done
  ylevel[which(is.na(match(ylevel, spp.done)))] -> spp.missing
  spp.missing.sim <- vector("list", length=length(spp.missing))
  names(spp.missing.sim) <- spp.missing
  ### Get spp. unresolved
  if (length(spp.missing) > 0) {
    fit$x -> dat0
    rownames(dat0) <- ylevel[fit$y]
    for (i in 1:length(spp.missing)) {
      spp.missing[i] -> sp.m
      dat0[match(sp.m, rownames(dat0)),] -> d1
      which(is.na(d1)) -> rem0
      if (length(rem0) > 0) {
        dat0[,-rem0] -> dat0
      }
      apply(dat0, MARGIN = 1, paste, collapse="") -> d0
      d0[which(names(d0) == sp.m)] -> d1
      d0[which(is.na(match(d0, d1))==F)] -> d2
      d2[-which(names(d2) == sp.m)] -> d2
      names(d2) -> sp0
      match(sp0, species) -> sp.n
      unique(as.character(na.omit(species[sp.n]))) -> spp.missing.sim[[i]]
    }
    
    for (i in 1:length(spp.done)) {
      spp.done[i] -> sp0
      lapply(spp.missing.sim, match, sp0) -> m0
      unlist(lapply(lapply(m0, na.omit), length)) -> m0
      names(which(m0 > 0)) -> spp.sim
      if (length(spp.sim) > 0) {
        paste(spp.sim, collapse = " | ") -> spp.sim
        paste(sp0, spp.sim, sep=" | ") -> species[match(sp0, species)]
      }
    }
  }
  ### Get additional characters per step
  z[-1] -> z
  if (length(z) > 0) {
    depth[-1] -> depth.n
    names(depth.n) <- species[-1]
    if (length(depth.n) > 0) {
      max(depth.n) -> max.d
      table(depth.n) -> depth.n.t
      
      for (i in 1:max.d) {
        grep(i, depth.n) -> g0
        depth.n.t[i]/2 -> steps
        s1=1
        s2=2
        for (k in 1:steps) {
          c0 <- z[g0[s1]]
          strsplit(c0, "=")[[1]][1] -> c0
          child.l <- depth.n[g0[s1]:(g0[s2]-1)]
          child.r <- depth.n[g0[s2]:length(depth.n)]
          names(which(child.l >= i)) -> child.l
          names(which(child.r >= i)) -> child.r
          unlist(strsplit(child.l, " | ", fixed=T)) -> child.l
          unlist(strsplit(child.r, " | ", fixed=T)) -> child.r
          dat[which(is.na(match(dat[,1], child.l))==F),] -> dat.l
          dat[which(is.na(match(dat[,1], child.r))==F),] -> dat.r
          subset(dat.l, select=c(2:ncol(dat.l))) -> dat.l
          subset(dat.r, select=c(2:ncol(dat.r))) -> dat.r
          as.list(apply(dat.l, MARGIN=2, unique)) -> states.l
          as.list(apply(dat.r, MARGIN=2, unique)) -> states.r
          unlist(states.l[which(unlist(lapply(states.l, length)) == 1)]) -> states.l
          unlist(states.r[which(unlist(lapply(states.r, length)) == 1)]) -> states.r
          intersect(names(states.l),names(states.r)) -> int
          int[which(is.na(match(int, c0)))] -> int
          if (length(int) > 0) {
            states.l[match(int, names(states.l))] -> states.l
            states.r[match(int, names(states.r))] -> states.r
            which(states.l != states.r) -> keep
            if (length(keep) > 0) {
              states.l[keep] -> states.l
              states.r[keep] -> states.r
              paste(names(states.l), "=", states.l, sep="") -> states.l
              paste(names(states.r), "=", states.r, sep="") -> states.r
              paste(states.l, collapse = "; ") -> states.l
              paste(states.r, collapse = "; ") -> states.r
              tolower(states.l) -> states.l
              tolower(states.r) -> states.r
              paste(z[g0[s1]], states.l, sep="; ") -> z[g0[s1]]
              paste(z[g0[s2]], states.r, sep="; ") -> z[g0[s2]]
            } 
          }
          s1+2 -> s1
          s2+2 -> s2
        }
      }
    }
    
    ### build key
    depth[-1] -> depth
    if (clean.characters) {
      gsub("=", " ", z, fixed=T) -> z
      gsub(",(?! )", "/", z, perl=TRUE) -> z
      gsub('([[:digit:]]).([[:digit:]])','\\1XAACSDCYCASDF\\2',z) -> z
      gsub(".", " ", z, fixed=T) -> z
      gsub('([[:digit:]])XAACSDCYCASDF([[:digit:]])','\\1.\\2',z) -> z
    }
    paste("...........", species) -> species
    species[which(term != "*")] <- ""
    species[-1] -> species
    
    key.n <- depth
    table(depth) -> key.t
    n=0
    c(1:length(depth)) -> names(depth)
    for (i in 1:length(key.t)) {
      key.t[i] -> t0
      which(depth == names(t0)) -> n0
      seq(from=1, to=length(n0), by=2) -> splits0
      for (k in splits0) {
        n=n+1
        n0[k] -> k0
        n0[k+1] -> k1
        key.n[match(names(k0), names(depth))] <- n
        key.n[match(names(k1), names(depth))] <- n
      }
    }
    for (i in 1:length(key.n)) {
      paste(rep("  ", depth[i]), collapse = "") -> s0
      paste(s0, key.n[i], ")", sep="") -> key.n[i]
    }
    sub("  ", "", key.n) -> key.n
    paste(key.n, z, species) -> z
    paste(z, collapse = "\n") -> z
    
    list(rpart=fit, key=z, depth=depth.n, unresolved=spp.missing) -> z
  } else {
    list(rpart=NULL, key="Unresolved", depth=NULL, unresolved=species) -> z
  }
  return(z)
}
