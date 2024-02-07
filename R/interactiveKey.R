interactiveKey <-
function(dat=NULL, txt.labels=NULL, poly.sep="/", taxa.in.italics=TRUE, theme = "lumen", about.first=FALSE) {
  if (is.null(txt.labels)) {
    interactiveKeyLabels() -> txt.labels
  }
  keyCode = NULL
  state.sep=" = "
  data("keyCode", package = "monographaR", envir = environment())
  keyCode -> int.key.code
  int.key.code$server -> server.code
  if (about.first) {
    int.key.code$ui_abf -> ui.code
  } else {
    int.key.code$ui -> ui.code
  }
  int.key.code$about -> about.code
  
  ### Split characters
  as.matrix(dat[,-c(1:2)]) -> dat.n
  matrix(ncol=ncol(dat), nrow=0) -> dat.f
  colnames(dat.f) <- colnames(dat)
  data.frame(dat.f) -> dat.f -> dat.f.m
  for (i in 1:nrow(dat.n)) {
    dat.n[i,] -> d0
    d0[is.na(d0)] <- ""
    strsplit(d0, split=poly.sep) -> c0
    unique(unlist(c0)) -> states0
    matrix(nrow=length(states0), ncol=ncol(dat)) -> d1
    colnames(d1) <- colnames(dat)
    data.frame(d1) -> d1
    d1[,1] <- dat[i,1]
    d1[,2] <- paste(dat[i,2], states0, sep=state.sep)
    d1[,3:ncol(d1)] <- 0
    for (k in 1:length(c0)) {
      c0[k] -> c1
      match(names(c1), colnames(d1)) -> col0
      c1[[1]] -> c1
      match(c1, states0) -> row0
      d1[row0,col0] <- 1
    }
    ### check missing & treat as polymorphic
    d1 -> d2
    d2[,3:ncol(d2)] <- "ok"
    colSums(d1[,-c(1,2)]) -> csums
    which(csums == 0) -> miss
    if (length(miss) > 0) {
      miss+2 -> miss
      d1[,miss] <- 1
      d2[,miss] <- NA
    }
    ### return
    rbind(dat.f, d1) -> dat.f
    rbind(dat.f.m, d2) -> dat.f.m
  }
  ### Generate Data files
  ### Chars
  dat.chars <- matrix(ncol=4, nrow=nrow(dat.f))
  colnames(dat.chars) <- c("ID", "Character", "Check.box.label", "Check.box.id")
  data.frame(dat.chars) -> dat.chars
  dat.chars$ID <- c(1:nrow(dat.chars))
  dat.chars$Character <- dat.f$Character
  dat.chars$Check.box.label <- dat.f$Group
  dat.chars$Check.box.id <- paste("cb",1:nrow(dat.f), sep="")
  write.csv(dat.chars, file = "Dat_characters.csv", row.names = F)
  
  ### Matrix
  
  data.frame(ID=dat.chars$ID, dat.f[,-c(1:2)]) -> mat
  data.frame(ID=dat.chars$ID, dat.f.m[,-c(1:2)]) -> mat.m
  write.csv(mat, file="Dat_matrix.csv", row.names = F)
  write.csv(mat.m, file="Dat_matrix_missing.csv", row.names = F)

  ### Generate app.R
  
  file = "app.R"
  cat("library(shiny)\n", "library(shinyjs)\n", "library(shinyTree)\n", "library(shinyWidgets)\n", "library(shinythemes)\n", sep="", fill=F, file=file)
  cat("\nshinyApp(ui = \"ui.R\", server = \"server.R\")", sep="", file=file, fill=T, append=T)
  
  # Generate server.R
  
  file = "server.R"
  cat("library(shiny)\n", "library(shinyjs)\n", "library(shinyTree)\n", "library(shinyWidgets)\n", "library(shinythemes)\n", sep="", fill=F, file=file)
  cat("\ntaxa.in.italics = ", taxa.in.italics, sep="", file=file, append=T)
  cat("\nserver.taxa.remaining = ", "'", txt.labels[1,1], "'", sep="", file=file, append=T)
  cat("\nserver.characters.selected = ", "'", txt.labels[2,1], "'", sep="", file=file, append=T)
  cat("\n ", sep="", fill=T, file=file, append=T)
  cat(server.code, sep="\n", file=file, append=T)
  
  ### Generate ui.R
  file = "ui.R"
  cat("library(shiny)\n", "library(shinyjs)\n", "library(shinyTree)\n", "library(shinyWidgets)\n", "library(shinythemes)\n", sep="", fill=F, file=file)
  cat("\ntheme = ", "'", theme, "'", sep="", file=file, append=T)
  cat("\nui.bar.title = ", "'", txt.labels[3,1], "'", sep="", file=file, append=T)
  cat("\nui.title.panel1 = ", "'", txt.labels[4,1], "'", sep="", file=file, append=T)
  cat("\nui.title.panel2 = ", "'", txt.labels[5,1], "'", sep="", file=file, append=T)
  cat("\nui.title.panel3 = ", "'", txt.labels[6,1], "'", sep="", file=file, append=T)
  cat("\nui.title.characters = ", "'", txt.labels[7,1], "'", sep="", file=file, append=T)
  cat("\nui.instructions.characters = ", "'", txt.labels[8,1], "'", sep="", file=file, append=T)
  cat("\nui.title.taxa = ", "'", txt.labels[9,1], "'", sep="", file=file, append=T)
  cat("\nui.clean.button.label = ", "'", txt.labels[10,1], "'", sep="", file=file, append=T)
  cat("\nui.comp.radio = ", "'", txt.labels[11,1], "'", sep="", file=file, append=T)
  cat("\nui.comp.radio.choices.1 = ", "'", txt.labels[12,1], "'", sep="", file=file, append=T)
  cat("\nui.comp.radio.choices.2 = ", "'", txt.labels[13,1], "'", sep="", file=file, append=T)
  cat("\nui.comp.radio.choices.3 = ", "'", txt.labels[14,1], "'", sep="", file=file, append=T)
  cat("\nui.comp.main.title = ", "'", txt.labels[15,1], "'", sep="", file=file, append=T)
  cat("\nui.comp.title = ", "'", txt.labels[16,1], "'", sep="", file=file, append=T)
  cat("\nui.comp.help = ", "'", txt.labels[17,1], "'", sep="", file=file, append=T)
  cat("\nui.comp.dropdown = ", "'", txt.labels[18,1], "'", sep="", file=file, append=T)
  cat("\nui.title.panel4 = ", "'", txt.labels[19,1], "'", sep="", file=file, append=T)
  cat("\nui.title.dicho = ", "'", txt.labels[20,1], "'", sep="", file=file, append=T)
  cat("\n ", sep="", fill=T, file=file, append=T)
  cat(ui.code, sep="\n", file=file, append=T)
  
  ### Generate about.Rmd
  file = "about.Rmd"
  cat(about.code, sep="\n", file=file)
  
}
