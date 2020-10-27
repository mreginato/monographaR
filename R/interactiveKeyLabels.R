interactiveKeyLabels <-
function(taxon="species", language="english") {
  ### english
  if (language == "english") {
    ui.bar.title = "Interactive Key"
    ui.title.panel1 = "Identification"
    ui.title.panel2 = "Comparison"
    ui.title.panel3 = "About"
    ui.title.characters = "Characters"
    ui.clean.button.label = "Clean"
    ui.comp.radio = "Characters:"
    ui.comp.radio.choices.1 = "Distinctive"
    ui.comp.radio.choices.2 = "Similar"
    ui.comp.radio.choices.3 = "All"
    ui.comp.main.title = "Comparative table"
    server.characters.selected = "selected characters"
    if (taxon == "species") {
      server.taxa.remaining = "remaining species"
      ui.instructions.characters = "Select the characters present in your specimen. As characters are selected, species are excluded from the list (right panel). In the option \"Comparison\" it is possible to visualize a comparative table of selected species."
      ui.title.taxa = "Species"
      ui.comp.title = "Species"
      ui.comp.help = "Select 1 or more species to compare their characters."
      ui.comp.dropdown = "Species"
    }
    if (taxon == "family") {
      server.taxa.remaining = "remaining families"
      ui.instructions.characters = "Select the characters present in your specimen. As characters are selected, families are excluded from the list (right panel). In the option \"Comparison\" it is possible to visualize a comparative table of selected families."
      ui.title.taxa = "Families"
      ui.comp.title = "Famílies"
      ui.comp.help = "Select 1 or more families to compare their characters."
      ui.comp.dropdown = "Famílies"
    }
    if (taxon == "genus") {
      server.taxa.remaining = "remaining genera"
      ui.instructions.characters = "Select the characters present in your specimen. As characters are selected, genera are excluded from the list (right panel). In the option \"Comparison\" it is possible to visualize a comparative table of selected genera."
      ui.title.taxa = "Genera"
      ui.comp.title = "Genera"
      ui.comp.help = "Select 1 or more genera to compare their characters."
      ui.comp.dropdown = "Genera"
    }
  }
  ### portuguese
  if (language == "portuguese") {
    ui.bar.title = "Chave interativa"
    ui.title.panel1 = "Identificação"
    ui.title.panel2 = "Comparação"
    ui.title.panel3 = "Sobre"
    ui.title.characters = "Caracteres"
    ui.instructions.characters = "Selecione os caracteres presentes no seu espécime. Conforme os caracteres são adicionados, famílias que não possuem tais características são eliminadas da lista. Na aba \"Comparação\" é possível visualizar uma tabela comparativa entre famílias selecionadas."
    ui.title.taxa = "Família(s)"
    ui.clean.button.label = "Limpa"
    ui.comp.radio = "Caracteres:"
    ui.comp.radio.choices.1 = "Distintivos"
    ui.comp.radio.choices.2 = "Semelhantes"
    ui.comp.radio.choices.3 = "Todos"
    ui.comp.main.title = "Tabela comparativa"
    server.characters.selected = "caractere(s) selecionado(s)"
    if (taxon == "species") {
      server.taxa.remaining = "espécie(s) restante(s)"
      ui.instructions.characters = "Selecione os caracteres presentes no seu espécime. Conforme os caracteres são adicionados, espécies que não possuem tais características são eliminadas da lista. Na aba \"Comparação\" é possível visualizar uma tabela comparativa entre espécies selecionadas."
      ui.title.taxa = "Espécie(s)"
      ui.comp.title = "Espécies"
      ui.comp.help = "Selecione 1 ou mais espécies para comparar suas características."
      ui.comp.dropdown = "Espécies"
    }
    if (taxon == "family") {
      server.taxa.remaining = "família(s) restante(s)"
      ui.instructions.characters = "Selecione os caracteres presentes no seu espécime. Conforme os caracteres são adicionados, famílias que não possuem tais características são eliminadas da lista. Na aba \"Comparação\" é possível visualizar uma tabela comparativa entre famílias selecionadas."
      ui.title.taxa = "Família(s)"
      ui.comp.title = "Famílias"
      ui.comp.help = "Selecione 1 ou mais famílias para comparar suas características."
      ui.comp.dropdown = "Famílias"
    }
    if (taxon == "genus") {
      server.taxa.remaining = "gênero(s) restante(s)"
      ui.instructions.characters = "Selecione os caracteres presentes no seu espécime. Conforme os caracteres são adicionados, gêneros que não possuem tais características são eliminadas da lista. Na aba \"Comparação\" é possível visualizar uma tabela comparativa entre gêneros selecionados."
      ui.title.taxa = "Gênero(s)"
      ui.comp.title = "Gêneros"
      ui.comp.help = "Selecione 1 ou mais gêneros para comparar suas características."
      ui.comp.dropdown = "Gêneros"
    }
    
  }
  
  ### spanish
  if (language == "spanish") {
    if (taxon == "species") {
      
    }
    if (taxon == "family") {
      server.taxa.remaining = "remaining families"
      
    }
    if (taxon == "genus") {
      
    }
  }
  ### data.frame
  data.frame(server.taxa.remaining, server.characters.selected,
             ui.bar.title, ui.title.panel1, ui.title.panel2,
             ui.title.panel3, ui.title.characters, ui.instructions.characters,
             ui.title.taxa, ui.clean.button.label, ui.comp.radio, ui.comp.radio.choices.1, ui.comp.radio.choices.2,
             ui.comp.radio.choices.3, ui.comp.main.title, ui.comp.title,
             ui.comp.help, ui.comp.dropdown, stringsAsFactors = F) -> lab.dat
  t(lab.dat) -> lab.dat
  colnames(lab.dat) <- "text"
  Encoding(lab.dat) <- rep("latin1", nrow(lab.dat))
  return(lab.dat)
}
