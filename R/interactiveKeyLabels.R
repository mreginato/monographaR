interactiveKeyLabels <-
function(taxon="species", language="english") {
  ### english
  if (language == "english") {
    ui.bar.title = "My taxon"
    ui.title.panel1 = "Interactive Key"
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
    ui.title.panel4 =  "Dichotomous key" 
    ui.title.dicho = "Dichotomous key"	
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
      ui.comp.title = "Families"
      ui.comp.help = "Select 1 or more families to compare their characters."
      ui.comp.dropdown = "Families"
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
    ui.bar.title = "Meu taxon"
    ui.title.panel1 = "Chave interativa"
    ui.title.panel2 = "Comparacao"
    ui.title.panel3 = "Sobre"
    ui.title.characters = "Caracteres"
    ui.instructions.characters = "Selecione os caracteres presentes no seu especime. Conforme os caracteres sao adicionados, familias que nao possuem tais caracteristicas sao eliminadas da lista. Na aba \"Comparacao\" e possivel visualizar uma tabela comparativa entre familias selecionadas."
    ui.title.taxa = "Familia(s)"
    ui.clean.button.label = "Limpa"
    ui.comp.radio = "Caracteres:"
    ui.comp.radio.choices.1 = "Distintivos"
    ui.comp.radio.choices.2 = "Semelhantes"
    ui.comp.radio.choices.3 = "Todos"
    ui.comp.main.title = "Tabela comparativa"
    server.characters.selected = "caractere(s) selecionado(s)"
    ui.title.panel4 =  "Chave dicotomica" 
    ui.title.dicho = "Chave dicotomica"	
    if (taxon == "species") {
      server.taxa.remaining = "especie(s) restante(s)"
      ui.instructions.characters = "Selecione os caracteres presentes no seu especime. Conforme os caracteres sao adicionados, especies que nao possuem tais caracteristicas sao eliminadas da lista. Na aba \"Comparacao\" e possivel visualizar uma tabela comparativa entre especies selecionadas."
      ui.title.taxa = "Especie(s)"
      ui.comp.title = "Especies"
      ui.comp.help = "Selecione 1 ou mais especies para comparar suas caracteristicas."
      ui.comp.dropdown = "Especies"
    }
    if (taxon == "family") {
      server.taxa.remaining = "familia(s) restante(s)"
      ui.instructions.characters = "Selecione os caracteres presentes no seu especime. Conforme os caracteres sao adicionados, familias que nao possuem tais caracteristicas sao eliminadas da lista. Na aba \"Comparacao\" e possivel visualizar uma tabela comparativa entre familias selecionadas."
      ui.title.taxa = "Familia(s)"
      ui.comp.title = "Familias"
      ui.comp.help = "Selecione 1 ou mais familias para comparar suas caracteristicas."
      ui.comp.dropdown = "Familias"
    }
    if (taxon == "genus") {
      server.taxa.remaining = "genero(s) restante(s)"
      ui.instructions.characters = "Selecione os caracteres presentes no seu especime. Conforme os caracteres sao adicionados, generos que nao possuem tais caracteristicas sao eliminadas da lista. Na aba \"Comparacao\" e possivel visualizar uma tabela comparativa entre generos selecionados."
      ui.title.taxa = "Genero(s)"
      ui.comp.title = "Generos"
      ui.comp.help = "Selecione 1 ou mais generos para comparar suas caracteristicas."
      ui.comp.dropdown = "Generos"
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
             ui.comp.help, ui.comp.dropdown, ui.title.panel4, ui.title.dicho, stringsAsFactors = F) -> lab.dat
  t(lab.dat) -> lab.dat
  colnames(lab.dat) <- "text"
  Encoding(lab.dat) <- rep("latin1", nrow(lab.dat))
  return(lab.dat)
}
