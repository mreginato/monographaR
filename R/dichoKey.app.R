dichoKey.app <-
function() {
  
  
  
  #############################################
  ### server
  #############################################
  
  server <- function(input, output, session) { 
    
    ### csv
    df <- reactive({
      input$na.string -> na
      if (na == "empty") {na = ""}
      infile <- input$datafile
      req(infile) 
      read.csv(infile$datapath, header = TRUE, na.strings = na)
    })
    
    ### taxa 
    output$taxadrop <- renderUI({
      df() -> dat
      taxa.choices <- as.character(dat[,1])
      checkboxGroupInput(inputId = 'taxa.sel', label = "", choices = taxa.choices, selected = taxa.choices, inline=T)
      
    })
    
    ### characters
    
    output$chardrop <- renderUI({
      df() -> dat
      char.choices <- colnames(dat)[-1]
      checkboxGroupInput(inputId = 'chars.sel', label = "", choices = char.choices, selected = char.choices, inline=T)
      
    })
    
    ### costs
    
    output$sliders <- renderUI({
      df() -> dat
      subset(dat, select=c(2:ncol(dat))) -> dat
      colnames(dat) -> labs
      numSliders <- ncol(dat)
      lapply(1:numSliders, function(i) {
        sliderInput(
          inputId = labs[i],
          label = labs[i],
          min = 0.1,
          max = 2,
          value = 1,
          step = 0.1,
          width='100%')
      })
    })
    
    ### select all taxa
    observe({
      df() -> dat
      taxa.choices <- as.character(dat[,1])
      if (input$selectall.taxa == 0) { return(NULL) } 
      else if (input$selectall.taxa%%2 == 0) {
        updateCheckboxGroupInput(session,inputId="taxa.sel", label="", choices=taxa.choices, selected=NULL, inline = T)
      } else {
        updateCheckboxGroupInput(session,inputId="taxa.sel", label="", choices=taxa.choices, selected=taxa.choices, inline = T)
      }
    })
    
    ### select all chars
    observe({
      df() -> dat
      char.choices <- colnames(dat)[-1]
      if (input$selectall.chars == 0) { return(NULL) } 
      else if (input$selectall.chars%%2 == 0) {
        updateCheckboxGroupInput(session,inputId="chars.sel", label="", choices=char.choices, selected=NULL, inline = T)
      } else {
        updateCheckboxGroupInput(session,inputId="chars.sel", label="", choices=char.choices, selected=char.choices, inline = T)
      }
    })
    
    
    ### key
    observeEvent(c(input$chars.sel, input$taxa.sel, input$drop1, input$sliders.b), {
      input$chars.sel -> chars.keep
      input$taxa.sel -> spp.keep
      input$poly.sep.string -> poly.sep
      df() -> dat
      as.character(dat[,1]) -> spp
      subset(dat, select=c(2:ncol(dat))) -> dat.c
      colnames(dat.c) -> labs
      costs.k <- unlist(lapply(labs, function(x) input[[x]]))
      
      if (is.null(costs.k[1])) {
        rep(1, ncol(dat.c)) -> costs.k
        names(costs.k) <- colnames(dat.c)
      } else {
        names(costs.k) <- labs
      }
      
      ### subset characters
      match(chars.keep, colnames(dat.c)) -> keep.cols
      subset(dat.c, select = keep.cols) -> dat.c
      costs.k[keep.cols] -> costs.k
      data.frame(spp, dat.c) -> dat.c
      ### subset spp
      dat.c[match(spp.keep, spp),] -> dat.c
      
      ### dataKey
      
      if (inherits(ncol(dat.c), "integer")) {
        if (nrow(dat.c) > 1 && ncol(dat.c) > 1) {
          dataKey(dat.c, poly.sep = poly.sep) -> dat.out
          dichoKey(dat.out$dat, cost=costs.k) -> key.out
          dat.out$summary -> sum.missing
          t(data.frame(sum.missing)) -> sum.missing
          key.out$unresolved -> unresolved
          key.out$rpart -> tree.plot
          paste(key.out$key, sep="\n") -> key.out
          stats.out <- data.frame("Characters selected"=ncol(dat.c)-1, "Taxa selected"=nrow(dat.c), "Unresolved taxa"=length(unresolved))
        } else {
          unresolved = NULL
          sum.missing = NULL
          key.out = ""
          stats.out = NULL
          costs.k = NULL
          tree.plot = NULL
        }
      } else {
        unresolved = NULL
        sum.missing = NULL
        key.out = ""
        stats.out = NULL
        costs.k = NULL
        tree.plot = NULL
      }
      
      ### output
      output$dichokey <- renderText( {key.out} )
      output$stats.key <- renderTable( {stats.out}, rownames = F )
      output$missing.stats <- renderTable( {sum.missing}, rownames = F )
      output$costs <- renderTable({t(data.frame(costs.k))}, rownames = F)
      output$unresolvedtaxa <- renderTable( {data.frame(unresolved)}, rownames = F, colnames = F)
      output$tree <- renderPlot( {
        if (is.null(tree.plot) ==F) {
          plot(tree.plot)
          text(tree.plot, use.n = F, pretty=T, fancy=F)
        }
      })
      output$downloadKey <- downloadHandler(
        filename = function() { paste('Key-', Sys.Date(), '.txt', sep='') },
        content = function(con) {
        cat(key.out, file=con)
        }
      )
      
    })
    
  }
  

  #############################################
  ### input - options
  #############################################
  
  na.options = c("empty", "NA", "?")
  poly.sep.options = c("/", "&", "|")
  
  #############################################
  ### UI
  #############################################
  
  ui <- fluidPage(
    theme = shinytheme("cosmo"),
    title="dichoKey",
    titlePanel(h2("dichoKey", align = "left")),
    ### Input
    fluidRow(
      #tags$hr(),
      wellPanel(
        h3("Input", align = "center"),
      ),
      #tags$hr(),
      column(4, 
             fileInput('datafile', 'Select a .CSV File', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      ),
      column(4, 
             radioButtons(inputId = 'na.string',
                          label = "Missing data",
                          choices = na.options, selected=na.options[1], inline=T
             ),
             helpText('\n      ', '\n     '),
             helpText('Character coding missing values'),
      ),
      column(4,
             radioButtons(inputId = 'poly.sep.string',
                          label = "Polymorphic",
                          choices = poly.sep.options, selected=poly.sep.options[1], inline=T
             ),
             helpText('\n      ', '\n     '),
             helpText('Character separeting polymophic'),
      ),
    ),
    
    div(class = "inlay", style = "height:15px;width:100%;background-color:white;"),
    
    ### taxa
    fluidRow(
      column(12,
             wellPanel(
               h3("Taxa", align="center"),
               div(
                 uiOutput("taxadrop"),
                 align="center"
               ),
               div(
                 actionLink("selectall.taxa", "Select All"),
                 align="center"
               ),
             )
      ),
    ),
    
    div(class = "inlay", style = "height:15px;width:100%;background-color:white;"),
    
    
    fluidRow(
      ### characters
      column(7,
             wellPanel(
               h3("Characters", align="center"),
               div(
                 uiOutput("chardrop"),
                 align="center"
               ),
               div(
                 actionLink("selectall.chars", "Select All"),
                 align="center"
               ),
             )
      ),
      
      ### Costs
      column(5,
             wellPanel(
               h3("Costs", align="center"),
               div(
                 tags$style("#drop1 {color:black;}"),
                 dropdown(
                   actionBttn(inputId="sliders.b", label="Confirm changes", style="pill", width="95%", block=T),
                   uiOutput(outputId = "sliders", inline=F, inputId="teste"),
                   size="md",
                   style="pill",
                   width="95%",
                   label="   Change here   ",
                   inputId = "drop1",
                   block = T,
                   
                 ),
                 align="center"
               ),
             ),
      ),
      
    ), 
    
    ### key
    fluidRow(
      wellPanel(
        h3("Key", align="center"),
      ),
      tabBox(
        tabPanel("Key", 
                 verbatimTextOutput("dichokey"),
                 tags$head(tags$style("#dichokey{color:black; 
                                                font-size:13px; 
                                                overflow-y:scroll; 
                                                max-height: 100%; 
                                                max-width: 100%; 
                                                line-height: 200%;
                                                background: white;}")),
                 
                 
                 ),
        tabPanel("Tree",
                 plotOutput("tree")
                 ),
        width=12),
    ),
    
    ### download key 
    
    div(class = "inlay", style = "height:15px;width:100%;background-color:white;"),
    fluidRow(
      wellPanel(
        downloadLink("downloadKey", "Download Key"),
      align="center"),
    ),
    
    ### stats
    
    #div(class = "inlay", style = "height:15px;width:100%;background-color:white;"),
    fluidRow(  
      wellPanel(
        h3("Stats", align="center"),
      ),
      div(
        tableOutput("stats.key"),
        align="center",
      ),
      
      div(
        helpText('Percentage of spp. with polymorphic/missing data per character'), 
        tableOutput("missing.stats"),
        align="center",
      ),
      
      div(
        helpText('Costs'),
        tableOutput("costs"),
        align="center",
      ),
      
      div(
        helpText('List of unresolved taxa'),
        tableOutput("unresolvedtaxa"),
        align="center",
      ),
      
    )
  )
    
  #############################################
  ### app
  #############################################
  
  shinyApp(ui, server)

}
