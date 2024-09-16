search.ui = tabPanel(title = "Search",
  fluidRow(
    column( width = 4,
      tags$h3("\nSearch by metadata\n"),
      helpbox("Search by name or owner of dataset."),
      tags$hr(),
      radioButtons("search.data.type", label = "Search type", inline = TRUE,
        choices = c(
          # "Datasets" = 'datasets',
          "Analysis results" = 'results',
          "Protein lists" = 'protlists'
        )
      ),
      selectizeInput("search.field.name", label = "Name", choices = NULL, selected = NULL, multiple = TRUE),
      selectizeInput("search.field.owner", label = "Owner", choices = NULL, selected = NULL, multiple = TRUE),
      # textInput("search.field.long", label = "Long description"),
      # textInput("search.field.keywords", label = "keywords"),
      
      conditionalPanel(condition = "input['search.data.type'] == 'Datasets'",
        textInput("search.field.data", label = "Data (any)")
      )
    ),
    column( width = 4,
      conditionalPanel(condition = "input['search.data.type'] == 'results' || input['search.data.type'] == 'protlists'",
        tags$h3("\nSearch by protein/gene ID"),
        helpbox("Search for datasets containing a particular protein label."),
        tags$hr(),
        tags$strong("Separate IDs or ID substrings by commas or semicolons; spaces are ignored.\n"),
        tags$strong("e.g. 'akap1; mtor'\n\n"),
        tags$br(),
        tags$br(),
        textInput("search.field.gene", label = "Gene symbol"),
        textInput("search.field.uniprot", label = "Uniprot ID"),
        textInput("search.field.entrez", label = "Entrez ID"),
        textInput("search.field.misc", label = "Miscellaneous descriptive data")
      )  
    ),
    column( width = 4,
      conditionalPanel(condition = "input['search.data.type'] == 'results'",
        tags$h3("\nSearch by analysis output"),
        helpbox("Search for results conforming to specific significance criteria."),
        tags$hr(),
        numericInput("search.field.p", label = "P value threshold", min = 0, max = 1, value = 0.05, step = 0.001),
        radioButtons("search.p.rawadj", label = "P value type", choices = c("Unadjusted" = "raw", "Adjusted" = "adj", "Exclude from search" = "NA"), selected = "NA"),
        numericInput("search.field.logfc", label = "Log fold change", value = 0, step = 0.1),
        radioButtons("search.lfc.dir", label = "", choices = c("Up" = "up", "Down" = "down", "Both" = "both", "Exclude from search" = "NA"), selected = "NA")
      )    
    )
  ),
  
  actionButton("search.button", "Search"),

  tags$hr(),
  tags$h3("Search results"),
  DT::dataTableOutput("search.meta.hits"),
  tags$br(),
  conditionalPanel(condition = "output['search.deletevisible'] == 'TRUE'",
    actionButton("search.delete.dataset", "Delete selected datasets")
  ),
  conditionalPanel(condition = "output['search.deletesurevisible'] == 'TRUE'",
    tags$h4("Delete - are you sure?"),
    inline(
      actionButton("search.delete.yes", "Yes, delete!"),
      actionButton("search.delete.no", "Cancel")
    ),
    tags$br()
  ),
  tags$hr(),
  tags$h3("Data"),
  div(style = "float:right",
    checkboxInput("search.disp.flt", label = "Display only search-relevant rows", value = TRUE),
    checkboxInput("search.spec.col", label = "Display only special columns", value = TRUE)    
  ),
  DT::dataTableOutput("search.show.data"),
  div(style="float:right",
    tags$br(),
    conditionalPanel(condition = "input['search.spec.col'] == true",
      inline(
        actionButton("search.save.protlist", "Save to server as protein list"),
        " with name ",
        textInput("search.save.protlist.name", label = ""),
        " under owner ",
        textInput("search.save.protlist.owner", label = "")
      )
    ),
    
    tags$h5("Select column(s) or leave blank to download whole table."),
    downloadButton("search.export.data"),
    " as ",
    selectInput("search.export.filetype", label = "Download table",
      choices = c("Excel" = "xls", "Tab-delimited" = "tsv", "Comma-separated" = "csv", "RData" = "rdata"))
  )
)