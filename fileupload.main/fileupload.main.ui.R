### Uploader ###

upload.step = list()

# Step 1: Restore file
restore.panel = tagList(
  selectizeInput("select.load.owners", label = "Specify owners (optional)", choices = NULL, selected = NULL, multiple = T),
  selectInput("select.load.dataset", label = "Select a dataset", choices = NULL, selected = NULL, multiple = F),
  selectInput("select.load.table", label = "Select a table", choices = NULL, selected = NULL, multiple = F),
  checkboxInput('chk.load.header', 'Header row', TRUE),
  checkboxInput('chk.load.transpose', 'Transpose', FALSE),
  conditionalPanel(condition = "output['deletevisible'] == 'TRUE'",
    actionButton("od.delete.dataset", "Delete this dataset")
  ),
  conditionalPanel(condition = "output['deletesurevisible'] == 'TRUE'",
    tags$h4("Delete - are you sure?"),
    inline(
      actionButton("od.delete.yes", "Yes, delete!"),
      actionButton("od.delete.no", "Cancel")
    ),
    tags$br()
  )
)

# Step 1: Upload file
upload.step$"1.upload" = tagList(
  fileInput('up.file', 'Choose file to upload',
    accept = c(
      '.xls',
      '.xlsx',
      '.txt',
      '.csv',
      '.tsv',
      '.RData'
    )
  ),
  tags$br(),
  
  ############
  
  selectInput ("select.filetype", label = "Select a filetype",
    choices = c("Detect automatically" = "auto",
      "Excel spreadsheet" = "xlsx",
      "Tab-separated" = "tsv",
      "Comma-separated" = "csv",
      "RData file" = "rdata",
      "Custom delimiter" = "select"
    )
  ),
  conditionalPanel("input['select.filetype'] == 'select'",
    textInput("customDelimiter", label = "Enter a custom delimiter:", value = " ")
  ),
  selectInput("select.table", label = "", choices = NULL, multiple = FALSE),
  checkboxInput('header', 'Header row', TRUE),
  checkboxInput('transpose', 'Transpose', FALSE),
  textInput("stringquote", label = "String quotes", "\"")
)

upload.step$"2.fileinfo" = tagList(
  textInput("save.name", "Enter a short name", value = ""),
  textInput("save.owner", label = "Enter a new owner or select an existing owner below."),
  selectInput('owner.list', label="Select an existing owner", choices = NULL, multiple = FALSE),
  selectInput("save.organism", label = "Organism",
    choices = c(
      "Homo sapiens (human)" = "hsa",
      "Mus musculus (mouse)" = "mmu",
      "Rattus norvegicus (rat)" = "rno",
      "Drosophila melanogaster (fly)" = "dme"
    ), multiple = FALSE
  ),
  tags$label("Enter a description"),
  tags$textarea(id = "save.description", rows = 10, style = "width: 100%; font-size: 75%")
)

upload.step$"3.dstype" = radioButtons("sel.datatype", label = "Save as...",
  choices = c(
    "Raw data (whole file)" = "rawfile",
    "Raw data (just the displayed table)" = "rawtable",
    "Processed data" = "proctable",
    "Results table" = "results",
    "List of interesting proteins" = "protlist"
  )
)

upload.step$"4.results" = tagList(
  textInput("res.cont.name", label = "Contrast name"),
  textInput("res.contrast", label = "Contrast formula"),
  DT::dataTableOutput("res.col.selector")
)

upload.step$"4.protlist" = tagList(
  radioButtons("protlist.from.analysis", "Include analysis details", choices = c("Yes" = "true", "No" = "false")),
  conditionalPanel(condition = "input['protlist.from.analysis'] == 'true'",
    numericInput("protlist.p.cutoff", label = "Select p-value cutoff", min = 0, max = 0.1, value = 0.05, step = 0.0001),
    radioButtons("protlist.p.type", label = "Normal or adjusted p-values", choices = c("Normal" = "p.value", "Adjusted" = "adj.p.val"), selected = "adj.p.val"),
    radioButtons("protlist.lfc.comp", label = "Select log-fc direction", choices = c("Up" = "greater", "Down" = "less", "Both" = "both" ), selected = "both", inline = T),
    numericInput("protlist.lfc.cutoff", label = "Select log-fc cutoff", min = -5, max = 5, value = 1, step = 0.25)
  ),
  DT::dataTableOutput("protlist.col.selector")
)

step.CP.list = lapply(names(upload.step), function(nm) {
  cond = paste0("output['upl.step'] == '", nm, "'")
  out = conditionalPanel(condition = cond, upload.step[[nm]])
  return(out)
})

step.CP = tagList(step.CP.list)

fileupload.main.ui = sidebarLayout(
  sidebarPanel(width = 3,
    # helpbox(
    #   "In this tab, you may upload a tabular file containing mass spectrometry intensities or interesting proteins.
    # 
    # - To temporarily upload a table of MS intensitites and immediately begin exploration and/or analysis, click 'Upload file and explore/analyse'.
    # - To save a table of MS intensities or interesting proteins to the server for further use, click 'Upload file and save to server'.
    # - To restore a table of MS intensities from the server for exploration and/or analysis, click 'Restore file from server and explore/analyse'."),
    fluidRow(
      tabsetPanel(id = "fu.tabsetpanel",
        tabPanel(title = "File upload", value = "upload.save",
          tags$br(),
          step.CP,
          fluidRow(
            column(1),
            column(5,
              conditionalPanel(condition = 'output["upl.disp.prev"] == "TRUE"',
                actionButton("upload.prev", "Back", width = "100%")
              )
            ),
            column(5,
              conditionalPanel(condition = 'output["upl.disp.next"] == "TRUE"',
                actionButton("upload.next", "Next", width = "100%")
              ),
              conditionalPanel(condition = 'output["upl.disp.save"] == "TRUE"',
                actionButton("upload.save", "Save", width = "100%")
              )
            ),
            column(1)
          )
        ),
        tabPanel(title = "Restore dataset", value = "restore",
          tags$br(),
          restore.panel
        )
      ),
      tags$br(),
      conditionalPanel(condition = "output['fileman.data.loaded'] == 'true'",
        actionButton("fileman.startde", label = "Load dataset into DE Pipeline", width = "100%")
      )
    )
  ),
  mainPanel(
    div(style = "overflow-x: scroll; overflow-y: hidden", DT::dataTableOutput("load.data.preview"), tags$br(), tags$br()),
    tags$table(width = "40%", style = "table-layout: fixed",
      tags$tr(
        tags$td(tags$h4("Number of rows:", style = "text-align: left"), width = "25%"),
        tags$td(tags$h4(textOutput("load.data.numrows"), style = "text-align: right"), width = "25%")        
      ),
      tags$tr(
        tags$td(tags$h4("Number of columns:", style = "text-align: left"), width = "25%"),
        tags$td(tags$h4(textOutput("load.data.numcols"), style = "text-align: right"), width = "25%")
      )
    )
  )
)