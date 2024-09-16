library("shinyBS")
source("uiextensions.R", local = TRUE)
source("fileupload/fileupload.ui.R")
library("ggplot2")
library("plotly")

pathwayanalysis.ui = function(name = "hypergeometric")
{
  newsetbutton = paste0(name, "newsetbutton")   # Create a new set
  newsetpopup = paste0(name, "newsetpopup")     # Modal (popup dialogue) to create a new set
  newsetback = paste0(name, "newsetback")       # New set dialogue next button
  newsetnext = paste0(name, "newsetnext")       # New set dialogue back button
  newsetname = paste0(name, "newsetname")       # New set dialogue name input
  newsetspecies = paste0(name, "newsetspecies") # New set dialogue species input
  dispspec = paste0(name, "dispspec")           # Conditional variable for displaying species input
  dispspeccond = paste0("output['", dispspec, "'] == 'true'") # Conditional expression for displaying species input
  newsetidtype = paste0(name, "newsetidtype")   # New set ID type input (Entrez, Symbol etc)
  siglistseltbl = paste0(name, "siglistseltbl") # Table for selecting significance list
  uploadorsig = paste0(name, "uploadorsig")     # radiobutton - upload data or draw from significance list
  fileuploadnm = paste0(name, "fu")             # New set dialogue file upload
  protseltable = paste0(name, "protseltable")   # New set dialogue, select prot column
  setpreviewtable = paste0(name, "setpreviewtable")   # New set dialogue, show graph preview of uploaded data
  
  # main screen
  setlisttable = paste0(name, "setlisttable")   # List of protein sets/subgraphs
  delsetsbtn = paste0(name, "delsetsbtn")       # Delete selected sets
  clearsetsbtn = paste0(name, "clearsetsbtn")  # Clear all sets
  chklinkdbs = paste0(name, "chklinkdbs")       # Include links from HPRD/string database
  chklinktyp = paste0(name, "chklinktyp")       # Link types to show (internal/external)
  runfshbtn = paste0(name, "runfshbtn")     # Run graph
  
  fsh.rdy = paste0(name, "fsh.rdy")
  tblorplot = paste0(name, "tblorplot")
  fsh.table = paste0(name, "fsh.table")
  fsh.result.plot = paste0(name, "fsh.result.plot")
  downloadfisher = paste0(name, "downloadfisher")
  downloadtype = paste0(name, "downloadtype")
  download.fsh.png = paste0(name, "download.fsh.png")
  download.fsh.eps = paste0(name, "download.fsh.eps")
  
  saveresults = paste0(name, "saveresults")
  savename = paste0(name, "savename")
  saveowner = paste0(name, "saveowner")
  
  outputdisp = paste0(name, "disp")
  displaymodes = c("none", "source", "prot", "preview")
  disp.conditions = paste0("output['", outputdisp,"'] == '", displaymodes, "'")
  names(disp.conditions) = displaymodes
  
  out = sidebarLayout(
    sidebarPanel(
      tags$h2("Protein sets"),
      helpbox("Click 'Add new protein set to begin.' Once you have added one or more sets of proteins, click 'Run pathway enrichment test' to perform hypergeometric pathway analysis."),
      actionButton(newsetbutton, label = "Add new protein set"),
      tags$hr(),
      DT::dataTableOutput(setlisttable),
      tags$br(),
      actionButton(delsetsbtn, "Delete selected"),
      actionButton(clearsetsbtn, "Clear all"),
      tags$hr(),
      actionButton(runfshbtn, "Run pathway enrichment test")
    ),
    mainPanel(
      
      #################
      # Fisher Table  #
      #################
      conditionalPanel(paste0("output['", fsh.rdy, "'] == 'true'"),
        radioButtons(tblorplot, label = "Display results as:", inline = TRUE,
          choices = c("Table" = "table",
            "Plot" = "plot")),
        conditionalPanel(paste0("input['", tblorplot, "'] == 'table'"),
          DT::dataTableOutput(fsh.table),
          inline(style = "float: middle",
            downloadButton(downloadfisher, label = "Download table"), " as ",
            selectInput(downloadtype, label = "", 
              choices = c("excel spreadsheet" = "xls",
                "comma-separated values" = "csv",
                "tab-separated values" = "tsv",
                "RData file" = "rdata")
            )
          )
        ),
        conditionalPanel(paste0("input['", tblorplot, "'] == 'plot'"),
          fluidRow(
            column(width = 11),
            column(width = 1,
              helpbox("click and drag over a region of this plot to zoom in. Double-click or click the home icon to return to full view. To save the zoomed plot as a PNG, click the camera icon.")
            )
          ),
          fluidRow(
            plotlyOutput(fsh.result.plot),
            div(style = "float: right", downloadButton(download.fsh.png, label = "Save as PNG")),
            div(style = "float: right", downloadButton(download.fsh.eps, label = "Save as EPS"))
          )
        ),
        
        tags$hr(),
        tags$h3("Save pathway list to server"),
        inline(textInput(savename, "Name"), textInput(saveowner, "Owner")),
        actionButton(saveresults, "Save")
      ),
      
      ##################
      # INPUT DIALOGUE #
      ##################
      bsModal(newsetpopup, "New protein set", newsetbutton, size = "large",
        conditionalPanel(condition = disp.conditions[["source"]],
          tags$h3("Data source"),
          textInput(newsetname, label = "Enter a name"),
          radioButtons(uploadorsig, "Upload data or obtain from significance list?",
            choices = c("Upload" = "upload", "DE Significance list" = "sig", "Load a saved protein list" = "protlist")),
          conditionalPanel(condition = paste0("input['", uploadorsig,"'] == 'upload'"),
            fileupload.ui(name = fileuploadnm)
          ),
          conditionalPanel(condition = paste0("input['", uploadorsig,"'] == 'sig' || input['", uploadorsig,"'] == 'protlist'"),
            DT::dataTableOutput(siglistseltbl)
          ),
          conditionalPanel(dispspeccond,
            selectInput(newsetspecies, label = "Select species (same for all sets)",
              choices = c("Homo sapiens" = "hsa", "Mus Musculus" = "mmu"))
          )
        ),
        conditionalPanel(condition = disp.conditions[["prot"]],
          tags$h3("Select column"),
          DT::dataTableOutput(protseltable)
        ),
        conditionalPanel(condition = disp.conditions[["preview"]],
          tags$h3("Preview"),
          DT::dataTableOutput(setpreviewtable),
          selectInput(newsetidtype, label = "Select ID type for this set", choices = c(
            "Gene symbol" = "SYMBOL",
            "Entrez ID" = "ENTREZID",
            "Uniprot ID" = "UNIPROT"))
        ),
        
        actionButton(newsetback, label = "Back"),
        actionButton(newsetnext, label = "Next")
      )
    )
  )
  
  return(out)
}