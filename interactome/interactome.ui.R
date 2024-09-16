library("shinyBS")
source("uiextensions.R", local = TRUE)
source("fileupload/fileupload.ui.R")

interactome.ui = function(name = "interactome")
{
  # testoutput = paste0(name, "testoutput")
  
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
  baitseltext = paste0(name, "baitseltext")     # New set dialogue, select bait (single string)
  baitseltable = paste0(name, "baitseltable")   # New set dialogue, select bait column
  preyseltable = paste0(name, "preyseltable")   # New set dialogue, select prey column
  previewgraph = paste0(name, "previewgraph")   # New set dialogue, show graph preview of uploaded data
  resetgraph = paste0(name, "resetgraph")   # New set dialogue, show graph preview of uploaded data
  
  # Network main screen
  setlisttable = paste0(name, "setlisttable")   # List of protein sets/subgraphs
  delsetsbtn = paste0(name, "delsetsbtn")       # Delete selected sets
  clearsetsbtn = paste0(name, "clearsetsbtn")  # Clear all sets
  chklinkdbs = paste0(name, "chklinkdbs")       # Include links from HPRD/string database
  chklinktyp = paste0(name, "chklinktyp")       # Link types to show (internal/external)
  rungraphbtn = paste0(name, "rungraphbtn")     # Run graph
  
  gphorstats = paste0(name, "gphorstats")
  graphview = paste0(name, "graphview")
  hubtable = paste0(name, "hubtable")
  bottlenecktbl = paste0(name, "bottlenecktbl")
  
  # Save buttons and related
  dwnlhubs = paste0(name, "dwnlhubs")
  dwnlbtw = paste0(name, "dwnlbtw")
  dnhubtype = paste0(name, "dnhubtype")
  dnbtwtype = paste0(name, "dnbtwtype")
  
  dwnlgph = paste0(name, "dwnlgph")
  dngphtype = paste0(name, "dngphtype")
  
  gphsavetype = c("PDF" = "pdf",
                  "PNG" = "pdf",
                  "BMP" = "bmp",
                  "GraphML" = "graphml",
                  "DOT" = "dot",
                  "RData" = "rdata")

  outputdisp = paste0(name, "disp")
  displaymodes = c("none", "source", "bait", "prey", "preview")
  disp.conditions = paste0("output['", outputdisp,"'] == '", displaymodes, "'")
  names(disp.conditions) = displaymodes
  
  out = sidebarLayout(
    sidebarPanel(
      tags$h2("Protein sets"),
      helpbox("Click 'Add new protein set to begin.' Once you have added one or more sets of proteins, select whether to include links (internal to your combined list or with external proteins shared by at least 2 of your list proteins) and click 'Create graph.'"),
      actionButton(newsetbutton, label = "Add new protein set"),
      tags$hr(),
      DT::dataTableOutput(setlisttable),
      tags$br(),
      actionButton(delsetsbtn, "Delete selected"),
      actionButton(clearsetsbtn, "Clear all"),
      tags$hr(),
      # checkboxGroupInput(chklinkdbs, "Include links from these databases:",
      #                    c("String" = "string")),
      checkboxGroupInput(chklinktyp, "Include links from STRING database:",
                         c("Internal" = "int",
                           "External" = "ext")),
      actionButton(rungraphbtn, "Create graph")
    ),
    mainPanel(
      
      #################
      # NETWORK GRAPH #
      #################
      radioButtons(gphorstats, label = "Display network:", inline = TRUE,
                   choices = c("Graph" = "graph",
                               "Stats" = "stats")),
      
      # dataTableOutput(testoutput),
      
      conditionalPanel(paste0("input['", gphorstats, "'] == 'graph'"),
        forceNetworkOutput(graphview),
        actionButton(resetgraph, "Reset graph"),
        inline(style = "vertical-align: middle",
               downloadButton(dwnlgph, label = "Download network graph"), " as ",
               selectInput(dngphtype, label = "", 
                           choices = gphsavetype,
                           selected = "pdf"
                           
               )
        )
      ),
      
      conditionalPanel(paste0("input['", gphorstats, "'] == 'stats'"),
        DT::dataTableOutput(hubtable),
        inline(style = "vertical-align: middle",
               downloadButton(dwnlhubs, label = "Download hubs"), " as ",
               selectInput(dnhubtype, label = "", 
                           choices = c("excel spreadsheet" = "xls",
                                       "comma-separated values" = "csv",
                                       "tab-separated values" = "tsv",
                                       "RData file" = "rdata")
               )
        ),
 
        DT::dataTableOutput(bottlenecktbl),
        inline(style = "vertical-align: middle",
               downloadButton(dwnlbtw, label = "Download bottlenecks"), " as ",
               selectInput(dnbtwtype, label = "", 
                           choices = c("excel spreadsheet" = "xls",
                                       "comma-separated values" = "csv",
                                       "tab-separated values" = "tsv",
                                       "RData file" = "rdata")
               )
        )
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
        conditionalPanel(condition = disp.conditions[["bait"]],
          tags$h3("Enter bait (optional)"),
          textInput(baitseltext, ""),
          tags$h3("or select bait column (optional, use if list contains multiple baits)"),
          DT::dataTableOutput(baitseltable),
          selectInput(newsetidtype, label = "Select ID type for this set", choices = c(
            "Gene symbol" = "SYMBOL",
            "Entrez ID" = "ENTREZID",
            "Uniprot ID" = "UNIPROT"))
        ),
        conditionalPanel(condition = disp.conditions[["prey"]],
          tags$h3("Select prey column"),
          DT::dataTableOutput(preyseltable)
        ),
        conditionalPanel(condition = disp.conditions[["preview"]],
          tags$h3("Preview"),
          forceNetworkOutput(previewgraph)
        ),
        
        actionButton(newsetback, label = "Back"),
        actionButton(newsetnext, label = "Next")
      )
    )
  )
  
  return(out)
}