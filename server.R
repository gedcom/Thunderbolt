##############
### SERVER ###
##############

options(shiny.maxRequestSize = 200*1024^2)
library("readxl")
library("writexl")
library("parallel")
library("limma")
library("impute")
library("networkD3")
library("DT")
library("shinyjs")
library("plotly")
library("reshape2")
library("ggdendro")

do.serverside = TRUE

source("pre.functions.r")
source("EigenMS.R")
source("shinyextensions.R")
source("fisherfuncs.r", local = TRUE)
source("fileupload/fileupload.server.R", local = TRUE)

dbdir = "~/tb.data"

#SERVERSIDE
rootdir = "/srv/tb.data"
subdirs = c("datasets", "analyses", "results", "protlists", "pathlists")
alldirs = c(dbdir, paste(dbdir, subdirs, sep = "/"))
metadata.path = paste(dbdir, "metadata.RData", sep = "/")

metadata = if (do.serverside)
{
  dummy = lapply(alldirs, function(dirpath) {
    if (!dir.exists(dirpath)) dir.create(dirpath)
  })
  
  if (file.exists(metadata.path)) {
    load(metadata.path)
    do.call(reactiveValues, metadata.list)
  } else {
    
    all.owners = c("Example owner")
    all.datasets = data.frame(idx = integer(0), name = character(0), owner = character(0), organism = character(0), long.desc = character(0), orig.file = character(0), file = character(0), processed =  logical(0), stamp = character(0), filetype = character(0), stringsAsFactors = F)
    all.datasets$filetype = list()
    next.file = 1
    
    all.analyses = data.frame(idx = integer(0), name = character(0), owner = character(0), organism = character(0), long.desc = character(0), parent.file = integer(0), filepath = character(0), stamp = character(0), stringsAsFactors = F)
    next.analysis = 1
    
    all.results = data.frame(idx = integer(0), name = character(0), owner = character(0), cont.name = character(0), cont.formula = character(0), organism = character(0), long.desc = character(0), parent.analysis = integer(0), filepath = character(0), stamp = character(0), stringsAsFactors = FALSE)
    all.results$colnums = list()
    next.results = 1
    
    all.protlists = data.frame(idx = integer(0), name = character(0), owner = character(0), organism = character(0), long.desc = character(0), parent.results = integer(0), filepath = character(0),
      p.cutoff = numeric(0), lfc.comp = character(0), lfc.cutoff = numeric(0), p.type = character(0), stringsAsFactors = FALSE)
    all.protlists$colnums = list()
    next.protlist = 1
    
    all.pathlists = data.frame(idx = integer(0), name = character(0), owner = character(0), organism = character(0), filepath = character(0), stringsAsFactors = FALSE)
    next.pathlist = 1
    
    metadata = reactiveValues(all.owners = all.owners, all.datasets = all.datasets, next.file = next.file,
      all.analyses = all.analyses, next.analysis = next.analysis,
      all.results = all.results, next.results = next.results,
      all.protlists = all.protlists, next.protlist = next.protlist,
      all.pathlists = all.pathlists, next.pathlist = next.pathlist)
    
    metadata.list = isolate(reactiveValuesToList(metadata))
    save(metadata.list, file = metadata.path)
    metadata
  }  
}
#/SERVERSIDE

getKeggFile = lapply(fshOrganisms, function(orgCode) {
  filename = paste0("keggfiles/", orgCode, ".universe.rds")
    
  universe = if(file.exists(filename))
  {
    reactiveFileReader(5 * 60 * 1000, session = NULL, filePath =  filename, readFunc = readRDS)
  }

  return(universe)
})

names(getKeggFile) = fshOrganisms


shinyServer(function(input,output,session){
  
  checkpoints = reactiveValues(file.loaded = F,
                               variables.created = F,
                               design.table.created = F,
                               descriptive.exists = F,
                               numeric.exists = F
                               )
  
  hideTab("tb.nav", "Experimental design", session = session)
  hideTab("tb.nav", "Data exploration and pre-processing", session = session)
  hideTab("tb.nav", "Define and Run Analysis", session = session)
  hideTab("tb.nav", "DE Results", session = session)
  
  ### Load file
  name = "main."
  uploader = paste0(name, "uploader")
  typesel = paste0(name, "typesel")
  delimiter = paste0(name, "delimiter")
  tablesel = paste0(name, "tablesel")
  header = paste0(name, "header")
  transpose = paste0(name, "transpose")
  
  #SERVERSIDE
  source("fileupload.main/fileupload.main.server.R", local = TRUE)
  source("compare/compare.server.R", local = TRUE)
  source("search/search.server.R", local = TRUE)
  
  
  # Analysis code
  source("analysis/analysis.server.R", local = TRUE)
  
  # Example dataset
  source("analysis/example.server.R", local = TRUE)
 
  # Test interactome page
  source("interactome/interactome.server.R", local = TRUE)
  rct.interactome = interactome.module(name = "interactest", resultsrv$siglists)

  source("pathway/pathway.server.R", local = TRUE)
  rct.pathway = pathwayanalysis.module(name = "pathwaytest", resultsrv$siglists)
  
  ### End Shinyserver ###
  
  hide(id = "loading-content", anim = TRUE, animType = "fade")
})
