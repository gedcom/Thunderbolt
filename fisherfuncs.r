library(org.Hs.eg.db)
library(org.Mm.eg.db)

getOrgPathsGenes <- function(orgCode) {
  require(KEGGREST)
  
  orgGenesList = keggList(orgCode)
  Gene.IDs = unlist(
    lapply(names(orgGenesList), function(x) {
      gsub(paste0(orgCode, ":"), "", x)
    }))


  orgPaths = keggList("pathway", orgCode)

  Paths.IDs = unlist(lapply(names(orgPaths), function(x) {
    gsub(paste0("path:", orgCode), "", x)
  }))
  
  orgPathsGenes = lapply(names(orgPaths), function(x) {
    glist = keggGet(x)[[1]]$GENE
    if(length(glist) > 0) {
      glist[seq(from = 1, to = length(glist), by = 2)]
    } else {
      NULL
    }
  })

  names(orgPathsGenes) <- names(orgPaths)

  return (orgPathsGenes)
}

getOrgGenes <- function(orgCode) {
  
  require(KEGGREST)
  
  orgGenesList = keggList(orgCode)
  Gene.IDs = unlist(
    lapply(names(orgGenesList), function(x) {
      gsub(paste0(orgCode, ":"), "", x)
    }))
  
  return (Gene.IDs)
}

getMatVals <- function (reggenes, universegenes, pathgenes){
  OnPath = pathgenes
  NotOnPath = setdiff(universegenes, pathgenes)
  Regulated = reggenes
  NotRegulated = setdiff(universegenes, reggenes)
  vals <- c(length(intersect(OnPath,Regulated)),
            length(intersect(OnPath,NotRegulated)),
            length(intersect(NotOnPath,Regulated)),
            length(intersect(NotOnPath,NotRegulated)))
  return(vals)
}

### File management ###

fshOrganisms = c("Homo sapiens" = "hsa", "Mus musculus" = "mmu")
updfreq = list("num" = 7, "unit" = "days") # Set update frequency

getUniverse = function (orgCode)
{
  
  universe = list()
  
  universe$genes = getOrgGenes(orgCode)
  universe$paths = getOrgPathsGenes(orgCode)
  universe$pathnames = sapply(names(universe$paths), function (x)
    gsub('path:', '', x))
  universe$pathdesc = keggList('pathway', orgCode)
  names(universe$paths) = universe$pathnames
  
  return(universe)
}

# id.vec - a vector of IDs. If a cell contains multiple ID separated by ';', the first will be used
# orgCode - three letter code (as from KEGG) denoting species, ie hsa (human) or mmu (mouse)
# fromtype - "uniprot" or "symbol"
getEntrez = function(id.vec, orgCode, fromtype, sep = ";")
{
  id.list = strsplit(id.vec, sep, fixed = TRUE)
  id.first = sapply(id.list, function(ids) ids[1])
  drop.idx = is.na(id.first) | id.first == ""
  
  db.organism = switch(orgCode,
    "hsa" = "Hs",
    "mmu" = "Mm")
  db.conversion = switch(fromtype,
    "uniprot" = "UNIPROT",
    "symbol" = "SYMBOL2EG")
  
  db.name = paste0("org.", db.organism, ".eg", db.conversion)
  db = get(db.name)
  
  eid.list = mget(id.first[!drop.idx], db, ifnotfound = NA)
  eid.vec = character(length = length(id.list))
  eid.vec[drop.idx] = NA
  eid.vec[!drop.idx] = sapply(eid.list, function(eid) eid[1])
  
  return(eid.vec)
}                    