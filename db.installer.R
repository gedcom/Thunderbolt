library("biomaRt")

db.packs = c(
    "org.Hs.eg.db",
    "org.Mm.eg.db",
    "org.Rn.eg.db",
    "org.Dm.eg.db"
)

dbdir = "~/tb.data"
if (!dir.exists(dbdir)) dir.create(dbdir)

filedir = paste(dbdir, "org.dbs", sep = "/")
if (!dir.exists(filedir)) dir.create(filedir)

lapply(db.packs, function(db.pack) {
    do.call(library, list(db.pack))
    db = get(db.pack)
    
    symmapname = paste0(substring(db.pack, 1, 9), "SYMBOL")
    symmap = get(symmapname)
    ids = mappedkeys(symmap)
    
    dat.conv = select(db, columns = c("SYMBOL", "UNIPROT", "ENSEMBLPROT"), keys = ids)
    dat.conv = lapply(dat.conv, toupper)
    
    arglist = c(dat.conv, list(stringsAsFactors = FALSE, row.names = NULL))
    dat.conv = do.call(data.frame, arglist)
    fn = paste(filedir, db.pack, sep = "/")
    save(dat.conv, file = fn)
})

ensembl.names = c(
  "hsa" = "hsapiens_gene_ensembl",
  "mmu" = "mmusculus_gene_ensembl",
  "rno" = "rnorvegicus_gene_ensembl",
  "dme" = "dmelanogaster_gene_ensembl"
)

num.specs = length(ensembl.names)
filedir = paste(dbdir, "biomart.maps", sep = "/")
if (!dir.exists(filedir)) dir.create(filedir)

maps = lapply(1:num.specs, function(to.idx) {
  out = lapply(1:num.specs, function(from.idx) {
    if (from.idx != to.idx)
    {
      from.ensembl = useMart("ensembl", dataset = ensembl.names[from.idx])
      to.ensembl = useMart("ensembl", dataset = ensembl.names[to.idx])
      
      mapping = getLDS(attributes=c("entrezgene"), mart = from.ensembl, attributesL=c("entrezgene"), martL = to.ensembl, uniqueRows=TRUE)
      mapping = data.frame(from = as.character(mapping[,1]), to = as.character(mapping[,2]), stringsAsFactors = FALSE)
      colnames(mapping) = c("from", "to")
      
      filename = paste(names(ensembl.names)[from.idx], "to", names(ensembl.names)[to.idx], "RData", sep = ".")
      filepath = paste(filedir, filename, sep = "/")
      save(mapping, file = filepath)
      
      return(mapping) 
    }
  })
  names(out) = names(ensembl.names)
  return(out)
})

eid.list = mapply(FUN = function(spec.name, spec.ens) {
  mart = useMart("ensembl", dataset = spec.ens)
  spec.eids = as.character(getBM(attributes = c("entrezgene"), mart = mart)$entrezgene)
  
  out = list(from = spec.eids, species = spec.name)
  
  add.cols = mapply(FUN = function(to.spec.name, to.spec.ens) {
    
    if (spec.name == to.spec.name)
    {
      return(spec.eids)
    } else {
      filename = paste(spec.name, "to", to.spec.name, "RData", sep = ".")
      filepath = paste(filedir, filename, sep = "/")
      load(filepath)
      
      return(list(mapping$to[match(spec.eids, mapping$from, nomatch = NA)]))
    }
  }, names(ensembl.names), ensembl.names)  
  
  add.cols = data.frame(add.cols, stringsAsFactors = FALSE)
  names(add.cols) = names(ensembl.names)
  
  out = data.frame(out, add.cols, stringsAsFactors = FALSE)
  return(list(out))
}, names(ensembl.names), ensembl.names)

biom.table = do.call(rbind, eid.list)
rownames(biom.table) = NULL
filepath = paste(filedir, "biom.table.RData", sep = "/")
save(biom.table, file = filepath)
