library("writexl")

search.md = reactive({
  switch(input$search.data.type,
    "datasets" = metadata$all.datasets,
    "results" = metadata$all.results,
    "protlists" = metadata$all.protlists
  )
})

rv.searchfields = reactiveValues(all.names = NULL, all.owners = NULL)

observe ({
  md = search.md()
  all.owners = unique(md$owner)
  current.owners = isolate(input$search.field.owner)
  new.owners = current.owners[current.owners %in% all.owners]
  
  updateSelectizeInput(session, "search.field.owner", choices = all.owners, selected = new.owners, server = FALSE)

})

observe ({
  md = search.md()
  all.names = md$name
  all.owners = md$owner
  
  if (length(input$search.field.owner) > 0)
    all.names = all.names[all.owners %in% input$search.field.owner]
  
  current.names = isolate(input$search.field.name)
  new.names = current.names[current.names %in% all.names]
  
  updateSelectizeInput(session, "search.field.name", choices = all.names, selected = new.names, server = FALSE)
})

search.trigger = reactive ({
  input$search.button
  search.md()
})

search.mdfields = eventReactive (search.trigger(),
  {
    md = search.md()
    
    validate(need(nrow(md) > 0, message = "No data of this type are currently stored."))
    
    hits = list()
    if (isTruthy(input$search.field.name))
      hits$name = md$name %in% input$search.field.name
    if (isTruthy(input$search.field.owner))
      hits$owner = md$owner %in% input$search.field.owner
    if (isTruthy(input$search.field.long))
      hits$long = grepl(input$search.field.long, md$long.desc)
    
    if (length(hits) == 0)
    {
      md
    } else {
      md.hit.idx = apply(as.data.frame(hits), 1, all)
      md.hit = md[md.hit.idx,]
      validate(need(nrow(md.hit) > 0, message = "No matches found."))
      md.hit
    }
  })

search.data.type = eventReactive(search.trigger(), input$search.data.type)

search.row.hits = eventReactive (search.trigger(), {
  
  md = search.mdfields()
  delimiters = c(";", ",")
  gene.terms = input$search.field.gene
  uniprot.terms = input$search.field.uniprot
  entrez.terms = input$search.field.entrez
  misc.terms = input$search.field.misc
  
  lapply(delimiters, function(d) {
    gene.terms <<- gsub(pattern = d, replacement = delimiters[1], x = gene.terms)
    uniprot.terms <<- gsub(pattern = d, replacement = delimiters[1], x = uniprot.terms)
    entrez.terms <<- gsub(pattern = d, replacement = delimiters[1], x = entrez.terms)
    misc.terms <<- gsub(pattern = d, replacement = delimiters[1], x = misc.terms)
  })
  
  gene.terms = gsub(" ", "", gene.terms)
  uniprot.terms = gsub(" ", "", uniprot.terms)
  entrez.terms = gsub(" ", "", entrez.terms)
  misc.terms = gsub(" ", "", misc.terms)
  
  gene = strsplit(gene.terms, split = delimiters[1], fixed = TRUE)
  uniprot = strsplit(uniprot.terms, split = delimiters[1], fixed = TRUE)
  entrez = strsplit(entrez.terms, split = delimiters[1], fixed = TRUE)
  misc = strsplit(misc.terms, split = delimiters[1], fixed = TRUE)
  
  
  row.tf = switch(input$search.data.type,
    #       "datasets" = {
    #         fl = open.file()
    #       },
    "results" = {
      
      lapply(seq(nrow(md)), function(rw.num) {
        rw = md[rw.num,]
        fl = open.file(rw$filepath, "rdata")[[1]]
        
        id.hits = list()
        in.field = c(gene, uniprot, entrez, misc)
        genecols = rw$colnums[[1]]$Gene
        unipcols = rw$colnums[[1]]$Uniprot
        entrcols = rw$colnums[[1]]$Entrez
        misccols = rw$colnums[[1]]$Misc
        
        field.cols = c(
          genecols,
          unipcols,
          entrcols,
          misccols
        )
        
        
        
        id.hits = mapply(function(in.field, field.cols) {
          if (all(in.field == "")) return (NULL)
          if (is.na(field.cols[1])) return(NULL)
          
          out = lapply(in.field, function(pattern) {
            out = lapply(fl[field.cols], function(x) grepl(pattern, x, ignore.case = TRUE))
            out = do.call(cbind, out)
          })
          apply(as.data.frame(out), 1, any)
        }, in.field, field.cols)
        
        names(id.hits) = c("gene", "uniprot", "entrez", "misc")
        id.hits[sapply(id.hits, length) == 0] = NULL
        id.hits = as.data.frame(id.hits)
        
        hits = list()
        hits$id = if (length(id.hits) > 0)
        {
          apply(id.hits, 1, any)
        } else NULL
        
        raw.adj = input$search.p.rawadj
        p = input$search.field.p
        lfc = input$search.field.logfc
        lfc.dir = input$search.lfc.dir
        
        hits$p = switch (raw.adj,
          "raw" = {
            if (isTruthy(p) && !is.na(rw$colnums[[1]][["Raw P"]])) {
              dat = fl[rw$colnums[[1]][["Raw P"]] ]
              tf = dat < p
              tf = apply(tf, 1, any)
              tf[is.na(tf)] = FALSE
              tf
            } else rep(FALSE, nrow(fl))
          },
          "adj" = {
            if (isTruthy(p) && !is.na(rw$colnums[[1]][["Adj P"]])) {
              dat = fl[rw$colnums[[1]][["Adj P"]] ]
              tf = dat < p
              tf = apply(tf, 1, any)
              tf[is.na(tf)] = FALSE
              tf
            } else rep(FALSE, nrow(fl))
          },
          NULL
        )
        
        hits$lfc = switch (lfc.dir,
          "up" = {
            if (isTruthy(lfc) && !is.na(rw$colnums[[1]][["LogFC"]])) {
              dat = fl[rw$colnums[[1]][["LogFC"]] ]
              tf = dat > lfc
              tf = apply(tf, 1, any)
              tf[is.na(tf)] = FALSE
              tf
            } else rep(FALSE, nrow(fl))
          },
          "down" = {
            if (isTruthy(lfc) && !is.na(rw$colnums[[1]][["LogFC"]])) {
              dat = fl[rw$colnums[[1]][["LogFC"]] ]
              tf = dat < lfc
              tf = apply(tf, 1, any)
              tf[is.na(tf)] = FALSE
              tf
            } else rep(FALSE, nrow(fl))
          },
          "both" = {
            if (isTruthy(lfc) && !is.na(rw$colnums[[1]][["LogFC"]])) {
              dat = fl[rw$colnums[[1]][["LogFC"]] ]
              tf = abs(dat) > abs(lfc)
              tf = apply(tf, 1, any)
              tf[is.na(tf)] = FALSE
              tf
            } else rep(FALSE, nrow(fl))
          },
          NULL
        )
        
        if (length(hits) > 0)
        {
          return(apply(as.data.frame(hits), 1, all))
        } else {
          return(rep(TRUE, nrow(fl)))
        }
        
        
        return(out)
      })
    },
    "protlists" = {
      
      lapply(seq(nrow(md)), function(rw.num) {
        rw = md[rw.num,]
        fl = open.file(rw$filepath, "rdata")[[1]]
        
        id.hits = list()
        in.field = c(gene, uniprot, entrez, misc)
        genecols = rw$colnums[[1]]$Gene
        unipcols = rw$colnums[[1]]$Uniprot
        entrcols = rw$colnums[[1]]$Entrez
        misccols = rw$colnums[[1]]$Misc
        
        field.cols = c(
          genecols,
          unipcols,
          entrcols,
          misccols
        )
        
        id.hits = mapply(function(in.field, field.cols) {
          if (all(in.field == "")) return (NULL)
          if (is.na(field.cols[1])) return(NULL)
          
          out = lapply(in.field, function(pattern) {
            out = lapply(fl[field.cols], function(x) grepl(pattern, x, ignore.case = TRUE))
            out = do.call(cbind, out)
          })
          apply(as.data.frame(out), 1, any)
        }, in.field, field.cols)
        
        names(id.hits) = c("gene", "uniprot", "entrez", "misc")
        id.hits[sapply(id.hits, length) == 0] = NULL
        
        if (length(id.hits) == 0)
        {
          return(rep(TRUE, nrow(fl)))
        } else
        {
          id.hits = as.data.frame(id.hits)
          out = apply(as.data.frame(id.hits), 1, all)
          return(out)
        }
      })
    }
  )
  row.tf
})

search.set.hits = eventReactive(search.trigger(), {
  progress = shiny::Progress$new(session, min = 0, max = 1)
  on.exit(progress$close())
  progress$set(message = "Please wait. ", detail = paste0("Searching..."))
  
  md = search.mdfields()
  row.tf = search.row.hits()
  any.hit = sapply(row.tf, function(rw) any(rw))
  out = list(meta.hits = md[any.hit,], row.hits = row.tf[any.hit])
  out
})

output$search.meta.hits = DT::renderDataTable({
  results = search.set.hits()$meta.hits
  if (nrow(results) > 0)
  {
    show.cols = c("Name" = "name", "Owner" = "owner", "Organism" = "organism", "Created" = "stamp")
    show.cols = show.cols[show.cols %in% colnames(results)]
    out = results[show.cols]
    names(out) = names(show.cols)
    out
  } else data.frame()
}, rownames = FALSE, selection = list(target = "row", mode = "multiple"))

search.data.table = reactive({
  results = search.set.hits()$meta.hits
  show.rows = search.set.hits()$row.hits
  sel.rw = input$search.meta.hits_rows_selected
  sel.md = results[sel.rw,]
  validate(need(sel.rw, message = "One or more datasets must be selected."))
  spec = input$search.spec.col
  if (length(sel.rw) > 1) spec = TRUE
  
  if (nrow(results) > 0 && isTruthy(sel.rw))
  {
    if (spec)
    {
      out = lapply(sel.rw, function(rw.num) {
        rw = results[rw.num,]
        sr = show.rows[[rw.num]]
        tbl.nrow = sum(sr)
        
        fl = open.file(rw$filepath, "rdata")[[1]]
        out = NULL
        extra.names = NULL
        if (nrow(sel.md) > 1) {
          out = list("From" = rep(rw$name, tbl.nrow))
          extra.names = c(extra.names, "From")
        }
        if (length(unique(sel.md$organism)) > 1) {
          out = c(out, list("Organism" = rep(rw$organism, tbl.nrow)))
          extra.names = c(extra.names, "Organism")
        }
        fields = names(rw$colnums[[1]])
        fields = fields[!fields == "Misc"]
        out = c(out,
          lapply(fields, function(fld) {
            
            if (is.na(rw$colnums[[1]][[fld]][1])) {
              return(rep(NA, tbl.nrow))
            } else if (length(rw$colnums[[1]][[fld]]) > 1) {
              return(apply(fl[sr, rw$colnums[[1]][[fld]]], 1, function(x) {
                x = x[!x == ""]
                return(paste(x, collapse = ";"))
              }))
            } else {
              return(fl[sr, rw$colnums[[1]][[fld]]])
            }
          })
        )
        names(out) = c(extra.names, fields)
        return(as.data.frame(out))
      })
      
      col.names = names(out[[1]])
      out = do.call(rbind, out)
      names(out) = col.names
      na.cols = sapply(out, function(cl) all(is.na(cl)))
      out = out[!na.cols]
      
      return(out)
      
    } else {
      rw = results[sel.rw,]
      sr = show.rows[[sel.rw]]
      fl = open.file(rw$filepath, "rdata")[[1]]
      if (input$search.disp.flt)
        data.frame(fl[sr,])
      else fl
    }
  } else  {
    data.frame()
  }
})

output$search.show.data = DT::renderDataTable ({
  search.data.table()
}, rownames = FALSE, options = list(scrollX = TRUE), selection = list(target = "column", mode = "multiple"))

search.save.protlist = observeEvent(input$search.save.protlist,
{
  validate(need(!input$search.save.protlist.name %in% metadata$all.protlists$name, message = "Name already exists."))
  results = search.set.hits()$meta.hits[input$search.meta.hits_rows_selected,]
  
  validate(need(search.data.table(), label = ""))
  validate(need(input$search.save.protlist.name, label = "Name"))
  validate(need(input$search.save.protlist.owner, label = "Owner"))
  sel.col = input$search.show.data_columns_selected + 1
  next.protlist = metadata$next.protlist
  filepath = paste(rootdir, "protlists", next.protlist, sep = "/")
  
  organism = unique(results$organism)
  if (length(organism) > 1)  {
    organism = NA
    
  } 
  
  new.row = data.frame(idx = next.protlist, name = input$search.save.protlist.name, owner = input$search.save.protlist.owner, organism = organism, parent.results = NA, filepath = filepath,
    p.cutoff = NA, lfc.comp = NA, lfc.cutoff = NA, p.type = NA, stringsAsFactors = FALSE)
  metadata$next.protlist = next.protlist + 1

  col.types = c("Gene", "Entrez", "Uniprot", "Misc")
  if (is.na(organism))
  {
    col.types = c("Organism", col.types)
    sel.col = union(which(colnames(search.data.table()) == "Organism"), sel.col)
  }  
  
  currenttable = if (length(sel.col) > 0)
  {
    search.data.table()[sel.col]
  } else {
    search.data.table()[colnames(search.data.table()) %in% col.types]
  }
  
  tbl.cols = as.list(match(col.types, colnames(currenttable), nomatch = NA))
  names(tbl.cols) = col.types
  new.row$colnums = list(tbl.cols)
  
  progress = shiny::Progress$new(session, min = 0, max = 1)
  on.exit(progress$close())
  progress$set(message = "Please wait.", detail = "Saving list...")
  
  metadata$all.protlists = rbind(metadata$all.protlists, new.row)
  
  save(currenttable, file = filepath)
  
  metadata.list = reactiveValuesToList(metadata)
  save(metadata.list, file = metadata.path)
  
  progress$set(message = "Saved as protein list.", detail = "")
})

output$search.export.data = downloadHandler (
  filename = function() {
    savetype = input$search.export.filetype
    fext = switch(savetype,
      "xls" = ".xlsx",
      "tsv" = ".txt",
      "csv" = ".csv",
      "rdata" = ".RData"
    )
    
    fn = paste0("results.", Sys.Date(), fext)
    return(fn)
  },
  content = function(fn) {
    savetype = input$search.export.filetype
    
    sel.col = input$search.show.data_columns_selected + 1
    dat = if (length(sel.col) > 0)
    {
      search.data.table()[sel.col]
    } else {
      search.data.table()
    }
    
    row.names = FALSE
    col.names = TRUE
    
    switch(savetype,
      "xls" = write_xlsx(dat, file = fn, col_names = col.names),
      "tsv" = write.table(dat, file = fn, sep = "\t", row.names = row.names, col.names = col.names),
      "csv" = write.table(dat, file = fn, sep = ",", row.names = row.names, col.names = col.names),
      "rdata" = save(dat, file = fn)
    )
  }
)

# Delete 1 or more datasets, results tables or protein lists

rv.search.delete = reactiveValues(deletesurevisible = FALSE)

output$search.deletevisible = renderText({
  if (nrow(search.set.hits()$meta.hits) > 0 && !rv.search.delete$deletesurevisible) "TRUE" else "FALSE"
})
output$search.deletesurevisible = renderText({
  if (nrow(search.set.hits()$meta.hits) > 0 && rv.search.delete$deletesurevisible) "TRUE" else "FALSE"
})

outputOptions(output, "search.deletevisible", suspendWhenHidden = FALSE)
outputOptions(output, "search.deletesurevisible", suspendWhenHidden = FALSE)

observeEvent(input$search.delete.dataset,
{
  rv.search.delete$deletesurevisible = TRUE
})

observeEvent(input$search.delete.yes,
{
  results = search.set.hits()$meta.hits
  sel.rw = input$search.meta.hits_rows_selected
  validate(need(sel.rw, message = "One or more datasets must be selected."))
  subdirectory = search.data.type()
  mdtype = switch(subdirectory,
    "datasets" = "all.datasets",
    "results" = "all.results",
    "protlists" = "all.protlists"
  )
  i = metadata[[mdtype]]$idx %in% results$idx[sel.rw]

  filepath = paste(metadata[[mdtype]]$filepath[i], sep = "/")
  file.remove(filepath)
  metadata[[mdtype]] = metadata[[mdtype]][!i,]
  
  metadata.list = reactiveValuesToList(metadata)
  save(metadata.list, file = metadata.path)
  
  rv.search.delete$deletesurevisible = FALSE
})

observeEvent(input$search.delete.no,
{
  rv.search.delete$deletesurevisible = FALSE
})