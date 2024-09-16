library(VennDiagram)
library("writexl")

comp.sel.protlists = eventReactive(input$comp.docompare, input$comp.sel.protlists)

observe ({
  data.type = input$comp.prot.path
  comp.owner.choices = unique(metadata[[data.type]]$owner)
  updateSelectizeInput(session, "comp.sel.owners", choices = comp.owner.choices)
})

observeEvent (input$comp.clear.protlists, {
  updateSelectizeInput(session, "comp.sel.protlists", selected = character(0))
})

entrez.list = reactive ({
  
  comp.protlist.rows = metadata$all.protlists$idx %in% comp.sel.protlists()
  sufficient = sum(comp.protlist.rows) >= 2
  validate(need(sufficient, message = "Two or more protein lists required."))
  
  comp.protlists = metadata$all.protlists[comp.protlist.rows,]
  
  type.order = c("ENTREZ" = "Entrez", "UNIPROT" = "Uniprot", "SYMBOL" = "Gene")
  
  map.hierarchy = c("hsa", "mmu", "rno", "dme", NA)
  mixed.datasets = is.na(comp.protlists$organism)
  orgs = comp.protlists$organism
  
  if (any(mixed.datasets))
  {
    orgs[mixed.datasets] = sapply(seq(nrow(comp.protlists[mixed.datasets,])), function(rw) {
      load(rw$filepath)
      orgcol = rw$colnums[[1]]$Organism
      
      this.set.orgs = unique(currenttable[,orgcol])
      
      this.org = map.hierarchy[map.hierarchy %in% this.set.orgs][1]
      
      return(this.org)
    })
  }
  
  map.to.species = map.hierarchy[map.hierarchy %in% comp.protlists$organism][1]
  
  
  dat.list = lapply(seq(nrow(comp.protlists)), function(i) {
    
    pl.info = comp.protlists[i,]
    colnums = pl.info$colnums[[1]]
    col.avail = sapply(type.order, function(id) !is.na(colnums[[id]][1]))
    
    if (!any(col.avail)) return(NULL)
    
    dat.field = type.order[[which(col.avail)[1]]]
    conv.field = names(type.order)[which(col.avail)[1]]
    
    load(file = pl.info$filepath)
    
    in.col = currenttable[colnums[[dat.field]]]
    org.col = NULL
    if (is.na(pl.info$organism)) {
      org.col = currenttable[,colnums[["Organism"]]]
    } else {
      org.col = pl.info$organism
    }
    
    prot.dat = data.frame(organism = org.col, original = unique(do.call(c, in.col)), stringsAsFactors = FALSE)
    prot.dat$first = toupper(sapply(strsplit(prot.dat$original, split = ";", fixed = TRUE), function(x) x[1]))
    
    prot.dat$entrezid = if (dat.field == "Entrez")
    {
      prot.dat$first
    } else {
      db.name = switch(EXPR = pl.info$organism,
        "hsa" = "org.Hs.eg.db",
        "mmu" = "org.Mm.eg.db",
        "rno" = "org.Rn.eg.db",
        "dme" = "org.Dm.eg.db"
      )
      
      db.fn = paste(dbdir, "org.dbs", db.name, sep = "/")
      load(db.fn)
      idx = match(prot.dat$first, dat.conv[[conv.field]], nomatch = NA)
      dat.conv$ENTREZID[idx]
    }
    prot.dat = na.omit(prot.dat)
    
    
    if (pl.info$organism != map.to.species)
    {
      progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
      specname = switch(pl.info$organism,
        "mmu" = "mouse",
        "rno" = "rat",
        "dme" = "fly"
      )
      # submessage = paste0("Mapping \"", pl.info$name, "\" from ", specname, " to human...")
      progress$set(message = "Interspecies comparison. ", detail = "Converting entrez IDs...")
      
#       mapping.filename = paste(pl.info$organism, "to", map.to.species, "RData", sep = ".")
#       mapping.filepath = paste(dbdir, "biomart.maps", mapping.filename, sep = "/")
#       load(mapping.filepath)
      
      # prot.dat$entrezid = mapping$to[match(prot.dat$entrezid, mapping$from, nomatch = NA)]
      
      bmtable.filename = paste(dbdir, "biomart.maps", "biom.table.RData", sep = "/")
      load(bmtable.filename)
      
      prot.dat$entrezid = biom.table[[map.to.species]][match(prot.dat$entrezid, biom.table$from, nomatch = NA)]
      
      prot.dat = na.omit(prot.dat)
      progress$close()
    }
    return(prot.dat)
  })
  
  names(dat.list) = comp.protlists$name
  out.entrez.list = lapply(dat.list, function(x) {
    out = x$entrezid
    names(out) = x$original
    
    return(out)
  })
  
  out.entrez.list
})

comp.all.entrez = reactive ({
  
  temp = lapply(entrez.list(), function(x) {
    out = x
    names(out) = NULL
    return(out)
  })
  names(temp) = NULL
  
  all.ids = Reduce(union, temp)
  all.ids
})

comp.misc.list = reactive ({
  md.rows = metadata$all.protlists$idx %in% comp.sel.protlists()
  sufficient = sum(md.rows) >= 2
  validate(need(sufficient, message = "Two or more protein lists required."))
  
  md = metadata$all.protlists[md.rows,]
  protlists = lapply(seq_along(md$idx), function(rw) {
    flnm = md$filepath[rw]
    misccol = md$colnums[[rw]]$Misc
    
    load(flnm)
    out = do.call(c, currenttable[misccol])
    return(out)
  })
  names(protlists) = md$name
  
  protlists
})

comp.all.misc = reactive ({
  protlists = comp.misc.list()
  
  all.list = lapply(protlists, function(prt)
    as.character(prt))
  all.misc = do.call(c, all.list)
  all.misc = all.misc[!duplicated(all.misc)]
  all.misc = all.misc[order(all.misc)]
  names(all.misc) = NULL
  
  all.misc
})

comp.misc.table = reactive ({
  all.misc = comp.all.misc()
  pres.tbl = lapply(comp.misc.list(), function(x) {
    out = all.misc %in% x
    out[!out] = NA
    out = as.character(out)
    out
  })
  
  do.call(data.frame, c(pres.tbl, list(stringsAsFactors = FALSE)) )
})

comp.prot.table = reactive ({
  
  all.entrez = comp.all.entrez()
  pres.tbl = lapply(entrez.list(), function(pl) 
    names(pl)[match(all.entrez, pl, NA)])
  
  pres.tbl = as.data.frame(pres.tbl)
  rownames(pres.tbl) = all.entrez
  pres.tbl
})

comp.path.list = reactive ({
  md.rows = as.character(metadata$all.pathlists$idx) %in% comp.sel.protlists()
  validate(need(sum(md.rows) >= 2, message = "Please select 2 or more pathways."))
  
  md = metadata$all.pathlists[md.rows,]
  pathlists = lapply(md$filepath, function(flnm) {
    load(flnm)
    return(currenttable)
  })
  names(pathlists) = md$name
  
  pathlists
})

comp.all.paths = reactive ({
  pathlists = comp.path.list()
  
  validate(need(pathlists, message = "Please select two or more pathways"))
  
  path.list = lapply(pathlists, function(pth)
    data.frame(keggid = as.character(pth$keggid), description = as.character(pth$description), stringsAsFactors = FALSE))
  all.paths = Reduce(rbind, path.list)
  all.paths = all.paths[!duplicated(all.paths$keggid),]
  all.paths = all.paths[order(all.paths$keggid),]
  rownames(all.paths) = NULL
  all.paths
})

comp.path.table = eventReactive(input$comp.docompare, {
  pathlists = comp.path.list()
  all.keggids = comp.all.paths()$keggid
  
  pres.tbl = lapply(pathlists, function(pl) {
    idx = match(all.keggids, pl$keggid, NA)
    overlap = rep(NA, length(all.keggids))
    overlap[!is.na(idx)] = sapply(pl$genes[na.omit(idx)], length)
    
    return(overlap)
  })
  
  pres.tbl = as.data.frame(pres.tbl)
  
  pres.tbl
})

comp.table = eventReactive(input$comp.docompare, {
  out = switch(input$comp.prot.path,
    "all.protlists" = comp.prot.table(),
    "all.pathlists" = comp.path.table(),
    "misc" = comp.misc.table()
  )
  
  out
})

comp.finaltable = eventReactive(input$comp.docompare, {
  out = comp.table()
  out$Sum = apply(out, 1, function(rw) sum(!is.na(rw)))
  
  switch(input$comp.prot.path,
    "all.protlists" = {
      out = cbind(comp.all.entrez(), out)
      out = out[order(out$Sum, decreasing = TRUE),]
      colnames(out) = c("Entrez ID", paste(colnames(comp.table()), "identifier"), "in # lists")
    },
    "all.pathlists" = {
      paths = comp.all.paths()
      out = cbind(paths$keggid, paths$description, out)
      out = out[order(out$Sum, decreasing = TRUE),]
      colnames(out) = c("KEGG ID", "Description", paste(colnames(comp.table()), "overlap"), "in # lists")
    },
    "misc" = {
      paths = comp.all.misc()
      out = cbind(paths, out)
      out = out[order(out$Sum, decreasing = TRUE),]
      colnames(out) = c("Misc", paste(colnames(comp.table()), "contains"), "in # lists")
    }
  )
  
  rownames(out) = NULL
  out
})

output$comp.table = DT::renderDataTable(
  {
    out = comp.finaltable()
    DT::datatable(out, 
      options = list(scrollX = TRUE, scrollY = "100%"), rownames = FALSE)
  })

# Download comparison table

output$comp.download.table = downloadHandler (
  filename = function() {
    savetype = input$comp.download.type
    fext = switch(savetype,
      "xls" = ".xlsx",
      "tsv" = ".txt",
      "csv" = ".csv",
      "rdata" = ".RData"
    )
    
    fn = paste0("comp.", Sys.Date(), fext)
    return(fn)
  },
  content = function(fn) {
    savetype = input$comp.download.type
    
    save.data = comp.finaltable()
    
    row.names = FALSE
    col.names = TRUE
    
    switch(savetype,
      "xls" = write_xlsx(save.data, path = fn, col_names = col.names),
      "tsv" = write.table(save.data, file = fn, sep = "\t", row.names = row.names, col.names = col.names),
      "csv" = write.table(save.data, file = fn, sep = ",", row.names = row.names, col.names = col.names),
      "rdata" = save(save.data, file = fn)
    )
  }
)

output$comp.venn = renderPlot({
  ct = comp.table()
  sets = lapply(ct, function(cl) rownames(ct)[!is.na(cl)])
  protpath = isolate(input$comp.prot.path)
  protlists = comp.sel.protlists()
  if (protpath == "misc") protpath = "all.protlists"
  
  idx = as.character(metadata[[protpath]]$idx) %in% protlists
  setnames = metadata[[protpath]]$name[idx]
  
  names(sets) = NULL
  intersects = NULL
  set.colours = c("#FFFF00FF", "#FF00FFFF", "#00FFFFFF", "#FF0000FF", "#0000FFFF")[seq_along(sets)]
  venn.func = NULL
  
  switch(as.character(length(sets)),
    "2" = {
      intersects = list(1, 2, c(1,2))
      venn.func = draw.pairwise.venn
    },
    "3" = {
      intersects = list(1, 2, 3, c(1,2), c(2,3), c(1,3), c(1,2,3))
      venn.func = draw.triple.venn
    },
    "4" = {
      intersects = list(1, 2, 3, 4, c(1,2), c(1,3), c(1,4), c(2,3), c(2,4), c(3,4), c(1,2,3), c(1,2,4), c(1,3,4), c(2,3,4), c(1,2,3,4))
      venn.func = draw.quad.venn
    },
    "5" = {
      intersects = list(1, 2, 3, 4, 5, c(1,2), c(1,3), c(1,4), c(1,5), c(2,3), c(2,4), c(2,5), c(3,4), c(3,5), c(4,5), c(1,2,3), c(1,2,4), c(1,2,5), c(1,3,4), c(1,3,5), c(1,4,5), c(2,3,4), c(2,3,5), c(2,4,5), c(3,4,5), c(1,2,3,4), c(1,2,3,5), c(1,2,4,5), c(1,3,4,5), c(2,3,4,5), c(1,2,3,4,5))
      venn.func = draw.quintuple.venn
    },
    validate("Venn diagram only works up to 5 sets.")
  )
  
  venn.args = lapply(intersects, function(x) {
    if (length(x) > 1)
    {
      return(Reduce(intersect, sets[x]))
    } else {
      return(sets[[x]])
    }
  })
  
  venn.args = lapply(venn.args, length)
  names(venn.args) = NULL
  venn.args = c(venn.args, list(category = setnames, fill = set.colours, alpha = rep(0.5, length(sets)), cat.cex = rep(2, length(sets)), cex = rep(2.5,length(intersects))))
  do.call(venn.func, venn.args)
})

output$overlap.heatmap = d3heatmap::renderD3heatmap({
  
  ct = comp.table()
  
  hm = sapply(ct, function(cl) {
    out = sapply(ct, function(rw) {
      cl.tf = !is.na(cl)
      rw.tf = !is.na(rw)
      
      common = sum(cl.tf & rw.tf)
      
      switch(input$comp.count.prop,
        "count" = {
          return(common)
        },
        "prop" = {
          total = sum(cl.tf | rw.tf)
          return(common / total)
        }
      )
    })
    
    return(out)
  })
  
  d3heatmap::d3heatmap(hm, colors = "Blues")
})

observe ({
  data.type = input$comp.prot.path
  if (data.type == "misc") data.type = "all.protlists"
  if (nrow(metadata[[data.type]]) > 0) {
    
    comp.list.filt = metadata[[data.type]]$owner %in% input$comp.sel.owners
    temp = if (any(comp.list.filt))
    {
      metadata[[data.type]][comp.list.filt,]
    } else {
      metadata[[data.type]][,]
    }
    
    if (input$comp.prot.path == "misc")
    {
      keep = !sapply(temp$colnums, function(x) is.na(x$Misc))
      temp = temp[keep,]
    }
    
    comp.list.choices = temp$idx
    if (length(comp.list.choices))
      names(comp.list.choices) = paste0(temp$idx, ": ", temp$name, " (", temp$owner,")")
    
    updateSelectizeInput(session, "comp.sel.protlists", choices = comp.list.choices)
  } else {
    updateSelectizeInput(session, "comp.sel.protlists", choices = NULL)
  }
})