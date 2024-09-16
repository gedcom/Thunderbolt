library("writexl")
library("parallel")
library("ggplot2")
source("fileupload/fileupload.server.R", local = TRUE)
library("plotly")
library("scales")

######################
######################
### SETUP FUNCTION ###
######################
######################
pathwayanalysis.module = function(name, siglists) {
  mf = match.call()
  siglists = mf$siglists
  
  newsetbutton = paste0(name, "newsetbutton")
  newsetpopup = paste0(name, "newsetpopup")     # Modal (popup dialogue) to create a new set
  newsetback = paste0(name, "newsetback")
  newsetnext = paste0(name, "newsetnext")
  newsetname = paste0(name, "newsetname")
  uploadorsig = paste0(name, "uploadorsig")     # radiobutton - upload data or draw from significance list
  newsetspecies = paste0(name, "newsetspecies") # New set dialogue species input
  dispspec = paste0(name, "dispspec")           # Conditional variable for displaying species input
  newsetidtype = paste0(name, "newsetidtype")
  siglistseltbl = paste0(name, "siglistseltbl")
  fileuploadnm = paste0(name, "fu")
  sheetselect = paste0(name, "sheetselect")
  outputdisp = paste0(name, "disp")
  protseltable = paste0(name, "protseltable")
  setpreviewtable = paste0(name, "setpreviewtable")
  
  # Main screen
  setlisttable = paste0(name, "setlisttable")  # List of protein sets
  delsetsbtn = paste0(name, "delsetsbtn")       # Delete selected sets
  clearsetsbtn = paste0(name, "clearsetsbtn")  # Clear all sets
  chkhprd = paste0(name, "chkhprd")             # Include links from HPRD database
  chkstring = paste0(name, "chkstring")         # Include links from string
  chklinkdbs = paste0(name, "chklinkdbs")       # Include links from HPRD/string database
  chklinktyp = paste0(name, "chklinktyp")       # Link types to show (internal/external)
  runfshbtn = paste0(name, "runfshbtn")     # Run Fisher's exact test
  
  fsh.rdy = paste0(name, "fsh.rdy")
  fsh.table = paste0(name, "fsh.table")
  fsh.result.plot = paste0(name, "fsh.result.plot")
  downloadfisher = paste0(name, "downloadfisher")
  downloadtype = paste0(name, "downloadtype")
  download.fsh.png = paste0(name, "download.fsh.png")
  download.fsh.eps = paste0(name, "download.fsh.eps")
  
  saveresults = paste0(name, "saveresults")
  savename = paste0(name, "savename")
  saveowner = paste0(name, "saveowner")
  
  rct.list = list()
  rct.list$rv = reactiveValues(popupdisp = "none",
                               setinfo.name = character(0), setinfo.origin = character(0), setinfo.idtype = character(0),
                               setlist = list(), setinfo.species = character(0))
  rct.list$fu = fileupload.module(name = fileuploadnm)
  
  
  ##################
  # User interface #
  ##################
  
  # Protein list
  
  rct.list$startnewset = observeEvent(input[[newsetbutton]], {
    rct.list$rv$popupdisp = "source"
  })
  
  output[[dispspec]] <<- renderText ({
    if (length(rct.list$rv$setinfo.name) > 0) "false" else "true"
  })
  outputOptions(output, dispspec, suspendWhenHidden = FALSE)
  
  output[[siglistseltbl]] <<- DT::renderDataTable({
    switch(input[[uploadorsig]],
      "sig" = {
        sigtable = eval(siglists)
        drop.cols = colnames(sigtable) %in% c("sig.rows", "excluded.rows", "descriptive")
        sigtable[!drop.cols]
      },
      "protlist" = {
        md = metadata$all.protlists
        data.frame("Name" = md$name, "Owner" = md$owner, stringsAsFactors = FALSE)
      }
    )
  }, options = list(lengthChange = FALSE, scrollX = T, scrollY = "300px", paging = FALSE, scrollCollapse = TRUE, dom = 't'),
  selection = list(target = "row", mode = "single"),
  rownames = FALSE)
    
  rct.list$nextdisp = observeEvent(input[[newsetnext]], {
    switch(rct.list$rv$popupdisp,
      "source" = {
        nameexists = isTruthy(input[[newsetname]])
        sourceexists = isTruthy(rct.list$loadedtable())
        
        if (nameexists && sourceexists)
        {
          if (input[[uploadorsig]] == "protlist") rct.list$rv$popupdisp = "preview" else rct.list$rv$popupdisp = "prot"
        } else {
          actions = NULL
          if (!nameexists) actions = c(actions, "enter a name for this set of proteins")
          if (!sourceexists) actions = c(actions, "upload or select a set of proteins")
          
          message = paste0("Please ", paste(actions, collapse = " and "), ".")
          
          showModal(modalDialog(message, title = "Action required", easyClose = TRUE, size = "s", fade = FALSE), session = session)
        }
      },
      "prot" = {
        if (length(input[[paste0(protseltable, "_columns_selected")]]) == 1)
          rct.list$rv$popupdisp = "preview"
      },
      "preview" = {
        rct.list$rv$setinfo.name = c(rct.list$rv$setinfo.name, input[[newsetname]])
        rct.list$rv$setinfo.origin = switch(input[[uploadorsig]],
          "upload" = c(rct.list$rv$setinfo.origin, paste("file:", rct.list$fu$fileinput()$name)),
          "sig" = c(rct.list$rv$setinfo.origin, paste("list:", eval(siglists)$name[ input[[paste0(siglistseltbl, "_rows_selected")]] ])),
          "protlist" = c(rct.list$rv$setinfo.origin, paste("list:", metadata$all.protlists$name[ input[[paste0(siglistseltbl, "_rows_selected")]] ]))
        )
        rct.list$rv$setinfo.idtype = c(rct.list$rv$setinfo.idtype, input[[newsetidtype]])
        rct.list$rv$setinfo.species = input[[newsetspecies]]
       
        rct.list$rv$setlist = c(rct.list$rv$setlist, list(rct.list$preview.set()))
       
        rct.list$rv$popupdisp = "none"
        toggleModal(session, newsetpopup, toggle = "close")
      }
    )
  })
  
  rct.list$lastdisp = observeEvent(input[[newsetback]], {
    switch(rct.list$rv$popupdisp,
           "source" = {
             rct.list$rv$popupdisp = "none"
             toggleModal(session, newsetpopup, toggle = "close")
           },
           "prot" = {
             rct.list$rv$popupdisp = "source"
           },
           "preview" = {
             if (input[[uploadorsig]] == "protlist") rct.list$rv$popupdisp = "source" else rct.list$rv$popupdisp = "prot"
           }
    )
  })
  
  output[[outputdisp]] <<- renderText(rct.list$rv$popupdisp)
  outputOptions(output, outputdisp, suspendWhenHidden = FALSE)
  
  rct.list$loadedtable = reactive ({
    switch(EXPR = input[[uploadorsig]],
      "upload" = {
        rct.list$fu$file.final()
      },
      "sig" = {
        sel.sig = input[[paste0(siglistseltbl, "_rows_selected")]]
        if (isTruthy(sel.sig))
        {
          data.frame(eval(siglists)$descriptive[[sel.sig]], stringsAsFactors = FALSE)
        } else {
          NULL
        }
      },
      "protlist" = {
        sel.protlist = input[[paste0(siglistseltbl, "_rows_selected")]]
        if (isTruthy(sel.protlist))
        {
          md = metadata$all.protlists[sel.protlist,]
          fl = open.file(md$filepath, "rdata")[[1]]
          cn = md$colnums[[1]]
          df = list()
          df$Gene = if (!is.na(cn$Gene[1])) fl[cn$Gene] else NULL
          df$Uniprot = if (!is.na(cn$Uniprot[1])) fl[cn$Uniprot] else NULL
          df$Entrez = if (!is.na(cn$Entrez[1])) fl[cn$Entrez] else NULL
          
          as.data.frame(df) 
        } else {
          NULL
        }
      },
      NULL
    )
  })
  
  output[[protseltable]] <<- DT::renderDataTable({
    rct.list$loadedtable()
  }, options = list(lengthChange = FALSE, scrollX = T, scrollY = "400px", scrollCollapse = TRUE, rownames = FALSE, dom = 't'),
  selection = list(target = "column", mode = "single"),
  rownames = FALSE)
  
  rct.list$setinfotable = reactive ({
    data.frame("Name" = rct.list$rv$setinfo.name, "Origin" = rct.list$rv$setinfo.origin,
               "Species" = rct.list$rv$setinfo.species)
  })
  
  observe({
    rw = input[[paste0(siglistseltbl, "_rows_selected")]]
    idtypes = c(
      "Entrez ID" = "ENTREZID",
      "Uniprot ID" = "UNIPROT",
      "Gene symbol" = "SYMBOL"
    )
    
    if(input[[uploadorsig]] == "protlist" && length(rw) > 0)
    {
        cn = metadata$all.protlists$colnums[[rw]]
        available.ids = !is.na(c(cn$Entrez[1], cn$Uniprot[1], cn$Gene[1]))
        choices = idtypes[available.ids]
        
        updateSelectInput(session, newsetidtype, choices = choices)
    } else {
      updateSelectInput(session, newsetidtype, choices = idtypes)
    }
  })
  
  rct.list$preview.set = reactive ({
    
    prot.col = input[[paste0(protseltable, "_columns_selected")]] + 1
    prot.dat = switch (input[[uploadorsig]],
      "protlist" = {
        rw = input[[paste0(siglistseltbl, "_rows_selected")]]
        cn = metadata$all.protlists$colnums[[rw]]
        md = metadata$all.protlists[rw,]
        fl = open.file(md$filepath, "rdata")[[1]]
        
        coltypes = c("ENTREZID" = "Entrez", "UNIPROT" = "Uniprot", "SYMBOL" = "Gene")
        idtype = input[[newsetidtype]]
        cl = cn[[ coltypes[[idtype]] ]]
        original = fl[,cl]
        
        data.frame(original = original, stringsAsFactors = FALSE)
      },
      {
        data.frame(original = as.character(rct.list$loadedtable()[,prot.col]), stringsAsFactors = FALSE)
      }
    )

    prot.dat$first = toupper(sapply(strsplit(prot.dat$original, split = ";", fixed = TRUE),
                            function(x) x[1]))
    
    prot.dat$entrezid = if (input[[newsetidtype]] == "ENTREZID")
    {
      prot.dat$first
    } else {
      db.name = switch(EXPR = input[[newsetspecies]],
        "hsa" = "org.Hs.eg.db",
        "mmu" = "org.Mm.eg.db",
        "rno" = "org.Rn.eg.db",
        "dme" = "org.Dm.eg.db"
      )
      
      db.fn = paste(dbdir, "org.dbs", db.name, sep = "/")
      load(db.fn)
      conv.field = input[[newsetidtype]]
      idx = match(prot.dat$first, dat.conv[[conv.field]], nomatch = NA)
      dat.conv$ENTREZID[idx]
    }
    prot.dat = na.omit(prot.dat)
    
    prot.dat
    
  })
  
  output[[setpreviewtable]] <<- DT::renderDataTable (rct.list$preview.set()[,c("original", "entrezid")],
    options = list(lengthChange = FALSE, dom = 't'), rownames = FALSE,
    selection = list(mode = "none"))
  
  output[[setlisttable]] <<- DT::renderDataTable (rct.list$setinfotable(),
    options = list(lengthChange = FALSE, dom = 't'), rownames = FALSE,
    selection = list(target = "row", mode = "multiple"))
  
  rct.list$deleteSets = observeEvent (input[[delsetsbtn]], {
    validate(need(input[[paste0(setlisttable, "_rows_selected")]], label = ""))
    del.rows = input[[paste0(setlisttable, "_rows_selected")]]
    
    rct.list$rv$setinfo.name = rct.list$rv$setinfo.name[-del.rows]
    rct.list$rv$setinfo.origin = rct.list$rv$setinfo.origin[-del.rows]
    rct.list$rv$setinfo.idtype = rct.list$rv$setinfo.idtype[-del.rows]
    rct.list$rv$setlist = rct.list$rv$setlist[-del.rows]
    
    if (length(rct.list$rv$setinfo.name) == 0)
    {
      rct.list$rv$setinfo.species
    }
  })
  
  rct.list$clearAllSets = observeEvent (input[[clearsetsbtn]], {
    
    rct.list$rv$setinfo.name = NULL
    rct.list$rv$setinfo.origin = NULL
    rct.list$rv$setinfo.typeid = NULL
    rct.list$rv$setinfo.species = NULL
    rct.list$rv$setlist = NULL
  })
  
  ##################
  ### Main panel ###
  ##################
  
  rct.list$fsh.test = eventReactive (input[[runfshbtn]], {
    
    # Progress message
    progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
    on.exit(progress$close())
    
    progress$set(0, message = "Running hypergeometric test...")
    
    # Get hits from selected lists of significance

    sigtable.all = do.call(rbind, rct.list$rv$setlist)
    colnames(sigtable.all) = c("original", "first", "entrezid")
    sigtable.all = sigtable.all[!duplicated(sigtable.all$entrezid),]
    sig.eids = sigtable.all$entrezid
    
    # Retrieve pathway data for selected organism
    orgCode = rct.list$rv$setinfo.species
    universe = getKeggFile[[orgCode]]()
    uni.genes = universe$genes
        
    num.threads = max(1, detectCores() - 1)
    fisher.results = if (is.finite(num.threads) && num.threads > 1)
    {
      cl = makeCluster(num.threads)
      
      cur.env = environment()
      
      clusterExport(cl, list("uni.genes", "sig.eids", "getMatVals"), envir = cur.env)
      
      # Do test
      fisher.results = parSapply(cl, universe$paths, function(pathgenes) {
        vals = getMatVals(sig.eids, uni.genes, pathgenes)
        pathmat = matrix(vals, nrow = 2, dimnames = list(Regulated = c("Yes", "No"), OnPath = c("Yes", "No")))
        fisher.test(pathmat, alternative = "greater")$p.value
      }) 
      
      stopCluster(cl)
      
      fisher.results
    } else {
      sapply(universe$paths, function(pathgenes) {
        vals = getMatVals(sig.eids, uni.genes, pathgenes)
        pathmat = matrix(vals, nrow = 2, dimnames = list(Regulated = c("Yes", "No"), OnPath = c("Yes", "No")))
        fisher.test(pathmat, alternative = "greater")$p.value
      }) 
    }
    
    fsh.res.adj = p.adjust(fisher.results, method = "BH")
    
    sig.path.ind = which(fsh.res.adj < 0.05)
    sig.path.p = fisher.results[sig.path.ind]

    sig.path.ind = sig.path.ind[order(sig.path.p)]
    sig.path.p = fisher.results[sig.path.ind]
    fsh.p.adj = fsh.res.adj[sig.path.ind]
    
    sig.path.dsc = universe$pathdesc[sig.path.ind]
    
    overlap.eid = lapply(universe$paths[sig.path.ind], function(pathgenes) sig.eids[sig.eids %in% pathgenes])
    overlap = lapply(overlap.eid, function(olp) sigtable.all$original[match(olp, sigtable.all$entrezid)])
    
    num.sigs = sapply(overlap, length)
    num.in.path = sapply(universe$paths[sig.path.ind], length)
    
    pathsummary = data.frame("keggid" = universe$pathnames[sig.path.ind], "description" = sig.path.dsc, "p.value" = sig.path.p, "adj.p" = fsh.p.adj, "num.sigs" = num.sigs, "num.in.path" = num.in.path)
    pathsummary$genes = overlap
    
    pathsummary$description = switch(rct.list$rv$setinfo.species[1],
      "hsa" = {
        gsub(" - Homo sapiens (human)", replacement = "", pathsummary$description, ignore.case = TRUE, fixed = TRUE)
      },
      "mmu" = {
        gsub(" - Mus musculus (mouse)", replacement = "", pathsummary$description, ignore.case = TRUE, fixed = TRUE)
      }
    )
    
    pathsummary
    
  })
  
  rct.list$fisher.table = reactive ({
    
    results = rct.list$fsh.test()
    overlaps = sapply(results$genes, paste, collapse = " | ")
    overlaps = mapply(FUN = function(genes, num.sigs, num.in.path) paste0(genes, " (", num.sigs, "/", num.in.path, ")"), overlaps, results$num.sigs, results$num.in.path)
    
    data.frame("KEGG ID" = results$keggid, "Description" = results$description, "P value" = format(results$p.value, 3, scientific = TRUE), "Adjusted P value" = format(results$adj.p, 3, scientific = TRUE), "Overlap" = overlaps)
    
  })
  
  output[[fsh.rdy]] = renderText({
    if (isTruthy(rct.list$fisher.table())) "true" else "false"
  })
  outputOptions(output, fsh.rdy, suspendWhenHidden = FALSE)

  output[[fsh.table]] = DT::renderDataTable (rct.list$fisher.table(), options = list(lengthChange = FALSE, scrollX = T, scrollCollapse = TRUE, rownames = FALSE, dom = 'ftp'),
  selection = list(mode = "none"),
  rownames = FALSE)
  
  rct.list$fsh.result.plot = reactive ({
    dat.long = rct.list$fsh.test()
    dat.long$log.p.value = -log10(dat.long$p.value)
    splitdescriptions = strsplit(dat.long$description, split = " - ", fixed = TRUE)
    dat.long$description = sapply(splitdescriptions, function(x) x[1])
    # dat.long$description = factor(dat.long$description, levels = dat.long$description[order(-dat.long$p.value)])
    dat.long$text = paste0(
      "Pathway: ", dat.long$description, "<br>",
      "P-value: ", signif(dat.long$p.value, digits = 3), "<br>",
      "Overlap: ", sapply(dat.long$genes, function(genes) {
        splitby = (1:length(genes) - 1) %/% 4
        linechunks = split(genes, splitby)
        overlap.lines = sapply(linechunks, paste0, collapse = ", ")
        return(paste0(overlap.lines, collapse = ",<br>"))
      })
    )

    path_order = order(-dat.long$p.value)
    
    neglog10_trans = function() {
      trans = function(x) -log10(x)
      inv = function(x) 10^(-x)
      trans_new("neglog-10", trans, inv, log_breaks(base = 10), domain = c(1e-1000, Inf))
    }
    
    
    p <- ggplot(data = dat.long, aes(x = keggid, y = p.value, text = text)) +
      geom_bar(stat = "identity", fill = "dodgerblue", aes(size = 0)) +
      scale_x_discrete(limits = dat.long$keggid[path_order], labels = dat.long$description[path_order]) +
      scale_y_continuous(trans = neglog10_trans()) +
      theme(
        panel.background = element_rect(fill = "white", colour = "black"),
        axis.ticks.y = element_blank(),
        legend.position = "none"
      ) +
      xlab("Pathway") +
      ylab("P-Value (rendered on log scale)") +
      coord_flip()
  })
  
  output[[fsh.result.plot]] = renderPlotly({
    p = ggplotly(rct.list$fsh.result.plot(), tooltip = c("text"))
    
    p
  })
  
  output[[downloadfisher]] = downloadHandler (
    filename = function() {
      savetype = input[[downloadtype]]
      fext = switch(savetype,
                    "xls" = ".xlsx",
                    "tsv" = ".txt",
                    "csv" = ".csv",
                    "rdata" = ".RData"
      )
      
      fn = paste0("fisher.", Sys.Date(), fext)
      return(fn)
    },
    content = function(fn) {
      savetype = input[[downloadtype]]
      
      save.rdata = rct.list$fsh.test()
      save.data.other = rct.list$fisher.table()
      
      switch(savetype,
             "xls" = write_xlsx(save.data.other, path = fn, col_names = TRUE),
             "tsv" = write.table(save.data.other, file = fn, sep = "\t", col.names = NA),
             "csv" = write.table(save.data.other, file = fn, sep = ",", col.names = NA),
             "rdata" = save(save.rdata, file = fn)
      )
    }
  )
  
  
  
  output[[download.fsh.png]] = downloadHandler (
    filename = function() paste0("fsh.", Sys.Date(), ".png"),
    content = function(fn) {
      ggsave(fn, plot = rct.list$fsh.result.plot(), width = 16, height = 12, dpi = 600)
    }
  )
  
  output[[download.fsh.eps]] = downloadHandler (
    filename = function() paste0("fsh.", Sys.Date(), ".eps"),
    content = function(fn) {
      ggsave(fn, plot = rct.list$fsh.result.plot(), width = 16, height = 12, dpi = 600)
    }
  )
  
  rct.list$save.pathlist = observeEvent( input[[saveresults]],
  {
    currenttable = rct.list$fsh.test()
    validate(need(input[[savename]], label = "Name"))
    validate(need(input[[saveowner]], label = "Owner"))
    validate(need(currenttable, message = "You need to run the test first."))
    
    name = input[[savename]]
    owner = input[[saveowner]]
    
    next.pathlist = metadata$next.pathlist
    pathlist.filepath = paste0(dbdir, "pathlists", as.character(next.pathlist), sep = "/")
    metadata$next.pathlist = next.pathlist + 1
    
    new.pathlist = data.frame(idx = next.pathlist, name = input[[savename]], owner = input[[saveowner]], organism = rct.list$rv$setinfo.species[1], filepath = pathlist.filepath, stringsAsFactors = FALSE)
    new.pathlist = data.frame(idx = next.pathlist, name = input[["pathwaytestsavename"]], owner = input[["pathwaytestsaveowner"]], organism = rct.list$rv$setinfo.species[1], filepath = pathlist.filepath, stringsAsFactors = FALSE)

    progress = shiny::Progress$new(session, min = 0, max = 1)
    on.exit(progress$close())
    progress$set(message = "Please wait. ", detail = paste0("Saving pathway list as \"", name, "\"..."))
    
    metadata$all.pathlists = rbind(metadata$all.pathlists, new.pathlist)
    
    save(currenttable, file = pathlist.filepath)
    
    metadata.list = reactiveValuesToList(metadata)
    save(metadata.list, file = metadata.path)
    
    progress$set(message = "File saved.", detail = "")
    
  })
  
  return (rct.list)
}
