library("readxl")

get.leaves = function(tree, classes = "any", data.frame.as.list = F)
{
  leaves = list()
  
  if (data.frame.as.list)
  {
    permitted.tree.classes = c("list", "data.frame")
  } else {
    permitted.tree.classes = "list"
  }
  
  if ("list" %in% classes)
  {
    stop("'list' not a valid argument to classes")
  } else if ("data.frame" %in% classes && data.frame.as.list) {
    stop("'data.frame' not a valid argument to classes if data.frame.as.list is true")
  } else if (!class(tree) %in% permitted.tree.classes) {
    stop("argument to 'tree' is not of a permitted class ('list' only or 'data.frame' if data.frame.as.list is true)")
  }
  
  while(length(tree) > 0)
  {
    class.vec = sapply(tree, class)
    mode.vec = sapply(tree, mode)
    list.inds = which(mode.vec == "list" & (class.vec != "data.frame" | data.frame.as.list))
    new.tree = unlist(tree[list.inds], recursive = FALSE)
    
    all.leaves = tree[-list.inds]
    
    if ("any" %in% classes)
    {
      new.leaves = all.leaves
    } else {
      new.leaves = tree[class.vec %in% classes]
    }
    
    leaves = c(leaves, new.leaves)
    tree = new.tree
  }
  
  return(leaves)
}

# Upload input step controls
rv.uplnav = reactiveValues(step = "1.upload", disp.prev = FALSE, disp.next = FALSE, disp.save = FALSE,
  upload.steps.vec = c(
    "1.restore",
    "1.upload",
    "2.fileinfo",
    "3.dstype",
    "4"
  ))

output$upl.step = renderText({
  rv.uplnav$step
})
outputOptions(output, "upl.step", suspendWhenHidden = FALSE)

output$upl.disp.next = renderText(if (rv.uplnav$disp.next) "TRUE" else "FALSE")
output$upl.disp.prev = renderText(if (rv.uplnav$disp.prev) "TRUE" else "FALSE")
output$upl.disp.save = renderText(if (rv.uplnav$disp.save) "TRUE" else "FALSE")
outputOptions(output, "upl.disp.next", suspendWhenHidden = FALSE)
outputOptions(output, "upl.disp.prev", suspendWhenHidden = FALSE)
outputOptions(output, "upl.disp.save", suspendWhenHidden = FALSE)


# Set "Display next" based on current upload 'page' and whether or not all fields have been completed.
observe({
  rv.uplnav$disp.next = FALSE
  
  switch(rv.uplnav$step,
    "1.upload" = {
      validate(need(file.final(), message = FALSE))
      rv.uplnav$disp.next = TRUE
    },
    "2.fileinfo" = {
      rv.uplnav$disp.next = all(
        isTruthy(input$save.name),
        !input$save.name %in% metadata$all.datasets$name,
        isTruthy(input$save.owner)
      )
    },
    "3.dstype" = {
      rv.uplnav$disp.next = all(
        input$sel.datatype %in% c("results", "protlist")
      )
    }
  )
})

# Set "Display previous" based on current upload 'page' and whether or not all fields have been completed.
observe(
{
  if (rv.uplnav$step == rv.uplnav$upload.steps.vec[1])
    rv.uplnav$disp.prev = FALSE
  else
    rv.uplnav$disp.prev = TRUE
})

# Set "Display save button" based on current upload 'page' and whether or not all fields have been completed.
observe(
{
  switch(rv.uplnav$step,
    "3.dstype" = {
      rv.uplnav$disp.save = all(
        input$sel.datatype %in% c("rawfile", "rawtable", "proctable")
      )
    },
    "4.results" = {
      rv.uplnav$disp.save = all(
        isTruthy(input$res.cont.name),
        !is.na(rv.res$colnums$LogFC),
        any(
          !is.na(rv.res$colnums$"Raw P"),
          !is.na(rv.res$colnums$"Adj P")
        ),
        any(
          !is.na(rv.res$colnums$Gene),
          !is.na(rv.res$colnums$Entrez),
          !is.na(rv.res$colnums$Uniprot)
        )
      )
    },
    "4.protlist" = {
      rv.uplnav$disp.save = all(
        input$protlist.from.analysis == "false" || all(
          isTruthy(input$protlist.p.cutoff),
          isTruthy(input$protlist.lfc.comp),
          isTruthy(input$protlist.lfc.cutoff),
          isTruthy(input$protlist.p.type)
        ),
        any(
          !is.na(rv.protlist$colnums$Gene),
          !is.na(rv.protlist$colnums$Entrez),
          !is.na(rv.protlist$colnums$Uniprot)
        )
      )
    },
    {
      rv.uplnav$disp.save = FALSE
    }
  )
})

uploadstep.prev = observeEvent(input$upload.prev,
{
  currentstep = rv.uplnav$step
  if (grepl("4.*", currentstep, fixed = FALSE))
  {
    currentstep = "4"
  }
  rv.uplnav$step = switch(currentstep,
    "2.fileinfo" = "1.upload",
    "3.dstype" = "2.fileinfo",
    "4" = "3.dstype"
  )
})

uploadstep.next = observeEvent(input$upload.next,
{
  switch(rv.uplnav$step,
    "1.upload" = {
      rv.uplnav$step = "2.fileinfo"
    },
    "2.fileinfo" = {
      rv.uplnav$step = "3.dstype"
    },
    "3.dstype" = {
      if (input$sel.datatype %in% c("results", "protlist"))
      {
        rv.uplnav$step = paste0("4.", input$sel.datatype)
      }
    }
  )
})

uploadstep.save = observeEvent(input$upload.save,
  {
    switch(rv.uplnav$step,
      "3.dstype" = {
        name = input$save.name
        owner = input$save.owner
        orig.file = input$up.file$name
        next.file = metadata$next.file
        file.name = paste(dbdir, "datasets", as.character(next.file), sep = "/")
        metadata$next.file = next.file + 1
        filetype = list(up.file.type())
        new.row = data.frame(name = name, owner = owner, organism = input$save.organism, long.desc = input$save.description, orig.file = orig.file, file = file.name, processed = FALSE, stamp = as.character(Sys.time()), stringsAsFactors = FALSE)
        new.row$filetype = filetype
        
        switch(input$sel.datatype,
          "rawfile" = {
            
            if (!(owner %in% metadata$all.owners)) {
              metadata$all.owners = union(metadata$all.owners, owner)
            }
            
            progress = shiny::Progress$new(session, min = 0, max = 1)
            on.exit(progress$close())
            progress$set(message = "Please wait. ", detail = paste0("Saving ", orig.file, " as \"", name, "\"..."))
            metadata$all.datasets <- rbind(metadata$all.datasets, new.row)
            file.copy(from = input$up.file$datapath, to = file.name, copy.date = TRUE)
            
            metadata.list = reactiveValuesToList(metadata)
            save(metadata.list, file = metadata.path)
            
            progress$set(message = "File saved.", detail = "")           
          },
          "rawtable" = {
            new.row$filetype = list("rdata")
            
            if (!(owner %in% metadata$all.owners)) {
              metadata$all.owners = union(metadata$all.owners, owner)
            }
            
            metadata$all.datasets <- rbind(metadata$all.datasets, new.row)
            
            progress = shiny::Progress$new(session, min = 0, max = 1)
            on.exit(progress$close())
            progress$set(message = "Please wait. ", detail = paste0("Saving table from", orig.file, " as \"", name, "\"..."))
            currenttable = file.final()
            save(currenttable, file = file.name)
            
            metadata.list = reactiveValuesToList(metadata)
            save(metadata.list, file = metadata.path)
            
            progress$set(message = "Table saved.", detail = "")          
          },
          "proctable" = {
            new.row$processed = TRUE
            new.row$filetype = list("rdata")
            
            if (!(owner %in% metadata$all.owners)) {
              metadata$all.owners = union(metadata$all.owners, owner)
            }
            
            metadata$all.datasets <- rbind(metadata$all.datasets, new.row)
            
            progress = shiny::Progress$new(session, min = 0, max = 1)
            on.exit(progress$close())
            progress$set(message = "Please wait. ", detail = paste0("Saving table from", orig.file, " as \"", name, "\"..."))
            currenttable = file.final()
            save(currenttable, file = file.name)
            
            metadata.list = reactiveValuesToList(metadata)
            save(metadata.list, file = metadata.path)
            
            progress$set(message = "Table saved.", detail = "")
          }
        )
      },
      "4.results" = {
        name = input$save.name
        owner = input$save.owner
        orig.file = input$up.file$name
        
        next.results = metadata$next.results
        results.filepath = paste(dbdir, "results", as.character(next.results), sep = "/")
        metadata$next.results = next.results + 1
        
        new.results = data.frame(idx = next.results, name = name, owner = owner, cont.name = input$res.cont.name, cont.formula = input$res.contrast, organism = input$save.organism, long.desc = input$save.description, parent.results = next.results, filepath = results.filepath, stamp = as.character(Sys.time()), stringsAsFactors = FALSE)
        new.results$colnums = list(rv.res$colnums)
        
        progress = shiny::Progress$new(session, min = 0, max = 1)
        on.exit(progress$close())
        progress$set(message = "Please wait. ", detail = paste0("Saving ", orig.file, " as \"", name, "\"..."))
        
        metadata$all.results = rbind(metadata$all.results, new.results)
        
        currenttable = file.final()
        save(currenttable, file = results.filepath)
        
        metadata.list = reactiveValuesToList(metadata)
        save(metadata.list, file = metadata.path)
        
        progress$set(message = "File saved.", detail = "")
        
      },
      "4.protlist"  = {
        name = input$save.name
        owner = input$save.owner
        orig.file = input$up.file$name
        
        p.cutoff = NA
        lfc.comp = NA
        lfc.cutoff = NA
        p.type = NA
        
        if (input$protlist.from.analysis == "true")
        {
          p.cutoff = input$protlist.p.cutoff
          lfc.comp = input$protlist.lfc.comp
          lfc.cutoff = input$protlist.lfc.cutoff
          p.type = input$protlist.p.type
        }
        
        next.protlist = metadata$next.protlist
        filepath = paste(dbdir, "protlists", as.character(next.protlist), sep = "/")
        metadata$next.protlist = next.protlist + 1
        
        new.protlist = data.frame(idx = next.protlist, name = name, owner = owner, organism = input$save.organism, parent.results = NA, filepath = filepath,
          p.cutoff = p.cutoff, lfc.comp = lfc.comp, lfc.cutoff = lfc.cutoff, p.type = p.type, stringsAsFactors = FALSE)
        new.protlist$colnums = list(rv.protlist$colnums)
        
        progress = shiny::Progress$new(session, min = 0, max = 1)
        on.exit(progress$close())
        progress$set(message = "Please wait. ", detail = paste0("Saving ", orig.file, " as \"", name, "\"..."))
        
        metadata$all.protlists = rbind(metadata$all.protlists, new.protlist)
        
        currenttable = file.final()
        save(currenttable, file = filepath)
        
        metadata.list = reactiveValuesToList(metadata)
        save(metadata.list, file = metadata.path)
        
        progress$set(message = "File saved.", detail = "")
      }
    )
    
    updateTextInput(session, "save.name", value = "")
    
    updateTextInput(session, "res.cont.name", value = "")
    updateTextInput(session, "res.contrast", value = "")
    
    updateNumericInput(session, "protlist.p.cutoff", min = 0, max = 0.1, value = 0.05, step = 0.0001)
    updateRadioButtons(session, "protlist.p.type", selected = "adj.p.val")
    updateRadioButtons(session, "protlist.lfc.comp", selected = "both")
    updateNumericInput(session, "protlist.lfc.cutoff", value = 1)
    
    rv.res$colnums = list("Gene" = NA, "Entrez" = NA, "Uniprot" = NA, "Misc" = NA, "LogFC" = NA, "T stat" = NA, "Raw P" = NA, "Adj P" = NA)
    rv.protlist$colnums = list("Gene" = NA, "Entrez" = NA, "Uniprot" = NA, "Misc" = NA)
    DT::selectRows(res.col.proxy, NULL)
    DT::selectRows(protlist.col.proxy, NULL)
    DT::selectColumns(load.data.preview.proxy, NULL)
    
    showModal(
      modalDialog(title = "Saved to server", size = "s", easyClose = TRUE,
      HTML(paste("Data saved to Thunderbolt server.",
        switch(EXPR = input$sel.datatype,
          "3.dstype" = "Access through the 'Restore' tab under File Management.",
          "4.results" = "Access and filter results through the Search module.",
          "4.protlist" = "Access protein lists through the Search module or compare protein lists using the Compare module."
        ), sep = "<br><br>"))
      )
    )
    
    rv.uplnav$step = "1.upload"
  })

fileman.startde = observeEvent(input$fileman.startde, {
  rv$loaded.table = loaded.table()
  
  updateTextInput(session, "new.var.name", value = "")
  updateTextInput(session, "new.lev.name", value = "")
  updateTextInput(session, "new.lev.search", value = "")
  updateTextInput(session, "auto.numeric.string", value = "")
  
  rv$var.names = c()
  rv$var.levels = list()
  rv$var.search = list()
  rv$draft.varlevels = c()
  rv$draft.varsearch = c()
  
  updateTextInput(session, "new.var.name", value = "")
  updateTextInput(session, "new.lev.name", value = "")
  updateTextInput(session, "new.lev.search", value = "")
  
  checkpoints$variables.created = FALSE
  checkpoints$design.table.created = FALSE
  checkpoints$descriptive.exists = FALSE
  checkpoints$numeric.exists = FALSE
  resultsrv$results.exist = FALSE
  
  shinyjs::hide("variabletable", anim = TRUE)
  shinyjs::hide("delete.vt.row", anim = TRUE)
  shinyjs::hide("clear.var", anim = TRUE)
  shinyjs::show("varcreate.instructions", anim = TRUE)
  
  hideTab("tb.nav", target = "Data exploration and pre-processing", session = session)
  hideTab("tb.nav", target = "Define and Run Analysis", session = session)
  hideTab("tb.nav", target = "DE Results", session = session)
  showTab("tb.nav", target = "Experimental design", select = TRUE, session = session)
})

# When a new owner is added, update the list of owners
updateowners = observe ({
  updateSelectizeInput(session, 'owner.list', choices = metadata$all.owners, server = F)
})

# When an existing owner is selected, update the owner text field
updatenewowner = observe ({
  selectedowner = input$owner.list
  updateTextInput(session, 'save.owner', value = selectedowner)
})

# When A new file is uploaded, blank the file name text field
blanknewname = observe ({
  input$up.file
  updateTextInput(session, 'save.name', value = "")
  
})

# Read data
up.file.type = reactive ({
  fileinput = input$up.file
  if (is.null(fileinput)) {
    NULL
  } else {
    filetype = c(input$select.filetype, input$customDelimiter)
    
    if(filetype[1] == "auto") {
      filename = fileinput$name
      filename.vec = strsplit(filename, split = ".", fixed = TRUE)[[1]]
      filetype = tolower(filename.vec[length(filename.vec)])
      if(filetype[1] == "csv") filetype = c("dlm", ",")
      if(filetype[1] == "txt") filetype = c("dlm", "\t")
      if(filetype[1] == "select") filetype = c("dlm", input$customDelimiter)
      if(filetype[1] == "xls") filetype = "xlsx"
    }
    filetype
  }
})

open.file = function(filepath, filetype, header = TRUE)
{
  # Excel file
  case.excel = function() {
    sheets = excel_sheets(filepath)
    numsheets = length(sheets)
    
    out = lapply(seq(numsheets), read_excel, path = filepath, col_names = input$header, na = c("", "NA", "NaN", "Inf", "-Inf"))
    out = lapply(out, as.data.frame)
    
    if (length(names(sheets) == length(unique(names(sheets)))))
      names(out) = names(sheets)
    return(out)
  }
  
  # ASCII file with delimiter (CSV, TSV, etc)
  case.dlm = function(dlm) {
    out = list(read.table(filepath,
                          sep = dlm, stringsAsFactors = FALSE,
                          header = input$header, na.strings = c("NA", "NaN", "Inf", "-Inf"), quote = input$stringquote))
    names(out) = "Table"
    return(out)
  }
  
  # RData file
  case.rdata = function() {
    file.env = new.env()
    load(filepath, envir = file.env, verbose = FALSE)
    
    out = as.list(file.env)
    rm(file.env)
    
    out = get.leaves(out, classes = c("data.frame", "matrix"))
    out = lapply(out, as.data.frame)

    return(out)
  }
  
  switch(filetype[1],
         "xlsx" = case.excel(),
         "dlm" = case.dlm(filetype[2]),
         "rdata" = case.rdata()
  )
}

table.list = reactive ({
  updateSelectInput(session, "select.table", choices = c())
  fileinput = input$up.file
  validate(need(fileinput, message = "No file uploaded"))
  if (is.null(fileinput)) {
    NULL
  } else {
    filetype = up.file.type()
    
    out = tryCatch(
      expr = open.file(filepath = fileinput$datapath, filetype = filetype, header = input$header),
      error = function(x) {
        showModal(modalDialog(title = "Error loading file", size = "s", easyClose = TRUE,
          "There was an error loading the file; please check the file layout is correct."
        ))
        
        validate(
          "There was an error loading the file; please check the file layout is correct."
        )        
      }
    )
    
    nm.vec = seq(length(out))
    names(nm.vec) = names(out)
    updateSelectInput(session, "select.table", choices = nm.vec, label = "Choose a worksheet", selected = 1)
    
    return(out)
  }
})

output$dispselectsheet = renderText ({
  filetype = up.file.type()
  validate(need(filetype, message = "NA"))
  
  if (length(table.list()) > 1)
  {
    "TRUE"
  } else {
    "FALSE"
  }
})
outputOptions(output, "dispselectsheet", suspendWhenHidden = FALSE)

# Process data as required by options
file.final = reactive ({
  
  objfile = table.list()
  validate(need(input$select.table, label = "Table"))
  
  out = objfile[[strtoi(input$select.table)]]
  
  if (input$transpose == TRUE) {
    out = t(out)
  }
  
  out = as.data.frame(lapply(out, function(cl) if (class(cl) == "factor") as.character(cl) else cl), stringsAsFactors = FALSE)
  
  out
  
})

### Load file
updateloadowners = observe ({
  owner.vec = metadata$all.owners
  if (length(owner.vec) > 0)
    updateSelectizeInput(session, 'select.load.owners', choices = owner.vec, selected = NULL)
})


updatedataset = observe ({
  if (is.null(input$select.load.owners)) {
    name.vec = metadata$all.datasets$name
  } else {
    dsrows = which(metadata$all.datasets$owner %in% input$select.load.owners)
    name.vec = metadata$all.datasets[dsrows, "name"]
  }
  if (length(name.vec) > 0)
    updateSelectizeInput(session, 'select.load.dataset', choices = c("Choose a dataset" = "", name.vec), selected = NULL)
})


loaded.dataset = reactive ({
  nm = input$select.load.dataset
  
  if (nm == "")
  {
    checkpoints$file.loaded = F
    validate(need(nm, message = "Please select a dataset"))
  } else {
    i = which(metadata$all.datasets$name == input$select.load.dataset)
    filename = metadata$all.datasets$file[i]
    filetype = metadata$all.datasets$filetype[[i]]
    
    progress = shiny::Progress$new(session, min = 0, max = 1)
    on.exit(progress$close())
    progress$set(message = "Please wait. ", detail = paste0("Loading dataset..."))
    
    # Load file based on file type
    objfile = open.file(filepath = filename, filetype = filetype, header = input$chk.load.header)
    
    nm.vec = seq(length(objfile))
    names(nm.vec) = names(objfile)
    updateSelectInput(session, "select.load.table", choices = nm.vec, label = "Choose a worksheet")
    
    validate(need(objfile, message = "Dataset not loaded"))
    objfile
  }
})

loaded.table = reactive ({
  choice = input$fu.tabsetpanel
  if (choice == "restore")
  {
    tbl = loaded.dataset()
    validate(need(input$select.load.table, message = "Retrieving subtables..."))
    tbl = tbl[[strtoi(input$select.load.table)]]
    if (input$chk.load.transpose)
    {
      tbl = t(tbl)
    }
    checkpoints$file.loaded = TRUE
    
    tbl
  } else if (choice == "upload.save")
  {
    file.final()
  }
})

output$fileman.data.loaded = renderText({
  if (isTruthy(loaded.table())) {
    "true"
  } else {
    "false"
  }
})
outputOptions(output, "fileman.data.loaded", suspendWhenHidden = FALSE)

output$load.data.preview = DT::renderDataTable ({
  dat = loaded.table()
  
  conv.coltypes = !sapply(dat, class) %in% c("numeric", "integer", "character")
  dat[conv.coltypes] = lapply(dat[conv.coltypes], as.character)
  charcols = sapply(dat, class) == "character"
  dat[charcols] = lapply(dat[charcols], function(x) {
    x[nchar(x, keepNA = FALSE) > 20] = paste0(substring(x[nchar(x, keepNA = FALSE) > 20], first = 1, last = 17), "...")
    return(x)
  })
  
  dat
}, options = list(lengthChange = TRUE, pageLength = 5, rownames = TRUE, dom = 'lftipr'),
  filter = list(position = "bottom"), rownames = FALSE,
  selection = list(target = "column", mode = "multiple"))

load.data.preview.proxy = DT::dataTableProxy("load.data.preview")

output$load.data.numrows = renderText({
  req(loaded.table())
  dat = loaded.table()
  validate(need(dat, message = ""))
  nrow(dat)
})

output$load.data.numcols = renderText({
  req(loaded.table())
  dat = loaded.table()
  validate(need(dat, message = ""))
  ncol(dat)
})

rv.delete = reactiveValues(delete.sure.visible = FALSE)

output$deletevisible = renderText({
  if (!is.null(loaded.table()) && !rv.delete$delete.sure.visible) "TRUE" else "FALSE"
})
output$deletesurevisible = renderText(if (!is.null(loaded.table()) && rv.delete$delete.sure.visible) "TRUE" else "FALSE")

outputOptions(output, "deletevisible", suspendWhenHidden = FALSE)
outputOptions(output, "deletesurevisible", suspendWhenHidden = FALSE)

observeEvent(input$od.delete.dataset,
  {
    rv.delete$delete.sure.visible = TRUE
  })

observeEvent(input$od.delete.yes,
  {
    i = metadata$all.datasets$name == input$select.load.dataset
    filepath = paste(metadata$all.datasets$file[i], sep = "/")
    file.remove(filepath)
    metadata$all.datasets = metadata$all.datasets[!i,]
    rv.delete$delete.sure.visible = FALSE
    
    metadata.list = reactiveValuesToList(metadata)
    save(metadata.list, file = metadata.path)
  })

observeEvent(input$od.delete.no,
  {
    rv.delete$delete.sure.visible = FALSE
  })

# Upload Results page

rv.res = reactiveValues(colnums = list("Gene" = NA, "Entrez" = NA, "Uniprot" = NA, "Misc" = NA, "LogFC" = NA, "T stat" = NA, "Raw P" = NA, "Adj P" = NA), sel.row = c())

output$res.col.selector = DT::renderDataTable({
  current.step = rv.uplnav$step # Force table to update on step change
  # This gets around a bug caused in DT's
  # interaction with conditionalPanel.
  
  rv.res$sel.row = isolate(input$res.col.selector_rows_selected) # Save selected rows for after refresh
  colnums = lapply(rv.res$colnums, sort, decreasing = FALSE)
  colnums = sapply(colnums, paste, collapse = ", ")
  
  out = data.frame("Cols" = colnums, stringsAsFactors = FALSE)
  rownames(out) = names(colnums)
  out
}, options = list(dom = "t", colnames = NULL),
  selection = list(target = "row", mode = "single"))
res.col.proxy = DT::dataTableProxy("res.col.selector")

observeEvent(rv.res$sel.row, {
  DT::selectRows(res.col.proxy, rv.res$sel.row)
  rv.res$sel.row = NULL
})

observeEvent (length(input$load.data.preview_columns_selected),
  {
    validate(need(rv.uplnav$step == "4.results", message = "Select a field"))
    validate(need(length(input$res.col.selector_rows_selected) == 1, message = "Select a field"))
    
    sel.col = input$res.col.selector_rows_selected
    if (length(input$load.data.preview_columns_selected) == 0)
      rv.res$colnums[[sel.col]] = NA
    else
      rv.res$colnums[[sel.col]] = input$load.data.preview_columns_selected + 1
  })

observeEvent(input$res.col.selector_rows_selected,
  {
    validate(need(length(input$res.col.selector_rows_selected) == 1, message = "Select a field"))
    sel.col = input$res.col.selector_rows_selected
    DT::selectColumns(load.data.preview.proxy, rv.res$colnums[[sel.col]] - 1)
  })

observeEvent(input$select.table,
  {
    switch(rv.uplnav$step,
      "4.results" = {
        rv.res$colnums = lapply(rv.res$colnums, function(...) NA)
      },
      "4.protlist" = {
        rv.protlist$colnums = lapply(rv.protlist$colnums, function(...) NA)
      }
    )
  })

# Upload siglist page

rv.protlist = reactiveValues(colnums = list("Gene" = NA, "Entrez" = NA, "Uniprot" = NA, "Misc" = NA), sel.row = c())

output$protlist.col.selector = DT::renderDataTable({
  current.step = rv.uplnav$step # Force table to update on step change
  # This gets around a bug caused in DT's
  # interaction with conditionalPanel.
  
  rv.protlist$sel.row = isolate(input$protlist.col.selector_rows_selected) # Save selected rows for after refresh
  colnums = lapply(rv.protlist$colnums, sort, decreasing = FALSE)
  colnums = sapply(colnums, paste, collapse = ", ")
  
  out = data.frame("Cols" = colnums, stringsAsFactors = FALSE)
  rownames(out) = names(colnums)
  out
}, options = list(dom = "t", colnames = NULL),
  selection = list(target = "row", mode = "single"))
protlist.col.proxy = DT::dataTableProxy("protlist.col.selector")

observeEvent(rv.protlist$sel.row, {
  DT::selectRows(protlist.col.proxy, rv.protlist$sel.row)
  rv.protlist$sel.row = NULL
})

observeEvent (length(input$load.data.preview_columns_selected),
  {
    validate(need(rv.uplnav$step == "4.protlist", message = "Select a field"))
    validate(need(length(input$protlist.col.selector_rows_selected) == 1, message = "Select a field"))
    
    sel.col = input$protlist.col.selector_rows_selected
    if (length(input$load.data.preview_columns_selected) == 0)
      rv.protlist$colnums[[sel.col]] = NA
    else
      rv.protlist$colnums[[sel.col]] = input$load.data.preview_columns_selected + 1
  })

observeEvent(input$protlist.col.selector_rows_selected,
  {
    validate(need(length(input$protlist.col.selector_rows_selected) == 1, message = "Select a field"))
    sel.col = input$protlist.col.selector_rows_selected
    DT::selectColumns(load.data.preview.proxy, rv.protlist$colnums[[sel.col]] - 1)
  })
