# On clicking the "example dataset" link on the Getting Started page, load the example dataset.
example.run = eventReactive(eventExpr = input$loadDEexample, ignoreInit = TRUE, valueExpr =  {
  showModal(modalDialog("Loading DE pipeline...", title = "Please wait", footer = NULL, size = "s"), session)
  
  load("analysis/exampledata.RData")
  lapply(names(example.data$rv), function(nm) rv[[nm]] = example.data$rv[[nm]])
  rv$analysis.desc = NULL
  rv$analysis.finaldata = NULL
  rv$analysis.filtdata = NULL
  
  updateTextInput(session, "new.var.name", value = "")
  updateTextInput(session, "new.lev.name", value = "")
  updateTextInput(session, "new.lev.search", value = "")
  checkpoints$variables.created = TRUE
  checkpoints$design.table.created = FALSE
  
  shinyjs::show("variabletable", anim = TRUE)
  shinyjs::show("delete.vt.row", anim = TRUE)
  shinyjs::show("clear.var", anim = TRUE)
  shinyjs::hide("varcreate.instructions", anim = TRUE)
  
  updateTextInput(session, "auto.numeric.string", value = "LFQ")
  
  lapply(names(example.data$datrv), function(nm) datrv[[nm]] = example.data$datrv[[nm]])

  hide("eigenplot")
  
  checkpoints$descriptive.exists = TRUE
  checkpoints$numeric.exists = TRUE
  
  desccolnames = colnames(datrv$descriptive.data)[example.data$desc.columns]
  searchcol.choices = as.character(seq_along(desccolnames))
  names(searchcol.choices) = paste0(searchcol.choices, ". ", desccolnames)
  updateSelectizeInput(session, "expplot.searchcols", choices = searchcol.choices)
  updateSelectizeInput(session, "expplot.labelcol", choices = colnames(searchcol.choices))
  
  example.tickdesc$resume()
  example.fillcontrols$resume()
  
  example.data
})

observe(example.run())

example.tickdesc = observeEvent(checkpoints$design.table.created, suspended = TRUE, {
  
  validate(
    need(checkpoints$design.table.created, label = "Design table")
  )
  
  example.data = example.run()
  x = example.data$desc.columns
  descchkbxnames = paste0("desmatr", x,"chkbx", "desc")
  
  lapply(descchkbxnames, function(id) updateCheckboxInput(session, id, value = TRUE))
  example.tickdesc$suspend()
})

example.fillcontrols = observeEvent(update.group.opts(), suspended = TRUE, {
  
  validate(
    need(update.group.opts(), label = "Group update")
  )
  example.data = example.run()
  
  mapply(FUN = function(func, id, arg, val) {
    func.args = list(session, id, val)
    names(func.args) = c("session", "inputId", arg)
    do.call(func, func.args)
  }, example.data$preproc$funcs, example.data$preproc$ids, example.data$preproc$args, example.data$preproc$values)
  
  example.analyse$resume()
  example.fillcontrols$suspend()
})

example.analyse = observeEvent(final.data_todebounce(), ignoreInit = TRUE, suspended = TRUE, {
  
  doanalysisfunc()
  showTab("tb.nav", target = "Experimental design", select = TRUE, session = session)
  showTab("tb.nav", target = "Data exploration and pre-processing", select = FALSE, session = session)
  showTab("tb.nav", target = "Define and Run Analysis", select = FALSE, session = session)
  showTab("tb.nav", target = "DE Results", select = FALSE, session = session)
  removeModal(session)
  
  example.analyse$suspend()
}, priority = -1)