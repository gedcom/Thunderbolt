library(magrittr)
library("writexl")
library("tidyverse")

observeEvent(input$goto.fileman,
  updateNavbarPage(session, "tb.nav", "File management")
)

# Analysis metadata

newan = reactiveValues(keywords = character(0))

observeEvent(input$btn.ana.addkeyword,
{
  validate(need(input$btn.ana.addkeyword, label="Keyword"))
  newan$keywords = union(newan$keywords, input$text.ana.keyword)
})

output$newan.keywords = DT::renderDataTable(data.frame(newan$keywords), server = FALSE,
  options = list(dom = 't'),
  selection = list(target = "cell", mode = "single"),
  rownames = FALSE,
  colnames = NULL
)

# Design variables

rv = reactiveValues(loaded.table = NULL, var.names = character(0), var.levels = list(), var.search = list(), draft.varlevels = c(), draft.varsearch = c(),
                    contrasts = character(0), contnames = character(0), cont.comp.groups = list(), cont.inc.groups = list(),
                    doanalysiscnt = 0, analysis.mi = F, mi.num = 1, min.rawval.grps = c(), min.rawvals = 0,
                    analysis.desc = NULL, analysis.finaldata = NULL, analysis.filtdata = NULL)

obs.add.level = observeEvent (input$add.lev,
  {
    shinyjs::hide("lev.search.error")
    new.var = input$new.var.name
    new.lev = input$new.lev.name
    new.search = input$new.lev.search
    
    validate(need(new.var, label = "Variable name"))

    if (any(grepl("[\\+,,;+]", c(new.lev, new.search), fixed = FALSE)))
    {
      new.lev = strsplit(new.lev, split = "[\\+,,;+]", fixed = FALSE)[[1]]
      new.search = strsplit(new.search, split = "[\\+,,;+]", fixed = FALSE)[[1]]
      if (length(new.search) == 0) new.search = rep("", length(new.lev))
      new.lev = gsub("^\\s+|\\s+$", "", new.lev)
      new.search = gsub("^\\s+|\\s+$", "", new.search)
      new.lev = gsub("\\s+", "_", new.lev)
      new.search = gsub("\\s+", "_", new.search)
      new.lev = new.lev[new.lev != ""]
      new.search = new.search[new.search != ""]
      validate(need(new.lev, label = "Condition name"))
      if (length(new.search) == 0) new.search = rep("", length(new.lev))
      
      if (length(new.lev) != length(new.search))
      {
        shinyjs::show("lev.search.error")
        validate("Conditions and autofills different lengths")
      }
    }
    
    if (!new.var %in% rv$var.names)
    {
      rv$var.names = c(rv$var.names, new.var)
      rv$var.levels = c(rv$var.levels, list(character(0)))
      rv$var.search = c(rv$var.search, list(character(0)))
    }
    
    var.idx = match(new.var, rv$var.names)
    rv$var.levels[[var.idx]] = c(rv$var.levels[[var.idx]], new.lev)
    rv$var.search[[var.idx]] = c(rv$var.search[[var.idx]], new.search)
    
    updateTextInput(session, "new.lev.name", value = "")
    updateTextInput(session, "new.lev.search", value = "")
    
    shinyjs::show("variabletable", anim = FALSE)
    shinyjs::show("delete.vt.row", anim = TRUE)
    shinyjs::show("clear.var", anim = TRUE)
    shinyjs::hide("varcreate.instructions", anim = TRUE)
    
    if (length(rv$var.names) > 0 && all(sapply(rv$var.levels, length) > 1))
      checkpoints$variables.created = TRUE
  })

variabletable = reactive ({
  if (length(rv$var.names) > 0)
  {
    vartable = mapply(function (name, levels, search) {
      list(data.frame("Variable" = c(name, rep(NA, length(levels))), "Condition" = c(NA, levels), "Autofill" = c(NA, search),
        stringsAsFactors = FALSE, row.names = NULL))
    }, rv$var.names, rv$var.levels, rv$var.search)
    
    vartable = do.call(rbind, vartable)
    vartable
  } else NULL
})

variabletableindex = reactive({
  var.lengths = sapply(rv$var.levels, length)
  var = rep(seq_along(rv$var.names), var.lengths + 1)
  cond = lapply(rv$var.levels, function(v) c(NA, seq_along(v)))
  cond = do.call(c, cond)
  data.frame(var = var, cond = cond, stringsAsFactors = FALSE, row.names = NULL)
})

output$variabletable = DT::renderDataTable({
  variabletable()
}, rownames = FALSE, options = list(dom = "t", pageLength = 1000, lengthChange = FALSE), selection = list(target = "row", mode = "single"))

fill.cond.input = observeEvent(input$variabletable_rows_selected, {
  validate(need(input$variabletable_rows_selected, label = "selection"))
  validate(need(rv$var.names, label = "table"))
    
  sel.row = input$variabletable_rows_selected

  vti = variabletableindex()
  
  sel.var = vti$var[sel.row]
  sel.cond = vti$cond[sel.row]

  if(is.finite(sel.cond))
  {
    updateTextInput(session, "new.var.name", value = rv$var.names[sel.var])
    updateTextInput(session, "new.lev.name", value = rv$var.levels[[sel.var]][sel.cond])
    updateTextInput(session, "new.lev.search", value = rv$var.search[[sel.var]][sel.cond])
  } else if (is.finite(sel.var))
  {
    updateTextInput(session, "new.var.name", value = rv$var.names[sel.var])
    updateTextInput(session, "new.lev.name", value = "")
    updateTextInput(session, "new.lev.search", value = "")
  }
})

delete.vt.row = observeEvent(input$delete.vt.row, {

  del.row = input$variabletable_rows_selected
  validate(need(del.row, message = "Please select a row to delete."))
  validate(need(rv$var.names, label = "table"))
  vti = variabletableindex()
  
  del.var = vti$var[del.row]
  del.cond = vti$cond[del.row]
  
  if(is.na(del.cond))
  {
    rv$var.names = rv$var.names[-del.var]
    rv$var.levels = rv$var.levels[-del.var]
    rv$var.search = rv$var.search[-del.var]
  } else {
    rv$var.levels[[del.var]] = rv$var.levels[[del.var]][-del.cond]
    rv$var.search[[del.var]] = rv$var.search[[del.var]][-del.cond]
    
    if (length(rv$var.levels[[del.var]]) == 0) {
      rv$var.names = rv$var.names[-del.var]
      rv$var.levels = rv$var.levels[-del.var]
      rv$var.search = rv$var.search[-del.var]
    }
  }
  
  if (length(rv$var.names) == 0)
  {
    shinyjs::hide("variabletable", anim = TRUE)
    shinyjs::hide("delete.vt.row", anim = TRUE)
    shinyjs::hide("clear.var", anim = TRUE)
    shinyjs::show("varcreate.instructions", anim = TRUE)
  } else {
    shinyjs::show("variabletable", anim = TRUE)
    shinyjs::show("delete.vt.row", anim = TRUE)
    shinyjs::show("clear.var", anim = TRUE)
    shinyjs::hide("varcreate.instructions", anim = TRUE)
  }
})

clear.Variables = observeEvent (input$clear.var,
  {
    rv$var.names = c()
    rv$var.levels = list()
    rv$var.search = list()
    rv$draft.varlevels = c()
    rv$draft.varsearch = c()
    
    updateTextInput(session, "new.var.name", value = "")
    updateTextInput(session, "new.lev.name", value = "")
    updateTextInput(session, "new.lev.search", value = "")
    
    checkpoints$variables.created = F
    
    shinyjs::hide("variabletable", anim = TRUE)
    shinyjs::hide("delete.vt.row", anim = TRUE)
    shinyjs::hide("clear.var", anim = TRUE)
    shinyjs::show("varcreate.instructions", anim = TRUE)
  })

possible.groups = reactive ({
  if (checkpoints$variables.created)
  {
    rev.levels = rv$var.levels
    rev.levels = rev.levels[length(rev.levels):1]
    out = expand.grid(rev.levels)
    out = out[length(out):1]
    return (out)
  }
})

output$variables.created = renderText({
  if (checkpoints$variables.created) "TRUE" else "FALSE"
})
outputOptions(output, "variables.created", suspendWhenHidden = FALSE)

# Design selection - Datatables
output$sel.desc.cols = DT::renderDataTable({
  dat = rv$loaded.table
  
  conv.coltypes = !sapply(dat, class) %in% c("numeric", "integer", "character")
  dat[conv.coltypes] = lapply(dat[conv.coltypes], as.character)
  
  dat
}, options = list(lengthChange = FALSE, pageLength = 5, rownames = TRUE, dom = 't', scrollX = TRUE),
filter = list(position = "bottom"),
selection = list(target = "column", mode = "multiple"))

output$sel.num.cols = DT::renderDataTable({
  dat = rv$loaded.table
  
  conv.coltypes = !sapply(dat, class) %in% c("numeric", "integer", "character")
  dat[conv.coltypes] = lapply(dat[conv.coltypes], as.character)
  
  dat
}, options = list(lengthChange = FALSE, pageLength = 5, rownames = TRUE, dom = 't', scrollX = TRUE),
filter = list(position = "bottom"),
selection = list(target = "column", mode = "multiple"))

# Actual design selection
output$des.mat.int = renderUI({
  if (checkpoints$variables.created && isTruthy(rv$loaded.table)){
    
    data.cols = colnames(rv$loaded.table)
    
    num.str = input$auto.numeric.string
    numericcols = if (num.str != "") grepl(num.str, data.cols) else rep(FALSE, length(data.cols))
    
    table.data = lapply(1:length(data.cols), function(x) {
      
      # "Type" checkboxes - user selects Descriptive, Numeric or leaves row out of design
      chkbxname = paste0("desmatr",x,"chkbx")
      checkboxdesc = checkboxInput(paste0(chkbxname, "desc"), label = "Descriptive")
      
      checkboxnum  = checkboxInput(paste0(chkbxname, "num"), label = "Numeric", value = numericcols[x])
      checkboxes = list(tags$td(checkboxdesc, checkboxnum))
      
      row.name = list(tags$td(tags$h5(data.cols[x])))
      
      # Variable level input control
      row.data = lapply(1:length(rv$var.names), function(y) {
        ctrlname = paste0("desmatr",x,"c",y)
        
        autosel = NULL
        hits = which(sapply(rv$var.search[[y]], function(str) {
          return(grepl(str, row.name, fixed = T))
        }))
        
        if (length(hits) > 0)
          autosel = rv$var.levels[[y]][hits[1]]
        
        
        ctrl = selectizeInput(ctrlname, "", choices = c(rv$var.levels[[y]]), selected = autosel)
        ctrl = conditionalPanel(condition = paste0("input.",chkbxname,"num"), ctrl)
        return(tags$td(ctrl))
      })
      
      row.all = c(row.name, checkboxes, row.data)
      
      return(tags$tr(row.all))
    })
    
    col.header = c("", "Type", rv$var.names)
    col.header = lapply(col.header, tags$h4)
    col.header = lapply(col.header, tags$td, style = "padding: 0 100px 0 0")
    col.header = tags$tr(col.header)
    
    checkpoints$design.table.created = TRUE
    tags$table(col.header, table.data)
  }
})

datrv = reactiveValues(descriptive.data = NULL, numeric.data = NULL, design.matrix = NULL, group.matrix = NULL, group.names = NULL,
                       group.colours = NULL, sample.colours = NULL,  sample.groups = NULL)

save.design = observeEvent (input$save.design,
{
  validate(need(checkpoints$design.table.created, message = "Design table must exist"))
  
  progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
  on.exit(progress$close())
  progress$set(message = "Applying design")
  
  
  ### Begin making numeric data ###
  
  checkpoints$numeric.exists = F
  
  datrv$numeric.data = NULL
  datrv$design.matrix = NULL
  datrv$group.matrix = NULL
  datrv$group.names = NULL
  datrv$group.colours = NULL
  datrv$sample.colours = NULL
  datrv$sample.groups = NULL
  datrv$sample.names = NULL
  
  retrieved.cols = NULL
  numeric.cols = NULL
  data.cols = NULL
  variables = NULL
  
  hideTab("tb.nav", "DE Results", session = session)
  
  data.cols = colnames(rv$loaded.table)
  variables = rv$var.names
  
  # Create names of checkboxes and select-inputs to retrieve design
  retrieved.cols = sapply(1:length(data.cols), function(x) {
    chkbxname = paste0("desmatr", x, "chkbxnum")
    return(input[[chkbxname]])
  })
  
  if (length(retrieved.cols) > 0 && is.logical(retrieved.cols))
  {
    numeric.cols = which(retrieved.cols)
  }
  
  if (length(numeric.cols) == 0)
  {
    showModal(
      modalDialog("Please designate numeric columns in the dataset.", title = "Numeric columns required", size = "s", easyClose = TRUE)
    )
    
    validate("Numeric columns required")
  }
  
  # Create design matrix corresponding to original column order
  orig.dm = data.frame(lapply(1:length(variables), function(y) {
    slctnames = paste0("desmatr",numeric.cols,"c",y)
    var.values = sapply(slctnames, function(nm) {return (input[[nm]])})
    return (var.values)
  }), stringsAsFactors = FALSE)
  
  numeric.classes = sapply(rv$loaded.table[numeric.cols], class)
  if (!all(numeric.classes == "integer" | numeric.classes == "numeric"))
  {
    showModal(
      modalDialog("Columns containing non-numeric data cannot be designated numeric!", title = "Warning", size = "s", easyClose = TRUE)
    )
    
    validate("Non-numerics designated as numeric")
  }
  
  rownames(orig.dm) = data.cols[numeric.cols]
  colnames(orig.dm) = variables
  
  # Create group matrix and related data #
  all.grp = possible.groups()
  
  # Group numbers
  grp.nums = apply(as.matrix(orig.dm), 1, function(rw) {
    rw.grp = which(apply(all.grp, 1, function(grp) {
      return(all(rw == grp))
    }))
    
    return(rw.grp)
  })
  
  des.mat = orig.dm[order(grp.nums), , drop = FALSE]
  datrv$design.matrix = des.mat
  
  numeric.data = rv$loaded.table[,numeric.cols[order(grp.nums)]]
  datrv$numeric.data = numeric.data
  datrv$sample.names = colnames(numeric.data)
  grp.nums = sort(grp.nums)
  
  # Group matrix
  gm = lapply(1:nrow(all.grp), function(g) {
    grp.members = factor(rep(0, nrow(des.mat)), levels = c(0,1))
    grp.members[g == grp.nums] = 1
    return (grp.members)
  })
  
  g.keep = sapply(gm, function(cl) {
    return (any(cl == 1))
  })
  
  gm = data.frame(gm[g.keep])
  gnms = apply(all.grp, 1, paste0, collapse = ".")[g.keep]
  rownames(gm) = rownames(des.mat)
  colnames(gm) = gnms
  
  datrv$group.matrix = gm
  datrv$group.names = gnms
  
  gcolours = substr(better.colours(length(gnms)), 1, 7)
  names(gcolours) = gnms
  datrv$group.colours = gcolours
  
  datrv$sample.colours = gcolours[grp.nums]
  datrv$sample.groups = gnms[grp.nums]
  
  rv$contrasts = c()
  rv$contnames = c()
  rv$cont.comp.groups = c()
  rv$cont.inc.groups = c()
  rv$analysis.desc = NULL
  rv$analysis.finaldata = NULL
  rv$analysis.filtdata = NULL
  
  checkpoints$numeric.exists = T
  
  # Reset all preprocessing controls
  updateCheckboxInput(session, "chk.log.values", value = FALSE)
  updateNumericInput(session, "filter.minvals", value = 0)
  updateNumericInput(session, "filt.high.sd.perc", value = 0)
  updateCheckboxInput(session, "chk.rem.batch", value = FALSE)
  updateCheckboxInput(session, "chk,batch.knn", value = FALSE)
  updateNumericInput(session, "batchnum", value = 1)
  updateRadioButtons(session, "pre.norm", selected = "None")
  updateSelectInput(session, "imputemethod", selected = "none")
  updateNumericInput(session, "sel.impute.min.obs", value = 1)
  updateSliderInput(session, "num.imputations", value = 1)
  updateSliderInput(session, "RTI.shift", value = -1.8)
  updateSliderInput(session, "RTI.width", value = 0.3)
  updateRadioButtons(session, "post.norm", selected = "None")
  hide("eigenplot")
  
  # Pre-generate contrasts for selection
  
  num_variables = length(rv$var.levels)
  
  contrast_df = lapply(seq(num_variables), function(var_num) {
    conditions = rv$var.levels[[var_num]]
    pairs = combn(conditions, 2, simplify = FALSE)
    
    all_vars = rv$var.levels
    all_vars[[var_num]] = pairs
    combinations = expand.grid(all_vars, stringsAsFactors = FALSE)
    
    # new_pairs = combinations[[var_num]]
    # combinations = combinations[-var_num]
    
    lapply(seq_along(combinations), function(i) {
      pair = unlist(combinations[i, var_num])
      background = unlist(combinations[i, -var_num])
      
      pair_str = paste0(pair[2], " versus ", pair[1])
      bg_str = paste(background, collapse = " / ")
      name_str = paste0(pair_str, " under ", bg_str)
      
      formula_df = tidyr::unnest(combinations[i,, drop = FALSE], cols = var_num)
      formula1 = paste(as_vector(formula_df[1,]), collapse = ".")
      formula2 = paste(as_vector(formula_df[2,]), collapse = ".")
      formula_str = paste0(formula2, " - ", formula1)
      
      tibble(Name = name_str, Formula = formula_str) %>% return()
    }) %>% bind_rows %>% return()
  }) %>% bind_rows
  
  contrast_vec = paste(pull(contrast_df, Formula), pull(contrast_df, Name), sep = ";;;")
  names(contrast_vec) = contrast_df %>% pull(Name)
  contrast_vec %<>% c("Select a contrast" = ";;;", .)
  
  updateSelectInput(session, "contrast.dropdown", choices = contrast_vec)
  
  # Show downstream tabs
  
  showTab("tb.nav", "Data exploration and pre-processing", select = TRUE, session = session)
  showTab("tb.nav", "Define and Run Analysis", select = FALSE, session = session)
  
  ### Begin making descriptive data ###
  
  checkpoints$descriptive.exists = F
  datrv$descriptive.data = NULL
  desc.cols = NULL
  data.cols = NULL
  variables = NULL
  
  data.cols = colnames(rv$loaded.table)
  variables = rv$var.names
  
  desc.cols = sapply(1:length(data.cols), function(x) {
    chkbxname = paste0("desmatr", x, "chkbxdesc")
    return(input[[chkbxname]])
  })
  
  if (length(desc.cols) > 0)
  {
    checkpoints$descriptive.exists = T
    if (any(desc.cols))
    {
      datrv$descriptive.data = rv$loaded.table[desc.cols]
      row.names(datrv$descriptive.data) = 1:nrow(datrv$descriptive.data)
    } else {
      datrv$descriptive.data = data.frame("Row number" = 1:nrow(rv$loaded.table), stringsAsFactors = FALSE)
    }
  }
  
  desccolnames = colnames(datrv$descriptive.data)
  searchcol.choices = as.character(seq_along(desccolnames))
  names(searchcol.choices) = paste0(searchcol.choices, ". ", desccolnames)
  updateSelectizeInput(session, "expplot.searchcols", choices = searchcol.choices)
  updateSelectizeInput(session, "expplot.labelcol", choices = searchcol.choices)
  
  resultsrv$results.exist = FALSE
  
  showTab("tb.nav", "Data exploration and pre-processing", select = TRUE, session = session)
})

observeEvent(input$goto.expdes2,
  updateNavbarPage(session, "tb.nav", selected = "Experimental design")  
)
observeEvent(input$goto.expdes3,
  updateNavbarPage(session, "tb.nav", selected = "Experimental design")  
)
observeEvent(input$goto.expdes4,
  updateNavbarPage(session, "tb.nav", selected = "Experimental design")  
)

goto.preproc = observeEvent(input$goto.preproc1, 
  updateNavbarPage(session, "tb.nav", selected = "Data exploration and pre-processing")
)
goto.preproc = observeEvent(input$goto.preproc3, 
updateNavbarPage(session, "tb.nav", selected = "Data exploration and pre-processing")
)
goto.preproc = observeEvent(input$goto.preproc4, 
updateNavbarPage(session, "tb.nav", selected = "Data exploration and pre-processing")
)

goto.defineDE = observeEvent(input$goto.defineDE1,
  updateNavbarPage(session, "tb.nav", selected = "Define and Run Analysis")
)
goto.defineDE = observeEvent(input$goto.defineDE2,
  updateNavbarPage(session, "tb.nav", selected = "Define and Run Analysis")
)
goto.defineDE = observeEvent(input$goto.defineDE4,
  updateNavbarPage(session, "tb.nav", selected = "Define and Run Analysis")
)

goto.DEresults = observeEvent(input$goto.DEresults1,
  updateNavbarPage(session, "tb.nav", selected = "DE Results")
)
goto.DEresults = observeEvent(input$goto.DEresults2,
  updateNavbarPage(session, "tb.nav", selected = "DE Results")
)
goto.DEresults = observeEvent(input$goto.DEresults3,
  updateNavbarPage(session, "tb.nav", selected = "DE Results")
)

output$datasetexists = renderText({
  out = if (checkpoints$numeric.exists) "true" else "false"
  out
})
outputOptions(output, "datasetexists", suspendWhenHidden = FALSE)

# Pre-analysis #
updateDESearchBox = observe ({
  validate(need(final.descriptive(), label = "Descriptive table"))
  if (checkpoints$descriptive.exists) {
    validate(need(input$expplot.searchcols, label = "Search columns"))
    desc = final.descriptive()[as.integer(input$expplot.searchcols)]
    rownames(desc) = NULL
    desc = as.list(desc)
    desc = lapply(seq_along(desc), function(i) {
      out = desc[[i]]
      names(out) = paste0(names(desc)[i], ": ", desc[[i]])
      out = out[!duplicated(out)]
      return(out)
    })
    desc = do.call(c, desc)
    desc = na.omit(desc)
    desc = desc[!desc == ""]
    
    updateSelectizeInput(session, 'expplot.searchdesc', choices = desc, server = TRUE)
  }
})

searchvec = reactive({
  
  validate(need(input$expplot.searchdesc, label = "Search targets"))
  
  desc = final.descriptive()
  hit.mat = apply(desc, 2, function(cl) {
    colhits = cl %in% input$expplot.searchdesc
    return(colhits)
  })
  
  hit.vec = apply(hit.mat, 1, any)
  hits = which(hit.vec)
  
  validate(need(hits, label = "Search targets"))

  hits
})

expplot.descriptive = reactive ({
  sv = searchvec()
  validate(need(sv, message = "No search hits"))
  
  dd = data.frame(final.descriptive()[sv, ,drop = FALSE])
  
  dd
})

long.values = reactive ({
  require(reshape2)
  
  sv = searchvec()
  validate(need(sv, message = "No search hits"))
  
  design = datrv$sample.groups
  
  if (input$show.raw.adj == "Raw")
  {
    dat.wide = filtsd.data()[sv,]
  } else {
    dat.wide = final.data()[sv,]
  }
  num.samples = ncol(dat.wide)
  num.hits = length(sv)
  
  dd = expplot.descriptive()
  validate(need(dd, label = "Descriptive data"))
  dat.wide$Text = apply(dd, 1, function(ddrow) {
    out = paste0(names(ddrow), ": ", ddrow)
    out = strwrap(out, width = 50)
    out = paste(out, collapse = "<br>")
    
    return(out)
  })
  colnames(dd) = paste0("desc.", colnames(dd))
  rownums = rownames(dd)
  Protein = as.character(rownums)
  Protein = rep(Protein, times = num.samples)
  Group = rep(design, each = num.hits)
  out = data.frame(Group = Group, Protein = Protein, melt(dat.wide, id.vars = "Text", variable.name = "Sample", value.name = "Value"), dd)
  out$Text = paste0(out$Text, "<br><br>Group: ", out$Group)
  out
})

pr.mns = reactive({
  
  lv = long.values()
  means = aggregate(Value ~ Protein + Group + Text, data = lv, FUN=mean, na.rm = T, na.action = NULL)
  means$Value[!is.finite(means$Value)] = 0
  dd = expplot.descriptive()
  colnames(dd) = paste0("desc.", colnames(dd))
  means = data.frame(means, dd)
  
  means
})

expressionggplot = reactive ({
  
  if (length(searchvec()) > 0)
  {
    loc.dat = pr.mns()
    loc.dat$Text = paste(loc.dat$Text, "<br>Mean: ", signif(loc.dat$Value, 5))
    
    point.vals = long.values()
    point.vals$Text = paste0(
      point.vals$Text,
      "<br>Sample: ", point.vals$Sample,
      "<br>Value: ", signif(point.vals$Value, 5)
    )
    
    num.probes = length(searchvec())
    num.conditions = length(datrv$group.names)
    probes = unique(pr.mns()$Protein)
    
    fill.colours = datrv$group.colours
    
    xlabels = if (length(input$expplot.labelcol) > 0)
    {
      mn.desccols = substring(colnames(pr.mns()), first = 1, last = 5) == "desc."
      mn.descnames = substring(colnames(pr.mns())[mn.desccols], first = 6)
      mn.desc = pr.mns()[mn.desccols]
      lab.desccols = as.integer(input$expplot.labelcol)
      pasteargs = as.list(mn.desc[lab.desccols])
      pasteargs = c(pasteargs, list(paste("Row", pr.mns()$Protein)))
      names(pasteargs) = NULL
      pasteargs = c(pasteargs, list(sep = " / "))
      do.call(paste, pasteargs)
    } else {
      paste("Row", as.character(loc.dat$Protein))
    }
    
    loc.dat$Group = factor(loc.dat$Group, levels = datrv$group.names)
    point.vals$Group = factor(point.vals$Group, levels = datrv$group.names)
    
    p <- ggplot() +
      geom_bar(data = loc.dat, aes(x = Protein, y = Value, fill = Group, text = Text), colour = "black", stat = "identity", position = position_dodge(width = 0.9), size = 0.25) +
      geom_jitter(data = point.vals, pch = 21, aes(x = Protein, y = Value, fill = Group, text = Text), position = position_dodge(width = 0.9), stroke = 0.25) +
      scale_fill_manual(values = fill.colours) +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      scale_x_discrete(labels = xlabels) +
      theme(axis.text.x = element_text(hjust = 0.5)) +
      coord_cartesian(ylim=c(min(long.values()$Value, na.rm = TRUE) - 1, max(long.values()$Value, na.rm = TRUE) + 1))
      
    return(p)
  }
})

output$expplotexists = renderText({
  if (isTruthy(expressionggplot())) "true" else "false"
})
outputOptions(output, "expplotexists", suspendWhenHidden = FALSE)

output$expressionplotly = renderPlotly({
  
  p = ggplotly(expressionggplot(), tooltip = "text")
  p = config(p, displayModeBar = FALSE)
  p = config(p, showLink = FALSE)
  
  p
})

output$save.expression = downloadHandler (
  filename = function() paste0("expressionplot.", Sys.Date(), ".png"),
  content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 100, plot = expressionggplot() )
)

output$eps.expression = downloadHandler (
  filename = function() paste0("expressionplot.", Sys.Date(), ".eps"),
  content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 100, plot = expressionggplot() )
)

# Pre-analysis plots! #

update.group.opts = eventReactive (datrv$group.names, ignoreInit = TRUE, {
  gn = datrv$group.names
  validate(need(gn, label = "Group names"))

  updateSelectizeInput(session, "sel.filter.groups", choices = c("all", gn))
  updateSelectizeInput(session, "sel.impute.groups", choices = c(gn))
  updateSelectizeInput(session, "sel.impute.groups.sparse", choices = c(gn))
  updateSelectizeInput(session, "sel.minrawval.groups", choices = c(gn))
  
  TRUE
})
observe(update.group.opts())

nonempty.rows = reactive ({
  kept.rows = !apply(log.data(), 1, function(row) all(is.na(row)))
  
  kept.rows
})

row.filter = function(dat) {
  
  minvals = input$filter.minvals
  if (is.na(minvals)) minvals = 0
  kept.rows = if (minvals > 0 && length(input$sel.filter.groups) > 0)
  {
    flt.grps = c()
    min.vals = input$filter.minvals
    
    if ("all" %in% input$sel.filter.groups)
    {
      flt.grps = datrv$group.names
    } else {
      flt.grps = input$sel.filter.groups
      all(flt.grps %in% datrv$group.names) %>%
        need(label = "Selected filter group[s] not in dataset") %>%
        validate
    }
    
    keep.rows = sapply(flt.grps, function(gp) {
      samples.in.grp = which(datrv$sample.groups == gp)
      
      include = apply(dat[,samples.in.grp], 1, function(rw) {
        return(sum(is.finite(rw)) >= min.vals)
      })
      
      return(include)
    })
    
    if ("all" %in% input$sel.filter.groups)
      keep.rows = apply(keep.rows, 1, all)
    else
      keep.rows = apply(keep.rows, 1, any)
    
    if(is.logical(keep.rows))
      keep.rows
    else
      rep(FALSE, nrow(dat))
  } else {
    
    rep(TRUE, nrow(dat))
  }

  return(kept.rows)
}

workflow = reactiveValues(
  doRowFilter = FALSE
)

log.data = reactive ({

  validate(need(checkpoints$numeric.exists, message = ""))
  
  out = datrv$numeric.data
  
  if (input$chk.log.values)
  {
    progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
    on.exit(progress$close())
    progress$set(message = "Applying log transform", detail = "")
    
    out = log2(out)
    
    for (i in 1:ncol(out)) {
      out[!is.finite(out[,i]),i] = NA
    }
  }
  
  out
})

kept.rows = reactive({
  validate(need(checkpoints$numeric.exists, message = ""))
  
  ld = log.data()
  out = row.filter(ld)
  out
})

set.do.rowfilter = observe({
  
  if (isTruthy(input$filter.minvals) && isTruthy(input$sel.filter.groups))
  {
    if (isolate(workflow$doRowFilter) == FALSE) workflow$doRowFilter = TRUE
  } else {
    if (isolate(workflow$doRowFilter) == TRUE) workflow$doRowFilter = FALSE
  }
}, priority = 1)

output$rowfilter.text = renderText({
  validate(need(checkpoints$numeric.exists, message = ""))
  kr = if (workflow$doRowFilter)
  {
    kept.rows()
  } else if (input$filter.empty.rows) {
    nonempty.rows()
  } else {
    rep(TRUE, length(nonempty.rows()))
  }
  
  num.kept = sum(kr)
  num.filtered = length(kr) - num.kept
  
  paste0(num.filtered, " rows filtered, ", num.kept, " retained.")
})

filter.data = reactive ({
  validate(need(checkpoints$numeric.exists, label = "Numeric data"))
  
  out = log.data()
  
  if (workflow$doRowFilter)
  {
    progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
    on.exit(progress$close())
    progress$set(message = "Filtering missing values...", detail = "")
    
    out[kept.rows(),]
  } else if (input$filter.empty.rows) {
    progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
    on.exit(progress$close())
    progress$set(message = "Filtering empty rows...", detail = "")
    
    out[nonempty.rows(),]
  } else {
    out
  }
})

filter.highsd = function (dat, design, filt.perc) {
  n = nrow(dat)
  ngrp = ncol(design)
  
  discards = c()
  
  for (grp in 1:ngrp) {
    dat.grp = dat[,design[,grp] == 1]
    sd.vec = apply(dat.grp, 1, function (x) sd(x, na.rm=TRUE))
    sd.vec.sorted = sort(sd.vec, decreasing = TRUE)
    discards = union(discards, names(sd.vec.sorted[1:floor(n * filt.perc)]))
  }
  
  discards = match(discards, rownames(dat))
  return (discards)
  
}

highsd.discards = reactive ({

  validate(need(checkpoints$numeric.exists, message = ""))
  
  filt.perc = if (is.na(input$flt.high.sd.perc))
  {
    0
  } else {
    input$flt.high.sd.perc / 100
  }
  
  if (filt.perc > 0)
  {
    progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
    on.exit(progress$close())
    progress$set(message = "Filtering high-variance rows...", detail = "")
    
    filter.highsd(filter.data(), design = datrv$group.matrix, filt.perc)
  }
})

filtsd.data = reactive({

  validate(need(checkpoints$numeric.exists, message = ""))
  
  out = filter.data()
  
  if (length(highsd.discards()) > 0)
  {
    out = out[-highsd.discards(),]
  }
  
  out
})

output$filtsd.text = renderText({
  validate(need(checkpoints$numeric.exists, message = ""))
  
  num.inrows = nrow(filter.data())
  num.filtered = length(highsd.discards())
  num.kept = num.inrows - num.filtered
  
  paste0(num.filtered, " rows filtered, ", num.kept, " retained.")
})

source("analysis/batch.server.R", local=TRUE)

pre.imp.med = reactive({

  validate(need(checkpoints$numeric.exists, message = ""))
  
  out = batchrem.data()
  # out = filtsd.data()
  
  switch(input$pre.norm,
    "None" = out,
    "Median" = {
      progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
      on.exit(progress$close())
      progress$set(message = "Median-normalising...", detail = "")
      glob.med = median(do.call(c, as.list(out)), na.rm = T)
      col.meds = sapply(as.list(out), median, na.rm = T)
      colshift = col.meds - glob.med
      
      for (i in 1:ncol(out))
      {
        out[,i] = out[,i] - colshift[i]
      }
      
      out
    },
    "Quantile" = {
      progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
      on.exit(progress$close())
      progress$set(message = "Quantile-normalising...", detail = "")
      
      require("limma")
      as.data.frame(normalizeBetweenArrays(as.matrix(out)))
    }
  )
})

final.data_todebounce = reactive ({
  validate(need(checkpoints$numeric.exists, message = ""))
  dat.in = pre.imp.med()
  resultsrv$results.exist = FALSE
  
  imp.dat = switch(input$imputemethod,
    "rti" = {
      progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
      progress$set(message = "Random tail imputing...", detail = "")
      
      imp.smps = datrv$sample.groups %in% input$sel.impute.groups
      cond.imp.smps = datrv$sample.groups %in% input$sel.impute.groups.sparse
      
      group.mns = sapply(datrv$group.names, function(grp) {
        grp.dat = dat.in[datrv$sample.groups == grp]
        grp.dat = do.call(c, as.list(grp.dat))
        return(mean(grp.dat, na.rm = T))
      })
      
      group.sds = sapply(datrv$group.names, function(grp) {
        grp.dat = dat.in[datrv$sample.groups == grp]
        grp.dat = do.call(c, as.list(grp.dat))
        return(sd(grp.dat, na.rm = T))
      })
      
      group.imp.rows = sapply(datrv$group.names, function(grp) {
        if (grp %in% input$sel.impute.groups)
        {
          return (rep(TRUE, nrow(dat.in)))
        } else {
          min.obs = input$sel.impute.min.obs
          if (is.na(min.obs)) min.obs = 0
          grp.dat = dat.in[datrv$sample.groups == grp]
          row.insuf.obs = apply(grp.dat, 1, function(rw) sum(is.finite(rw)) < min.obs)
          return(row.insuf.obs) 
        }
      })
      colnames(group.imp.rows) = datrv$group.names
      
      imp.out = dat.in
      for (i in which(imp.smps | cond.imp.smps))
      {
        grp = datrv$sample.groups[i]
        imp.out[group.imp.rows[,grp], i] = RTI.data(dat.in[group.imp.rows[,grp],i], input$RTI.shift, input$RTI.width, group.mns[grp], group.sds[grp])
      }
      progress$close()
      imp.out
    },
    "knn" = {
      
      progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
      progress$set(message = "k-nearest neighbours imputing...", detail = "")
      
      imp.smps = datrv$sample.groups %in% input$sel.impute.groups
      cond.imp.smps = datrv$sample.groups %in% input$sel.impute.groups.sparse
      
      group.imp.rows = sapply(datrv$group.names, function(grp) {
        if (grp %in% input$sel.impute.groups)
        {
          return (rep(TRUE, nrow(dat.in)))
        } else {
          min.obs = input$sel.impute.min.obs
          if (is.na(min.obs)) min.obs = 0
          grp.dat = dat.in[datrv$sample.groups == grp]
          row.insuf.obs = apply(grp.dat, 1, function(rw) sum(is.finite(rw)) < min.obs)
          return(row.insuf.obs) 
        }
      })
      colnames(group.imp.rows) = datrv$group.names
      
      # We impute the whole dataset first to use max
      # information relating to gene neighbours,
      # but we want to output a set where only columns
      # from selected groups are imputed
      
      imp.dat = as.matrix(dat.in)
      imp.dat = impute.knn(data = imp.dat)$data
      imp.out = dat.in
      
      for (i in which(imp.smps | cond.imp.smps))
      {
        grp = datrv$sample.groups[i]
        imp.out[group.imp.rows[,grp], i] = imp.dat[group.imp.rows[,grp], i]
      }
      
      progress$close()
      imp.out
    },
    dat.in
  )
  
  switch(input$post.norm,
    "None" = imp.dat,
    "Median" = {
      progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
      on.exit(progress$close())
      progress$set(message = "Median-normalising...", detail = "")
      
      glob.med = median(do.call(c, as.list(imp.dat)), na.rm = T)
      col.meds = sapply(as.list(imp.dat), median, na.rm = T)
      colshift = col.meds - glob.med
      
      for (i in 1:ncol(imp.dat))
      {
        imp.dat[,i] = imp.dat[,i] - colshift[i]
      }
      
      imp.dat
    },
    "Quantile" = {
      progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
      on.exit(progress$close())
      progress$set(message = "Quantile-normalising...", detail = "")
      
      require("limma")
      as.data.frame(normalizeBetweenArrays(as.matrix(imp.dat)))
    }
  )
})

final.data = final.data_todebounce %>% throttle(500)

final.data.mi = reactive ({
  dat.in = pre.imp.med()
  
  # Progress message
  progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
  on.exit(progress$close())
  
  # Set up cluster for parallel computing
  
  num.threads = max(1, detectCores() - 1)
  cl = if (!is.na(num.threads)) makeCluster(num.threads) else NULL
  
  imp.dat = switch(input$imputemethod,
    "rti" = {
      progress$set(message = "Please wait. ", detail = "RTI imputing...")
      on.exit(progress$close())
      
      imp.smps = datrv$sample.groups %in% input$sel.impute.groups
      cond.imp.smps = datrv$sample.groups %in% input$sel.impute.groups.sparse
      
      group.mns = sapply(datrv$group.names, function(grp) {
        grp.dat = dat.in[datrv$sample.groups == grp]
        grp.dat = do.call(c, as.list(grp.dat))
        return(mean(grp.dat, na.rm = T))
      })
      
      group.sds = sapply(datrv$group.names, function(grp) {
        grp.dat = dat.in[datrv$sample.groups == grp]
        grp.dat = do.call(c, as.list(grp.dat))
        return(sd(grp.dat, na.rm = T))
      })
      
      group.imp.rows = sapply(datrv$group.names, function(grp) {
        if (grp %in% input$sel.impute.groups)
        {
          return (rep(TRUE, nrow(dat.in)))
        } else {
          min.obs = input$sel.impute.min.obs
          if (is.na(min.obs)) min.obs = 0
          grp.dat = dat.in[datrv$sample.groups == grp]
          row.insuf.obs = apply(grp.dat, 1, function(rw) sum(is.finite(rw)) < min.obs)
          return(row.insuf.obs) 
        }
      })
      colnames(group.imp.rows) = datrv$group.names
      
      if (!is.null(cl))
      {
        cur.env = environment()
        
        clusterExport(cl, "RTI.data", envir = cur.env)
        
        parLapply(cl, 1:rv$mi.num, function(imp, imp.dat, shift, width, group.mns, group.sds, sample.groups) {
          
          for (i in which(imp.smps | cond.imp.smps))
          {
            grp = sample.groups[i]
            imp.dat[group.imp.rows[,grp], i] = RTI.data(dat.in[group.imp.rows[,grp],i], shift, width, group.mns[grp], group.sds[grp])
          }
          
          return (imp.dat)
          
        }, dat.in, input$RTI.shift, input$RTI.width, group.mns, group.sds, datrv$sample.groups)
      } else {
        lapply(1:rv$mi.num, function(imp, imp.dat, shift, width, group.mns, group.sds, sample.groups) {
          
          for (i in which(imp.smps | cond.imp.smps))
          {
            grp = sample.groups[i]
            imp.dat[group.imp.rows[,grp], i] = RTI.data(dat.in[group.imp.rows[,grp],i], shift, width, group.mns[grp], group.sds[grp])
          }
          
          return (imp.dat)
          
        }, dat.in, input$RTI.shift, input$RTI.width, group.mns, group.sds, datrv$sample.groups)
      }
      
    },
    "knn" = {
      imp.smps = datrv$sample.groups %in% input$sel.impute.groups
      
      progress$set(message = "Please wait. ", detail = "k-NN imputing...")
      on.exit(progress$close())
      
      if (!is.null(cl))
      {
        clusterEvalQ(cl, library("impute"))
        parLapply(cl, 1:rv$mi.num, function(idx, dat.in, imp.smps) {
          imp.dat = as.matrix(dat.in)
          imp.dat = impute.knn(data = imp.dat)$data
          
          # We impute the whole dataset first to use max
          # information relating to gene neighbours,
          # but we want to output a set where only columns
          # from selected groups are imputed
          imp.out = dat.in
          imp.out[,imp.smps] = imp.dat[,imp.smps]
          
          return(imp.out)
          
        }, dat.in, imp.smps) 
      } else {
        library("impute")
        lapply(1:rv$mi.num, function(idx, dat.in, imp.smps) {
          imp.dat = as.matrix(dat.in)
          imp.dat = impute.knn(data = imp.dat)$data
          
          # We impute the whole dataset first to use max
          # information relating to gene neighbours,
          # but we want to output a set where only columns
          # from selected groups are imputed
          imp.out = dat.in
          imp.out[,imp.smps] = imp.dat[,imp.smps]
          
          return(imp.out)
          
        }, dat.in, imp.smps)
      }
    },
    dat.in
  )
  
  # Global Median Normalization
  out = switch(input$post.norm,
    "None" = imp.dat,
    "Median" = {
      parLapply(cl, imp.dat, function(set) {
        
        glob.med = median(do.call(c, as.list(set)), na.rm = T)
        col.meds = sapply(as.list(set), median, na.rm = T)
        colshift = col.meds - glob.med
        
        for (i in 1:ncol(set))
        {
          set[,i] = set[,i] - colshift[i]
        }
        
        set
      })
    },
    "Quantile" = {
      require("limma")
      clusterEvalQ(cl, library(limma))
      parLapply(cl, imp.dat, function(set) {
        as.data.frame(normalizeBetweenArrays(as.matrix(set)))
      })
    }
  )
  
  if (!is.null(cl)) stopCluster(cl)
  
  out
})

sparsefilter.descriptive = reactive ({
  
  validate(need(checkpoints$numeric.exists, message = ""))

  mvfilter.keep = if (workflow$doRowFilter)
  {
    kept.rows()
  } else if (input$filter.empty.rows) {
    nonempty.rows()
  } else {
    rep(TRUE, length(nonempty.rows()))
  }
    
  dsc = datrv$descriptive.data
  rn = rownames(dsc)
  dsc = data.frame(dsc[mvfilter.keep,], stringsAsFactors = F)
  rownames(dsc) = rn[mvfilter.keep]
  names(dsc) = names(datrv$descriptive.data)
  dsc
})

final.descriptive = reactive ({

  validate(need(checkpoints$numeric.exists, message = ""))
  
  dsc = sparsefilter.descriptive()
  rn = rownames(dsc)
  
  discards = highsd.discards()
  if (length(discards) > 0)
  {
    dsc = data.frame(dsc[-discards,], stringsAsFactors = F)
    rownames(dsc) = rn[-discards]
    names(dsc) = names(datrv$descriptive.data)
  }
  
  dsc
})

RTI.data = function (dat, shift, width, forced.mean = NULL, forced.sd = NULL) {
  dat.mn = c()
  dat.sd = c()
  
  if (is.null(forced.mean))
  {
    dat.mn = mean(dat, na.rm = T)
  } else {
    dat.mn = forced.mean
  }
  
  if (is.null(forced.sd))
  {
    dat.sd = sd(dat, na.rm = T)
  } else {
    dat.sd = forced.sd
  }
  
  imp.mn = dat.mn + shift * dat.sd
  imp.sd = width * dat.sd
  
  na.inds = !is.finite(dat)
  cnt.na = sum(na.inds)
  
  imp.vals = rnorm(cnt.na, mean = imp.mn, sd = imp.sd)
  orig.dat = na.omit(dat)
  dat[na.inds] = imp.vals
  
  return(dat)
}

output$RTI.dp = renderPlotly ({
  
  dat = if (length(input$sel.impute.groups) > 0 | length(input$sel.impute.groups.sparse) > 0)
  {
    imp.smps = datrv$sample.groups %in% union(input$sel.impute.groups, input$sel.impute.groups.sparse)
    orig.dat = do.call(c, as.list(pre.imp.med()[,imp.smps]))
    imp.dat = RTI.data(orig.dat, input$RTI.shift, input$RTI.width)[is.na(orig.dat)]
    orig.dat = na.omit(orig.dat)
    dataset = factor(rep(c("Original", "Imputed"), times = c(length(orig.dat), length(imp.dat))), levels = c("Imputed", "Original"))
    data.frame(Intensity = c(orig.dat, imp.dat), Dataset = dataset)
  } else {
    data.frame(Intensity = do.call(c, as.list(pre.imp.med())), Dataset = factor("Original", levels = c("Imputed", "Original")))
  }
    
  require(ggplot2)
  
  p = ggplot(dat, aes(x=Intensity, fill=Dataset)) +
    geom_histogram(binwidth=.5, alpha=0.5, position=ifelse(input$stackRTIdp, "stack", "identity")) +
    scale_fill_manual(values = c("#F8766D", "#00BFC4"), drop = FALSE) +
    scale_y_continuous(expand = c(0,0)) +
    theme(
      legend.title = element_blank(),
      legend.background = element_rect(fill = "#00000000"),
      panel.background = element_rect(fill = "#f5f5f5", colour = "#f5f5f5"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#00000000"),
      axis.line.y = element_line(colour = "#000000"),
      axis.line.x = element_line(colour = "#000000")
    )
  
  ggplotly(p) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 1.2))
})

output$btnDownloadProcessed = downloadHandler (
  filename = function() {
    savetype = input$processed.savetype
    fext = switch(savetype,
      "xls" = ".xlsx",
      "tsv" = ".txt",
      "csv" = ".csv",
      "rdata" = ".RData"
    )
    
    fn = paste0("preprocessed.", Sys.Date(), fext)
    return(fn)
  },
  content = function(fn) {
    savetype = input$processed.savetype
    
    desc = final.descriptive()
    numdat = final.data()
    
    processed.data = data.frame(desc, numdat, stringsAsFactors = FALSE)
    
    row.names = FALSE
    col.names = TRUE
    
    switch(savetype,
      "xls" = write_xlsx(processed.data, path = fn, col_names = col.names),
      "tsv" = write.table(processed.data, file = fn, sep = "\t", row.names = row.names, col.names = col.names, na = ""),
      "csv" = write.table(processed.data, file = fn, sep = ",", row.names = row.names, col.names = col.names, na = ""),
      "rdata" = save(processed.data, file = fn)
    )
  }
)

source("analysis/barplot.server.R", local = TRUE) # Crash
source("analysis/group.variance.server.R", local = TRUE) # Crash
source("analysis/sample.variance.server.R", local = TRUE) # Crash
source("analysis/pre.boxplot.server.R", local = TRUE)

source("analysis/dendrogram.server.R", local = TRUE) # Crash
source("analysis/PCA.server.R", local = TRUE)
source("analysis/heatmap.server.R", local = TRUE)
source("analysis/intvsmiss.server.R", local = TRUE) # Crash
source("analysis/eigenplot.server.R", local = TRUE)

# DE analysis #

select.contrast = observeEvent (input$contrast.dropdown, {
  validate(
    need(input$contrast.dropdown, label = "Contrast dropdown")
  )

  contrast_info = str_split(input$contrast.dropdown, pattern = ";;;") %>% unlist
  contrast_form = contrast_info[1]
  contrast_name = contrast_info[2]
  
  updateTextInput(session, "contrast.name", value = contrast_name)
  updateTextInput(session, "contrast.input", value = contrast_form)
})

add.contrast = observeEvent (input$addcontbutton,
{
  validate(
    need(input$contrast.name, label = "Contrast name"),
    need(input$contrast.input, label = "Contrast formula")
  )
  
  rv$contnames = c(rv$contnames, input$contrast.name)
  rv$contrasts = c(rv$contrasts, input$contrast.input)
  rv$cont.inc.groups = c(rv$cont.inc.groups, list(seq(length(datrv$group.names))))
  
  cont.group.names = strsplit(input$contrast.input, split = "[^.a-zA-Z]")[[1]]
  cont.group.names = cont.group.names[cont.group.names != ""]
  cont.group.names = unique(cont.group.names)
  cont.group.idx = which(datrv$group.names %in% cont.group.names)
  rv$cont.comp.groups = c(rv$cont.comp.groups, list(cont.group.idx))
  
  updateTextInput(session, "contrast.name", value = "")
  updateTextInput(session, "contrast.input", value = "")
})

observeEvent(rv$contrasts, {
  resultsrv$results.exist = FALSE
})


output$current.contrasts = DT::renderDataTable({
  if (length(rv$contnames) > 0)
  {
    data.frame("Contrast name" = rv$contnames, "Contrast formula" = rv$contrasts)
  }
},options = list(lengthChange = FALSE, pageLength = 1000, rownames = FALSE, dom = 't'),
selection = list(target = "row", mode = "single"))

output$displaycontrasts = renderText({
  if (length(rv$contnames) > 0)
    "TRUE"
  else
    "FALSE"
})
outputOptions(output, "displaycontrasts", suspendWhenHidden = FALSE)

deletecontrasts = observeEvent (input$delcontsbtn, 
                                {
                                  del.rows = input$current.contrasts_rows_selected
                                  if (length(del.rows) > 0)
                                  {
                                    rv$contnames = rv$contnames[-del.rows]
                                    rv$contrasts = rv$contrasts[-del.rows]
                                    rv$cont.inc.groups = rv$cont.inc.groups[-del.rows]
                                    rv$cont.comp.groups = rv$cont.comp.groups[-del.rows]
                                  }
                                })

clear.contrasts = observeEvent (input$clearcontbutton,
                                {
                                  rv$contnames = c()
                                  rv$contrasts = c()
                                  rv$cont.inc.groups = c()
                                  rv$cont.comp.groups = c()
                                })

doanalysisfunc = function()
{
  validate(need(length(rv$contrasts) > 0, message = ""))
  rv$doanalysiscnt = rv$doanalysiscnt + 1
  
  rv$analysis.desc = final.descriptive()
  rv$analysis.finaldata = final.data()
  rv$analysis.filtdata = filtsd.data()
  
  resultsrv$siglists = resultsrv$siglists[integer(),]
  updateSelectInput(session, "sel.plot.contrast", choices = rv$contnames)
  if (input$imputemethod == "rti" && input$num.imputations > 1)
  {
    rv$analysis.mi = T
    rv$mi.num = input$num.imputations
  } else {
    rv$analysis.mi = F
  }
  
  if (length(input$sel.minrawval.groups) > 0 && is.finite(as.integer(input$filter.minvals)))
  {
    min.rawval.grps = rep(F, length(datrv$group.names))
    names(min.rawval.grps) = datrv$group.names
    min.rawval.grps[input$sel.minrawval.groups] = T
    
    rv$min.rawval.grps = min.rawval.grps
    
    rv$min.rawvals = as.integer(input$filter.minvals)
  } else {
    rv$min.rawvals = as.integer(input$filter.minvals)
    rv$min.rawval.grps = rep(F, length(datrv$group.names))
  }
  
  resultsrv$results.exist = TRUE 
}

doanalysis = observeEvent (input$doanalysisbutton, {
  doanalysisfunc()
  showTab("tb.nav", "DE Results", select = TRUE, session = session)
})

output$deresultsexist = renderText({
  if (resultsrv$results.exist) "true" else "false"
})
outputOptions(output, "deresultsexist", suspendWhenHidden = FALSE)

# LINEAR FIT #
do.lm = function(dat, cont.name.vec, cont.vec, group.names, group.matrix)
{
  cmat.args = c(as.list(cont.vec), list(group.names))
  names(cmat.args) = c(cont.name.vec, "levels")
  
  design = data.matrix(group.matrix)
  array.weights = arrayWeights(dat, design)
  
  fit1 = lmFit(dat, design, weights = array.weights)
  
  contrast.mat = do.call(makeContrasts, cmat.args)
  
  fit2 = contrasts.fit(fit1, contrast.mat)
  fit2 = eBayes(fit2)
  
  return (fit2)
}

# Linear fit with no multiple imputation
linear.fit = function() {
  contnames = rv$contnames
  contrasts = rv$contrasts
  cont.inc.groups = rv$cont.inc.groups
  group.names = datrv$group.names
  group.matrix = datrv$group.matrix
  
  cont.dat = mapply(FUN = function (contname, contform, cont.groups) {
    cont.group.names = datrv$group.names[cont.groups]
    cont.samples = datrv$sample.groups %in% cont.group.names
    cont.group.matrix = datrv$group.matrix[cont.samples, cont.groups]
    
    ret.dat = list(name = contname, form = contform, group.names = cont.group.names, samples = cont.samples, group.matrix = cont.group.matrix)
    return(ret.dat)
  }, contnames, contrasts, cont.inc.groups, SIMPLIFY = FALSE)

  if (rv$analysis.mi) # Fit from MI datasets
  {
    dat.mi = final.data.mi()
    num.sets.done = 0
    num.datasets = length(dat.mi)
    progress = shiny::Progress$new(session, min = 0, max = num.datasets)
    progress$set(num.sets.done, message = "Please wait. ", detail = "Setting up parallel analysis...")
    on.exit(progress$close())
    
    # Parallel analyses
    num.threads = max(1, detectCores() - 1)
    cl = if (is.na(num.threads)) NULL else makeCluster(num.threads)
    
    progress$set(num.sets.done, message = "Please wait. ", detail = "Analysing...")

    
    if (!is.null(cl))
    {
      # Export libraries
      clusterEvalQ(cl, library(limma))
      out = lapply(cont.dat, function (contrast) {
        dat.in = lapply (dat.mi, function(dat.x) dat.x[,contrast$samples])
        ret.dat = parLapply(cl, dat.in, fun = do.lm, contrast$name, contrast$form, contrast$group.names, contrast$group.matrix)
        return(ret.dat)
      })
      
      stopCluster(cl)
      
      out
    } else {
      library(limma)
      out = lapply(cont.dat, function (contrast) {
        dat.in = lapply (dat.mi, function(dat.x) dat.x[,contrast$samples])
        ret.dat = parLapply(cl, dat.in, fun = do.lm, contrast$name, contrast$form, contrast$group.names, contrast$group.matrix)
        return(ret.dat)
      })
    }
    
  } else { # Fit from single dataset
    out = lapply(cont.dat, function (contrast) {
      dat.in = rv$analysis.finaldata[,contrast$samples]
      ret.dat = do.lm(dat.in, contrast$name, contrast$form, contrast$group.names, contrast$group.matrix)
      
      adj.p = apply(ret.dat$p.value, 2, p.adjust, method = "BH")
      ret.dat$adj.p.val = adj.p
      
      return(ret.dat)
    })
    
    out
  }
}

output$grouplist = DT::renderDataTable ({
  data.frame("Group names" = datrv$group.names)
}, options = list(dom = "t", pageLength = 1000, lengthChange = FALSE),
  selection = list(target = "row", mode = "multiple"))

grouplist.proxy = DT::dataTableProxy("grouplist")
#BOOKMARK

observeEvent(length(input$current.contrasts_rows_selected), {
  
  cont.idx = input$current.contrasts_rows_selected
  if (length(cont.idx) == 1)
  {
    selectRows(grouplist.proxy, rv$cont.inc.groups[[cont.idx]])
  } else {
    selectRows(grouplist.proxy, NULL)
  }
})

observeEvent(length(input$grouplist_rows_selected), {
  cont.idx = input$current.contrasts_rows_selected
  
  validate(need(length(input$current.contrasts_rows_selected) == 1, message = ""))
  
  old.groups = rv$cont.inc.groups[[cont.idx]]
  new.groups = input$grouplist_rows_selected
  
  # Only update groups included in contrast if a group has actually been selected/unselected
  # (As opposed to updating them every time a different contrast is selected)
  if (length(old.groups) != length(intersect(old.groups, new.groups)))
  {
    rv$cont.inc.groups[[cont.idx]] = sort(union(rv$cont.comp.groups[[cont.idx]], new.groups))
    if (!all(rv$cont.comp.groups[[cont.idx]] %in% new.groups))
      selectRows(grouplist.proxy, rv$cont.inc.groups[[cont.idx]])
  }
})

mi.combine = function() {
  if (rv$analysis.mi)
  {
    
    allcont.lfs = linear.fit()
    
    out = lapply(allcont.lfs, function (lfs) {
      m = length(lfs)
      
      # coefficient estimates
      all.coefs = sapply(lfs, function(lf) return(lf$coef))
      coef.ests = apply(all.coefs, 1, mean, na.rm = T)
      coef.ests[!is.finite(coef.ests)] = NA
      
      # Within Variance estimate
      w.var = {
        all.vars = sapply(lfs, function(lf) return(lf$s2.post))
        
        apply(all.vars, 1, mean)
      }
      
      b.var = apply(all.coefs, 1, var, na.rm = T)
      
      t.var = w.var + (1 + 1/m)*b.var
      
      t = coef.ests / sqrt(t.var)
      
      deg.f = (m - 1) * (1 + (m * w.var) / (b.var*(m+1)))^2
      
      p = pt(abs(t), df = deg.f, lower.tail = F) * 2
      
      adj.p = p.adjust(p, method = "BH")
      
      t.sd = sqrt(t.var)
      
      return( list(coefficients = coef.ests, coef.sd = t.sd, t = t, p.value = p, adj.p.val = adj.p) )
    })
    
    out
  }
}

de.results = eventReactive (rv$doanalysiscnt, {
  validate(need(rv$doanalysiscnt > 0, "Before first analyis"))
  if (rv$analysis.mi) mi.combine()
  else linear.fit()
})

all.tts = eventReactive (de.results(), {
  require(limma)
  out = NULL
  if (rv$analysis.mi)
  {
    micom = de.results()
    out = mapply(function(cont.lf, contname) {
      tb = data.frame(logFC = cont.lf$coefficients,
                      logFC.SD = cont.lf$coef.sd,
                      t = cont.lf$t,
                      P.Value = cont.lf$p.value,
                      adj.P.Val = cont.lf$adj.p.val)
      
      tb = cbind(rv$analysis.desc, tb)
      
      return(tb)
    }, micom, rv$contnames, SIMPLIFY = FALSE)
    
  } else {
    #bookmark
    cont.lfs = de.results()
    out = mapply(function(cont.lf, contname) {
      return(topTable(cont.lf, coef = contname, number = nrow(cont.lf), genelist = rv$analysis.desc, sort.by = "none"))
    }, cont.lfs, rv$contnames, SIMPLIFY = FALSE)
  }
  names(out) = rv$contnames
  out
})

final.resultstable = eventReactive(list(rv$analysis.finaldata, input$sel.plot.contrast), {
  selected.contrast = input$sel.plot.contrast
  alltops = all.tts()
  alltops[[selected.contrast]]
})

output$toptable = DT::renderDataTable ({
  
  out = if (input$disp.sig.only)
  {
    siglist = current.siglist()
    keeprows = unlist(siglist$sig.rows)
    
    final.resultstable()[keeprows,] 
  } else {
    final.resultstable()
  }
  
  numeric.columns = sapply(as.list(out), class) == "numeric" & colnames(out) != "AveExpr"
  out[,numeric.columns] = signif(out[,numeric.columns, drop = FALSE], digits = 2)
  if ("AveExpr" %in% colnames(out)) {
    out[,"AveExpr"] = signif(out[,"AveExpr", drop = FALSE], digits = 5) 
  }
  
  out
}, options = list(scrollX = T, serverSide = T, pageLength = 10), rownames = FALSE)

output$downloadresults = downloadHandler (
  filename = function() {
    savetype = input$results.savetype
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
    
    savetype = input$results.savetype

    results = final.resultstable()

    keeprows = if (input$disp.sig.only)
    {
      siglist = current.siglist()
      unlist(siglist$sig.rows)
    } else {
      seq(nrow(results))
    }

    save.data.rdata = list(rv$analysis.desc[keeprows,,drop=FALSE], rv$analysis.filtdata[keeprows,,drop=FALSE], results[keeprows,,drop=FALSE])
    names(save.data.rdata) = c("desc", "dat.filter", "results")
    save.data.other = cbind(rv$analysis.desc, rv$analysis.filtdata, results)[keeprows,,drop=FALSE]

    row.order = order(save.data.other$P.Value)
    save.data.other = save.data.other[row.order,,drop=FALSE]
    save.data.rdata = lapply(save.data.rdata, function(tb) return(tb[row.order,,drop=FALSE]))

    row.names = FALSE
    col.names = TRUE
    
    switch(savetype,
      "xls" = write_xlsx(save.data.other, path = fn, col_names = col.names),
      "tsv" = write.table(save.data.other, file = fn, sep = "\t", row.names = row.names, col.names = col.names, na = ""),
      "csv" = write.table(save.data.other, file = fn, sep = ",", row.names = row.names, col.names = col.names, na = ""),
      "rdata" = save(save.data.rdata$desc, save.data.rdata$dat.filter, save.data.rdata$results, file = fn)
    )
  }
)

source("analysis/tdist.server.R", local = TRUE)
source("analysis/pdist.server.R", local = TRUE)
source("analysis/pzoom.server.R", local = TRUE)

sig.grps = reactive ({
  validate(need(de.results(), label = "DE analysis"))
  
  contrast = input$sel.plot.contrast
  
  x = de.results()[[contrast]]$coefficients
  p = de.results()[[contrast]][[input$norm.adj.p]]
  
  siglist = current.siglist()
  sig.pts = unlist(siglist$sig.rows)
  excluded.pts = unlist(siglist$excluded.rows)
  
  out = data.frame(
    Up = seq(length(p)) %in% sig.pts & x >= 0,
    Down = seq(length(p)) %in% sig.pts & x < 0,
    Upex = seq(length(p)) %in% excluded.pts & x >= 0,
    Downex = seq(length(p)) %in% excluded.pts & x < 0
  )
  
  out = data.frame(!apply(out, 1, any), out)
  colnames(out) = c("NS", "Up", "Down", "Upex", "Downex")
  out
})

output$deresults.upsignum = renderText(sum(sig.grps()$Up))
output$deresults.downsignum = renderText(sum(sig.grps()$Down))
output$deresults.bothsignum = renderText(sum(sig.grps()$Up) + sum(sig.grps()$Down))
output$deresults.upexnum = renderText(sum(sig.grps()$Upex))
output$deresults.downexnum = renderText(sum(sig.grps()$Downex))
output$deresults.bothexnum = renderText(sum(sig.grps()$Upex) + sum(sig.grps()$Downex))

source("analysis/volcano.server.R", local = TRUE)

resultsrv = reactiveValues(
  siglists = data.frame(
    name = character(0),
    contname = character(0),
    p.cutoff = character(0),
    p.type = character(0),
    lfc.cutoff = numeric(0),
    lfc.comp = numeric(0),
    
    stringsAsFactors = FALSE
  ),
  results.exist = FALSE
)

current.siglist = reactive ({
  
  # Exclude rows from
  forbid.rows = if(!is.null(resultsrv$siglists) && length(input$sel.exclude.siglists) > 0)
  {
    forbid.list.idx = resultsrv$siglists$name %in% input$sel.exclude.siglists
    forbid.row.idx = resultsrv$siglists[forbid.list.idx,"sig.rows"]
    forbid.row.idx = unlist(forbid.row.idx)
    forbid.row.idx = unique(forbid.row.idx)
    out = rep(FALSE, nrow(rv$analysis.finaldata))
    out[forbid.row.idx] = TRUE
    out
  } else {
    rep(FALSE, nrow(rv$analysis.finaldata))
  }
    
  if (length(input$siglist.datatable_rows_selected) == 1 && nrow(resultsrv$siglists) > 0)
  {
    new.row = resultsrv$siglists[input$siglist.datatable_rows_selected,]
    new.row$excluded.rows = list(union(new.row$excluded.rows[[1]], which(forbid.rows)))
    new.row$sig.rows = list(setdiff(new.row$sig.rows, new.row$excluded.rows))
    
    new.row
  } else {
    p.cutoff = input$num.sig.max.p
    lfc.cutoff = input$sld.sig.min.fc
    lfc.comp = input$rad.logfc.comp
    p.type = input$norm.adj.p
    
    validate(
      need(
        p.cutoff, label = "P-value cutoff"
      ),
      need(
        lfc.cutoff, label = "LogFC change cutoff"
      ),
      need(
        lfc.comp, label = "LogFC comparison type"
      ),
      need(
        p.type == "p.value" || p.type == "adj.p.val",
        label = "Normal or adjusted P value"
      ),
      need(
        length(rv$contrasts) > 0,
        label = "Contrast"
      )
    )
    
    # Get current contrast
    contname = input$sel.plot.contrast
    contrast = rv$contrasts[which(rv$contnames == contname)]
    groupsincontrast = datrv$group.names[(sapply(datrv$group.names, grepl, contrast, fixed = TRUE))]
    
    # Check which rows have at least 2 values in which groups
    noconf.matrix = sapply(groupsincontrast, function(grp) {
      samplesingroup = datrv$sample.groups == grp
      
      out = apply(rv$analysis.filtdata[,samplesingroup], 1, function(row) sum(!is.na(row)) >= 2)
      
      return(out)
    })
    
    noconf.rows = !apply(noconf.matrix, 1, any)
    
    
    # Get p and lfc values from selected contname
    lfc = de.results()[[contname]]$coefficients
    p = de.results()[[contname]][[input$norm.adj.p]]
    
    if (is.na(p.cutoff)) p.sig.lvl = 0
    
    sig.p = p < p.cutoff
    
    sig.fc = switch(input$rad.logfc.comp,
      "greater" = {lfc > lfc.cutoff},
      "less" = {lfc < -lfc.cutoff},
      "both" = {abs(lfc) > lfc.cutoff}
    )
    
    all.sig.rows = sig.p & sig.fc
    excluded.bool = all.sig.rows & (forbid.rows | noconf.rows)
    sig.rows = which(all.sig.rows & !excluded.bool) 
    excluded.rows = which(excluded.bool)
    
    new.row = data.frame(
      name = NA,
      contname = contname,
      p.cutoff = p.cutoff,
      p.type = p.type,
      lfc.cutoff = lfc.cutoff,
      lfc.comp = lfc.comp,
      
      stringsAsFactors = FALSE
    )
    new.row$sig.rows = list(sig.rows)
    new.row$excluded.rows = list(excluded.rows)
    
    new.row
  }
  
})

do.save.siglist = eventReactive (input$btn.save.siglist, {
  name = input$siglist.name
  
  validate(
    need(
      !(name %in% resultsrv$name),
      message = "A significance list with this name exists."
    ),
    need(
      name, label = "Name"
    )
  )
  
  new.row = current.siglist()
  
  new.row$name = name
  new.row$descriptive = list(rv$analysis.desc[unlist(new.row$sig.rows),])
  
  resultsrv$siglists = rbind(resultsrv$siglists, new.row)
  
  paste("Significance list", name, "saved")
  
})
observe(do.save.siglist())
output$save.siglist.msg = renderText(do.save.siglist())

siglist.datatable = reactive ({
  validate(need(nrow(resultsrv$siglists) > 0, message = ""))
  
  ptypestring = sapply(resultsrv$siglists$p.type, switch, "p.value" = "p", "adj.p.val" = "Adj. p", NA)
  
  pstring = paste(ptypestring, "<", resultsrv$siglists$p.cutoff)
  pstring[is.na(ptypestring)] = NA
  
  lfcstring = unlist(mapply(function(comptype, cutoff) {
    switch(comptype,
      "greater" = {
        paste0("LFC > ", cutoff)
      }, "less" = {
        paste0("LFC < ", -cutoff)
      }, "both" = {
        paste0("|LFC| > ", cutoff)
      },
      NA
    )
  }, resultsrv$siglists$lfc.comp, resultsrv$siglists$lfc.cutoff))
  
  out = data.frame(Name = resultsrv$siglists$name,
    "Contrast" = resultsrv$siglists$contname,
    "Sig cond" = pstring,
    "LFC cond" = lfcstring,
    row.names = NULL
  )
  out
})

output$displaysigtable = renderText({
  if (nrow(resultsrv$siglists) > 0)
    "TRUE"
  else
    "FALSE"
})
outputOptions(output, "displaysigtable", suspendWhenHidden = FALSE)

# Show table of significance list conditions
output$siglist.datatable = DT::renderDataTable(siglist.datatable(),
  options = list(lengthChange = FALSE, pageLength = 1000, rownames = FALSE, dom = 't'),
  selection = list(target = "row", mode = "multiple"))

deletesiglists = observeEvent (input$delsiglistsbtn, {
  del.rows = input$siglist.datatable_rows_selected
  if (length(del.rows) > 0)
    resultsrv$siglists = resultsrv$siglists[-del.rows,]
})

clearallsiglists = observeEvent (input$clearsiglistsbtn, {
  resultsrv$siglists = resultsrv$siglists[-(1:nrow(resultsrv$siglists)),]
})

do.union.siglist = observeEvent (input$unionsiglistsbtn, {
  validate(need(length(input$siglist.datatable_rows_selected) >= 2, message = "Must select 2 or more remembered significance lists"))
  name = input$siglist.name
  
  validate(
    need(
      !(name %in% resultsrv$name),
      message = "A significance list with this name exists."
    ),
    need(
      name, label = "Name"
    )
  )
  
  new.row = data.frame(
    name = name,
    contname = NA,
    p.cutoff = NA,
    p.type = NA,
    lfc.cutoff = NA,
    lfc.comp = NA,
    
    stringsAsFactors = FALSE
  )
  
  rows.to.combine = input$siglist.datatable_rows_selected
  
  sig.rows = Reduce(union, resultsrv$siglists$sig.rows[rows.to.combine])
  new.row$sig.rows = list(sig.rows)
  new.row$excluded.rows = list(integer(0))
  
  new.row$descriptive = list(rv$analysis.desc[unlist(new.row$sig.rows),])
  
  resultsrv$siglists = rbind(resultsrv$siglists, new.row)
})

do.insect.siglist = observeEvent (input$insectsiglistbtn, {
  validate(need(length(input$siglist.datatable_rows_selected) >= 2, message = "Must select 2 or more remembered significance lists"))
  name = input$siglist.name
  
  validate(
    need(
      !(name %in% resultsrv$name),
      message = "A significance list with this name exists."
    ),
    need(
      name, label = "Name"
    )
  )
  
  new.row = data.frame(
    name = name,
    contname = NA,
    p.cutoff = NA,
    p.type = NA,
    lfc.cutoff = NA,
    lfc.comp = NA,
    
    stringsAsFactors = FALSE
  )
  
  rows.to.combine = input$siglist.datatable_rows_selected
  
  sig.rows = Reduce(intersect, resultsrv$siglists$sig.rows[rows.to.combine])
  new.row$sig.rows = list(sig.rows)
  new.row$excluded.rows = list(integer(0))
  
  new.row$descriptive = list(rv$analysis.desc[unlist(new.row$sig.rows),])
  
  resultsrv$siglists = rbind(resultsrv$siglists, new.row)
})

# Update siglist exclude selectize
observe ({
  updateSelectizeInput(session, "sel.exclude.siglists", choices = resultsrv$siglists$name)
  updateSelectizeInput(session, "fsh.sel.siglist", choices = resultsrv$siglists$name)
  updateSelectizeInput(session, "gph.sel.siglist", choices = resultsrv$siglists$name)
})

updateDescCols = observe({
  updateSelectInput(session, "fsh.desc.column", choices = colnames(rv$analysis.desc))
  updateSelectInput(session, "gph.desc.column", choices = colnames(rv$analysis.desc))
})

# output$descriptive.preview = DT::renderDataTable({
#   rv$analysis.desc
# }, options = list(scrollX = T, dom = "t", pageLength = 10))
