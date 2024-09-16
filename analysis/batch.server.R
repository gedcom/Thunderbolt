library(DT)
library(sva)

batchrv = reactiveValues(batches = NULL, wrkBatches = NULL, numBatches = 1, wrkNumBatches = 0, do.batch = FALSE)

observe({
  batchrv$batches = rep(1, length(datrv$sample.groups))
  batchrv$numBatches = 1
})

observe({
  validate(need(input$batchnum, message = ""))
  batchrv$wrkNumBatches = max(1, min(length(datrv$sample.groups), input$batchnum))
  batchrv$wrkBatches[batchrv$wrkBatches > batchrv$wrkNumBatches] = 1
})

observeEvent(input$btnSetBatches, {
  batchrv$wrkBatches = batchrv$batches
  batchrv$wrkNumBatches = batchrv$numBatches
  
  showModal(modalDialog(title = "Group samples into batches", size = "m",
    numericInput("batchnum", label = "Number of batches", min = 1, max = length(datrv$sample.groups), value = batchrv$numBatches),
    dataTableOutput("batchtable"),
    footer = actionButton("btnCloseBatchTable", label = "Save and close"),
    easyClose = FALSE
  ))
})

observeEvent(input$btnCloseBatchTable, {
  batchrv$batches = batchrv$wrkBatches
  batchrv$numBatches = batchrv$wrkNumBatches
  removeModal()
})

output$batchtable = DT::renderDataTable({
  out.list = lapply(seq(batchrv$wrkNumBatches), function(bch) {
    out = rep("", length(datrv$sample.groups))
    out[batchrv$wrkBatches == bch] = "x"
    return(out)
  })
  
  out = do.call(data.frame, list(out.list, stringsAsFactors = FALSE))
  colnames(out) = as.character(seq(ncol(out)))
  rownames(out) = datrv$sample.names
  
  out
  
}, options = list(dom = "t", lengthChange = FALSE, pageLength = 100, scrollX = "100%",# scrollY = "50vh", scrollCollapse = TRUE, 
  columnDefs = list(list(className="dt-center", targets = "_all"))), 
  rownames = TRUE, class = "cell-border", selection = list(target = "cell", mode = "single"))

observe ({
  celsel = input$batchtable_cells_selected
  validate(need(nrow(celsel)>0, message = "No cell selected"))
  smp = celsel[1,1]
  smp.batch = celsel[1,2]
  batchrv$wrkBatches[smp] = smp.batch
})

batchfilt.keeprows = reactive({
  
  do.batch = FALSE
  if (input$chk.rem.batch)
    if (length(unique(batchrv$batches)) > 1)
      do.batch = TRUE
    
  if (do.batch)
  {
    batches.keeprows = do.call(data.frame, lapply(unique(batchrv$batches), function(batch) {
      batchdat = filtsd.data()[batchrv$batches == batch]
      batchfinite = as.data.frame(lapply(batchdat, is.finite))
      keeprows = apply(batchfinite, 1, any)
      return(keeprows)
    }))
    
    dataset.keeprows = apply(batches.keeprows, 1, all)

    dataset.keeprows
   } else {
    NULL
  }
})

batchrem.knn = reactive ({
  
  do.knn = FALSE
  # if (input$chk.rem.batch)
  if (batchrv$do.batch)
    if (input$chk.batch.knn)
      do.knn = TRUE
    
  if (do.knn)
  {
    dat.in = filtsd.data()
    
    progress = shiny::Progress$new(session, min = 0, max = rv$mi.num)
    progress$set(message = "k-NN imputing for batch effect removal...", detail = "")
    
    imp.dat = as.matrix(dat.in)
    imp.dat = impute.knn(data = imp.dat)$data
    imp.dat = as.data.frame(imp.dat)
    
    progress$close()
    imp.dat
  } else {
    filtsd.data()
  }
})

setDoBatch = observe({
  if (input$chk.rem.batch)
  {
    if (length(unique(batchrv$batches)) > 1) {
      batchrv$do.batch = TRUE
    } else {
      if (isolate(batchrv$do.batch == TRUE))
        batchrv$do.batch = FALSE
      showModal(modalDialog(title = "Please set batches", "Batch removal will occur only after batches have been specified. Please select 'Set Batches' after closing this dialogue."))
    }
    
  } else if (isolate(batchrv$do.batch)) {
    batchrv$do.batch = FALSE
  }
})

batchrem.data = reactive ({
  out = filtsd.data()
  na.df = NULL
  do.batch = batchrv$do.batch
  get.knn = FALSE
  
  if (do.batch)
  {
    if (input$chk.batch.knn)
    {
      get.knn = TRUE
      
      out = batchrem.knn()
      na.df = is.na(filtsd.data())
    }
    
    batches = as.character(batchrv$batches)
    
    # desmat = datrv$group.matrix
    # form.args = as.list(paste0("desmat[[", seq_along(desmat), "]]"))
    # modmat.formula = paste(form.args, collapse = " + ")
    # modmat.formula = paste0("~", modmat.formula)
    # modmat.formula = as.formula(modmat.formula)
    # 
    # modmat = model.matrix(modmat.formula, data = out)
    
    nsamp = length(batchrv$batches)
    modmat = matrix(rep(1,nsamp), nrow = nsamp)
    colnames(modmat) = "Intercept"
    
    
    # keeprows = batchfilt.keeprows()
    # out = out[keeprows, , drop = FALSE]
    
    out = as.matrix(out)

    out = tryCatch({
      ComBat(out, batches, mod = modmat)
    }, warning = function (x) {
      if (get.knn == FALSE) {
        showModal(modalDialog(title = "Batch removal failed; imputing missing values.", easyClose = TRUE,
          "Batch removal failed. This is most commonly related to missing values in the input dataset. Thunderbolt will activate kNN-imputation for batch removal and re-attempt. These values are removed following batch removal."
        ))
        updateCheckboxInput(session, "chk.batch.knn", value = TRUE)
        validate("")
      } else {
        validate("Batch removal failed with kNN-imputation.")
      }
    }, error = function (x) {
      if (get.knn == FALSE) {
        showModal(modalDialog(title = "Batch removal failed; imputing missing values.", easyClose = TRUE,
          "Batch removal failed. This is most commonly related to missing values in the input dataset. Thunderbolt will activate kNN-imputation for batch removal and re-attempt. These values are removed following batch removal."
        ))
        updateCheckboxInput(session, "chk.batch.knn", value = TRUE)
        validate("")
      } else {
        validate("Batch removal failed with kNN-imputation.")
      }
    })
    out = as.data.frame(out)
    
    if (get.knn)
    {
      out[na.df] = NA
    }
    
    
    out
  } else {
    out
  }
})
