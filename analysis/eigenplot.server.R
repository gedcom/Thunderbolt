eigenplot = eventReactive (input$eigenMSButton, 
  {
    info = 1:nrow(final.data())
    info = cbind(info, info)
    
    dat = as.matrix(final.data())
    
    progress = shiny::Progress$new(session, min = 0, max = 1)
    on.exit(progress$close())
    progress$set(0, message = "Beginning SVD...")
    
    eig_norm1(m=dat, treatment=as.factor(datrv$sample.groups), prot.info=info, progress = progress)
  })

output$eigenplot = renderPlot ({
  eigenplot()
})
# outputOptions(output, "eigenplot", suspendWhenHidden = FALSE)

observeEvent(input$eigenMSButton, shinyjs::show("eigenplot"))