PCA.fit = reactive({
  dat.array = final.data()
  
  validate(need(dat.array, label = "Numeric data"))
  
  dat.array = na.omit(dat.array)
  prcomp(t(dat.array), center = TRUE, scale = FALSE)  
})

PCA = reactive ({
  
  fill.colours = datrv$group.colours
  dim1 = input$dim1
  dim2 = input$dim2
  
  cum.imp = summary(PCA.fit())$importance[3,]
  dim.imp = cum.imp - c(0, cum.imp)[1:length(cum.imp)] # Convert cumulative importance to component importance
  dim.imp = round(dim.imp*100, digits = 1)
  
  plot.data = data.frame(x = round(PCA.fit()$x[,dim1], 2), y = round(PCA.fit()$x[,dim2], 2), Sample = colnames(final.data()),
    Group = factor(datrv$sample.groups, levels = datrv$group.names), stringsAsFactors = FALSE)
  
  p <- ggplot(plot.data, aes(x = x, y = y, fill = Group)) +
    geom_point(shape = 21, size = 2, stroke = 0.3) +
    scale_fill_manual(values = fill.colours) +
    theme(panel.background = element_rect(fill = "white", colour = "black"), legend.position = "bottom") +
    xlab(paste0("Principal Component ", dim1, " (", dim.imp[dim1], "%)")) +
    ylab(paste0("Principal Component ", dim2, " (", dim.imp[dim2], "%)"))
  
  p
})

output$PCA = renderPlotly({
  
  p = ggplotly(PCA())
  p = config(p, displayModeBar = FALSE)
  p = config(p, showLink = FALSE)
  
  dim1 = input$dim1
  dim2 = input$dim2
  
  sample.names = colnames(final.data())
  
  name.list = lapply(datrv$group.names, function(nm) {
    return (sample.names[datrv$sample.groups == nm])
  })
  
  for(i in seq_along(p$x$data))
  {
    p$x$data[[i]]$text = paste0(
      "Sample: ", name.list[[i]], "<br>",
      "Group: ", p$x$data[[i]]$name, "<br>"
    )
  }
  
  p
})

output$save.PCA = downloadHandler (
  filename = function() paste0("PCA.", Sys.Date(), ".png"),
  content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 100, plot = PCA() )
)

output$eps.PCA = downloadHandler (
  filename = function() paste0("PCA.", Sys.Date(), ".eps"),
  content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 100, plot = PCA() )
)