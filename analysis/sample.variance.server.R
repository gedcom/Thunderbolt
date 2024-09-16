sample.variance.plot = reactive ({
  
  sample.variance = sapply(final.data(), var, na.rm = TRUE)

  fill.colours = datrv$group.colours

  sample.names = factor(colnames(final.data()), levels = colnames(final.data()))
  plot.data = data.frame(Sample = sample.names, Group = factor(datrv$sample.groups, levels = datrv$group.names), Variance = sample.variance, stringsAsFactors = FALSE)
  
  p <- ggplot(data=plot.data, aes(x = Sample, y = Variance, fill = Group)) +
    geom_bar(colour="black", stat="identity") +
    scale_fill_manual(values = fill.colours) +
    theme(panel.background = element_rect(fill = "white", colour = "black"), legend.position = "right") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
})

output$sample.variance.plot = renderPlotly ({
  
  p = ggplotly(sample.variance.plot())
  p = config(p, displayModeBar = FALSE)
  p = config(p, showLink = FALSE)

  for(i in seq_along(p$x$data))
  {
    p$x$data[[i]]$text = paste0(
      "Sample: ", colnames(final.data())[p$x$data[[i]]$x], "<br>",
      "Group: ", p$x$data[[i]]$name, "<br>",
      "Variance: ", p$x$data[[i]]$y
    )
  }
  
  p
})

output$save.sample.variance = downloadHandler (
  filename = function() paste0("smpvar.", Sys.Date(), ".png"),
  content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 100, plot = sample.variance.plot() )
)

output$eps.sample.variance = downloadHandler (
  filename = function() paste0("smpvar.", Sys.Date(), ".eps"),
  content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 100, plot = sample.variance.plot() )
)