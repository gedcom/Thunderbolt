missing.barplot = reactive ({
  validate(need(final.data(), label = "Numeric data"))
  fill.colours = datrv$group.colours
  
  # Get missing value percentages
  mv.percs = apply(final.data(), 2, function (x) {
    100 * sum(is.na(x)) / length(x)
  })
  
  sample.names = factor(colnames(final.data()), levels = colnames(final.data()))  
  plot.data = data.frame(Sample = sample.names, Group = factor(datrv$sample.groups, levels = datrv$group.names), Missing = mv.percs, stringsAsFactors = FALSE)
  
  p <- ggplot(data=plot.data, aes(x = Sample, y = Missing, fill = Group)) +
    geom_bar(colour="black", stat="identity") +
    scale_fill_manual(values = fill.colours) +
    theme(panel.background = element_rect(fill = "white", colour = "black"), legend.position = "right") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("% Missing")
  
  return(p)
})

output$missing.barplot = renderPlotly({
  
  p = ggplotly(missing.barplot())
  p = config(p, displayModeBar = FALSE)
  p = config(p, showLink = FALSE)
  
  for(i in seq_along(p$x$data))
  {
    p$x$data[[i]]$text = paste0(
      "Sample: ", colnames(final.data())[p$x$data[[i]]$x], "<br>",
      "Group: ", p$x$data[[i]]$name, "<br>",
      "% Missing: ", p$x$data[[i]]$y
    )
  }
  
  p
})


output$save.missing.barplot = downloadHandler (
  filename = function() paste0("missing.", Sys.Date(), ".png"),
  content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 100, plot = missing.barplot())
)

output$eps.missing.barplot = downloadHandler (
  filename = function() paste0("missing.", Sys.Date(), ".eps"),
  content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 100, plot = missing.barplot())
)