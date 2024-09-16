pre.boxplot = function() {
  fill.colours = datrv$group.colours  
  plot.data = melt(data = final.data(), na.rm = TRUE)
  plot.data$Group = factor(datrv$sample.groups[match(plot.data$variable, colnames(final.data()))], levels = datrv$group.names)
  
  p <- ggplot(data=plot.data, aes(x = variable, y = value, fill = Group)) +
    geom_boxplot() +
    scale_fill_manual(values = fill.colours) +
    theme(panel.background = element_rect(fill = "white", colour = "black"), legend.position = "right") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

output$pre.boxplot = renderPlotly({
  
  p = ggplotly(pre.boxplot())
  p = config(p, displayModeBar = FALSE)
  p = config(p, showLink = FALSE)
  
  p
})

# output$save.boxplot = downloadHandler (
#   filename = function() paste0("boxplot.", Sys.Date(), ".png"),
#   content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 100, plot = pre.boxplot() )
# )
# 
# output$eps.boxplot = downloadHandler (
#   filename = function() paste0("boxplot.", Sys.Date(), ".eps"),
#   content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 100, plot = pre.boxplot() )
# )