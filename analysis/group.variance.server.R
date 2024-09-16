group.variance.plot = reactive ({
  
  grp.dat = lapply(datrv$group.names, function(grp) {
    return(final.data()[,datrv$sample.groups == grp])
  })
  
  group.mn.var = sapply(grp.dat, function(grp) {
    row.var = apply(grp, 1, var, na.rm = TRUE)
    
    return(mean(row.var, na.rm = TRUE))
  }) 
  
  fill.colours = datrv$group.colours
  plot.data = data.frame(Group = factor(datrv$group.names, levels = datrv$group.names), Variance = group.mn.var)
  
  
  p <- ggplot(data=plot.data, aes(x = Group, y = Variance, fill = Group)) +
    geom_bar(colour="black", stat="identity") +
    scale_fill_manual(values = fill.colours) +
    theme(panel.background = element_rect(fill = "white", colour = "black"), legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  return(p)
})

output$group.variance.plot = renderPlotly({
  
  p = ggplotly(group.variance.plot())
  p = config(p, displayModeBar = FALSE)
  p = config(p, showLink = FALSE)
  
  num.groups = length(datrv$group.names)
  
  for(i in seq(num.groups))
  {
    p$x$data[[i]]$text = paste0(
      "Group: ", p$x$data[[i]]$name, "<br>",
      "Variance: ", p$x$data[[i]]$y
    )
  }
  
  p
  
})

output$save.group.variance = downloadHandler (
  filename = function() paste0("grpvar.", Sys.Date(), ".png"),
  content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 100, plot = group.variance.plot(), device = "png")
)

output$eps.group.variance = downloadHandler (
  filename = function() paste0("grpvar.", Sys.Date(), ".eps"),
  content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 100, plot = group.variance.plot(), device = "eps")
)