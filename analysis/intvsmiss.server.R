intvsmiss = reactive ({
  dat.all = final.data()
  
  row.means = apply(dat.all, 1, mean, na.rm = T)
  ord = order(row.means, na.last = NA)
  
  group.dats = lapply(datrv$group.names, function(grp) {
    samples = datrv$sample.groups == grp
    out = as.matrix(dat.all[ord, samples])
    return(out)
  })
  
  names(group.dats) = datrv$group.names
  
  plot.groups = lapply(datrv$group.names, function(grp) {
    group.dat = group.dats[[grp]]
    
    bins = list()
    bin.start = 1
    for (i in 1:nrow(group.dat))
    {
      bin.cells = as.vector(group.dat[bin.start:i,])
      cum.missing = sum(is.na(bin.cells))
      cum.values = length(bin.cells) - cum.missing
      if ((cum.missing >= 30 && cum.values >= 30) || length(bin.cells) >= nrow(group.dat)*ncol(group.dat) / 50)
      {
        bins = c(bins, list(bin.cells))
        
        bin.start = i + 1
      } else if (i == nrow(group.dat)) {
        bins = c(bins, list(bin.cells))
      }
    }
    
    grp.out = data.frame(Mean.intensity = sapply(bins, mean, na.rm = T)[-(1:5)],
      Mean.missing.values = sapply(bins, function(bin) sum(is.na(bin)) / length(bin))[-(1:5)],
      Group = factor(grp, levels = datrv$group.names),
      stringsAsFactors = FALSE)
    
    return(grp.out)
  })
  
  # plot.groups
  
  fill.colours = datrv$group.colours
  plot.data = do.call(rbind, plot.groups)
  names(plot.data) = c("Mean.intensity", "Mean.missing.values", "Group")
  plot.data = na.omit(plot.data)
  plot.data = plot.data[order(plot.data$Mean.intensity),] # Ensures plotted lines move from right to left
  
  p <- ggplot(plot.data, aes(x = Mean.intensity, y = Mean.missing.values, colour = Group)) +
    geom_line() +
    scale_colour_manual(values = fill.colours) +
    theme(panel.background = element_rect(fill = "white", colour = "black"), legend.position = "bottom") +
    xlab("Mean intensity") +
    ylab("% missing values")
  
  p
})

output$intvsmiss = renderPlotly ({
  
  p = ggplotly(intvsmiss())
  p = config(p, displayModeBar = FALSE)
  p = config(p, showLink = FALSE)
  
  for(i in seq_along(p$x$data))
  {
    p$x$data[[i]]$text = paste0(
      "Group: ", p$x$data[[i]]$name, "<br>",
      "Mean intensity: ", p$x$data[[i]]$x, "<br>",
      "% missing values: ", p$x$data[[i]]$y
    )
  }
  
  p
  
})

output$save.intvsmiss = downloadHandler (
  filename = function() paste0("intvsmiss.", Sys.Date(), ".png"),
  content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 300, plot = intvsmiss() )
)

output$eps.intvsmiss = downloadHandler (
  filename = function() paste0("intvsmiss", Sys.Date(), ".eps"),
  content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 300, plot = intvsmiss() )

)