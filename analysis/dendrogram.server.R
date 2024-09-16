dendrogram = reactive ({
  dat = final.data()
  sample.groups = datrv$sample.groups
  group.colours = datrv$group.colours
  
  dist.mat = dist(t(dat))
  names(sample.groups) = names(dat)
  hc = as.dendrogram(hclust(dist.mat, method="ward.D2"))
  ddata = dendro_data(hc)
  
  ymax = max(ddata$segments$y) + 10
  
  ddata$labels$colour = factor(sample.groups[match(x = ddata$labels$label, table = names(sample.groups))], levels = datrv$group.names)
  
  p <- ggplot(segment(ddata)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
      axis.line = element_line(colour = "black"), legend.position="right", legend.key=element_rect(fill=NA),
      axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    guides(colour = guide_legend(override.aes = list(size = 10))) +
    ylab("Distance") + xlab("Sample") +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
    coord_flip(ylim = c(ymax, -30)) + 
    geom_text(data = ddata$labels, aes(x = x, y = y, label = label, colour = colour), size = 5, hjust = 0, vjust = 0.5) +
    scale_y_reverse(expand = c(0.2, 0)) + 
    scale_colour_manual(name = "Groups", values = group.colours, labels = names(group.colours))
  
  p
})

output$dendrogram = renderPlot(dendrogram())

output$save.dendrogram = downloadHandler (
  filename = function() paste0("dendrogram.", Sys.Date(), ".png"),
  content = function(fn) {
    png(fn, width = 900, height = 450)
    plot(dendrogram())
    dev.off()
  }
)

output$eps.dendrogram = downloadHandler (
  filename = function() paste0("dendrogram.", Sys.Date(), ".eps"),
  content = function(fn) {
    postscript(fn, width = 900, height = 450)
    plot(dendrogram())
    dev.off()
  }
)