pre.heatmap = function () {
  dat.array = as.matrix(final.data())
  
  require(ggplot2)
  require(gplots)
  require(reshape2)
  require(scales)
  
  num.rows = min(1500, nrow(dat.array))
  row.ord = switch(input$rad.heatmap.order,
    "mean" = {
      row.means = apply(dat.array, 1, mean, na.rm = TRUE)
      row.means[!is.finite(row.means)] = NA
      order(row.means, decreasing = FALSE, na.last = TRUE)
    },
    "missing" = {
      order(-apply(dat.array, 1, function(x) sum(is.na(x))))
    }
  )
  
  mult = floor(x = nrow(dat.array) / num.rows)
  hm.rows = row.ord[1:num.rows * mult]
  hm.data = dat.array[hm.rows,]
  rownames(hm.data) = hm.rows
  
  rownames(dat.array) = 1:nrow(dat.array)
  hm.data = melt(dat.array[hm.rows,])
  colnames(hm.data) = c("hm.row", "hm.col", "value")
  
  #Rescale to a flat distribution between 0 and 1
  rows.w.values = which(!is.na(hm.data$value))
  hm.data[rows.w.values, "value"] = rescale(rank(hm.data[rows.w.values,"value"]))
  
  hm.data[,1] = factor(hm.data[,1], levels = unique(as.character(hm.data[,1])))
  
  (p <- ggplot(hm.data, aes(hm.col, hm.row)) +
      geom_tile(aes(fill = value)) +
      scale_fill_gradient(low = "blue", high = "red", na.value = "black", guide = "colourbar", name = "Global intensity ranking", breaks = c(0, 1), label = c("Minimum", "Maximum")) +
      xlab("Samples") + ylab("Proteins ordered by mean intensity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, colour = datrv$sample.colours),
        axis.ticks = element_blank(), axis.text.y = element_blank()))
}

# output$pre.heatmap = renderPlotly ({
#   p = ggplotly(pre.heatmap(), tooltip = "none")
#   p = config(p, displayModeBar = FALSE)
#   p = config(p, showLink = FALSE)
#   
#   p
# })

output$pre.heatmap = renderPlot ({
  pre.heatmap()
})

output$save.heatmap = downloadHandler (
  filename = function() paste0("heatmap.", Sys.Date(), ".png"),
  content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 100, plot = pre.heatmap() )
)

output$eps.heatmap = downloadHandler (
  filename = function() paste0("heatmap.", Sys.Date(), ".eps"),
  content = function(fn) ggsave(filename = fn, width = 8, height = 4, dpi = 100, plot = pre.heatmap() )
)