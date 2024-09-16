t.dist = reactive ({
    validate(need(de.results(), label = "DE analysis"))
    contrast = input$sel.plot.contrast
    breaknum = 101
    
    
    tt = data.frame(t = as.vector(de.results()[[contrast]]$t))
    x.label = "T-statistic"
    
    p = ggplot(tt, aes(x = t), fill = "lightgrey", colour = "black", stroke = 1.5) +
    geom_histogram(aes(y = ..density..), bins = breaknum) +
    xlab(x.label) +
    theme(panel.background = element_rect(fill = "white", colour = "black"), legend.position = "none")
    
    return(p)
})

output$t.dist = renderPlotly ({
  p = ggplotly(t.dist(), tooltip = NULL) %>%
  config(displayModeBar = FALSE) %>%
  config(showLink = FALSE)
  p
})


output$t.dist.png = downloadHandler (
  filename = function() paste0("t.dist.", Sys.Date(), ".png"),
  content = function(fn) ggsave(filename = fn, width = 6, height = 6, dpi = 100, plot = t.dist())
)

output$t.dist.eps = downloadHandler (
  filename = function() paste0("t.dist.", Sys.Date(), ".eps"),
  content = function(fn) ggsave(filename = fn, width = 6, height = 6, dpi = 100, plot = t.dist())
)