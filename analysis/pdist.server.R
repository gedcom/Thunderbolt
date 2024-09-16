p.dist = reactive({
  validate(need(de.results(), label = "DE analysis"))
  
  contrast = input$sel.plot.contrast
  gen.log.seq <- function(base = 10, bins = 10)
    sort(1 - log(seq(from = 1, to = base, by = (base - 1) / bins), base))
  p.vals = na.omit(as.vector(de.results()[[contrast]][[input$norm.adj.p]]))
  
  p.sig.max = input$num.sig.max.p
  if(is.na(p.sig.max)) p.sig.max = 0.05
  p.vals = data.frame("p" = p.vals)
  x.label = if(input$norm.adj.p == "P.Value") "P value" else "Adjusted P Value"
  
  breakpoints = c(0, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  gp = ggplot(p.vals, aes(x = p), fill = "lightgrey", colour = "black", stroke = 1.5) +
    geom_histogram(aes(y = ..density..), breaks = breakpoints) + 
    xlab(x.label) +
    theme(panel.background = element_rect(fill = "white", colour = "black"), legend.position = "none")
  
  p.sig.max = input$num.sig.max.p
  if (!is.na(p.sig.max)) gp = gp + geom_vline(xintercept = input$num.sig.max.p, colour = "red", linetype = "dashed")
  gp
})


output$p.dist = renderPlotly({
  p = ggplotly(p.dist(), tooltip = NULL) %>%
    config(displayModeBar = FALSE) %>%
    config(showLink = FALSE)
  p
})

output$p.dist.png = downloadHandler (
  filename = function() paste0("p.dist.", Sys.Date(), ".png"),
  content = function(fn) ggsave(filename = fn, width = 6, height = 6, dpi = 100, plot = p.dist())
)

output$p.dist.eps = downloadHandler (
  filename = function() paste0("p.dist.", Sys.Date(), ".eps"),
  content = function(fn) ggsave(filename = fn, width = 6, height = 6, dpi = 100, plot = p.dist())
)