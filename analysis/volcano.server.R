volcano.plot = function ()
{
  
  validate(need(de.results(), label = "DE analysis"))
  
  contrast = input$sel.plot.contrast
  
  x = de.results()[[contrast]]$coefficients
  p = de.results()[[contrast]][[input$norm.adj.p]]
  y = -log10(p)
  
  siglist = current.siglist()
  sig.pts = unlist(siglist$sig.rows)
  excluded.pts = unlist(siglist$excluded.rows)
  
  fills = rep("Not significant", length(p))
  fills[sig.grps()$Up] = "Up"
  fills[sig.grps()$Down] = "Down"  
  fills[sig.grps()$Upex] = "Up (excluded)"
  fills[sig.grps()$Downex] = "Down (excluded)"
  
  fillspresent = sapply(sig.grps(), any)
  
  fills = factor(fills, levels = c("Not significant", "Up", "Down", "Up (excluded)", "Down (excluded)")[fillspresent])
  fill.colours = c("darkgrey", "red", "blue", "pink", "lightblue")[fillspresent]
  
  outlines = rep(NA, length(p))
  outlines[c(sig.pts, excluded.pts)] = "white"
  
  biggest.lfc = max(abs(x[sig.pts]))
  sizes = rep(1, length(p))
  sizes[sig.pts] = 5*abs(x[sig.pts])/biggest.lfc + 1
  sizes[excluded.pts] = 1
  
  dat = data.frame(x = x, y = y, Direction = fills)
  dat = data.frame(rv$analysis.desc, dat, stringsAsFactors = FALSE)
  
  vplot = ggplot(dat, aes(x = x, y = y, fill = Direction, colour = "white")) +
    geom_point(shape = 21, size = sizes, stroke = 0.3) +
    scale_fill_manual(values = fill.colours) +
    scale_colour_manual(values = c(white = "white", red = "red")) +
    theme(panel.background = element_rect(fill = "white", colour = "black"), legend.position = "none") +
    xlab("Log2 fold change") +
    ylab("-Log10 p-value")
  
  if (!is.na(siglist$lfc.comp))
  {
    vplot = vplot + geom_hline(yintercept=-log10(siglist$p.cutoff), colour = "red", linetype = "longdash")
    if (siglist$lfc.comp == "greater" || siglist$lfc.comp == "both")
      vplot = vplot + geom_vline(xintercept = siglist$lfc.cutoff, colour = "red", linetype = "longdash")
    if ((siglist$lfc.comp == "both" && siglist$lfc.cutoff > 0) || siglist$lfc.comp == "less")
      vplot = vplot + geom_vline(xintercept = -siglist$lfc.cutoff, colour = "red", linetype = "longdash")
  }
  
  vplot
}

output$volcano.plot = renderPlotly ({
  p = ggplotly(volcano.plot())
  p = config(p, displayModeBar = FALSE)
  p = config(p, showLink = FALSE)
  
  contrast = input$sel.plot.contrast
  logfc = de.results()[[contrast]]$coefficients
  pval = de.results()[[contrast]][[input$norm.adj.p]]
  desc.names = colnames(rv$analysis.desc)
  
  pstring = switch(input$norm.adj.p,
    "p.value" = "P value",
    "adj.p.value" = "Adjusted P value"
  )
  
  desc.data = as.matrix(rv$analysis.desc)
  descstrings = apply(desc.data, 1, function(descrow) paste0(desc.names, ": ", descrow, "<br>", collapse = ""))
  hoverstrings = paste0(
    descstrings,
    "Fold change: ", signif(2^logfc,3), "<br>",
    "Log2 fold change: ", signif(logfc,3), "<br>",
    "P value: ", signif(pval,3), "<br>"
  )
  
  inc.sig.grps = sig.grps()
  inc.sig.grps = inc.sig.grps[sapply(inc.sig.grps, any)]
  
  for(i in seq_along(inc.sig.grps))
  {
    p$x$data[[i]]$text = hoverstrings[ inc.sig.grps[[i]] ]
  }
  p
})

output$v.plot.png = downloadHandler (
  filename = function() paste0("volcano.plot.", Sys.Date(), ".png"),
  content = function(fn) {
    ggsave(fn, plot = volcano.plot(), width = 4, height = 4, dpi = 200)
    volcano.plot()
  }
)

output$v.plot.eps = downloadHandler (
  filename = function() paste0("volcano.plot.", Sys.Date(), ".eps"),
  content = function(fn) {
    ggsave(fn, plot = volcano.plot(), width = 4, height = 4, dpi = 200)
    volcano.plot()
  }
)