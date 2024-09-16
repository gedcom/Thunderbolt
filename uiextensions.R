inline = function(...)
{
  args = list(...)
  argnames = names(args)
  named.idx = if (length(argnames) == 0)
  {
    rep(FALSE, length(args))
  } else {
    nzchar(argnames)
  }
  properties = args[named.idx]
  elements = args[!named.idx]
  
  if ("style" %in% names(properties))
  {
    properties$style = paste(properties$style, "display:inline-block", sep = "; ")
  } else {
    properties = c(properties, list(style = "display:inline-block"))
  }
  
  out = lapply(elements, function(elem) do.call(div, c(list(elem), properties)))
  out = tagList(out)
  
  return(out)
}
