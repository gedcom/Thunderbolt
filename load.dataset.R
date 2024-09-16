# Find all tables in a complex R tree structure
get.leaves = function(tree, classes = "any", data.frame.as.list = F)
{
  leaves = list()
  
  if (data.frame.as.list)
  {
    permitted.tree.classes = c("list", "data.frame")
  } else{
    permitted.tree.classes = "list"
  }
  
  if ("list" %in% classes)
  {
    stop("'list' not a valid argument to classes")
  } else if ("data.frame" %in% classes && data.frame.as.list) {
    stop("'data.frame' not a valid argument to classes if data.frame.as.list is true")
  } else if (!class(tree) %in% permitted.tree.classes) {
    stop("argument to 'tree' is not of a permitted class ('list' only or 'data.frame' if data.frame.as.list is true)")
  }
  
  while(length(tree) > 0)
  {
    class.vec = sapply(tree, class)
    mode.vec = sapply(tree, mode)
    list.inds = which(mode.vec == "list" & (class.vec != "data.frame" | data.frame.as.list))
    new.tree = unlist(tree[list.inds], recursive = FALSE)
    
    all.leaves = tree[-list.inds]
    
    if ("any" %in% classes)
    {
      new.leaves = all.leaves
    } else {
      new.leaves = tree[class.vec %in% classes]
    }
    
    leaves = c(leaves, new.leaves)
    tree = new.tree
  }
  
  return(leaves)
}

# Load dataset

load.dataset = function(filepath, filetype = "auto", delimiter = switch(filetype, "tsv" = "\t", "csv" = ",", " "))
{
  switch(filetype,
    "rdata" = {
      file.env = new.env()
      load(filepath, envir = file.env, verbose = FALSE)
      
      out = as.list(file.env)
      rm(file.env)
      
      out = get.leaves(out, classes = c("data.frame", "matrix"))
      out = lapply(out, as.data.frame)
      
      return(out)
    },
    ""
  )
}