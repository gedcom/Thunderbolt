library("readxl")

get.leaves = function(tree, classes = "any", data.frame.as.list = F)
{
  leaves = list()
  
  if (data.frame.as.list)
  {
    permitted.tree.classes = c("list", "data.frame")
  } else {
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

#################################
### CREATE FILE UPLOAD MODULE ###
#################################
fileupload.module = function(name = "",
                             uploader = paste0(name, "uploader"),
                             typesel = paste0(name, "typesel"),
                             delimiter = paste0(name, "delimiter"),
                             tablesel = paste0(name, "tablesel"),
                             header = paste0(name, "header"),
                             transpose = paste0(name, "transpose"))
{
  rct.list = list()
  
  rct.list$fileinput = reactive ({
    input[[uploader]]
  })
  
  # Read data
  rct.list$up.file.type = reactive ({

    if (isTruthy(rct.list$fileinput())) {
      filetype = input[[typesel]]
      
      if(filetype == "auto") {
        filename = rct.list$fileinput()$name
        filename.vec = strsplit(filename, split = ".", fixed = TRUE)[[1]]
        filetype = tolower(filename.vec[length(filename.vec)])
        if(filetype == "txt") filetype = "tsv"
        if(filetype == "xls") filetype = "xlsx"
      }
      
      filetype
      
    } else {
      NULL
    }
  })
  
  rct.list$file.upload1 = reactive ({
    if (isTruthy(rct.list$fileinput()))
    {
      filetype = rct.list$up.file.type()
      
      updateSelectInput(session, tablesel, choices = c())
      
      # Excel file
      case.excel = function() {
        require(readxl)
        sheets = excel_sheets(rct.list$fileinput()$datapath)
        
        out = list(path = rct.list$fileinput()$datapath, sheetnames = names(sheets))
        
        return(out)
      }
      
      # ASCII file with delimiter (CSV, TSV, etc)
      case.dlm = function(dlm) {
        return(list(read.table(rct.list$fileinput()$datapath,
          sep = dlm, stringsAsFactors = FALSE,
          header = input[[header]])))
      }
      
      # RData file
      case.rdata = function() {
        file.env = new.env()
        load(rct.list$fileinput()$datapath, envir = file.env, verbose = FALSE)
        
        out = as.list(file.env)
        rm(file.env)
        
        out = get.leaves(out, classes = c("data.frame", "matrix"))
        out = lapply(out, as.data.frame)
        
        return(out)
      }
      
      switch(filetype,
        "xlsx" = case.excel(),
        "csv" = case.dlm(","),
        "tsv" = case.dlm("\t"),
        "select" = case.dlm(input[[delimiter]]),
        "rdata" = case.rdata()
      )
    } else {
      NULL
    }
  })
  
  # Update table/sheet options for excel/RData files
  # Responds to upload events
  rct.list$updatetableselect = observeEvent (input[[uploader]], {
    if (!is.null(input[[uploader]]))
    {
      filetype = rct.list$up.file.type()
      
      fileupload = rct.list$file.upload1()
      
      switch(EXPR = filetype,
        "xlsx" = {
          updateSelectInput(session, tablesel, choices = fileupload$sheetnames, label = "Choose a worksheet")
          shinyjs::show(tablesel, anim = TRUE, animType = "slide")
        },
        "rdata" = {
          updateSelectInput(session, tablesel, choices = names(fileupload), label = "Choose an R object")
          shinyjs::show(tablesel, anim = TRUE, animType = "slide")
        },
        {
          shinyjs::hide(tablesel, anim = TRUE, animType = "slide")
        }
      )
    }
  })
  
  # Process data as required by options
  rct.list$file.final = reactive ({
    objfile = rct.list$file.upload1()
    filetype = rct.list$up.file.type()
    
    if (isTruthy(objfile))
    {
      out = switch(filetype,
        "xlsx" = { # Uploaded file is an excel file
          sheet = read_excel(objfile$path, seq(numsheets), col_names = input[[header]], na = c("", "NA", "NaN", "Inf", "-Inf"))
        },
        
        "rdata" = { # Uploaded file is an RData file
          objfile[[ input[[tablesel]] ]]
        },
        
        # Default (anything aside from excel or RData)
        {
          objfile[[1]]
        }
      )
      
      data.frame(out, stringsAsFactors = FALSE) 
    } else {
      NULL
    }
  })
  
  return(rct.list)
}