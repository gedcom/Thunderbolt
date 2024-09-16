require("shinyjs")

fileupload.ui = function(name = "",
                          uploader = paste0(name, "uploader"),
                          typesel = paste0(name, "typesel"),
                          tablesel = paste0(name, "tablesel"),
                          header = paste0(name, "header"),
                          transpose = paste0(name, "transpose"),
                          sheetsfilled = paste0(name, "sheetsfilled"))
{
  out = tagList(fileInput(uploader, 'Choose file to upload',
            accept = c(
              '.xls',
              '.xlsx',
              '.txt',
              '.csv',
              '.tsv',
              '.RData'
            )
  ),
  
  tags$hr(),
  
  ############
  
  selectInput (typesel, label = "Select a filetype",
               choices = c("Detect automatically" = "auto",
                           "Excel spreadsheet" = "xlsx",
                           "Tab-separated" = "tsv",
                           "Comma-separated" = "csv",
                           "RData file" = "rdata",
                           "Custom delimiter" = "select"
               )
  ),
  conditionalPanel(paste0("input['", typesel, "'] == 'select'"),
                   textInput("customDelimiter", label = "Enter a custom delimiter:", value = " ")
  ),
  shinyjs::hidden(
    selectInput(tablesel, label = "",
      choices = NULL, multiple = FALSE)
  ),
  checkboxInput(header, 'Header row', TRUE),
  checkboxInput(transpose, 'Transpose', FALSE))
  
  return(out)
}