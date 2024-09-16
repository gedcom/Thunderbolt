compare.table.tooltiptext =  
"Each row represents a protein contained in at least one of the selected lists.

EntrezID: The EntrezID of the protein.
in # lists: The number of selected lists in which this protein appears in some form.

Each additional column represents one of the selected lists and displays the label by
which the protein is known in that list (for example, the gene name or Uniprot ID)."

compare.ui = tabPanel("Compare lists",
  sidebarLayout(
    sidebarPanel(
      helpbox("This module allows you to compare lists of significant proteins (or pathways output from the functional enrichment analysis module) that have been saved to the server. This is done by selecting the type of comparison you want to perform (protein or pathway), selecting the lists you want to compare and clicking 'Compare'. You may compare columns from your protein lists that have been marked as miscellaneous, but Thunderbolt cannot perform any interspecies or format conversions using these data - they must match exactly.

You may choose to display a heatmap or Venn diagram showing overlap between the selected lists, or a table showing each protein or pathway and whether it is present in each list."),
      radioButtons("comp.prot.path", label = "List type", choices = c("Protein" = "all.protlists", "Pathway" = "all.pathlists", "Compare misc columns (exact match)" = "misc")),
      selectizeInput("comp.sel.owners", label = "Specify owners (optional)", choices = NULL, selected = NULL, multiple = TRUE),
      selectizeInput("comp.sel.protlists", label = "Lists to compare", choices = NULL, selected = NULL, multiple = TRUE),
      inline(
        actionButton("comp.docompare", label = "Compare"),
        actionButton("comp.clear.protlists", label = "Clear") 
      ), 
      tags$br(), tags$br(),
      selectInput("comp.sel.display", label = "Select output type",
        choices = c("Overlap heatmap" = "overlap", "Table" = "table", "Venn diagram" = "venn"), selected = "overlap", multiple = FALSE)
    ),
    mainPanel(
      conditionalPanel(condition = "input['comp.sel.display'] == 'table'",
        helpbox(compare.table.tooltiptext),
        DT::dataTableOutput("comp.table"),
        inline(style = "vertical-align: middle",
          downloadButton("comp.download.table", label = "Download table"), " as ",
          selectInput("comp.download.type", label = "", 
            choices = c("excel spreadsheet" = "xls",
              "comma-separated values" = "csv",
              "tab-separated values" = "tsv",
              "RData file" = "rdata")
          )
        )
      ),
      conditionalPanel(condition = "input['comp.sel.display'] == 'overlap'",
        helpbox("This heatmap shows, for all pairs of selected lists, the number of proteins present in both lists (proportionally or absolute)."),
        radioButtons("comp.count.prop", label = "", choices = c("Count" = "count", "Proportion" = "prop")),
        d3heatmap::d3heatmapOutput("overlap.heatmap")
      ),
      conditionalPanel(condition = "input['comp.sel.display'] == 'venn'",
        helpbox("This Venn diagram displays a circle representing each selected list. The numbers displayed indicate the number of proteins or pathways in each overlap (or belonging to only one list)."),
        plotOutput("comp.venn")
      )
    )
  )
)