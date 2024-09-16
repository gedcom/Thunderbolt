helpbox = function(tooltiptext = "") {
  out = div(style = "width: 30px; height: 30px; margin: 5px; border: 1px solid rgba(0,0,0,1);
      background-color: #444444; display: flex; float: right; justify-content: center; align-items: center; color: white;
      font-size: 1.5em; font-weight: bold", "?", title = tooltiptext)
  
  return(out)
}

######################
### USER INTERFACE ###
######################
library("DT")
library("networkD3")
library("shinyjs")
library("plotly")
library("shinycssloaders")
library("magrittr")

do.serverside = TRUE

source("uiextensions.R", local = TRUE)
source("interactome/interactome.ui.R", local = TRUE) # Changed from local = FALSE
source("pathway/pathway.ui.R", local = TRUE) # Changed from local = FALSE
source("fileupload/fileupload.ui.R", local = TRUE) # Changed from local = FALSE

if (do.serverside)
{
  source("fileupload.main/fileupload.main.ui.R", local = TRUE)
  source("compare/compare.ui.R", local = TRUE)
  source("search/search.ui.R", local = TRUE)
}

spintype = 4
spinColour = "#cccccc"
spinBackground = "#FFFFFF"

ui = navbarPage(includeCSS("www/progress.css"), windowTitle = "Thunderbolt", id = "tb.nav", position = "static-top", ### Dataset selector and design ###
  tabPanel("Welcome",
    shinyjs::useShinyjs(),
    div(id = "loading-content", style = paste(sep = "; ",
      "position: absolute",
      "display: table",
      "z-index: 100000",
      "padding:0",
      "margin:0",
      "top:0",
      "left:0","width:100%",
      "height: 100%",
      "background: #000000"),
      img(src = 'Thunderbolt_loading_black.png', style = "position: absolute; margin: auto; left: 0; right: 0; top: 0; bottom: 0")
      # div(
      #   style = "position: relative; margin: auto; left: 0; right: 0; top: 0; bottom: 0; color: white", h2("Loading...")
      # )
    ),
    img(src = 'Thunderbolt_logo.png', style = "position: absolute; margin: auto; left: 0; right: 0; top: 0; bottom: 0"),
    absolutePanel(top = "80%", bottom = "25%", left = "25%", right = "25%",
      div(style = "text-align: center",
        "To begin, upload or restore a dataset from the ", actionLink("goto.fileman", label = "file management"), " tab.", tags$br(), tags$br(),
        "Alternatively, load and automatically analyse an ", actionLink("loadDEexample", "example dataset"), " to see the Differential Expression Analysis pipeline in action."
      )
    )
  ),

  # if (do.serverside)
  # {
  # tagList(
  compare.ui,
  search.ui,
  # )
  # },

  tabPanel("File management",
    fluidRow(
      fileupload.main.ui
    )
  ),
  navbarMenu("DE Pipeline",
    tabPanel("Getting started",
      absolutePanel(top = "25%", bottom = "25%", left = "25%", right = "25%",
        div(style = "text-align: center",
          "The Differential Expression Analysis Pipeline allows you explore your data and to perform DE analysis on experimental groups within a dataset.", tags$br(), tags$br(),
          "To begin, upload or restore a dataset from the ", actionLink("goto.fileman", label = "file management"), " tab.", tags$br(), tags$br(),
          "Alternatively, load and automatically analyse an ", actionLink("loadDEexample", "example dataset"), " to see the pipeline in action."
        )
      )
    ),
    tabPanel("Experimental design",
      fluidRow(
        tags$h2("Experimental design"),
        column(width = 9,
          div(id = "varcreate.instructions",
            p("Thunderbolt needs to know about the experimental design for this dataset to properly group sample data. This design is comprised of ", strong("experimental variables"), " and ", strong("conditions"), "."),
            tags$h4(strong("Experimental variables")),
            p("Aspects of the experiment which were varied. Examples may include:"),
            tags$ul(
              tags$li("Diet"),
              tags$li("Drug treatment"),
              tags$li("IP bait")
            ),
            tags$h4(strong("Conditions")),
            p("Different perturbations used for each experimental variables, including controls. Examples may include:"),
            tags$ul(
              tags$li("For ", strong("diet"), ": chow, HFD (high-fat diet)"),
              tags$li("For ", strong("treatment"), ": vehicle, drugX1nM, drugX10nM, drugX100nM, drugY1nM, drugY10nM, drugY100nM"),
              tags$li("For ", strong("IP bait"), ": control, AKAP1")
            ),
            tags$br(),
            p("To begin inputting conditions:"),
            tags$ol(
              tags$li("To the right, enter the name of the ONE variable associated with the condition[s] to be input. If there are multiple variables, you can add conditions for those later."),
              tags$li("Enter the name of a condition. Conditions can be entered one at a time or simultaneously. To enter multiple conditions simultaneously, separate with a comma or semi-colon."),
              tags$li("Optionally, enter a string of characters (or one per condition, separated by commas/semi-colons) in the “autofill” text box which appears in the column names in your data associated with the condition."),
              tags$li("Hit 'Add condition'. The variable and condition[s] will appear in a table."),
              tags$li("Repeat steps 1-4 as needed to add to this table. To add a new condition to an existing variable, leave the same variable name in the variable input box and enter your new condition. When satisfied, scroll down.")
            )
          ),
          hidden(
            DT::dataTableOutput("variabletable"),
            tags$br(),
            tags$br(),
            actionButton("delete.vt.row", "Delete selected variable/condition"), actionButton("clear.var", label = "Clear all")
          )
        ),
        column(width = 3,
          textInput("new.var.name", label = "Experimental variable (eg 'Diet')"),
          textInput("new.lev.name", label = "Conditions/treatments for this variable (eg 'Chow', 'HFD')"),
          textInput("new.lev.search", label = "Column header string for condition (optional)"),
          actionButton("add.lev", "Add condition"),
          hidden(div(id = "lev.search.error", style = "color: red;", "List of conditions and list of autofill strings must be same length!"))
        )
      ),
      conditionalPanel("output['variables.created'] == 'TRUE'",
        tags$hr(),
        tags$h2("Annotate columns"),
        fluidRow(
          column(width = 9,
            p("Mark columns from your dataset (listed below) as descriptive data (such as Protein ID, Gene name, description etc.) or numeric data (protein abundance). You only need to mark columns you want included in your analysis.
              If every numeric data column has a common string of characters in its column name, you may automatically mark them by entering them in the text box to the right."),
            p("Next, assign a set of conditions to each numeric column. When finished, hit 'Apply design' below the table. Thunderbolt will assign columns to unique groups for the analysis. Then, proceed to the next page.")
            ),
          column(width = 3,
            textInput("auto.numeric.string", label = "Mark all columns containing this string as numeric (optional):", value = "")
          )
        ),
        tags$br(),
        uiOutput("variablesummary"),
        span(uiOutput("draftvariablesummary"), style= "color:gray"),
        uiOutput("des.mat.int"),
        div(
          style = "height: 100px"
        )
        ),
      fixedPanel(right = 1, bottom = 1, draggable = TRUE, width = 300,
        wellPanel(
          actionButton("save.design", "Apply design and continue", width = "100%"),
          tags$br(),
          tags$h4("Pipeline navigation"),
          tags$strong("1. Experimental Design"), tags$br(),
          conditionalPanel("output['datasetexists'] == 'true'",
            actionLink("goto.preproc1", label = "2. Exploration and preprocessing"), tags$br(),
            actionLink("goto.defineDE1", "3. Define and Run Analysis"), tags$br()
          ),
          conditionalPanel("output['deresultsexist'] == 'true'",
            actionLink("goto.DEresults1", "4. DE Results")
          )
        )
      )
  ),
    tabPanel("Data exploration and pre-processing",
      conditionalPanel("output['datasetexists'] == 'true'",
        fluidRow(
          sidebarLayout(
            sidebarPanel(width = 3,
              fluidRow(
                column(width = 10, tags$h3("Pre-processing")),
                column(width = 2,
                  helpbox("In this panel, you may select choose to apply various operations to the dataset including log transformation, filtering, normalisation and imputation. Any changes made to the dataset are reflected in the diagnostic plots to the right. After the dataset has been pre-processed to your satisfaction, you may proceed to DE Analysis in the above menu.")
                )
              ),
              tags$hr(),
              fluidRow(
                column(width = 10, 
                  checkboxInput('chk.log.values', 'Take log values', F)
                ),
                column(width = 2,
                  helpbox("Take log values: Applies a log2 transform to all numerical values in the dataset. The data must approximate a normal distribute for linear statistical analysis; this typically involves working with log fold-changes instead of raw fold-changes.")
                )
              ),
              tags$hr(),
              fluidRow(
                column(width = 10, tags$h4("Filtering")),
                column(width = 2,
                  helpbox("You may filter the data in two ways:
                    1. Retain only proteins/rows where selected groups contain at least a selected number of observations (i.e. non-missing values). If a statistical comparison is made under DE analysis without sufficient values present, this may produce an error (but see Imputation below).
                    2. Filter % of highest SD rows: This allows you to discard a percentage of the noisiest (i.e. highest-variance) rows in the dataset. Note that the x% noisiest rows in each group are separately marked for removal from the entire dataset."
                  )
                )
              ),
              checkboxInput("filter.empty.rows", label = "Filter empty rows", value = TRUE),
              numericInput("filter.minvals", label = "Filter: retain proteins with at least", value = 0, min = 0, step = 1),
              selectizeInput("sel.filter.groups", label = "values in at least one of these groups (or in all groups):", choices = NULL, multiple = T),
              textOutput("rowfilter.text"),
              tags$hr(),
              numericInput("flt.high.sd.perc", label = "Filter % of highest SD rows", value = 0, min = 0, max = 99, step = 0.1),
              textOutput("filtsd.text"),
              tags$hr(),
              tags$h4("Batch effect removal"),
              checkboxInput("chk.rem.batch", label = "Remove batch effect"),
              checkboxInput("chk.batch.knn", label = "kNN-impute for batch removal"),
              actionButton("btnSetBatches", label = "Set Batches"),
              tags$hr(),
              tags$h4("Imputation and normalization"),
              helpbox("You may apply median normalisation or quantile normalisation across all samples. If imputation is being carried out, you may opt to normalise the data either before or after imputation."),
              radioButtons("pre.norm", label = "Pre-imputation normalization",
                choices = c("None", "Median", "Quantile"), selected = "None", inline = TRUE),
              helpbox("Imputation: If your dataset contains missing values, You may impute/estimate these values prior to analysis to allow fewer proteins to be filtered. This may be done in one of two ways:
                1. For a given missing value, k-nearest neighbours finds the most similar rows in the dataset and takes the average of those rows from the same sample.
                2. Random Tail Imputation (RTI) draws values randomly from a normal distribution, typically at the lower end of the observed dataset. As this is random, the results of DE analysis for single imputation depend on the specific values generated. You may opt to use Multiple Imputation: this imputes the dataset multiple times. DE analysis will be performed on all imputed datasets and the results averaged to find robustly significant proteins."),
              selectInput("imputemethod", label = "Imputation method (if any):", choices = c("None" = "none", "k Nearest Neighbours" = "knn", "Random Tail Imputation" = "rti")),
              conditionalPanel("input['imputemethod'] != 'none'",
                selectizeInput("sel.impute.groups", label = "Impute ALL missing in groups:", choices = NULL, multiple = T),
                div("and/or", style = "text-align: center"),
                tags$br(),
                selectizeInput("sel.impute.groups.sparse", label = "Impute ONLY rows in groups", choices = NULL, multiple = T),
                numericInput("sel.impute.min.obs", label = "with fewer than how many observations", min = 0, step = 1, value = 1)
              ),
              conditionalPanel("input['imputemethod'] == 'rti'",
                sliderInput("num.imputations", label = "How many imputations for DE analysis?", min = 1, max = 100, value = 1, step = 1),
                checkboxInput("stackRTIdp", label = "Stack histogram", value = TRUE),
                plotlyOutput("RTI.dp") %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground),
                sliderInput("RTI.shift", label = "Shift", min = -3, max = 0, value = -1.8, step = 0.05),
                sliderInput("RTI.width", label = "Width", min = 0, max = 5, value = 0.3, step = 0.05)
              ),
              radioButtons("post.norm", label = "Post-imputation normalization",
                choices = c("None", "Median", "Quantile"), selected = "None", inline = TRUE),
              tags$hr(),
              tags$h4("Download processed dataset"),
              downloadButton("btnDownloadProcessed", label = "Download"), " as ",
              selectInput("processed.savetype", label = "", 
                choices = c("excel spreadsheet" = "xls",
                  "comma-separated values" = "csv",
                  "tab-separated values" = "tsv",
                  "RData file" = "rdata")
              )
              ),
            mainPanel(
              # Expression plot
              
              fluidRow(
                column(width = 11,
                  tags$h3("Individual protein expression")
                ),
                column(width = 1,
                  helpbox("You may use this interactive plot to display values for selected proteins. Firstly, select the descriptive columns (such as Uniprot ID or Gene Names) you wish to search for specific proteins. Then, under 'for value[s]:' select those values pertaining to the desired protein. A bar/scatter plot will appear showing the mean group intensities (or log-intensities) and individual values for each sample. Selecting a descriptive column from 'Label by column[s]' will label the plot using data from the selected volumn. You may view raw filtered values or values adjusted by normalization and imputation.")
                )
              ),
              fluidRow(
                inline(
                  selectizeInput("expplot.searchcols", label="Search column[s]:", choices = NULL, multiple = T),
                  selectizeInput('expplot.searchdesc', label="for value[s]:", choices = NULL, multiple=T),
                  selectizeInput("expplot.labelcol", label="Label by column[s]", choices = NULL, multiple = T)
                )
              ),
              fluidRow(
                radioButtons("show.raw.adj", label = "Show raw or adjusted data in expression plot?", choices = c("Raw", "Adjusted"), inline = TRUE)                
              ),
              conditionalPanel(condition = "output['expplotexists'] == 'true'",
                fluidRow(
                  plotlyOutput("expressionplotly") %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground) 
                ),
                fluidRow(
                  div(style = "float: right", downloadButton("save.expression", label = "Save as PNG")),
                  div(style = "float: right", downloadButton("eps.expression", label = "Save as EPS"))
                ) 
              ), tags$hr(),
              
              fluidRow(
                column(width = 11,
                  tags$h3("Missingness per sample")
                ),
                column(width = 1,
                  helpbox("This plot displays the % of rows containing missing values for each sample.")
                )
              ),
              fluidRow(
                plotlyOutput("missing.barplot") %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground)
              ),
              fluidRow(
                div(style = "float: right", downloadButton("save.missing.barplot", label = "Save as PNG")),
                div(style = "float: right", downloadButton("eps.missing.barplot", label = "Save as EPS"))
              ),
              tags$hr(),
              
              fluidRow(
                column(width = 11,
                  tags$h3("Sample boxplots")
                ),
                column(width = 1,
                  helpbox("This boxplot displays the distribution of values for each sample in your dataset. Assuming a relatively small number of regulated proteins and comparable missingness, these should appear similar across the dataset.")
                )
              ),
              fluidRow(
                plotlyOutput("pre.boxplot") %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground)
              ),
              fluidRow(
                div(style = "float: right", downloadButton("save.boxplot", label = "Save as PNG")),
                div(style = "float: right", downloadButton("eps.boxplot", label = "Save as EPS"))
              ),
              tags$hr(),
              
              fluidRow(
                column(width = 11,
                  tags$h3("Hierarchical clustering")
                ),
                column(width = 1,
                  helpbox("This is a dendrogram showing degree of similarity between samples based on their intensity values. Samples which are more similar cluster together in this plot.")
                )
              ),
              fluidRow(
                plotOutput("dendrogram") %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground)
              ),
              fluidRow(
                div(style = "float: right", downloadButton("save.dendrogram", label = "Save as PNG")),
                div(style = "float: right", downloadButton("eps.dendrogram", label = "Save as EPS"))
              ), tags$hr(),
              
              fluidRow(
                column(width = 11,
                  tags$h3("Mean row variance by group")
                ),
                column(width = 1,
                  helpbox("This plot displays the average within-row variance for each group.")
                ),
                fluidRow(
                  plotlyOutput("group.variance.plot") %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground)
                ),
                fluidRow(
                  div(style = "float: right", downloadButton("save.group.variance", label = "Save as PNG")),
                  div(style = "float: right", downloadButton("eps.group.variance", label = "Save as EPS"))
                )
              ), tags$hr(),
              
              fluidRow(
                column(width = 11,
                  tags$h3("Variance by sample")
                ),
                column(width = 1,
                  helpbox("This plot displays the variance for each sample in the dataset.")
                )
              ),
              fluidRow(
                plotlyOutput("sample.variance.plot") %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground)
              ),
              fluidRow(
                div(style = "float: right", downloadButton("save.sample.variance", label = "Save as PNG")),
                div(style = "float: right", downloadButton("eps.sample.variance", label = "Save as EPS"))
              ), tags$hr(),
              
              
              fluidRow(
                column(width = 11,
                  tags$h3("PCA")
                ),
                column(width = 1,
                  helpbox("Principal component analysis applies a transformation to the complete dataset (i.e. all rows without missing values) to show a simplified representation of the data in two dimensions. Each point in this plot represents a sample. Samples that are more similar to one another appear closer together; ideally, points should cluster by experimental group. Axis titles display the % contribution of each principle component to total variation in the dataset.")
                )
              ),
              fluidRow(
                sliderInput("dim1", "X axis principal component", min = 1, max = 5, value = 1, step = 1), sliderInput("dim2", "Y axis principal component", min = 1, max = 5, value = 2, step = 1),
                plotlyOutput("PCA") %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground)                
              ),
              fluidRow(
                div(style = "float: right", downloadButton("save.PCA", label = "Save as PNG")),
                div(style = "float: right", downloadButton("eps.PCA", label = "Save as EPS"))
              ), tags$hr(),
              
              
              fluidRow(
                column(width = 11,
                  tags$h3("Data heatmap")
                ),
                column(width = 1,
                  helpbox("This heatmap orders rows according to mean intensity or the number of missing values they contain. Missing values are displayed in black. This provides an idea as to the relationship between missingness and intensity in the dataset and shows whether values are similar across rows; normalisation techniques which improve boxplot homogeneity but which reduce similarity within rows are likely to be introducing noise to the dataset.")
                )
              ),
              fluidRow(
                radioButtons("rad.heatmap.order", label = "Order by:", choices = c("row means" = "mean", "No. missing values" = "missing"), selected = "mean", inline = T),
                # plotlyOutput("pre.heatmap")  %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground)
                plotOutput("pre.heatmap")  %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground)
              ),
              fluidRow(
                div(style = "float: right", downloadButton("save.heatmap", label = "Save as PNG")),
                div(style = "float: right", downloadButton("eps.heatmap", label = "Save as EPS"))
              ), tags$hr(),
              
              fluidRow(
                column(width = 11,
                  tags$h3("Intensity vs missingness")
                ),
                column(width = 1,
                  helpbox("This plot displays the relationship between mean intensity of rows and % values for each group. In many mass spectrometry datasets, missingness decreases with intensity due to the failure of MS to identify or quantify low intensity peaks.")
                )
              ),
              fluidRow(
                plotlyOutput("intvsmiss") %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground)
              ),
              fluidRow(
                div(style = "float: right", downloadButton("save.intvsmiss", label = "Save as PNG")),
                div(style = "float: right", downloadButton("eps.intvsmiss", label = "Save as EPS"))
              ), tags$hr(),
              
              fluidRow(
                column(width = 11,
                  tags$h3("SVD plot")
                ),
                column(width = 1,
                  helpbox("This plot applies Singular Value Decomposition to the dataset to find sources of structure, with greater contributors appearing in higher rows. Samples are shown across the X axis. Ideally, samples within the same group should exhibit similar values on the Y axis. Residuals are displayed in the column on the right.")
                )
              ),
              fluidRow(
                actionButton("eigenMSButton", label = "Run SVD plot"),
                hidden(
                  plotOutput("eigenplot")# %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground)
                )
              )
            )
                )
        ),
        div(
          style = "height: 50px"
        ),
        fixedPanel(right = 1, bottom = 1, draggable = TRUE, width = 300,
          wellPanel(
            tags$h4("Pipeline navigation"),
            actionLink("goto.expdes2", label = "1. Experimental Design"), tags$br(),
            tags$strong("2. Exploration and preprocessing"), tags$br(),
            actionLink("goto.defineDE2", "3. Define and Run Analysis"), tags$br(),
            conditionalPanel("output['deresultsexist'] == 'true'",
              actionLink("goto.DEresults2", "4. DE Results")
            )
          )
        )
    ),
      conditionalPanel("output['datasetexists'] == 'false'",
        "Please select 'Open dataset' to begin differential expression analysis."
      )
    ),
    tabPanel("Define and Run Analysis",
      conditionalPanel("output['datasetexists'] == 'true'",
        fluidRow(
          column(width = 4,
            tags$h2("Enter contrasts"), tags$br(),
            helpbox(
              "You must enter contrasts to indicate the statistical comparisons you wish to make between groups. Available groups are displayed under 'Groups' at the right. To enter a contrast, first enter a name (your choice) and then a contrast formula which can be parsed by the R differential expression analysis package Limma. Examples include:
              
              HFD.Basal - Chow.Basal // Tests intensities under high fat diet compared to chow diet under basal conditions
              (HFD.Insulin - HFD.Basal) - (Chow.Insulin - Chow.Basal) // Compares insulin-induced fold change under high fat diet to the same stimulation under a chow diet.
              Timepoint1 // Compares all rows at Timepoint1 to a raw value of 1 (log value of 0). This is appropriate e.g. for datasets produced by SILAC, where values are ratios over a negative control.
              
              Click 'Add' to include a contrast you've entered; after you've entered any number of contrasts, click 'Analyse!', scroll down and wait for the results of your analyses."),
            "e.g. 'HFD.Basal - Chow.Basal'", tags$br(),
            "e.g. '(HFD.Insulin - Chow.Insulin) - (HFD.Basal - Chow.Basal)'", tags$br(), tags$br(),
            selectInput("contrast.dropdown", label = "Select a contrast or enter manually:", choices = c()),
            textInput("contrast.name", label = "Name:"), tags$br(),
            textInput("contrast.input", "Formula:"),
            actionButton("addcontbutton", "Add")
            ),
          column(width = 4,
            tags$h2("Current contrasts"), tags$br(),
            dataTableOutput("current.contrasts"),
            conditionalPanel("output['displaycontrasts'] == 'TRUE'",
              tags$br(),
              actionButton("delcontsbtn", "Delete selected"),
              actionButton("clearcontbutton", "Clear"),
              "To exclude groups from a contrast, select a contrast and deselect the groups you want to exclude."
            )
          ),
          column(width = 4,
            tags$h2("Groups:"), tags$br(),
            dataTableOutput("grouplist"), tags$br()
          )
          ),
        fixedPanel(right = 1, bottom = 1, draggable = TRUE, width = 300,
          wellPanel(
            actionButton("doanalysisbutton", "Analyse!"), tags$br(),
            tags$h4("Pipeline navigation"),
            actionLink("goto.expdes3", label = "1. Experimental Design"), tags$br(),
            actionLink("goto.preproc3", "2. Exploration and preprocessing"), tags$br(),
            tags$strong("3. Define and Run Analysis"), tags$br(),
            conditionalPanel("output['deresultsexist'] == 'true'",
              actionLink("goto.DEresults3", "4. DE Results")
            )
          )
        )
        )
      ),
    tabPanel("DE Results",
      conditionalPanel("output['datasetexists'] == 'true'",
        tags$h2("Results"),
        fluidRow(
          column(width = 4,
            tags$h3("Select contrast"),
            selectInput("sel.plot.contrast", label = "", choices = NULL, selected = NULL)
          ),
          column(width = 8,
            tags$table(width = "80%", style = "table-layout: fixed",
              tags$tr(
                tags$td(tags$h3("Summary"), width = "25%"),
                tags$td(tags$h4("Up", style = "color: red; text-align: right"), width = "25%"),
                tags$td(tags$h4("Down", style = "color: blue; text-align: right"), width = "25%"),
                tags$td(tags$h4("Both", style = "color: #551a8b; text-align: right"), width = "25%")
              ),
              tags$tr(
                tags$td(tags$h4("Significant")),
                tags$td(tags$h4(textOutput("deresults.upsignum"), style = "color: red; text-align: right")),
                tags$td(tags$h4(textOutput("deresults.downsignum"), style = "color: blue; text-align: right")),
                tags$td(tags$h4(textOutput("deresults.bothsignum"), style = "color: #551a8b; text-align: right"))
              ),
              tags$tr(
                tags$td(tags$h4("Excluded")),
                tags$td(tags$h4(textOutput("deresults.upexnum"), style = "color: #FF5555; text-align: right")),
                tags$td(tags$h4(textOutput("deresults.downexnum"), style = "color: #5555FF; text-align: right")),
                tags$td(tags$h4(textOutput("deresults.bothexnum"), style = "color: #AA6AE0; text-align: right"))
              )
            )
          )
        ),
        fluidRow(
          column(width = 4,
            fluidRow(
              column(width = 10, tags$h3("Significance criteria")),
              column(width = 2,
                helpbox(
                  "These controls allow you to set the criteria for a protein to be considered significant. These include:
                  
                  - p-value cutoff: p-values must be below this threshold for a protein to be considered significant.
                  - log-fold change cutoff and direction: the minimum magnitude and direction of the change.
                  - Normal or adjusted p-values: whether to apply Benjamini-Hochberg p-value adjustment before determining determining significance."
                )
                )
                ),
            
            numericInput("num.sig.max.p", label = "Select p-value cutoff", min = 0, max = 0.1, value = 0.05, step = 0.0001),
            radioButtons("rad.logfc.comp", label = "Select log-fc direction", choices = c("Up" = "greater", "Down" = "less", "Both" = "both" ), selected = "both", inline = T),
            sliderInput("sld.sig.min.fc", label = "Select log-fc cutoff", min = 0, max = 5, value = 1, step = 0.25),
            radioButtons("norm.adj.p", label = "Normal or adjusted p-values", choices = c("Normal" = "p.value", "Adjusted" = "adj.p.val"), selected = "adj.p.val"),
            tags$h3("Remember"),
            inline(
              actionButton("btn.save.siglist", label = "Remember criteria"),
              " with name: ",
              textInput("siglist.name", label = "")
            ),
            inline("Excluding hits from: ",
              selectizeInput("sel.exclude.siglists", label = "", multiple = TRUE, choices = NULL)
            ), tags$br(),
            textOutput("save.siglist.msg"),
            conditionalPanel("output['displaysigtable'] == 'TRUE'",
              dataTableOutput("siglist.datatable"),
              tags$br(),
              actionButton("delsiglistsbtn", "Delete selected"),
              actionButton("clearsiglistsbtn", "Clear all"),
              actionButton("unionsiglistsbtn", "Union selected"),
              actionButton("insectsiglistbtn", "Intersect selected")
              
              #                 tags$br(),
              #                 tags$h3("Save significant proteins as protein list"),
              #                 textInput("sig2pl.name", "Enter a short name", value = ""),
              #                 textInput("sig2pl.owner", label = "Enter a new owner or select an existing owner below."),
              #                 selectInput('sig2pl.owner.list', label="Select an existing owner", choices = NULL, multiple = FALSE),
              #                 actionButton("sig2pl.save.btn", "Save to server")
            )
                ),
          column(width = 8,
            tags$h3("Global results plots"),
            fluidRow(
              column(width = 6,
                plotlyOutput("t.dist") %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground),
                div(style = "text-align: center",
                  downloadButton("t.dist.png", label = "Save PNG"),
                  downloadButton("t.dist.eps", label = "Save EPS") 
                )
              ),
              
              column(width = 6, 
                plotlyOutput("p.dist") %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground),
                div(style = "text-align: center",
                  downloadButton("p.dist.png", label = "Save PNG"),
                  downloadButton("p.dist.eps", label = "Save EPS")
                )
              )
            ),
            fluidRow(
              column(width = 6,
                plotlyOutput("volcano.plot") %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground),
                div(style = "text-align: center",
                  downloadButton("v.plot.png", label = "Save PNG"),
                  downloadButton("v.plot.eps", label = "Save EPS")
                )
              ),
              
              column(width = 6,
                plotlyOutput("p.zoom") %>% withSpinner(type = spintype, color = spinColour, color.background = spinBackground),
                div(style = "text-align: center",
                  downloadButton("p.zoom.png", label = "Save PNG"),
                  downloadButton("p.zoom.eps", label = "Save EPS")
                )
              )
            )
          )
              ),
        tags$hr(), ##################################################################
        div(style = "float: right",
          checkboxInput("disp.sig.only", "Display and download only significant rows", value = FALSE)
        ),
        dataTableOutput("toptable"),
        inline(style = "vertical-align: middle",
          downloadButton("downloadresults", label = "Download results"), " as ",
          selectInput("results.savetype", label = "", 
            choices = c("excel spreadsheet" = "xls",
              "comma-separated values" = "csv",
              "tab-separated values" = "tsv",
              "RData file" = "rdata")
          )
        ),
        fixedPanel(right = 1, bottom = 1, draggable = TRUE, width = 300,
          wellPanel(
            tags$h4("Pipeline navigation"),
            actionLink("goto.expdes4", label = "1. Experimental Design"), tags$br(),
            actionLink("goto.preproc4", "2. Exploration and preprocessing"), tags$br(),
            actionLink("goto.defineDE4", "3. Define and Run Analysis"), tags$br(),
            tags$strong("4. DE Results")
          )
        )
          ),
      conditionalPanel("output['datasetexists'] == 'false'",
        "Please select 'Open dataset' to begin differential expression analysis."
      )
      )
    ),
  tabPanel("Functional enrichment analysis",
    pathwayanalysis.ui("pathwaytest")
  ),
  tabPanel("Network analysis",
    interactome.ui("interactest")
  )
)

shinyUI(
    ui
)