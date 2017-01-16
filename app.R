# install missing pacakges if needed
list.of.packages <- c("shiny", "shinyBS","shinyjs","SPARQL","data.table","DT","stringr","plyr","rstudioapi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#####FOR DEVonly
#.libPaths("C:/Rstudio/Library")

library(shiny)
library(shinyBS)
library(shinyjs)
library(SPARQL)
library(data.table)
library(DT)
library(stringr)
library(plyr)
library(rstudioapi)

# set a working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Source hyperlink function and directories
#source('ontology_links.R')
sapply(list.files(pattern="[.]R$", path="var/", full.names=TRUE), source)
sapply(list.files(pattern="[.]R$", path="query/", full.names=TRUE), source)

# Point to js file
jsfile <- "www/jsfile.js"
if (interactive()) {
  # UI ---------------------------------
  ui <- fluidPage(
    useShinyjs(),
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$title("SAPP query"),
      #tags$script(HTML(jscode)),
      tags$script(src = 'jsfile.js'),
      tags$link(rel = "stylesheet", type = "text/css", href = "sapp.css")
    ),
    tags$div(class = "Header",
             tags$div(
               class = "banner",
               tags$img(
                 src = "CIGENE.png",
                 width = 'auto',
                 height = "50px"
               ), 
               tags$img(
                 src = "DS.png",
                 width = 'auto',
                 style = 'position:relative;left:50px;',
                 height = "50px"
               )
             )),
    navbarPage( 
      "SAPP Query",
      # Reaction NavTab =================================
      tabPanel(
        "Reaction",
        ### Reaction Search feild ##########################
        tags$div(class = "content ",
                 tags$div(class = "container setwidth",
                          fluidRow(
                            column(4,
                                   tags$form(
                                     class = "form-inline",
                                     tags$div(
                                       class = "form-group",
                                       tagAppendAttributes(
                                         textInput(
                                           "variable",
                                           "EC number query",
                                           width = "125px",
                                           value = "5.4.2.11",
                                           placeholder = NULL
                                         ),
                                         `data-proxy-click` = "submit"
                                       ),
                                       actionButton("submit", "submit")
                                     )
                                   )),
                            column(6,
                                   selectInput("select", label = ("Select a species"), 
                                               choices = list(
                                                 "Salmon Salar" = 'http://10.209.0.227:8030/blazegraph/namespace/SalmoDB/sparql', 
                                                 "Zebrafish" = 'http://10.209.0.227:7955/blazegraph/namespace/ZebraDB/sparql' ), 
                                               selected = 'http://10.209.0.227:8030/blazegraph/namespace/SalmoDB/sparql')
                                   
                            )
                          ))),
        tags$br(),
        # Reaction DataTables #############################
        tags$div(class = "content",
                 tags$div(class = "container tables setwidth",
                          fluidRow(
                            column(
                              12,
                              mainPanel(
                                width = 12,
                                style = "height=100%;width:100%",
                                textOutput("exampleOutput"),
                                bsAlert("alert"),
                                tabsetPanel(
                                  # Reaction SAPP Tab =================================
                                  tabPanel("SAPP",
                                           tags$div(class = "info",
                                                    tags$ul(
                                                      tags$li(""),
                                                      tags$li("")
                                                    )),
                                           DT::dataTableOutput('myTable')
                                  ),
                                  # Reaction BLAST TAB =================================
                                  tabPanel("BLAST",
                                           tags$div(class = "info",
                                                    tags$ul(
                                                      tags$li(textOutput("tool")),
                                                      tags$li(textOutput("version"))
                                                    )),
                                           
                                           DT::dataTableOutput('swissprot_table')
                                           
                                  ),
                                  # Reaction PRIAM Tab =================================
                                  tabPanel("PRIAM",
                                           tags$div(class = "info",
                                                    tags$ul(tags$li(textOutput("tool_priam")),
                                                            tags$li(textOutput("version_priam")))), 
                                           DT::dataTableOutput('priamdata_table')
                                           
                                  ),
                                  # Reaction Interpro Tab =================================
                                  tabPanel("Interpro",
                                           tags$div(class = "info",
                                                    tags$ul(tags$li(textOutput("tool_interpro")),
                                                            tags$li(textOutput("version_interpro")))), 
                                           DT::dataTableOutput('interprodata_table')
                                  ),

                                  # Reaction EnzDP Tab =================================
                                  tabPanel("EnzDP",
                                           tags$div(class = "info",
                                                    tags$ul(
                                                      tags$li(textOutput("tool_enzdp")),
                                                      tags$li(textOutput("version_enzdp"))
                                                    )),
                                           DT::dataTableOutput('enzdp_table')
                                  ),
                                  # Reaction Result Summary =================================
                                  tabPanel("Result Summary",
                                           tags$div(class = "info",
                                                    tags$ul(
                                                      tags$li(""),
                                                      tags$li("")
                                                    )),
                                           DT::dataTableOutput('resultsummarydata_table')
                                  ),
                                  # Reaction Manual Anotation =================================
                                  tabPanel("Manual Annotation",
                                           DT::dataTableOutput('ma_table'),
                                           tags$div(class="ma",
                                                    tags$div(class = "container",style ="height:650px",
                                                             fluidRow(
                                                               ### Input feilds --------------------------------
                                                               column(3,
                                                                      mainPanel(width=3,
                                                                                h4("Insert Data"),
                                                                                tags$form(
                                                                                  class = "form-inline",
                                                                                  tags$div(
                                                                                    class = "form-group",
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "author",
                                                                                        "Author",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "date",
                                                                                        "Date",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "comment",
                                                                                        "comment",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "gene",
                                                                                        "gene",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "protein",
                                                                                        "Protein",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "reaction",
                                                                                        "Reaction",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "goterm",
                                                                                        "Goterm",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "doi",
                                                                                        "Doi",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "url",
                                                                                        "URL",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      `data-proxy-click` = "ma_submit",
                                                                                      actionButton("ma_submit", "Submit")  
                                                                                    )
                                                                                    
                                                                                  )# tags div end
                                                                                ))),#tags from and columns ends
                                                               # Reaction Database view -----------------------------
                                                               column (8,
                                                                       mainPanel(style="margin-left:15px;padding-top:10px;",
                                                                                 p("Open the database view the latest annotations"),
                                                                                 actionButton("opendb","Open"),
                                                                                 actionButton("closedb","Close"),
                                                                                 #textOutput('updatequery'),
                                                                                 tags$hr(),
                                                                                 DT::dataTableOutput('contents')
                                                                       #textOutput('test')
                                                                       
                                                                       ) # mainPanel 
                                                                       )

                                           ) # Fluid row ends
                                  ) # div container ends
                                ) # div info ends
                                ), # tabPanel
                                tabPanel("Federated query",
                                       actionButton("fd","Submit"),
                                       DT::dataTableOutput('federated_table')
                                       )
                                ) # tabset ends
                              ) # main panael ends
                            )
                          )
                 )
        )
      ),
      # Protein NavTab =================================
      tabPanel(
        "Protein",
        # Search Feild ###########################
        tags$div(class = "content",
                 tags$div(class = "container setwidth",
                          fluidRow(
                            column(4,
                                   tags$form(
                                     class = "form-inline",
                                     tags$div(
                                       class = "form-group",
                                       tagAppendAttributes(
                                         textInput(
                                           "variableprot",
                                           "NP number query",
                                           width = "125px",
                                           value = "NP_001130025.1",
                                           placeholder = NULL
                                         ),
                                         `data-proxy-click` = "submitprot"
                                       ),
                                       actionButton("submitprot", "submit")
                                     )
                                   )),
                            column(6,
                                   selectInput("select", label = ("Select a species"), 
                                               choices = list(
                                                 "Salmon Salar" = 'http://10.209.0.227:8030/blazegraph/namespace/SalmoDB/sparql', 
                                                 "Zebrafish" = 'http://10.209.0.227:7955/blazegraph/namespace/ZebraDB/sparql' ), 
                                               selected = 'http://10.209.0.227:8030/blazegraph/namespace/SalmoDB/sparql')
                                   
                            )
                          ))),
        tags$br(),
        # DataTables #############################
        tags$div(class = "content",
                 tags$div(class = "container tables setwidth",
                          fluidRow(
                            column(
                              12,
                              mainPanel(
                                width = 12,
                                style = "height=100%;width:100%",
                                textOutput("exampleOutput2"),
                                bsAlert("alert2"),
                                tabsetPanel(
                                  # SAPP Tab =================================
                                  tabPanel("SAPP",
                                           tags$div(class = "info",
                                                    tags$ul(
                                                      tags$li(""),
                                                      tags$li("")
                                                    )),
                                           DT::dataTableOutput('myTableprot')
                                  ),
                                  # BLAST TAB =================================
                                  tabPanel("BLAST",
                                           tags$div(class = "info",
                                                    tags$ul(
                                                      tags$li(textOutput("tool_blast")),
                                                      tags$li(textOutput("version_blast"))
                                                    )),
                                           DT::dataTableOutput('blastresult_table')
                                  ),

                                  # PRIAM =================================
                                  tabPanel("PRIAM",
                                           tags$div(class = "info",
                                                    tags$ul(
                                                      tags$li(textOutput("tool_priam_prot")),
                                                      tags$li(textOutput("version_priam_prot"))
                                                    )),
                                           DT::dataTableOutput('priamprot_table')
                                  ),
                                  # SIGNAL IP Tab =================================
                                  tabPanel("SIGNALP",
                                           tags$div(class = "info",
                                                    tags$ul(
                                                      tags$li(textOutput("tool_signalIP")),
                                                      tags$li(textOutput("version_signalIP"))
                                                    )),
                                           
                                           DT::dataTableOutput('signalIP_table')
                                  ),
                                  # Interpro Tab =================================
                                  tabPanel("Interpro",
                                           tags$div(class = "info",
                                                    tags$ul(
                                                      tags$li(textOutput("tool_interpro_prot")),
                                                      tags$li(textOutput("version_interpro_prot"))
                                                    )),
                                           DT::dataTableOutput('ipr_table')
                                  ),
                                  # Interpro Domains Tab =================================
                                  tabPanel("Interpro Domains ",
                                           tags$div(class = "info",
                                                    tags$ul(
                                                      tags$li(""),
                                                      tags$li("")
                                                    )),
                                           DT::dataTableOutput('interpro_table')
                                  ),
                                  # Manual Anotation =================================
                                  tabPanel("Manual Annotation",
                                           DT::dataTableOutput('ma_table_prot'),
                                           tags$div(class="ma",
                                                    tags$div(class = "container",style ="height:650px",
                                                             fluidRow(
                                                               column(3,
                                                                      # Input feilds -----------------------------------
                                                                      mainPanel(width=3,
                                                                                h4("Insert Data"),
                                                                                tags$form(
                                                                                  class = "form-inline",
                                                                                  tags$div(
                                                                                    class = "form-group",
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "author",
                                                                                        "Author",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "date",
                                                                                        "Date",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "comment",
                                                                                        "comment",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "gene",
                                                                                        "gene",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "protein",
                                                                                        "Protein",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "reaction",
                                                                                        "Reaction",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "goterm",
                                                                                        "Goterm",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "doi",
                                                                                        "Doi",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      class ="input_feilds",
                                                                                      textInput(
                                                                                        "url",
                                                                                        "URL",
                                                                                        width = "125px",
                                                                                        value = "",
                                                                                        placeholder = NULL
                                                                                      )
                                                                                    ),
                                                                                    tagAppendAttributes(
                                                                                      `data-proxy-click` = "ma_submit_prot", # Change this
                                                                                      actionButton("ma_submit_prot", "Submit")  
                                                                                    )
                                                                                    
                                                                                  )# tags div end
                                                                                ))),#tags from and columns ends
                                                               # Open Annotation database #########################
                                                             mainPanel(style="margin-left:15px;padding-top:10px;",
                                                                       p("Open database view the latest annotations"),
                                                                       actionButton("opendb_prot","Open"),
                                                                       actionButton("closedb_prot","close"),
                                                                       textOutput('updatequery_prot'),
                                                                       tags$hr(),
                                                                       DT::dataTableOutput('contents_prot')
                                                             )
                                                    ) # Fluid row ends
                                           ) # div container ends
                                  ) # div tab panel
                                  )
                                ) 
                              ) 
                            ) 
                          ) # Fluid Row ends
                 ))
      ) # TabsetPanel ends

    ) # Navbar page
  ) # FluidPAge
  # Server ---------------------------------
  server <- function(input,output,session){
    
    # Database endpoint
    endpoint <- isolate(input$select)
    #endpoint <- 'http://10.209.0.227:8030/blazegraph/namespace/SalmoDB/sparql'
    output$value <- renderPrint({ input$select })

    # SAPP Reaction ============================
    Sappdata <- observeEvent(input$submit,{
      
      ### Clear all data.frames #############################
      results <- NULL;results_interpro <- NULL;results_priam <- NULL;results_uniprot <- NULL;swiss_table <- NULL;enzdp_table <- NULL;results_enzdp<-NULL;
      fetch_query <- NULL
      
      ### Variable and js scripts addCLass #############################
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      # Render EC number input and render to text
      ECnumber <- input$variable
      output$text <- renderText({ECnumber})
      
      ### Manual Annotation Blazegraph query
      # ECnumber <- '5.4.2.11'
      # endpoint <- "http://10.209.0.227:8030/blazegraph/namespace/SalmoDB/sparql"
      
      # endpoint2 <- "http://localhost:9999/blazegraph/namespace/ManualAnno/sparql"
      endpoint2 <- "http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/sparql"
      
      maquery <- paste("
          prefix ma: <http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/>
          prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          prefix dc: <http://purl.org/dc/elements/1.1/>

          select ?ECnumber ?Author ?Date ?Comment ?Gene ?Protein ?GOterm ?Doi ?Url
          where{
          <ma:",ECnumber,"> <ma:reaction> ?ECnumber;
          			            <dc:creator> ?Author;
                            <dc:date> ?Date;
                            <dc:description> ?Comment;
                            <ma:gene> ?Gene;
                            <ma:protein> ?Protein;
                            <ma:goterm> ?GOterm;
                            <ma:doi> ?Doi;
                            <ma:url> ?Url
          }",sep="")

      fetch_query <- SPARQL(endpoint2,maquery)$results
      fetch_query <-as.data.table(fetch_query)
      
      if (empty(fetch_query) == TRUE){fetch_query <- noframe()}

      output$ma_table <- DT::renderDataTable({
        fetch_query
      })
      

      # Start the progression bar =================================
      withProgress(
        message = 'Fetching data',
        detail = 'This may take a few minutes',
        value = 0,
        {
          Sys.sleep(0.50)
          incProgress(0.1, detail = "Starting SAPP query")
          
          # Grab queries from source file
          prefixes_all <- source('query/prefixes_reactive.R')
          # Extract relevant data from the list.
          prefixes <- prefixes_all$value[1]
          basequery_interpro <- prefixes_all$value[2]
          basequery_priam <- prefixes_all$value[3]
          basequery_uniprot <- prefixes_all$value[4]
          basequery_enzdp <- prefixes_all$value[5]

          ### Query functions #############################
          queryfun <- function(basequery, ecnumber) { return(sub('4.2.1.11', ecnumber, basequery))  }
          rename_head <- function(x){rename(x,c("?header" = "header", "?colname" = "colname", "?value" = "value", "?feature" = "feature")) }
          clean_post <- function(data){data[, value := sub('"','',value, perl = TRUE)] ; data[, value := sub('".+>','',value, perl = TRUE)] }
          
          # sparql <- source(/var/sparql.R)
          
          #Test the query and rename columns
          results_interpro <- data.table(sparql(endpoint, paste(prefixes,queryfun(basequery_interpro, ECnumber))))
          results_interpro <- rename_head(results_interpro)
          results_interpro <- clean_post(results_interpro)
          
          results_priam <- data.table(sparql(endpoint, paste(prefixes,queryfun(basequery_priam, ECnumber))))
          results_priam <- rename_head(results_priam)
          results_priam <- clean_post(results_priam)
          
          results_uniprot <- data.table(sparql(endpoint, paste(prefixes,queryfun(basequery_uniprot, ECnumber))))
          results_uniprot <- rename_head(results_uniprot)
          results_uniprot <- clean_post(results_uniprot)
          
          results_enzdp <- data.table(sparql(endpoint, paste(prefixes,queryfun(basequery_enzdp, ECnumber))))
          results_enzdp <- rename_head(results_enzdp)
          results_enzdp <- clean_post(results_enzdp)

          incProgress(0.4, detail = "Fetching data")
          
          ### Check if dataframes are empty #############################
          if (empty(results_interpro) && empty(results_priam) && empty(results_uniprot) == TRUE) {
            output$exampleOutput <- renderText({
              createAlert(session,"alert","exampleAlert",title = "No Query Found",
                content = "The number you enterd is not a valid ECnumber or a EC number wich returned an
                empty results", append = FALSE)
            })
          }
          # Build dataframes =================================
          else{
            ### Rename columns #############################
            results <- data.table(ldply(.id = "tool",list(Interpro = results_interpro,Priam = results_priam, Blast = results_uniprot,Enzdp = results_enzdp )))
            
            incProgress(0.6, detail = "Building tables")
            
            # in order to get nice tables we replace them.
            results <- rename_reaction(results)
            
            ### BLAST #############################
            if ("Blast" %in% results$tool) {
              swiss_table <- dcast(results[tool == 'Blast'], ncbiprotein + feature ~ colname,fun.aggregate = paste,collapse = "__")
              
              # Grab names of tool and version to display in header
              swiss_tool <- toupper(swiss_table$tool[1])
              swiss_version <- swiss_table$version[1]
              
              # Send swiss_Table to a function that proceses the headers
              swiss_table <- blast_reaction_rename(swiss_table)
            } else {swiss_tool <- toupper("N/A"); swiss_version <- "N/A";swiss_table <- noframe()}
            
            ### PRIAM #############################
            if ("Priam" %in% results$tool) {
              priam_table <- dcast(results[tool == 'Priam'],ncbiprotein + feature ~ colname,fun.aggregate = paste,collapse = "__")
              
              # Save tool and version vales for displaying
              priam_tool <- toupper(priam_table$tool[1])
              priam_version <- priam_table$version[1]
              
              # Parse priam table
              priam_table <- priam_reaction_rename(priam_table)
            } else {priam_tool <- "N/A"; priam_version <- "N/A";priam_table <- noframe();}
            
            ### InterproScan #############################
            if ("Interpro" %in% results$tool) {
              interpro_table <-dcast(results[tool == 'Interpro'], ncbiprotein + feature ~ colname)
              
              # Grab tool and version varibles
              interpro_tool <- toupper(interpro_table$tool[1])
              interpro_version <- interpro_table$version[1]
              
              # Parse interpro_table
              interpro_table<-interpro_reaction_rename(interpro_table)
            } else {interpro_tool <- "N/A";interpro_version <- "N/A";interpro_table <- noframe();}
           
            ### EnzDP #############################
            if ("Enzdp" %in% results$tool) {
              
              enzdp_table <-dcast(results[tool == 'Enzdp'], ncbiprotein + feature ~ colname)
              enzdp_table[, likelihoodscore := as.numeric(likelihoodscore)]
              setorder(enzdp_table, -likelihoodscore)
              enzdp_table <- rename(enzdp_table, c("<http://www.biopax.org/release/bp-level3.owl#xref" = "ECurl"))
              
              # Grab the tool and version variables
              enzdp_tool <- toupper(enzdp_table$tool[1])
              enzdp_version <- enzdp_table$version[1]
              
              # Drop the tool and variable
              enzdp_table <- enzdp_table[,.(ncbiprotein,ECurl,likelihoodscore,maxbitscore)]
            } else {enzdp_tool <- "N/A"; enzdp_version <- "N/A"; enzdp_table <- noframe();}
            
            ### Comparing results across tools #############################
            results_summary <-
              rbind(results[tool == 'Priam' & colname == 'evalue', list('value' = value), by = c('Ncbiprotein','header', 'tool')],
                    results[tool == 'Enzdp' & colname == 'likelihoodscore', list('value' = value), by = c('Ncbiprotein','header', 'tool')],
                    results[tool %in% c('Blast','Interpro') & colname == 'tool', list('value' = .N), by = c('Ncbiprotein','header', 'tool')])
            results_summary <-dcast(results_summary,Ncbiprotein + header ~ tool,fill = NA,fun.aggregate = paste,collapse = " ")
            
            #map salmon ncbiproteinid to ncbigeneid
            if (file.exists('ncbilink.RData'))
              {load('ncbilink.RData')}
            else{
            #download Salmon gff from NCBI 
            gff_url <- 'ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF_000233375.1_ICSASG_v2/GCF_000233375.1_ICSASG_v2_genomic.gff.gz'
            gff_name <- 'GCF_000233375.1_ICSASG_v2_genomic.gff.gz'
            if (!file.exists(gff_name))
              #downlod using curl, for some reason the default method is slow in Rstudio but not plain R (both on orion cn5) 
              download.file(gff_url,gff_name,method='curl') 
            
            #process CDS lines from GFF file
            gff <- fread(paste('zgrep [[:space:]]CDS[[:space:]]',gff_name))
            gff <- gff[V3=='CDS']
            ncbigene <- sub('GeneID:','',str_match(gff$V9,'GeneID:[0-9]*'))
            ncbiname <- sub('gene=','',str_match(gff$V9,'gene=[a-z,A-Z,0-9]*'))
            ncbiprotein <- sub('protein_id=','',str_match(gff$V9,'protein_id=[N,X]P_[0-9]*\\.[0-9]*'))
            ncbilink <- data.table(ncbigene,ncbiprotein,ncbiname)
            setnames(ncbilink,c('ncbigene','ncbiprotein','ncbiname'))
            ncbilink <- unique(ncbilink[!is.na(ncbiprotein)])
            save(ncbilink,file='ncbilink.RData')
            }
            # Adjust types of columns if they are present
            try(results_summary[, Interpro := as.numeric(Interpro)])
            try(results_summary[, Blast := as.numeric(Blast)])
            try(results_summary[is.na(Interpro), Interpro := 0])
            try(results_summary[is.na(Blast), Blast := 0])
            
            results_summary <- merge(ncbilink,results_summary,by.y='Ncbiprotein',by.x='ncbiprotein',all.y=T)
            results_summary[,ncbiprotein:=NULL]
            try(setorder(results_summary, -Enzdp))
            
            ### Isolate the variables to be renderd #############################
            # Send the versions and tools name to header
            isolate ({
              output$tool <- renderText({ paste("Tool: ",swiss_tool)})
              output$version <- renderText({paste("Version: ",swiss_version) })
              output$tool_priam <- renderText({ paste("Tool: ",priam_tool)})
              output$version_priam <- renderText({paste("Version: ",priam_version) })
              output$tool_interpro <- renderText({ paste("Tool: ",interpro_tool)})
              output$version_interpro <- renderText({paste("Version: ",interpro_version) })
              output$tool_enzdp <- renderText({ paste("Tool: ",enzdp_tool)})
              output$version_enzdp <- renderText({paste("Version: ",enzdp_version) })
            })
            
            ### Start rendering the datatables #############################
            incProgress(0.6, detail = "Creating output data")
            output$myTable <- DT::renderDataTable(
              results,
              options = list(
                iDisplayLength = 10,
                scrollX = TRUE,
                fixedColumns = TRUE,
                rowCallback = JS(
                  "function(row, data){",
                  "var match = anchor(data[6]);",
                  "$('td:eq(6)', row).html(match)",
                  "}"
                )
              )
            )
            output$swissprot_table <- DT::renderDataTable(
              swiss_table,
              extensions = 'Responsive',
              options = list(
                iDisplayLength = 10,
                scrollX = TRUE,
                fixedColumns = TRUE,
                fnInitComplete = JS(
                  "function (settings, json){tool_header();}"),
                rowCallback = JS(
                  "function(row, data){",
                  "if(data[1] =='N/A'){}else{var b_match = anchor(data[1])}",
                  "$('td:eq(1)', row).html(b_match)",
                  "if(data[2]=='N/A' ){}else{var match2 = anchor(data[2])}",
                  "$('td:eq(2)', row).html(match2)",
                  "}"
                )
              )
            )
            output$priamdata_table <- DT::renderDataTable(
              priam_table,
              # extensions = 'Responsive', # This is for making the table clickable
              options = list(
                iDisplayLength = 10,
                scrollX = TRUE,
                fixedColumns = TRUE,
                # Add a tooltip to the header after table has finished rendering
                fnInitComplete = JS("function (settings, json){
                                    tool_header();
          }"),
                # Add a anchor tag to the table while rendering
                rowCallback = JS(
                  "function(row, data){",
                  "if(data[1] =='N/A'){}else{var match = anchor(data[1]);}",
                  "$('td:eq(1)', row).html(match);",
                  "if(data[2] =='N/A'){}else{var new_links = urls(data[2])}",
                  "$('td:eq(2)', row).html(new_links);",
                  "}"
                )
                )
            )
            output$interprodata_table <- DT::renderDataTable(
              interpro_table,
              options = list(
                iDisplayLength = 10,
                scrollX = TRUE,
                fixedColumns = TRUE,
                rowCallback = JS(
                  "function(row, data){",
                  "if(data[1] =='N/A'){}else{var match = anchor(data[1]);}",
                  "$('td:eq(1)', row).html(match)",
                  "if(data[1] =='N/A'){}else{var match2 = anchor(data[2]);}",
                  "$('td:eq(2)', row).html(match2)",
                  "}"
                )
              )
            )
            output$enzdp_table <- DT::renderDataTable(
              enzdp_table,
              options = list(
                iDisplayLength = 10,
                scrollX = TRUE,
                fixedColumns = TRUE,
                rowCallback = JS(
                  "function(row, data){",
                  "if(data[1] =='N/A'){}else{var match = anchor(data[1]);}",
                  "$('td:eq(1)', row).html(match)",
                  "if(data[1] =='N/A'){}else{var new_links = urls(data[2])}",
                  "$('td:eq(2)', row).html(new_links);",
                  "}"
                )
              )
            )
            output$resultsummarydata_table <- DT::renderDataTable(
              results_summary,
              options = list(
                iDisplayLength = 10,
                scrollX = TRUE,
                fixedColumns = TRUE,
                rowCallback =JS(
                  "function(row,data){",
                  "if(data[1] =='N/A'){}else{var match = geneurls(data[1]);}",
                  "$('td:eq(1)', row).html(match)",
                  "}"
                )
              )
            )
            incProgress(1, detail = "Done")
            }
          })
          })
    
    # SAPP Protein =============================
    Sappdataprot <- observeEvent(input$submitprot, {
      
      ### Clear all data.frames #############################
      results <- NULL;result <- NULL;blastresults <- NULL;signalIP <- NULL;ipr <- NULL;
      priam_table_prot <- NULL;iprresults <- NULL;

      ### Variable and js scripts addCLass #############################
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      ncbiprotein <- input$variableprot
      
      ### Run the basequery #############################
      basequery <- source('query/protein_query.R')$value
       
      ### Manual Annotation Blazegraph query
      #ncbiprotein <- 'NP_001130025.1'
      #endpoint <- "http://10.209.0.227:8030/blazegraph/namespace/SalmoDB/sparql"
      
      
      endpoint2 <- "http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/sparql"
      maquery <- paste("prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/>
      SELECT ?proteinName ?column ?value 
      WHERE{<csb:protein> <csb:name> ?proteinName. 
      FILTER(contains(?proteinName, '",ncbiprotein,"'))
      <csb:",ncbiprotein,"> ?column ?value.}",sep="")
      
      fetch_query <- SPARQL(endpoint2,maquery)$results
      fetch_query<-as.data.table(fetch_query)
      if (empty(fetch_query) == TRUE){results <- noframe()
      }else{
        fetch_query[,column:=sub('>','',sub('<csb:','',column))]  
      }
      output$ma_table_prot <- DT::renderDataTable({
        fetch_query
      })
      
      
      # Start the progression bar =================================
      withProgress(
        message = 'Fetching data',
        detail = 'This may take a while...',
        value = 0,
        {
          Sys.sleep(0.50)
          incProgress(0.1, detail = "Starting SAPP protein query")
          
          ### Start querying and load results #############################
          queryfun <- function(basequery, ncbiprotein) {return(sub('changeme', ncbiprotein, basequery))}
          
          #results_interpro <- data.table(sparql(endpoint, paste(prefixes,queryfun(basequery_interpro, ECnumber))))
          #results_interpro <- rename_head(results_interpro)
          #results_interpro <- clean_post(results_interpro)
          
          
          res <- SPARQL(endpoint, queryfun(basequery,ncbiprotein))
          results <- data.table(res$results)
          
          ### Check if dataframe is empty #############################
          if (empty(results) == TRUE) {
            output$exampleOutput <- renderText({
              createAlert(session,"alert2","exampleAlert2",title = "No Query Found",
                content = "The number you enterd is not a valid NP number or a NP number wich returned an
                empty results", append = FALSE)
            })
          }
          # Build dataframes and parse columns =================================
          else{
            ### Change names of results columns #############################
            #source('var/rename_protein.R')
            results <- rename_protein(results)
            
            # Create a empty dataframe if needed
            if (empty(results) == TRUE){results <- noframe()}
            
            ### Blast against Swissprot and COG #############################
            incProgress(0.3, detail = "Fetching BLAST data")
            blastresults <- dcast(results[tool=='Blast'],feature~colname)
            result <- blastresults[tool=='cog']
            
            # Swissprot results
            result <- blastresults[tool=='swiss']
            
            # Send tool and version to header
            result_version <- result$result_version[1]
            result_tool <- result$tool[1]
            
            result <- result[,.(ncbiprotein,alignment_length,bitscore,evalue,gaps,mismatches,percidentity,qend,qstart,send,sstart,subjectname)]
            result <- rename(result,c("alignment_length" = "aln","evalue" = "e","mismatches" = "mm","percidentity" = "pi","bitscore" = "bs",
                                      "qend" = "qe","send" = "se","qstart" = "qs","sstart" = "ss"))
            # Create a empty dataframe if needed
            if (empty(result) == TRUE){result_tool <- "N/A";result_version <- "N/A";result <- noframe();}
            
            ### Priam table created #############################
            incProgress(0.3, detail = "Fetching PRIAM data")
            priam_table_prot <- dcast(results[tool=='Priam'],feature~colname)
            
            # Get tool and verison information
            priam_tool_prot <- toupper(priam_table_prot$tool[1])
            priam_version_prot <- priam_table_prot$version[1]
            priam_table_prot <-rename(priam_table_prot,c("<http://www.biopax.org/release/bp-level3.owl#xref" = "xref"))
            # rename the headers in the priam table
            priam_table_prot <-rename(priam_table_prot,c("align_length" = "aln","bit_score" = "bs","evalue" = "e","is_best_overlap" = "isb","positive_hit_probability" = "php",
                                                         "profile_from" = "pf","profile_length" = "pl","profile_proportion" = "pp","profile_to" = "pt","query_from" = "qf","query_length" = "ql","query_strand" = "qs","query_to" = "qt"))
            priam_table_prot <- priam_table_prot[,.(ncbiprotein,xref,aln,bs,e,isb,php,profile_ID,pf,pl,pp,pt,qf,ql,qs,qt)]
            
            #priam_table_prot <- priam_protein_rename(priam_table_prot)
            
            # Create a empty dataframe if needed
            if (empty(priam_table_prot) == TRUE){
              priam_tool_prot <- "N/A"
              priam_version_prot <- "N/A"
              priam_table_prot <- noframe()
            }
  
            incProgress(0.4, detail = "Fetching Interpro data")
            
            ### Interproscan #############################
            ipr <- dcast(results[tool=='Interpro'],feature~colname)
            # Grab tool and version varibles
            interpro_tool_prot <- toupper(ipr$tool[1])
            interpro_version_prot <- ipr$version[1]
            
            # Drop the columns that are not needed
            ipr <- ipr[,.(ncbiprotein,signature,analysis,begin,end,score)]
            
            # Create a empty dataframe if needed
            if (empty(ipr) == TRUE){
              interpro_tool_prot <- "N/A"
              interpro_version_prot <- "N/A"
              ipr <- noframe()
            }
            
            iprdomains <- unique(results[tool=='Interpro'&colname=='signature']$value)
            
            ipr_query <- source('query/ipr_query.R')
            
            ### Interproscan Domains #############################
            incProgress(0.6, detail = "Fetching Interpro domain data")
            iprres <- SPARQL(endpoint,sub('domains',paste(iprdomains,collapse=' '),ipr_query))
            iprresults <- data.table(iprres$results)
            iprresults[grep('identifiers.org/go',xref),xreftype:='GOterm']
            iprresults[grep('identifiers.org/ec-code',xref),xreftype:='ECnumber']
            iprresults[grep('identifiers.org/interpro',xref),xreftype:='IPRdomain']
            
            # Create a empty dataframe if needed
            if (empty(iprresults) == TRUE){iprresults <- noframe()}
            
            ### Try the Tmhm #############################
            #try(tmhmm <- dcast(results[tool=='Tmhmm'],feature~colname))
            
            ### SignalP #############################
            try(signalIP <- dcast(results[tool=='SignalP'],feature~colname))
            signalIP_tool <- signalIP$tool[1]
            signalIP_version <- signalIP$version[1]
            signalIP <- signalIP [,.( feature,cmax,cpos,d,dmaxcut,network,signal,smax,smean,spos,ymax,ypos)]
            signalIP <- rename(signalIP, c('cpos' = 'c', 'dmaxcut'= "dmc",'signal'='s') )
            
            ### Isolate the variables to be renderd #############################
            isolate ({             # Send the versions and tools name to header
              output$tool_blast <- renderText({ paste("Tool: ",result_tool)})
              output$version_blast <- renderText({paste("Version: ",result_version) })
              output$tool_priam_prot <- renderText({ paste("Tool: ",priam_tool_prot)})
              output$version_priam_prot <- renderText({paste("Version: ",priam_version_prot) })
              output$tool_interpro_prot <- renderText({ paste("Tool: ",interpro_tool_prot)})
              output$version_interpro_prot <- renderText({paste("Version: ",interpro_version_prot) })
              output$tool_signalIP <- renderText({ paste("Tool: ",signalIP_tool)})
              output$version_signalIP <- renderText({paste("Version: ",signalIP_version) })
            })
            ### Render Data tables #############################
            incProgress(0.8, detail = "Rendering data tables")
            # Drop the feature column from SAPP before rendering
            results$feature <- NULL
            #NP_001133193.1
           
            output$myTableprot <- DT::renderDataTable(
              results,
              options = list(
                iDisplayLength = 10,
                scrollX = TRUE,
                fixedColumns = TRUE,
                rowCallback = JS(
                  "function(row, data){",
                  "var match = anchor(data[5]);",
                  "$('td:eq(5)', row).html(match)",
                  "}"
                )
              )
            )
            output$blastresult_table <- DT::renderDataTable(
              #This is the BLAST result(s)
              result,
              options = list(
                iDisplayLength = 10,
                scrollX = TRUE,
                fixedColumns = TRUE,
                fnInitComplete = JS(
                  "function (settings, json){tool_header();}")
              )
            )
            output$priamprot_table <- DT::renderDataTable(
              priam_table_prot,
              options = list(
                iDisplayLength = 10,
                scrollX = TRUE,
                fixedColumns = TRUE,
                rowCallback = JS(
                  "function(row, data){",
                  "if(data[2] =='N/A'){}else{var new_links = urls(data[2])}",
                  "$('td:eq(2)', row).html(new_links);",
                  "}"
                )
              )
            )
            output$signalIP_table <- DT::renderDataTable(
              signalIP,
              options = list(
                iDisplayLength = 10,
                scrollX = TRUE,
                fixedColumns = TRUE,
                fnInitComplete = JS(
                  "function (settings, json){tool_header();}")
              )
            )
            output$ipr_table <- DT::renderDataTable(
              ipr,
              options = list(
                iDisplayLength = 10,
                scrollX = TRUE,
                fixedColumns = TRUE,
                rowCallback = JS(
                  "function(row, data){",
                  "if(data[2] =='N/A'){}else{var new_links = iprsearch(data[2])}",
                  "$('td:eq(2)', row).html(new_links);",
                  "}"
                )
              )
            )
            # interpro Domains
            output$interpro_table <- DT::renderDataTable(
              iprresults,
              options = list(
                iDisplayLength = 10,
                scrollX = TRUE,
                fixedColumns = TRUE,
                rowCallback = JS(
                  "function(row, data){",
                  "if(data[1] =='N/A'){}else{var new_links = urls(data[1])}",
                  "$('td:eq(1)', row).html(new_links);",
                  "if(data[3] =='N/A'){}else{var new_links = iprd(data[3])}",
                  "$('td:eq(3)', row).html(new_links);",
                  "}"
                )
              )
            )
            incProgress(1, detail = "Done")
          }
          }) # Progress bar ends
        }) # SappprotData ends
    
    
    # Reaction Annotation ======================
    endpoint2 <- "http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/sparql"
    #prefix ma: <http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/>
    observeEvent (input$ma_submit,{
                  # Save inputs from text fields 
                  ecnumber <- isolate(input$variable)
                  author <- isolate(shQuote(input$author))
                  date <-isolate(shQuote(input$date))
                  comment <- isolate(shQuote(input$comment))
                  gene <- isolate(shQuote(input$gene))
                  protein <- isolate(shQuote(input$protein))
                  reaction <- isolate(shQuote(input$reaction))
                  goterm <- isolate(shQuote(input$goterm))
                  doi <- isolate(shQuote(input$doi))
                  url <- isolate(shQuote(input$url))
                  
                  #Build update query
                  update <- paste("
                    prefix ma: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/>
                    INSERT DATA{
                                <ma:manualannotatin> <ma:a> <rdf:Class>.
                                <ma:",ecnumber,"> <ma:a> <ma:manualannotatin>.
                                <ma:",ecnumber,"> <dc:creator> ",author,".
                                <ma:",ecnumber,"> <dc:date> ",date,".
                                <ma:",ecnumber,"> <dc:description> ",comment,".
                                <ma:",ecnumber,"> <ma:gene> ",gene,".
                                <ma:",ecnumber,"> <ma:protein> ",protein,".
                                <ma:",ecnumber,"> <ma:reaction> ",reaction,".
                                <ma:",ecnumber,"> <ma:goterm> ",goterm,".
                                <ma:",ecnumber,"> <ma:doi> ",doi,".
                                <ma:",ecnumber,"> <ma:url> ",url,"
                  }",sep="")
                  output$test <- renderText({
                    update
                  })
                  # Contruct a delete query
                  # delete_query <- paste("prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/>
                  #                       DELETE DATA{ <csb:",subject,"> <csb:",predicate,"> ",object,". }",sep="")
                  # SPARQL update request using post 
                  SPARQL(endpoint2, update=update, curl_args = list(style="post"))
                  
                  # Render the update query
                  output$updatequery <- renderText({
                    update
                  })
                  
                  ### Fetch database and explore -----------------------------
                  # Idea put in a check to see if the data entry worked.
                  # Allow user to choose prefixes
                  # Aloow users to Delete data
                  # set up a button remove last query
                  # Fix Div structure in Manual anno, the tables is off
                  
                  # query <- "prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/> 
                  # select ?subject ?predicate ?object where {?subject ?predicate ?object.}"
                  # fetch_query <- SPARQL(endpoint2,query)$results
                  # 
                  # data<-as.data.frame(fetch_query)
                  # 
                  # output$contents <- DT::renderDataTable({
                  #   data  
                  # })
                  observeEvent (input$delete,{
                    # Contruct a delete query
                    delete_query <- paste("prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/> DELETE DATA{ <csb:",subject,"> <csb:",predicate,"> ",object,". }",sep="")
                    output$updatequery <- renderText({
                      delete_query
                    })
                    SPARQL(endpoint2, update=delete_query, curl_args = list(style="post"))
                    
                    query <- "prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno//> 
                    select ?subject ?predicate ?object where {?subject ?predicate ?object.}"
                    fetch_query <- SPARQL(endpoint2,query)$results
                    
                    data<-as.data.frame(fetch_query)
                    
                    output$contents <- DT::renderDataTable({
                      data  
                    })
                  })
                  
                  
                  # Update text field after a submition and set value to empty
                  updateTextInput(session,'a_subject', value = "")
                  updateTextInput(session,'a_object', value = "")
                  updateTextInput(session,'a_predicate', value = "")
    })
    
    # Protein Annotation =======================
    observeEvent (input$ma_submit_prot,{
      # Save inputs from text fields 
      subject <- isolate(input$a_subject_prot)
      predicate <-isolate(input$a_predicate_prot)
      object <- isolate(shQuote(input$a_object_prot))
      
      # Build update query
      update <- paste("prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/>
                      INSERT DATA{ <csb:",subject,"> <csb:",predicate,"> ",object,". }",sep="")
      # Contruct a delete query
      delete_query <- paste("prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/>
                            DELETE DATA{ <csb:",subject,"> <csb:",predicate,"> ",object,". }",sep="")
      # SPARQL update request using post 
      SPARQL(endpoint2, update=update, curl_args = list(style="post"))
      
      # REnder the update query
      output$updatequery_prot <- renderText({
        update
      })
      
      ### Fetch database and explore -----------------------------
      # Idea put in a check to see if the data entry worked.
      # Allow user to choose prefixes
      # Aloow users to Delete data
      # set up a button remove last query
      # Fix Div structure in Manual anno, the tables is off
      # Add a link creation for GeneID and Pubmed fx.
      # remove <csb: > using regexp
      # Have pre defined predicates and ability to create new ones.
      
      query <- "prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/> 
                  select ?subject ?predicate ?object where {?subject ?predicate ?object.}"
      fetch_query <- SPARQL(endpoint2,query)$results
      
      fetch_query<-as.data.frame(fetch_query)
      
      output$contents_prot <- DT::renderDataTable({
        fetch_query  
      })
      observeEvent (input$delete_prot,{
        # Contruct a delete query
        delete_query <- paste("prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/> 
                              DELETE DATA{ <csb:",subject,"> <csb:",predicate,"> ",object,". }",sep="")
        output$updatequery_prot <- renderText({
          delete_query
        })
        SPARQL(endpoint2, update=delete_query, curl_args = list(style="post"))
        
        query <- "prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/> 
                select ?subject ?predicate ?object where {?subject ?predicate ?object.}"
        fetch_query <- SPARQL(endpoint2,query)$results
        
        fetch_query<-as.data.frame(fetch_query)
        
        output$contents_prot <- DT::renderDataTable({
          fetch_query  
        })
        updateTextInput(session,'a_subject', value = "")
        updateTextInput(session,'a_object', value = "")
        updateTextInput(session,'a_object', value = "")
      })
  })
    
    # Delete a specific entry ==================  
    observeEvent(input$delete_reaction,{
      subject <- isolate(input$d_subject_reaction)
      predicate <-isolate(input$d_predicate_reaction)
      object <- isolate(shQuote(input$d_object_reaction))
      
      # Contruct a delete query
      delete_query <- paste("prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/> DELETE DATA{ <csb:",subject,"> <csb:",predicate,"> ",object,". }",sep="")
      output$updatequery <- renderText({
        delete_query
      })
      SPARQL(endpoint2, update=delete_query, curl_args = list(style="post"))
      
      query <- "prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/> 
      select ?subject ?predicate ?object where {?subject ?predicate ?object.}"
      fetch_query <- SPARQL(endpoint2,query)$results
      fetch_query<-as.data.frame(fetch_query)
      output$contents <- DT::renderDataTable({
        fetch_query  
      })
      # Update text field after a submition and set value to empty
      updateTextInput(session,'d_subject_reaction', value = "")
      updateTextInput(session,'d_object_reaction', value = "")
      updateTextInput(session,'d_object_reaction', value = "")
    })  
   
    observeEvent(input$delete_submit_prot,{
        subject <- isolate(input$d_subject_prot)
        predicate <-isolate(input$d_predicate_prot)
        object <- isolate(shQuote(input$d_object_prot))
        
        # Contruct a delete query
        delete_query <- paste("prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/> DELETE DATA{ <csb:",subject,"> <csb:",predicate,"> ",object,". }",sep="")
        output$updatequery_prot <- renderText({
          delete_query
        })
        SPARQL(endpoint2, update=delete_query, curl_args = list(style="post"))
        
        query <- "prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/> 
        select ?subject ?predicate ?object where {?subject ?predicate ?object.}"
        fetch_query <- SPARQL(endpoint2,query)$results
        fetch_query<-as.data.frame(fetch_query)
        output$contents_prot <- DT::renderDataTable({
          fetch_query  
        })
        # Update text field after a submition and set value to empty
        updateTextInput(session,'d_subject_prot', value = "")
        updateTextInput(session,'d_object_prot', value = "")
        updateTextInput(session,'d_predicate_prot', value = "")
      })

    # Open Database for both tabs protein and reaction ===================
    observeEvent (input$opendb,{
      shinyjs::show("contents")
      query <- "prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/> select ?subject ?predicate ?object where {?subject ?predicate ?object.}"
      fetch_query <- SPARQL(endpoint2,query)$results
      fetch_query<-as.data.frame(fetch_query)
      output$contents <- DT::renderDataTable({
        fetch_query
      })
    })
    observeEvent (input$opendb_prot,{
      shinyjs::show("contents_prot")
      query <- "prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/> select ?subject ?predicate ?object where {?subject ?predicate ?object.}"
      fetch_query <- SPARQL(endpoint2,query)$results
      fetch_query<-as.data.frame(fetch_query)
      output$contents_prot <- DT::renderDataTable({
        fetch_query
      })
    })
    # closeDB ===========================
    observeEvent(input$closedb,{
      hide("contents")
    })
    observeEvent(input$closedb_prot,{
      hide("contents_prot")
    })
    
    
    # Federated query TEST ======================
    endpoint3 <- "http://localhost:9999/blazegraph/namespace/ManualAnno/sparql"
    observeEvent(input$fd,{
      
    query_fed <-" 
    prefix csb: <http://128.39.179.17:9999/blazegraph/namspace/ManualAnno/>
    prefix ssb: <http://csb.wur.nl/genome/>
    select ?header ?tool ?feature ?colname ?value ?header2 ?tool2 ?feature2 ?colname2 ?value2 {      
      <csb:protein> <csb:name> ?header.
      FILTER(contains(?header, 'NP_001133193.1'))
      <csb:NP_001130025.1> <csb:tool> ?tool.
      <csb:NP_001130025.1> <csb:hasGeneid> ?feature .
      ?s ?colname '100192341'.
      <csb:NP_001130025.1> <csb:PubmedID> ?value .    
        service <http://10.209.0.227:8030/blazegraph/namespace/SalmoDB/sparql> {      
          ?cds ssb:header ?header2.
          FILTER(contains(?header2,'NP_001130025.1'))
          ?cds ssb:protein ?protein.
          ?protein ssb:feature ?feature2.
          ?feature2 a ?tool2.
        ?feature2 ?colname2 ?value2.
      }
    } "

    fetch_query <- SPARQL(endpoint3,query=query_fed)$results
    fetch_query <-as.data.frame(fetch_query)
    output$federated_table <- DT::renderDataTable(
      fetch_query,
      extensions = 'Responsive'
      )
    })
    
  }
  shinyApp(ui, server)
  }