library(shiny)
library(RCurl)
library(shinyBS)
library(shinyjs)
library(SPARQL)
library(data.table)
library(DT)
library(stringr)
library(plyr)
library(rJava)
library(RGBOLApi)

# Source hyperlink function and directories
sapply(list.files(pattern="[.]R$", path="var/", full.names=TRUE), source)
sapply(list.files(pattern="[.]R$", path="query/", full.names=TRUE), source)

# Point to js file
jsfile <- "www/jsfile.js"

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$title("SAPP query"),
    tags$script(src = 'jsfile.js'),
    tags$link(rel = "stylesheet", type = "text/css", href = "sapp.css"),
    tags$link(rel="icon",type="image/png", href ="favicon.ico", sizes="32x32")
    ),
    
  tags$div(class = "Header",
           tags$div(class = "banner", tags$img( src = "CIGENE.png", width = 'auto', height = "50px" ),
                    tags$img( src = "DS.png", width = 'auto', style = 'position:relative;left:50px;', height = "50px" ) ) ),
  navbarPage (
   "SAPP Query",
   # Reaction NavTab =================================
     tabPanel(
      "Reaction",
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
                                               "Salmo salar" = 'http://10.209.0.227:8030/blazegraph/namespace/SalmoDB/sparql',
                                               "Danio rerio" = 'http://10.209.0.227:7955/blazegraph/namespace/ZebraDB/sparql' ),
                                             selected = 'http://10.209.0.227:8030/blazegraph/namespace/SalmoDB/sparql'))
                        ) # Fluid Row ends
               ) # Div container setwidth
      ), # Div ends Content
       # Reaction DataTables
       tags$div(class = "content" ,
                tags$div(class = "container tables setwidth",
                        fluidRow(
                           column(
                             12,
                             mainPanel(
                               width = 12,
                               style = "height=100%;width:100%;min-height:600px",
                               textOutput("exampleOutput"),
                               bsAlert("alert"),
                               tabsetPanel(
                                # Reaction SAPP Tab
                                tabPanel("SAPP",
                                         tags$div(class = "info",
                                                  tags$ul(
                                                    tags$li(""),
                                                    tags$li("")
                                                  )),
                                         dataTableOutput('myTable')
                                ),
                                # Reaction BLAST TAB
                                tabPanel("BLAST",
                                         tags$div(class = "info",
                                                  tags$ul(
                                                    tags$li(textOutput("tool")),
                                                    tags$li(textOutput("version"))
                                                  )),

                                         dataTableOutput('swissprot_table')

                                ),
                                # Reaction PRIAM Tab
                                tabPanel("PRIAM",
                                         tags$div(class = "info",
                                                  tags$ul(tags$li(textOutput("tool_priam")),
                                                          tags$li(textOutput("version_priam")))),
                                         dataTableOutput('priamdata_table')

                                ),
                                # Reaction Interpro Tab
                                tabPanel("Interpro",
                                         tags$div(class = "info",
                                                  tags$ul(tags$li(textOutput("tool_interpro")),
                                                          tags$li(textOutput("version_interpro")))),
                                         dataTableOutput('interprodata_table')
                                ),

                                # Reaction EnzDP Tab
                                tabPanel("EnzDP",
                                         tags$div(class = "info",
                                                  tags$ul(
                                                    tags$li(textOutput("tool_enzdp")),
                                                    tags$li(textOutput("version_enzdp"))
                                                  )),
                                         dataTableOutput('enzdp_table')
                                ),
                                # Reaction Result Summary
                                tabPanel("Result Summary",
                                         tags$div(class = "info",
                                                  tags$ul(
                                                    tags$li(""),
                                                    tags$li("")
                                                  )),
                                         dataTableOutput('resultsummarydata_table')
                                ),
                                # Reaction Manual Anotation
                                tabPanel(width ="12", "Manual Annotation",
                                   mainPanel(width = 12,
                                     tabsetPanel(
                                       tabPanel("Input Annotations",
                                          tags$div(class="ma",
                                             tags$div(class = "container",style ="min-height:650px",
                                                fluidRow(
                                                  ### Input feilds
                                                  column(12,
                                                    tags$form(class = "form-inline",
                                                               tags$div(class="information_top",
                                                                        h4("Personal information and institute"),
                                                                        tags$hr(),
                                                                        tagAppendAttributes(
                                                                          class ="input_feilds authors",
                                                                          textInput(
                                                                            "author",
                                                                            "Author",
                                                                            width = "125px",
                                                                            value = "",
                                                                            placeholder = NULL
                                                                          )
                                                                         )
                                                               ),
                                                              tags$div(class="information_mid",
                                                                       h4("Annotation information"),
                                                                       tags$hr(),
                                                                       tagAppendAttributes(
                                                                         class ="input_feilds gene",
                                                                         textInput(
                                                                           "geneid",
                                                                           "GeneID",
                                                                           width = "125px",
                                                                           value = "0",
                                                                           placeholder = "Use 0 if geneid is not known"
                                                                         )
                                                                       ),
                                                                       tagAppendAttributes(
                                                                         class ="input_feilds gene_name",
                                                                         textInput(
                                                                           "gene_name",
                                                                           "Gene name",
                                                                           width = "125px",
                                                                           value = "",
                                                                           placeholder = NULL
                                                                         )),
                                                                       tagAppendAttributes(
                                                                         class ="input_feilds protein",
                                                                         textInput(
                                                                           "protein",
                                                                           "Protein",
                                                                           width = "125px",
                                                                           value = "",
                                                                           placeholder = NULL
                                                                         )
                                                                       ),
                                                                       tagAppendAttributes(
                                                                         class ="input_feilds reaction",
                                                                         textInput(
                                                                           "reaction",
                                                                           "Reaction",
                                                                           width = "125px",
                                                                           value = "",
                                                                           placeholder = NULL
                                                                         )
                                                                       ),
                                                                       tagAppendAttributes(
                                                                         class ="input_feilds comment_prot",
                                                                         textInput(
                                                                           "comment",
                                                                           "Comment",
                                                                           width = "250px",
                                                                           value = "",
                                                                           placeholder = NULL
                                                                         )
                                                                       )
                                                              ),
                                                               tags$div(class="information_bottom", style ="display:none",
                                                                        tagAppendAttributes(
                                                                          class ="input_feilds goterm",
                                                                          textInput(
                                                                            "goterm",
                                                                            "Goterm",
                                                                            width = "125px",
                                                                            value = "",
                                                                            placeholder = NULL
                                                                          )
                                                                        ),
                                                                        tagAppendAttributes(
                                                                          class ="input_feilds doi",
                                                                          textInput(
                                                                            "doi",
                                                                            "Doi",
                                                                            width = "125px",
                                                                            value = "",
                                                                            placeholder = NULL
                                                                          )
                                                                        ),
                                                                        tagAppendAttributes(
                                                                          class ="input_feilds url",
                                                                          textInput(
                                                                            "url",
                                                                            "URL",
                                                                            width = "125px",
                                                                            value = "",
                                                                            placeholder = NULL
                                                                          )
                                                                        )
                                                               ),

                                                               tagAppendAttributes(
                                                                 `data-proxy-click` = "ma_submit",
                                                                 actionButton("ma_submit", "Submit")
                                                               )

                                                    )
                                            ),#tags from and columns ends
                                            column(6, class="dropdown",
                                                   selectInput("select_reaction", label = ("Select a institute"),
                                                               choices = list(
                                                                 "NMBU" = 'NMBU',
                                                                 "University of Sterling" = 'uos',
                                                                 "wageningen university" = 'wur' ),
                                                               selected = 'NMBU'))
                                ) # Fluid row ends
                             ) # div container ends
                          ) # div ma ends
                        ),
                        # Viewing the manual annotation output
                        tabPanel("View", width = 12, 
                                 ### Gene Database view
                                 column (12,
                                 dataTableOutput('contents')
                        ))
                      )
                    )
                  ) # Tabpanel manul annotations ends
                ) # tabset ends
              ) # main panel ends
            ) # column ends
          ) # Fluid Row
        ) # Div container tables setwidth ends
      ) # Div content ends
    ), # Tab Panel reaction ends
   # Protein NavTab =================================
   tabPanel(
     "Protein",
     # Protein Search Feild
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
                                              "Salmo salar" = 'http://10.209.0.227:8030/blazegraph/namespace/SalmoDB/sparql',
                                              "Danio rerio" = 'http://10.209.0.227:7955/blazegraph/namespace/ZebraDB/sparql' ),
                                            selected = 'http://10.209.0.227:8030/blazegraph/namespace/SalmoDB/sparql')
                                
                         )
                       ))),
     # Protein DataTables
     tags$div(class = "content",
              tags$div(class = "container tables setwidth",
                       fluidRow(
                         column(
                           12,
                           mainPanel(
                             width = 12,
                             style = "height=100%;width:100%;;min-height:600px",
                             textOutput("exampleOutput2"),
                             bsAlert("alert2"),
                             tabsetPanel(
                               # Protein SAPP Tab
                               tabPanel("SAPP",
                                        tags$div(class = "info",
                                                 tags$ul(
                                                   tags$li(""),
                                                   tags$li("")
                                                 )),
                                        dataTableOutput('myTableprot')
                               ),
                               # Protein BLAST TAB
                               tabPanel("BLAST",
                                        tags$div(class = "info",
                                                 tags$ul(
                                                   tags$li(textOutput("tool_blast")),
                                                   tags$li(textOutput("version_blast"))
                                                 )),
                                        dataTableOutput('blastresult_table')
                               ),
                               
                               # Protein PRIAM
                               tabPanel("PRIAM",
                                        tags$div(class = "info",
                                                 tags$ul(
                                                   tags$li(textOutput("tool_priam_prot")),
                                                   tags$li(textOutput("version_priam_prot"))
                                                 )),
                                        dataTableOutput('priamprot_table')
                               ),
                               # Protein SIGNAL IP Tab
                               tabPanel("SIGNALP",
                                        tags$div(class = "info",
                                                 tags$ul(
                                                   tags$li(textOutput("tool_signalIP")),
                                                   tags$li(textOutput("version_signalIP"))
                                                 )),
                                        
                                        dataTableOutput('signalIP_table')
                               ),
                               # Protein Interpro Tab
                               tabPanel("Interpro",
                                        tags$div(class = "info",
                                                 tags$ul(
                                                   tags$li(textOutput("tool_interpro_prot")),
                                                   tags$li(textOutput("version_interpro_prot"))
                                                 )),
                                        dataTableOutput('ipr_table')
                               ),
                               # Protein Interpro Domains Tab
                               tabPanel("Interpro Domains ",
                                        tags$div(class = "info",
                                                 tags$ul(
                                                   tags$li(""),
                                                   tags$li("")
                                                 )),
                                        dataTableOutput('interpro_table')
                               ),
                               # Protein Manual Anotation
                               tabPanel("Manual Annotation",
                                mainPanel( width = 12,
                                  tabsetPanel(
                                    tabPanel("Input Annotations",
                                       tags$div(class="ma",
                                          tags$div(class = "container",style ="height:650px",
                                             fluidRow(
                                               ### Input feilds
                                               column(12,
                                                  tags$form(class = "form-inline",
                                                            tags$div( #class = "form-group",
                                                              tags$div(class="information_top",
                                                                       h4("Personal information and institute"),
                                                                       tags$hr(),
                                                                       tagAppendAttributes(
                                                                         class ="input_feilds",
                                                                         textInput(
                                                                           "author_prot",
                                                                           "Author",
                                                                           width = "125px",
                                                                           value = "",
                                                                           placeholder = NULL
                                                                         )
                                                                       )
                                                              ),
                                                              tags$div(class="information_mid",
                                                                       h4("Annotation information"),
                                                                       tags$hr(),
                                                                       tagAppendAttributes(
                                                                         class ="input_feilds gene",
                                                                         textInput(
                                                                           "geneid_prot",
                                                                           "GeneID",
                                                                           width = "125px",
                                                                           value = "0",
                                                                           placeholder = "Use 0 if geneid is not known"
                                                                         )
                                                                       ),
                                                                       tagAppendAttributes(
                                                                         class ="input_feilds gene_name",
                                                                         textInput(
                                                                           "gene_name_prot",
                                                                           "Gene name",
                                                                           width = "125px",
                                                                           value = "",
                                                                           placeholder = NULL
                                                                         )),
                                                                       tagAppendAttributes(
                                                                         class ="input_feilds protein",
                                                                         textInput(
                                                                           "protein_prot",
                                                                           "Protein",
                                                                           width = "125px",
                                                                           value = "",
                                                                           placeholder = NULL
                                                                         )
                                                                       ),
                                                                       tagAppendAttributes(
                                                                         class ="input_feilds reaction",
                                                                         textInput(
                                                                           "reaction_prot",
                                                                           "Reaction",
                                                                           width = "125px",
                                                                           value = "",
                                                                           placeholder = NULL
                                                                         )
                                                                       ),
                                                                       tagAppendAttributes(
                                                                         class ="input_feilds comment_prot",
                                                                         textInput(
                                                                           "comment_prot",
                                                                           "Comment",
                                                                           width = "250px",
                                                                           value = "",
                                                                           placeholder = NULL
                                                                         )
                                                                       )

                                                              ),
                                                              tags$div(class="information_bottom", style ="display:none",
                                                                       tagAppendAttributes(
                                                                         class ="input_feilds goterm",
                                                                         textInput(
                                                                           "goterm_prot",
                                                                           "Goterm",
                                                                           width = "125px",
                                                                           value = "",
                                                                           placeholder = NULL
                                                                         )
                                                                       ),
                                                                       tagAppendAttributes(
                                                                         class ="input_feilds doi",
                                                                         textInput(
                                                                           "doi_prot",
                                                                           "Doi",
                                                                           width = "125px",
                                                                           value = "",
                                                                           placeholder = NULL
                                                                         )
                                                                       ),
                                                                       tagAppendAttributes(
                                                                         class ="input_feilds url",
                                                                         textInput(
                                                                           "url_prot",
                                                                           "URL",
                                                                           width = "125px",
                                                                           value = "",
                                                                           placeholder = NULL
                                                                         )
                                                                       )
                                                              ),
                                                              
                                                              tagAppendAttributes(
                                                                `data-proxy-click` = "ma_submit_prot",
                                                                actionButton("ma_submit_prot", "Submit")
                                                              )
                                                              
                                                            )# tags div end
                                                  )
                                           ),#tags from and columns ends
                                           column(6, class="dropdown",
                                                  selectInput("select_protein", label = ("Select a institute"),
                                                              choices = list(
                                                                "NMBU" = 'NMBU',
                                                                "University of Sterling" = 'uos',
                                                                "wageningen university" = 'wur' ),
                                                              selected = 'NMBU'))

                                         ) # Fluid row ends
                                ) # div container ends
                             ) # div ma ends
                          ),
                          tabPanel("View", width = 12, 
                            ### Reaction Database view
                            column (12, 
                                 dataTableOutput('contents_prot')
                            )
                          )
                       )
                    )
                  )
                )
              )
            )
          ) 
        )
      )
    ), # Tab ends
   # Gene DataTables #############################
    tabPanel(
      "Gene",
      tags$div(class = "content",
     tags$div(class = " container setwidth",style ="min-height:650px", # tables ma
        mainPanel(width = 12,
          tabsetPanel( id ="geneTabset",
               tabPanel("Manual Annotation",
                        fluidRow(
                          ### Input feilds
                          column(12,
                                 tags$form(class = "form-inline",
                                           tags$div( class = "form-group",
                                             tags$div(class="information_top",
                                                      h4("Personal information and institute"),
                                                      tags$hr(),
                                                      tagAppendAttributes(
                                                        class ="input_feilds",
                                                        textInput(
                                                          "author_gene",
                                                          "Author",
                                                          width = "125px",
                                                          value = "",
                                                          placeholder = NULL
                                                        )
                                                      )
                                             ),
                                             tags$div(class="information_mid",
                                                      h4("Annotation information"),
                                                      tags$hr(),
                                                      tagAppendAttributes(
                                                       class ="input_feilds gene",
                                                       textInput(
                                                         "gene_gene",
                                                         "GeneID",
                                                         width = "125px",
                                                         value = "0",
                                                         placeholder = "Use 0 if geneid is not known"
                                                      )),
                                                      tagAppendAttributes(
                                                       class ="input_feilds gene_name",
                                                       textInput(
                                                         "name_gene",
                                                         "Gene name",
                                                         width = "125px",
                                                         value = "",
                                                         placeholder = NULL
                                                      )),
                                                      tagAppendAttributes(
                                                        class ="input_feilds protein_name",
                                                        textInput(
                                                          "name_protein",
                                                          "Protein name",
                                                          width = "125px",
                                                          value = "",
                                                          placeholder = NULL
                                                        )),
                                                      tagAppendAttributes(
                                                        class ="input_feilds reaction",
                                                        textInput(
                                                          "reaction_gene",
                                                          "Reaction",
                                                          width = "125px",
                                                          value = "",
                                                          placeholder = NULL
                                                        )),
                                                      tagAppendAttributes(
                                                        class ="input_feilds comment_gene",
                                                        textInput(
                                                          "comment_gene",
                                                          "Comment",
                                                          width = "250px",
                                                          value = "",
                                                          placeholder = NULL
                                                        ))
                                                      
                                             ),
                                             tags$div(class="information_bottom", style ="display:none",
                                                      tagAppendAttributes(
                                                        class ="input_feilds goterm",
                                                        textInput(
                                                          "goterm_gen",
                                                          "Goterm",
                                                          width = "125px",
                                                          value = "",
                                                          placeholder = NULL
                                                        )
                                                      ),
                                                      tagAppendAttributes(
                                                        class ="input_feilds doi",
                                                        textInput(
                                                          "doi_prot",
                                                          "Doi",
                                                          width = "125px",
                                                          value = "",
                                                          placeholder = NULL
                                                        )
                                                      ),
                                                      tagAppendAttributes(
                                                        class ="input_feilds url",
                                                        textInput(
                                                          "url_prot",
                                                          "URL",
                                                          width = "125px",
                                                          value = "",
                                                          placeholder = NULL
                                                        )
                                                      )
                                             ),

                                             tagAppendAttributes(
                                               `data-proxy-click` = "ma_submit_gene",
                                               actionButton("ma_submit_gene", "Submit")
                                             )
                                           )# tags div end
                                 )
                          ), #tags from and columns ends
                          column(6, class="dropdown_gene",
                                 selectInput("select_gene", label = ("Select a institute"),
                                             choices = list(
                                               "NMBU" = 'NMBU',
                                               "University of Sterling" = 'uos',
                                               "wageningen university" = 'wur' ),
                                             selected = 'NMBU'))
                        ) # Fluid row ends
                ),
                tabPanel(width = 12, "View annotations", value ="gene_tab",
                        ### Gene Database view
                        column (12,
                                dataTableOutput('contents_gene')
                ))
            ) # second TabsetPanel ends
          ) # mainPanel div ends
        ) # div container ends
      ) # Gene tabPanel ends
    ) # TabsetPanel ends
  ) # Navbar page
) # FluidPAge

# Server ---------------------------------
server <- function(input, output, session){
  # Database endpoint
  endpoint <- isolate(input$select)
  endpoint2 <- "http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/sparql"

  # endpoint <- 'http://10.209.0.227:8030/blazegraph/namespace/SalmoDB/sparql'
  # ncbiprotein <- 'NP_001130025.1'
  # ECnumber <- '5.4.2.11'

  output$value <- renderPrint({ input$select })
  # Get Date for Manual annotations

  # SAPP Reaction ============================
  observeEvent(input$submit,{

    ### Clear all data.frames #############################
    results <- NULL;results_interpro <- NULL;results_priam <- NULL;results_uniprot <- NULL;swiss_table <- NULL;enzdp_table <- NULL;results_enzdp<-NULL;
    fetch_query <- NULL
    ecnumber <- shQuote(ECnumber)
    ### Open manual annotation blazegraph ###########################
    ECnumber <- input$variable
    ### Querying and rendering Reaction annotations to UI #####################
    reaction_query <- paste("
                            SELECT ?ECnumber ?GeneID ?GeneName ?Protein ?Author ?Date ?Institute ?Comment ?url where {
                            ?gene	<http://gbol.life#number> ?GeneID .
                            ?gene   <http://gbol.life#gene>	?GeneName .
                            ?gene <http://gbol.life#xref> ?author .
                            ?gene <http://gbol.life#provenance> ?geneProv .
                            ?geneProv  <http://gbol.life#origin> ?manR .
                            ?manR <http://www.w3.org/ns/prov#wasGeneratedBy> ?manActive .
                            ?manActive  <http://www.w3.org/ns/prov#startedAtTime> ?Date . 
                            ?author <http://gbol.life#name> ?Author .
                            ?author <http://www.w3.org/ns/prov#actedOnBehalfOf> ?org .
                            ?org 	<http://gbol.life#legalName> ?Institute  .
                            ?gene <http://gbol.life#note>	?comment .
                            ?comment <http://gbol.life#text> ?Comment .
                            ?gene <http://gbol.life#xref>	?protein .
                            ?protein <http://gbol.life#protein_id> ?Protein .
                            ?gene <http://gbol.life#xref> ?ecnumber .
                            ?ecnumber <http://gbol.life#accession> ",ecnumber," .
                            ?ecnumber <http://gbol.life#accession> ?ECnumber .
                            }",sep="")
    
    fetch_query <- SPARQL(endpoint2,reaction_query)$results
    
    fetch_query <-as.data.table(fetch_query)
    output$contents <- renderDataTable({
      fetch_query
    })
    
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
        rename_head <- function(x){
          rename(x, c("?header" = "header", "?colname" = "colname", "?value" = "value", "?feature" = "feature"))
        }

        # CLean up post requests. New sparql functing has quotes around everything
        clean_post <- function(data){
          data[, value := sub('"','',value, perl = TRUE)] ; data[, value := sub('".+>','',value, perl = TRUE)]
        }

        #Test the query and rename columns
        results_interpro <- data.table(sparql(endpoint, paste(prefixes, queryfun(basequery_interpro, ECnumber))))
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
          }else {
            swiss_tool <- toupper("N/A"); swiss_version <- "N/A";swiss_table <- noframe()
          }

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

          # map salmon ncbiproteinid to ncbigeneid
          if (file.exists('ncbilink.RData'))
          {load('ncbilink.RData')}
          else{
            # download Salmon gff from NCBI
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

          # results_summary <- merge(ncbilink,results_summary,by.y='Ncbiprotein',by.x='ncbiprotein',all.y=T)
          # results_summary[,ncbiprotein:=NULL]
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
          output$myTable <- renderDataTable(
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
          output$swissprot_table <- renderDataTable(
            swiss_table,
            #extensions = 'Responsive',
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
          output$priamdata_table <- renderDataTable(
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
          output$interprodata_table <- renderDataTable(
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
          output$enzdp_table <- renderDataTable(
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
          output$resultsummarydata_table <- renderDataTable(
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
        }) # With progress bar ends
      }) # ObserveEvent ends

  # SAPP Protein =============================
  observeEvent(input$submitprot, {
    ### Clear all data.frames #############################
    results <- NULL;result <- NULL;blastresults <- NULL;signalIP <- NULL;ipr <- NULL;
    priam_table_prot <- NULL;iprresults <- NULL;

    ### Variable and js scripts addCLass #############################
    ncbiprotein <- input$variableprot

    ### Run the basequery #############################
    basequery <- source('query/protein_query.R')$value

    ncbiprotein_numb <- shQuote(ncbiprotein)
    ### Querying and rendering Reaction annotations to UI #####################
    protein_query <- paste("
                            SELECT ?ECnumber ?GeneID ?GeneName ?Protein ?Author ?Date ?Institute ?Comment ?url where {
                            ?gene	<http://gbol.life#number> ?GeneID .
                            ?gene   <http://gbol.life#gene>	?GeneName .
                            ?gene <http://gbol.life#xref> ?author .
                            ?gene <http://gbol.life#provenance> ?geneProv .
                            ?geneProv  <http://gbol.life#origin> ?manR .
                            ?manR <http://www.w3.org/ns/prov#wasGeneratedBy> ?manActive .
                            ?manActive  <http://www.w3.org/ns/prov#startedAtTime> ?Date . 
                            ?author <http://gbol.life#name> ?Author .
                            ?author <http://www.w3.org/ns/prov#actedOnBehalfOf> ?org .
                            ?org 	<http://gbol.life#legalName> ?Institute  .
                            ?gene <http://gbol.life#note>	?comment .
                            ?comment <http://gbol.life#text> ?Comment .
                            ?gene <http://gbol.life#xref>	?protein .
                            ?protein <http://gbol.life#protein_id> ",ncbiprotein_numb," .
                            ?protein <http://gbol.life#protein_id> ?Protein .
                            ?gene <http://gbol.life#xref> ?ecnumber .
                            ?ecnumber <http://gbol.life#accession> ?ECnumber .
                            }",sep="")
    
    fetch_prot_query <- SPARQL(endpoint2,reaction_query)$results

    fetch_prot_query <-as.data.table(fetch_prot_query)
    output$contents_prot <- renderDataTable({
      fetch_prot_query
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

          output$myTableprot <- renderDataTable(
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
          output$blastresult_table <- renderDataTable(
            #This is the BLAST results
            result,
            options = list(
              iDisplayLength = 10,
              scrollX = TRUE,
              fixedColumns = TRUE,
              fnInitComplete = JS(
                "function (settings, json){tool_header();}")
            )
          )
          output$priamprot_table <- renderDataTable(
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
          output$signalIP_table <- renderDataTable(
            signalIP,
            options = list(
              iDisplayLength = 10,
              scrollX = TRUE,
              fixedColumns = TRUE,
              fnInitComplete = JS(
                "function (settings, json){tool_header();}")
            )
          )
          output$ipr_table <- renderDataTable(
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
          output$interpro_table <- renderDataTable(
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
  observeEvent (input$ma_submit,{
    ### Save inputs from text fields, reactions #################
    creator <- input$author   
    description <- input$comment
    geneid <- input$geneid
    genecard <- input$gene_name
    org <- input$select_reaction
    prot <- input$name_protein
    ecnumber <- input$reaction

    goterm <- input$goterm
    doi <- input$doi
    url <- input$url
    
    manual_annnotation(creator, geneid, genecard, description, org, prot, ecnumber)

    # Update text field after a submition and set value to empty
    updateTextInput(session,'author', value = "")
    updateTextInput(session,'comment', value = "")
    updateTextInput(session,'gene_name', value = "")
    updateTextInput(session,'geneid', value = "0")
    updateTextInput(session,'protein', value = "")
    updateTextInput(session,'reaction', value = "")
    updateTextInput(session,'goterm', value = "")
    updateTextInput(session,'doi', value = "")
    updateTextInput(session,'url', value = "")
  })

  # Protein Annotation =======================
  observeEvent (input$ma_submit_prot,{
    ### Save inputs from text fields ########################
    # Protein to search for
    ncbiprotein <- input$variableprot
    
    # Grab variables from text boxes
    creator <- input$author_prot
    description <- input$comment_prot
    genecard <- input$gene_name_prot
    geneid <- input$gene_prot
    ecnumber <- input$reaction_prot
    prot <- input$protein_prot
    org <- input$select_protein
    
    goterm <- input$goterm_prot
    doi <- input$doi_prot
    url <- input$url_prot

    # send to the
    manual_annnotation(creator, geneid, genecard, description, org, prot, ecnumber)
    
    # Update text field after a submition and set value to empty
    updateTextInput(session,'author_prot', value = "")
    updateTextInput(session,'geneid_prot', value = "0")
    updateTextInput(session,'gene_name_prot', value = "")
    updateTextInput(session,'comment_prot', value = "")
    updateTextInput(session,'protein_prot', value = "")
    updateTextInput(session,'reaction_prot', value = "")
    
    updateTextInput(session,'goterm_prot', value = "")
    updateTextInput(session,'doi_prot', value = "")
    updateTextInput(session,'url_prot', value = "")

  })
  # Gene Annotation ========================
  observeEvent(input$ma_submit_gene,{
    ### Save inputs from text fields, reactions #################
    creator <- input$author_gene   
    description <- input$comment_gene   
    geneid <- input$gene_gene  
    genecard <- input$name_gene  
    org <- input$select_gene  
    prot <- input$name_protein
    ecnumber <- input$reaction_gene
  

    
    # Call the RGBOL API function
    manual_annnotation(creator, geneid, genecard, description, org, prot, ecnumber)
    
    # Clear the texinputs after use
    updateTextInput(session,'author_gene', value = "")
    updateTextInput(session,'comment_gene', value = "")
    updateTextInput(session,'gene_gene', value = "0")
    updateTextInput(session,'name_gene', value = "")
    updateTextInput(session,'name_protein', value = "")
    updateTextInput(session,'reaction_gene', value = "")
  })

  # output$text <- renderUI({
  #   div(HTML("<p style='color:green'>Data transfer complete</p>"))
  # })
  

  
  ### Querying and rendering Gene annotation to UI ##########################
  query <- paste("
          SELECT ?GeneID ?GeneName ?Protein ?Author ?Date ?Institute ?Comment where {
            ?gene	<http://gbol.life#number> ?GeneID .
            ?gene   <http://gbol.life#gene>	?GeneName .
            ?gene <http://gbol.life#xref> ?author .
            ?gene <http://gbol.life#provenance> ?geneProv .
            ?geneProv  <http://gbol.life#origin> ?manR .
            ?manR <http://www.w3.org/ns/prov#wasGeneratedBy> ?manActive .
            ?manActive  <http://www.w3.org/ns/prov#startedAtTime> ?Date . 
            ?author <http://gbol.life#name> ?Author .
            ?author <http://www.w3.org/ns/prov#actedOnBehalfOf> ?org .
            ?org 	<http://gbol.life#legalName> ?Institute  .
            ?gene <http://gbol.life#note>	?comment .
            ?comment <http://gbol.life#text> ?Comment .
            ?gene <http://gbol.life#xref>	?protein .
            ?protein <http://gbol.life#protein_id> ?Protein .
            ?gene <http://gbol.life#xref> ?ecnumber .
            ?ecnumber <http://gbol.life#accession> ?ECnumber .
          } 
          ",sep="")
  
  fetch_query <- SPARQL(endpoint2,query)$results

  fetch_query <-as.data.table(fetch_query)
  output$contents_gene <- renderDataTable({
    fetch_query
  })
  # Quering for a specific gene number
#   reaction_query <- paste("
#                       SELECT ?GeneID ?GeneName ?Protein ?Author ?Date ?Institute ?Comment ?url where {
#                           ?gene	<http://gbol.life#number> ",Genenumber," .
#                           #FILTER(regex (?GeneID, 100270809))
#                           ?gene <http://gbol.life#number> ?GeneID .
#                           ?gene <http://gbol.life#gene>	?GeneName .
#                           ?gene <http://gbol.life#xref> ?author .
#                           ?gene <http://gbol.life#provenance> ?geneProv .
#                           ?geneProv  <http://gbol.life#origin> ?manR .
#                           ?manR <http://www.w3.org/ns/prov#wasGeneratedBy> ?manActive .
#                           ?manActive  <http://www.w3.org/ns/prov#startedAtTime> ?Date . 
#                           ?author <http://gbol.life#name> ?Author .
#                           ?author <http://www.w3.org/ns/prov#actedOnBehalfOf> ?org .
#                           ?org 	<http://gbol.life#legalName> ?Institute  .
#                           ?gene <http://gbol.life#note>	?comment .
#                           ?comment <http://gbol.life#text> ?Comment .
#                           ?gene <http://gbol.life#xref>	?protein .
#                           ?protein <http://gbol.life#protein_id> ?Protein .
# } ")
  
}

shinyApp(ui, server)
