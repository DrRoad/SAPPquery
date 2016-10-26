# install missing pacakges if needed
list.of.packages <- c("shiny", "shinyBS","shinyjs","SPARQL","data.table","DT","stringr","plyr","rstudioapi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
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

#Source Jon Olav's hyperlink function
source('ontology_links.R')
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
      # Reavtive NavTab =================================
      tabPanel(
        "Reaction",
        # Search feild ##########################
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
        # DataTables #############################
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
                                  # SAPP Tab =================================
                                  tabPanel("SAPP",
                                           tags$div(class = "info",
                                                    tags$ul(
                                                      tags$li(""),
                                                      tags$li("")
                                                    )),
                                           DT::dataTableOutput('myTable')
                                  ),
                                  # BLAST TAB =================================
                                  tabPanel("BLAST",
                                           tags$div(class = "info",
                                                    tags$ul(
                                                      tags$li(textOutput("tool")),
                                                      tags$li(textOutput("version"))
                                                    )),
                                           
                                           DT::dataTableOutput('swissprot_table')
                                           
                                  ),
                                  # PRIAM Tab =================================
                                  tabPanel("PRIAM",
                                           tags$div(class = "info",
                                                    tags$ul(tags$li(textOutput("tool_priam")),
                                                            tags$li(textOutput("version_priam")))), 
                                           DT::dataTableOutput('priamdata_table')
                                           
                                  ),
                                  # Interpro Tab =================================
                                  tabPanel("Interpro",
                                           tags$div(class = "info",
                                                    tags$ul(tags$li(textOutput("tool_interpro")),
                                                            tags$li(textOutput("version_interpro")))), 
                                           DT::dataTableOutput('interprodata_table')
                                  ),
                                  # EnzDP Tab =================================
                                  tabPanel("EnzDP",
                                           tags$div(class = "info",
                                                    tags$ul(
                                                      tags$li(textOutput("tool_enzdp")),
                                                      tags$li(textOutput("version_enzdp"))
                                                    )),
                                           DT::dataTableOutput('enzdp_table')
                                  ),
                                  # Result Summary =================================
                                  tabPanel("Result Summary",
                                           tags$div(class = "info",
                                                    tags$ul(
                                                      tags$li(""),
                                                      tags$li("")
                                                    )),
                                           DT::dataTableOutput('resultsummarydata_table')
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
                                           value = "NP_001133193.1",
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
                                  # # Interpro Domains Tab =================================
                                  tabPanel("Interpro Domains ",
                                           tags$div(class = "info",
                                                    tags$ul(
                                                      tags$li(""),
                                                      tags$li("")
                                                    )),
                                           DT::dataTableOutput('interpro_table')
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
    
    # Sappdata ===========================
    Sappdata <- observeEvent(input$submit,{
      
      ### Clear all data.frames #############################
      results <- NULL
      results_interpro <- NULL
      results_priam <- NULL
      results_uniprot <- NULL
      swiss_table <- NULL
      enzdp_table <- NULL
      #ECnumber <- NULL
      
      ### Variable and js scripts addCLass #############################
      #shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      source('noframe.R')
      
      ECnumber <- input$variable
      #ECnumber <- '1.2.3.4'
      #ECnumber <- '5.4.2.11'
      
      # Render EC number to text
      output$text <- renderText({ECnumber})
      
      # Start the progression bar =================================
      withProgress(
        message = 'Fetching data',
        detail = 'This may take a few minutes',
        value = 0,
        {
          Sys.sleep(0.50)
          incProgress(0.1, detail = "Starting SAPP query")
          
          # Grab queries from source file
          prefixes_all <- source('prefixes_reactive.R')
          # Extract relevant data from the list.
          prefixes <- prefixes_all$value[1]
          basequery_interpro <- prefixes_all$value[2]
          basequery_priam <- prefixes_all$value[3]
          basequery_uniprot <- prefixes_all$value[4]
          basequery_enzdp <- prefixes_all$value[5]
          
          ### Query functions #############################
          queryfun <- function(basequery, ecnumber) { return(sub('4.2.1.11', ecnumber, basequery))  }
          
          results_interpro <- data.table(SPARQL(endpoint, paste(prefixes, queryfun(basequery_interpro, ECnumber)))$results)
          incProgress(0.2, detail = "Fetching Interpro data")
          
          results_priam <- data.table(SPARQL(endpoint, paste(prefixes, queryfun(basequery_priam, ECnumber)))$results)
          incProgress(0.25, detail = "Fetching PRIAM data")
          
          results_enzdp <- data.table(SPARQL(endpoint, paste(prefixes, queryfun(basequery_enzdp, ECnumber)))$results)
          incProgress(0.3, detail = "Fetching Enzdp data")
          
          results_uniprot <-data.table(SPARQL(endpoint, paste(prefixes, queryfun(basequery_uniprot, ECnumber)))$results)
          incProgress(0.4, detail = "Fetching Uniprot data")
          
          ### Check if dataframes are empty #############################
          if (empty(results_interpro) && empty(results_priam) && empty(results_uniprot) == TRUE) {
            output$exampleOutput <- renderText({
              createAlert(
                session,
                "alert",
                "exampleAlert",
                title = "No Query Found",
                content = "The number you enterd is not a valid ECnumber or a EC number wich returned an
                empty results",
                append = FALSE
              )
            })
          }
          # Build dataframes =================================
          else{
            ### Rename columns #############################
            results <- data.table(ldply(.id = "tool",list(Interpro = results_interpro,Priam = results_priam, Blast = results_uniprot,Enzdp = results_enzdp )))
            incProgress(0.6, detail = "Building tables")
            
            # in order to get nice tables we replace them.
            results[, ncbiprotein := str_match(header, '[N,X]P_[[:digit:]]+.[[:digit:]]+')]
            results[, colname := sub('>', '', sub('<http://csb.wur.nl/genome/', '', colname))]
            results[, tool := sub('>', '', sub('<http://csb.wur.nl/genome/', '', tool))]
            results[, feature := sub('>','',sub('<http://csb.wur.nl/genome/protein/[[:alnum:]]+/','',feature))]
            results <-results[!grepl('http://www.w3.org/1999/02/22-rdf-syntax-ns#type',colname)]
            results[,Ncbiprotein:=ncbiprotein]
            results[, ncbiprotein := o(paste('ncbiprotein', ncbiprotein, sep ='/'))]
            
            ### BLAST #############################
            if ("Blast" %in% results$tool) {
              swiss_table <- dcast(results[tool == 'Blast'], ncbiprotein + feature ~ colname,fun.aggregate = paste,collapse = "__")
              swiss_table[, uniprot := o(paste('uniprot', substr(subjectid, 4, 9), sep ='/'))]
              # Feature aln bitscore eval gaps mm %ident qend qstart send sstart subjectid subjectname tool version
              
              # Grab names of tool and version to display in header
              swiss_tool <- toupper(swiss_table$tool[1])
              swiss_version <- swiss_table$version[1]
              # Re-order and renmae the datatable
              swiss_table <- swiss_table[,.(ncbiprotein,uniprot,alignment_length,bitscore,evalue,gaps,mismatches,percidentity,qend,qstart,send,sstart,subjectname)]
              swiss_table <- rename(swiss_table,c("alignment_length" = "aln","evalue" = "e","mismatches" = "mm","percidentity" = "pi","bitscore" = "bs",
                                                  "qend" = "qe","send" = "se","qstart" = "qs","sstart" = "ss"))
            } else {
              swiss_tool <- toupper("N/A")
              swiss_version <- "N/A"
              # Create a "empty" dataframe needed for the rendering of the datatables.
              swiss_table <- noframe()
            }
            ### PRIAM #############################
            if ("Priam" %in% results$tool) {
              priam_table <- dcast(results[tool == 'Priam'],ncbiprotein + feature ~ colname,fun.aggregate = paste,collapse = "__")
              head(priam_table)
              priam_table <-rename(priam_table,c("<http://www.biopax.org/release/bp-level3.owl#xref" = "ECurl"))
              priam_table[, evalue := as.numeric(evalue)]
              setkey(priam_table, evalue)
              
              # Save tool and version vales for displaying
              priam_tool <- toupper(priam_table$tool[1])
              priam_version <- priam_table$version[1]
              
              # rename the headers in the priam table
              priam_table <-rename(priam_table,c("align_length" = "aln","bit_score" = "bs","evalue" = "e","is_best_overlap" = "isb","positive_hit_probability" = "php",
                                                 "profile_from" = "pf","profile_length" = "pl","profile_proportion" = "pp","profile_to" = "pt","query_from" = "qf","query_length" = "ql","query_strand" = "qs","query_to" = "qt"))
              priam_table <- priam_table[,.(ncbiprotein,ECurl,aln,bs,e,isb,php,profile_ID,pf,pl,pp,pt,qf,ql,qs,qt)]
              
            } else  {
              priam_tool <- "N/A"
              priam_version <- "N/A"
              priam_table <- noframe()
            }
            ### InterproScan #############################
            if ("Interpro" %in% results$tool) {
              interpro_table <-dcast(results[tool == 'Interpro'], ncbiprotein + feature ~ colname)
              
              # Change the analysis column slightly so the identifiers mathces the terms in identifiers.org, then link signature column
              interpro_table[analysis == 'prints', analysis := 'sprint']
              interpro_table[analysis == 'prositepatterns', analysis :='prosite']
              interpro_table[, signature := o(paste(analysis, sub('>','',sub('<http://csb.wur.nl/genome/', '', signature)), sep = '/'))]
              
              # Grab tool and version varibles
              interpro_tool <- toupper(interpro_table$tool[1])
              interpro_version <- interpro_table$version[1]
              
              # Drop the columns that are not needed
              interpro_table <- interpro_table[,.(ncbiprotein,signature,analysis,begin,end,score)]
            } else {
              interpro_tool <- "N/A"
              interpro_version <- "N/A"
              interpro_table <- noframe()
            }
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
              
            } else {
              enzdp_tool <- "N/A"
              enzdp_version <- "N/A"
              enzdp_table <- noframe()
            }
            
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
                fixedColumns = TRUE
              )
            )
            incProgress(1, detail = "Done")
                  }
          })
          })
    
    # SAPP Protein ===========================
    Sappdataprot <- observeEvent(input$submitprot, {
      
      ### Clear all data.frames #############################
      results <- NULL
      result <- NULL
      blastresults <- NULL
      signalIP <- NULL
      ipr <- NULL
      priam_table_prot <- NULL
      iprresults <- NULL
      #input$submitprot <- NULL
      
      ### Variable and js scripts addCLass #############################
      #shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      ncbiprotein <- input$variableprot
      
      ### Run the basequery #############################
      basequery <- "prefix ssb: <http://csb.wur.nl/genome/>
      select ?header ?tool ?feature ?colname ?value
      where
      {
      ?cds ssb:header ?header.
      FILTER(contains(?header,'changeme'))
      ?cds ssb:protein ?protein.
      ?protein ssb:feature ?feature.
      ?feature a ?tool.
      ?feature ?colname ?value
      }"
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
          res <- SPARQL(endpoint, queryfun(basequery,ncbiprotein))
          results <- data.table(res$results)
          
          ### Check if dataframe is empty #############################
          if (empty(results) == TRUE) {
            output$exampleOutput <- renderText({
              createAlert(
                session,
                "alert2",
                "exampleAlert2",
                title = "No Query Found",
                content = "The number you enterd is not a valid NP number or a NP number wich returned an
                empty results",
                append = FALSE
              )
            })
          }
          # Build dataframes and parse columns =================================
          else{
            ### Change names of results columns #############################
            results[,ncbiprotein:=str_match(header,'[N,X]P_[[:digit:]]+.[[:digit:]]+')]
            results[,colname:=sub('>','',sub('<http://csb.wur.nl/genome/','',colname))]
            results[,tool:=sub('>','',sub('<http://csb.wur.nl/genome/','',tool))]
            results[,feature:=sub('>','',sub('<http://csb.wur.nl/genome/protein/[[:alnum:]]+/','',feature))]
            #simple report per protein and tool, some hickups where duplicate values per feature trigger aggregation dcast
            results <- results[!grepl('http://www.w3.org/1999/02/22-rdf-syntax-ns#type',colname)] #remove, duplicate values triggers aggregation in dcast
            results[, ncbiprotein := o(paste('ncbiprotein', ncbiprotein, sep ='/'))] #TEST
            # Create a empty dataframe if needed
            if (empty(results) == TRUE){
              results <- noframe()
            }
            
            ### Blast against Swissprot and COG #############################
            incProgress(0.3, detail = "Fetching BLAST data")
            blastresults <- dcast(results[tool=='Blast'],feature~colname)
            
            #blastresults$queryid <- NULL #COG
            result <- blastresults[tool=='cog']
            
            #Swissprot
            result <- blastresults[tool=='swiss']
            result_tool <- result$tool[1]
            result_version <- result$version[1]
            result <- result[,.(ncbiprotein,alignment_length,bitscore,evalue,gaps,mismatches,percidentity,qend,qstart,send,sstart,subjectname)]
            result <- rename(result,c("alignment_length" = "aln","evalue" = "e","mismatches" = "mm","percidentity" = "pi","bitscore" = "bs",
                                      "qend" = "qe","send" = "se","qstart" = "qs","sstart" = "ss"))
            
            # Create a empty dataframe if needed
            if (empty(result) == TRUE){
              result_tool <- "N/A"
              resukt_version <- "N/A"
              result <- noframe()
            }
            
            #**SAPP feature requests**:
            # * Swissprot and COG features are type ssb:Blast, while COG features also have type ssb:Cog, please add a ssb:Swiss type to make it easier to separate results.
            #* COGs are for bacteria/archae, please add a ekuaryote tool, [EggNOG](http://eggnogdb.embl.de/) would have returned KOG2670 (search for it at the site).
            #**Shiny suggestions**:
            
            # * For now, split on the tool column (swiss/cog) and create two separate tables,
            #* remove "queryid"" and move "tool and "version" to caption
            # * Connect to Uniprot and COG to extract EC numbers and GO annotation
            
            ### Priam table created #############################
            incProgress(0.3, detail = "Fetching PRIAM data")
            priam_table_prot <- dcast(results[tool=='Priam'],feature~colname)
            priam_table_prot <-rename(priam_table_prot,c("<http://www.biopax.org/release/bp-level3.owl#xref" = "xref"))
            priam_tool_prot <- toupper(priam_table_prot$tool[1])
            priam_version_prot <- priam_table_prot$version[1]
            
            # rename the headers in the priam table
            priam_table_prot <-rename(priam_table_prot,c("align_length" = "aln","bit_score" = "bs","evalue" = "e","is_best_overlap" = "isb","positive_hit_probability" = "php",
                                                         "profile_from" = "pf","profile_length" = "pl","profile_proportion" = "pp","profile_to" = "pt","query_from" = "qf","query_length" = "ql","query_strand" = "qs","query_to" = "qt"))
            priam_table_prot <- priam_table_prot[,.(ncbiprotein,xref,aln,bs,e,isb,php,profile_ID,pf,pl,pp,pt,qf,ql,qs,qt)]
            
            # Create a empty dataframe if needed
            if (empty(priam_table_prot) == TRUE){
              priam_tool_prot <- "N/A"
              priam_version_prot <- "N/A"
              priam_table_prot <- noframe()
            }
            
            #**Shiny suggestions:**
            #* Reorder columns and remove duplicate info, move "tool and "version" to caption
            
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
            ipr_query <- "prefix ssb: <http://csb.wur.nl/genome/>
            prefix biopax3: <http://www.biopax.org/release/bp-level3.owl>
            select *
            where
            {
            ?signature a ssb:SignatureAccession.
            VALUES ?signature { domains }
            ?signature ssb:interpro_description ?interpro_description;
            #ssb:signature_description ?signature_description;
            #ssb:unipathway ?unipathway;
            <http://www.biopax.org/release/bp-level3.owl#xref> ?xref
            }"
            ### Interproscan Domains #############################
            incProgress(0.6, detail = "Fetching Interpro domain data")
            iprres <- SPARQL(endpoint,sub('domains',paste(iprdomains,collapse=' '),ipr_query))
            iprresults <- data.table(iprres$results)
            iprresults[grep('identifiers.org/go',xref),xreftype:='GOterm']
            iprresults[grep('identifiers.org/ec-code',xref),xreftype:='ECnumber']
            iprresults[grep('identifiers.org/interpro',xref),xreftype:='IPRdomain']
            
            # Create a empty dataframe if needed
            if (empty(iprresults) == TRUE){
              iprresults <- noframe()
            }
            
            ### Try the Tmhm #############################
            #try(tmhmm <- dcast(results[tool=='Tmhmm'],feature~colname))
            
            ### SignalP #############################
            try(signalIP <- dcast(results[tool=='SignalP'],feature~colname))
            signalIP_tool <- signalIP$tool[1]
            signalIP_version <- signalIP$version[1]
            signalIP <- signalIP [,.( feature,cmax,cpos,d,dmaxcut,network,signal,smax,smean,spos,ymax,ypos)]
            signalIP <- rename(signalIP, c('cpos' = 'c', 'dmaxcut'= "dmc",'signal'='s') )
            
            ### Isolate the variables to be renderd #############################
            # Send the versions and tools name to header
            isolate ({
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
  }
  
  shinyApp(ui, server)
  }