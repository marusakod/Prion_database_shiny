
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(org.Mm.eg.db)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(ggrepel)
library(openxlsx)
library(DT)

source('loadData.R')

# ======================================================================================================================
#                             USER INTERFACE
#=======================================================================================================================

#---- HEADER-----------------------------------------------------------------------------

header <- dashboardHeader(title = "Prion Database",
                          tags$li(a(href = 'https://doi.org/10.1101/2023.10.31.564879',
                                    img(src = 'www/extraneural_logo.png',
                                        title = "Extraneural paper", height = "35px"),
                                    style = "padding-top:10px; padding-bottom:10px;"),
                                  class = "dropdown"), 
                          tags$li(a(href = 'https://doi.org/10.1371/journal.ppat.1008653',
                                    img(src = 'www/brain_logo.png',
                                        title = "Brain paper", height = "35px"),
                                    style = "padding-top:10px; padding-bottom:10px;"),
                                  class = "dropdown"),
                          tags$li(class = "dropdown",
                                  tags$style(".main-header {height: 55px}"),
                                  tags$style(".main-header .logo {height:55px}")
                          )
                          
)


#---- SIDEBAR ---------------------------------------------------------------------------

## Sidebar content
sidebar <-   dashboardSidebar(
  #useShinyjs(),
  sidebarMenu(id = "tabs",
              menuItem("Home", tabName = "description", icon = icon("house")),
              menuItem("Gene search", tabName = "genes", icon = icon("search"),
                       menuSubItem("Global gene expression view", tabName = "multiple_genes"),
                       menuSubItem("Single gene view", tabName = "one_gene")),
              menuItem("Download", tabName = "download", icon = icon("download"),
                       menuSubItem("Gene expression data", tabName = "download_expr"))
  )
)

#---- BODY -------------------------------------------------------------------------------

body <- ## Body content
  dashboardBody(
    #useShinyjs(),
    # make buttons on welcome screen clickable (and simultaneously update active menu panel)
    # tags$script(HTML("
    # var openTab = function(tabName){
    #  $('a', $('.sidebar')).each(function() {
    #   if(this.getAttribute('data-value') == tabName) {
    #      this.click()
    #   };
    #     });
    #      }
    #")),
    
    
    #switchInput color while on  --> match switchInput colors to the colors in the plot
    
    tags$head(tags$style(HTML('.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-danger,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-danger {
                                        background: #fe4a49;
                                        color: white;
                                        }'))),
    tags$head(tags$style(HTML('.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-success,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-success {
                                        background: #2ab7ca;
                                        color: white;
                                        }'))),
    tags$head(tags$style(HTML('.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-primary,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-primary {
                                        background: #fed766;
                                        color: white;
                                        }'))),
    tags$head(tags$style(HTML('.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-warning,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-warning {
                                        background: #B464C1;
                                        color: white;
                                        }'))),
    
    
    tabItems(
  
      tabItem(tabName = "description",
              fluidPage(
                column(width = 12,
                       div(h2(strong("Welcome to The Prion Database")), style = "text-align: center; color:#2F8BAE"),
                       #div(h3("-insert subtititle here-"), style = "text-align: center;"),
                       br()),
                column(width = 6, 
                       br(),
                       div( img(src='www/experiment_mouse.png', height="100%", width="100%"), style="text-align: center;"),
                       br()),
                column(width = 6,
                       p("Prion diseases (PrDs) are fatal neurodegenerative diseases caused by transmissible proteinaceous particles termed prions (PrPSc). Prions primarily consist of the misfolded form of the cellular prion protein (PrPC) incorporated into fibrillary β-sheet-rich structures, which are termed ‘amyloids’. Although prions primarily accumulate in the central nervous system leading to severe pathophysiological brain-related symptoms, they can be taken up by the digestive system, followed by accumulation in lymphoid tissue and neuroinvasion via peripheral nerves. Furthermore, prions can be present in the blood, which represents an efficient route of infection. Here, we present a searchable database of transcriptomic changes in the brain (hippocampus), blood, hindlimb skeletal muscle, and spleen during the entire life span of mice after intracerebral exposure to prions (4, 8, 12, 14, 16, 18 and 20 weeks-post-inoculation and terminal stage).", style="text-align: justify; font-size:18px;"),
                       br(),
                       p("User can navigate to the", span(strong("Gene search")), span("tab and select one of the following sections:"), style="text-align: justify; font-size:18px;"), 
                       tags$div(
                         tags$ul(
                           tags$li(actionLink("link_to_tabitem_multiple_genes", "Global gene expression view"), "tab generates volcano plots with information on differentially expressed across all selected organs and time points.", style="text-align: justify; font-size:18px;"), 
                
                           tags$li(actionLink("link_to_tabitem_one_gene", "Single gene view"), "tab allows the user to select a gene of interest and explore how its expression changes over time in selected organs.", style="text-align: justify; font-size:18px;")
                         )
                       ), 
                       br(),
                       p("In the", span(strong("Download tab")), "user can select", actionLink("link_to_tabitem_expression_data", "Gene expression data"), "to download either complete or filtered datasets.",  style="text-align: justify; font-size:18px;")
                       
                       
                ),
                tags$footer("This tool was developed by the Aguzzi lab at the", 
                            a("University of Zürich", href = "https://www.stroke.uzh.ch/en/team0/Adriano-Aguzzi.html"), 
                            br(), 
                            "Code is available through",
                            a("Github", href = "https://github.com/marusakod/Prion_database_shiny"), 
                            align = "right", 
                            style = "position:fixed; bottom:0; width:120%; right:5px; height:60px; color: black; padding: 10px; background-color: #D5DBDB; font-size:14px; text-align: right;"))),
      tabItem(tabName = "one_gene", 
              fluidRow(
                column(width = 12, 
                       box(width = 0, 
                           fluidPage(column(width= 11, 
                                            p("In the", strong("'Parameters'"), "section, user can search genes by Ensembl ID or gene Symbol. The expression levels of the selected gene throughout disease progression are displayed in the",
                                              strong("'Gene expression'"), "section with colors representing different organs. Points which meet the criteria for differential expression specified with", 
                                              strong("'Significance filter"), "and", strong("'Significance threshold'"), "parameters are marked with a triangle", 
                                              style = "text-align: justify; font-size:16px;")), 
                                     column(width = 1, 
                                            div(img(src='www/info.png', height="60%", width="60%"), style="text-align: center;"))))), 
                column(width = 3,
                       box(width = 0,
                           title = "Parameters",
                           solidHeader = TRUE,
                           status = "warning",
                           awesomeRadio(
                             inputId = "search_gene",
                             label = div(style = "font-size: 14px", "Search genes by:"),
                             choices = c("Gene Symbol" = 1, "Ensembl ID" = 2),
                             selected =  1,
                             inline = FALSE
                           ),
                           conditionalPanel(
                             "input.search_gene == 1",
                             selectizeInput(
                               inputId = "Entry_gene",
                               label = div(style = "font-size: 14px", "Gene Symbol"),
                               multiple = FALSE,
                               size = 20,
                               choices = NULL,
                               width = "200px",
                               options = list(
                                 placeholder = 'eg. Agap2',
                                 onInitialize = I('function() { this.setValue("");}')
                               )
                             )
                           ),
                           conditionalPanel(
                             "input.search_gene == 2",
                             selectizeInput(
                               inputId = "Entry_ID",
                               label = div(style = "font-size: 14px","Ensembl gene ID"),
                               multiple = FALSE,
                               size = 20,
                               choices = NULL,
                               width = "200px",
                               options = list(
                                 placeholder = 'eg. ENSMUS...',
                                 onInitialize = I('function() { this.setValue(""); }')
                               )
                             )
                           ),
                           
                           awesomeRadio(inputId = "one_gene_signif_filter",
                                        label = div(style = "font-size: 14px", "Significance filter"),
                                        choices = c("FDR" = "FDR", "p-value (unadjusted)" = "pvalue"),
                                        selected = "FDR",
                                        inline = FALSE),
                           awesomeRadio(inputId = "FDR_thr",
                                        label = div(style = "font-size: 14px", "Significance threshold"),
                                        choices = c("0.1" = 0.1,
                                                    "0.05" = 0.05,
                                                    "0.01" = 0.01,
                                                    "0.001" = 0.001,
                                                    "10-4" = 0.0001,
                                                    "10-5" = 0.00001),
                                        selected = 0.05),
                           
                           splitLayout(
                             switchInput(inputId = "muscle",
                                         label = "muscle",
                                         value = TRUE,
                                         onStatus = "danger"),
                             switchInput(inputId = "brain",
                                         label = "brain",
                                         value = TRUE,
                                         onStatus = "success")),
                           splitLayout(
                             switchInput(inputId = "spleen",
                                         label = "spleen",
                                         value = TRUE,
                                         onStatus = "primary"),
                             switchInput(inputId = "blood",
                                         label = "blood",
                                         value = TRUE, 
                                         onStatus = "warning"))
                       )),
                column(width = 9,
                       box(width = 0,
                           title = "Gene expression",
                           solidHeader = TRUE,
                           status = "warning",
                           plotOutput("gene_in_organs",
                                      width = "1000px",
                                      height = "500px",
                                      hover = hoverOpts("plot_hover", delay = 300, delayType = "debounce")),
                           uiOutput("hover_info"),
                           downloadBttn(outputId = "downloadPlot",
                                        label = "Download",
                                        style = "material-flat",
                                        color = "primary",
                                        size = "sm",
                                        icon = icon("download")))),
                # column(width = 12, 
                #        box(width = 0,
                #            title = "Alternative splicing",
                #            solidHeader = TRUE,
                #            status = "warning",
                #            uiOutput("get_splicing_out"))),
                # column(width = 12, uiOutput("get_splice_graph"))
                
              )),
      
      # GENE EXPRESSION DATA DOWNLOAD TAB-----------------------------------------------------------------------------------------------------
      
      tabItem(tabName = "download_expr",
              fluidRow(
                column(width = 3,
                       box(width = 0,
                           awesomeRadio(inputId = "expr_down_opts",
                                        label = "Download options",
                                        choices = c("Download entire dataset", "Select filters"),
                                        selected = "Download entire dataset"), 
                           conditionalPanel("input.expr_down_opts == 'Select filters'",
                                            pickerInput("down_exp_organs_filter","Select organs",
                                                        choices= levels(all_organs_data_list$organ),
                                                        options = list(`actions-box` = TRUE),
                                                        multiple = T,
                                                        width = "200px"),
                                            
                                            pickerInput("down_expr_time_filter", "Select timepoints",
                                                        choices = levels(all_organs_data_list$timepoint),
                                                        options = list(`actions-box` = TRUE),
                                                        multiple = TRUE,
                                                        width = "200px"),
                                            awesomeRadio(
                                              inputId = "search_gene_expr_down",
                                              label = div(style = "font-size: 14px", "Filter genes by:"),
                                              choices = c("Gene Symbol" = 1, "Ensembl ID" = 2),
                                              selected =  1,
                                              inline = FALSE
                                            ),
                                            conditionalPanel(
                                              "input.search_gene_expr_down == 1",
                                              selectizeInput(
                                                inputId = "Entry_gene_expr_down",
                                                label = div(style = "font-size: 14px", " Select Gene Symbol"),
                                                multiple = TRUE,
                                                size = 20,
                                                choices = NULL,
                                                width = "200px",
                                                options = list(
                                                  placeholder = 'eg. Agap2',
                                                  onInitialize = I('function() { this.setValue("");}')
                                                )
                                              )
                                            ),
                                            conditionalPanel(
                                              "input.search_gene_expr_down == 2",
                                              selectizeInput(
                                                inputId = "Entry_ID_expr_down",
                                                label = div(style = "font-size: 14px","Select Ensembl gene ID"),
                                                multiple = TRUE,
                                                size = 20,
                                                choices = NULL,
                                                width = "200px",
                                                options = list(
                                                  placeholder = 'eg. ENSMUS...',
                                                  onInitialize = I('function() { this.setValue(""); }')
                                                )
                                              )
                                            ),
                                            
                                            awesomeRadio("signif_filter_down_exp",
                                                         "Significance filter", 
                                                         choices = c("FDR" = "FDR", "p-value (unadjusted)" = "pvalue"),
                                                         selected = "FDR",
                                                         inline = FALSE),
                                            awesomeRadio(inputId = "FDR_thr_down_exp",
                                                         label = div(style = "font-size: 14px", "Significance Threshold"),
                                                         choices = c("no filter" = 2,
                                                                     "0.1" = 0.1,
                                                                     "0.05" = 0.05,
                                                                     "0.01" =  0.01,
                                                                     "0.001" = 0.001,
                                                                     "10-4" = 0.0001,
                                                                     "10-5" = 0.00001),
                                                         selected = 0.05),
                                            sliderInput(inputId = "absLFC_thr_down_exp",
                                                        label = div(style = "font-size: 14px", "Absolute Fold Change Threshold"),
                                                        value = 0.5,
                                                        min = 0,
                                                        max = 5,
                                                        step = 0.5,
                                                        ticks = TRUE,
                                                        width = "200px")),
                           pickerInput(inputId = "expr_down_filetype",
                                       label = "Select file type",
                                       choices = c("csv", "xlsx", "tsv", "rds"),
                                       selected = "csv",
                                       multiple = FALSE),
                           conditionalPanel("input.expr_down_opts == 'Select filters'",
                                            splitLayout(
                                              prettyCheckbox(inputId = "see_filtered_expr",
                                                             label = "PREVIEW DATA",
                                                             value = FALSE,
                                                             status = "warning",
                                                             icon = icon("eye"), 
                                                             plain = TRUE,
                                                             bigger = TRUE,
                                                             shape = "curve",
                                                             animation = "smooth",
                                                             outline = TRUE),
                                              downloadBttn(outputId = "down_filtered_expr",
                                                           label = "Download data",
                                                           style = "material-flat",
                                                           color = "primary",
                                                           size = "sm",
                                                           icon = icon("download")))),
                           conditionalPanel(condition = "input.expr_down_opts == 'Download entire dataset'",
                                            splitLayout(
                                              prettyCheckbox(inputId = "see_total_expr",
                                                             label = "PREVIEW DATA",
                                                             value = FALSE,
                                                             status = "warning",
                                                             icon = icon("eye"), 
                                                             plain = TRUE,
                                                             bigger = TRUE, 
                                                             shape = "curve",
                                                             animation = "smooth",
                                                             outline = TRUE),
                                              
                                              
                                              downloadBttn(outputId = "down_total_expr",
                                                           label = tags$span(id = "label_down_total_expr", "Download"),
                                                           style = "material-flat",
                                                           color = "primary",
                                                           size = "sm",
                                                           icon = icon("download"))),
                                            bsPopover("see_total_expr", title="Entire dataset preview", placement="bottom", options = list(container = "body"),
                                                      content="The entire dataset contains 1357312 rows. Only 20 rows will be displayed in preview."))
 
                       )),
                
                column(width = 9, uiOutput("get_box_for_preview")))),
      
    
      # GLOBAL GENE VIEW TAB------------------------------------------------------------------------------------------------
      
      tabItem(tabName = "multiple_genes",
              fluidRow(
                column(width = 7,
                       box(width = 0,
                           title = "Volcano plot parameters",
                           solidHeader = TRUE,
                           status = "warning",
                           fluidPage(
                             column(width = 4,
                                    conditionalPanel(
                                      condition = "input.groupBy == 'Organ'",
                                      pickerInput("global_select_one_organ", "Select organ",
                                                  choices = levels(all_organs_data_list$organ),
                                                  multiple = TRUE,
                                                  options = pickerOptions(maxOptions = 1),
                                                  selected = NULL,
                                                  width = "200px")
                                    ),
                                    conditionalPanel(
                                      condition = "input.groupBy == 'Time point'",
                                      pickerInput("global_select_one_tp", "Select timepoint",
                                                  choices = levels(all_organs_data_list$timepoint),
                                                  multiple = TRUE,
                                                  options = pickerOptions(maxOptions = 1),
                                                  selected = NULL, 
                                                  width = "200px")),
                                    conditionalPanel(
                                      condition = "input.groupBy == 'Time point'",
                                      pickerInput("global_select_organs","Select organs",
                                                  choices= levels(all_organs_data_list$organ),
                                                  options = list(`actions-box` = TRUE),
                                                  multiple = T,
                                                  width = "200px")),
                                    conditionalPanel(
                                      condition = "input.groupBy == 'Organ'",
                                      pickerInput("global_select_timepoints", "Select timepoints",
                                                  choices = levels(all_organs_data_list$timepoint),
                                                  options = list(`actions-box` = TRUE),
                                                  multiple = TRUE,
                                                  width = "200px")),
                                    awesomeRadio("groupBy",
                                                 "Group by:", 
                                                 choices = c("Organ", "Time point"),
                                                 selected = "Time point",
                                                 inline = FALSE),
                                    
                                    prettyCheckbox(inputId = "setScales",
                                                   label = "Fixed scales",
                                                   value = FALSE,
                                                   shape = "curve",
                                                   animation = "smooth",
                                                   icon = icon("check"),
                                                   status = "primary")),
                             column(width = 4,
                                    awesomeRadio("signif_filter_global",
                                                 "Significance filter", 
                                                 choices = c("FDR" = "FDR", "p-value (unadjusted)" = "pvalue"),
                                                 selected = "FDR",
                                                 inline = FALSE),
                                    
                                    awesomeRadio(inputId = "FDR_thr_global",
                                                 label = div(style = "font-size: 14px", "Significance threshold"),
                                                 choices = c("0.1" = 0.1,
                                                             "0.05" = 0.05,
                                                             "0.01" = 0.01,
                                                             "0.001" = 0.001,
                                                             "10-4" = 0.0001,
                                                             "10-5" = 0.00001),
                                                 selected = 0.05),
                                    sliderInput(inputId = "absLFC_thr_global",
                                                label = div(style = "font-size: 14px", "Absolute Fold Change Threshold"),
                                                value = 0.5,
                                                min = 0,
                                                max = 5,
                                                step = 0.5,
                                                ticks = TRUE,
                                                width = "200px")),
                             column(width = 4,
                                    prettyCheckbox(inputId = "ifAnno",
                                                   label = "Annotate genes",
                                                   value = FALSE,
                                                   shape = "curve",
                                                   animation = "smooth",
                                                   icon = icon("check"),
                                                   status = "primary"),
                                    conditionalPanel(condition = "input.ifAnno == 1", 
                                                     selectizeInput(
                                                       inputId = "GenesToAnno",
                                                       label = div(style = "font-size: 14px", "Select genes to annotate"),
                                                       multiple = TRUE,
                                                       size = 20,
                                                       choices = NULL,
                                                       width = "200px",
                                                       options = list(
                                                         placeholder = 'eg. Agap2',
                                                         onInitialize = I('function() { this.setValue("");}')
                                                       )
                                                     )),
                                    
                                    actionBttn("new_plot", "Update plot", icon("redo"), size = "sm", style = "material-flat", block = FALSE, color = "primary" 
                                               # style="color: #fff; background-color: #337ab7; border-color: #2e6da4; display:center-align"
                                    ),
                                    
                                    bsPopover("new_plot", title="Update volcano plot", placement="bottom", options = list(container = "body"),
                                              content="This will generate a new plot with speficied parameters")
                                    
                                    
                             )))),
                
                column(width = 5, align = "center",
                       box(width = 0,
                           fluidPage(
                             column(
                               width = 9,
                               p("In the", strong("‘Volcano plot parameters’"), "section, the user can select",
                                 strong("‘Group by: Organ’"), "to visualize gene expression changes at different selected timepoints in a particular organ. In contrast,", 
                                 strong("‘Group by: Time point’"), "allows the user to display the changes in different organs at a particular time point. Checking the box", 
                                 strong("‘Fixed scales’"), "will generate a faceted volcano plot with a shared x and y scale. User can select different thresholds for defining differentially expressed genes by adjusting the", 
                                 strong("‘Significance filter’"), "," ,strong("‘Significance threshold’"), "and", strong("‘Absolute Fold Change Threshold’"), "parameters. Gene/s of interest can be displayed on the volcano plot by checking the box",
                                 strong("‘Annotate genes’"), "and selecting gene symbols from a dropdown menu. Information on all annotated genes will be displayed in a table underneath the volcano plot upon plotting.",
                                 style="text-align: justify; font-size:16px;")), 
                             column(width = 3,
                                    div(img(src='www/info.png', height="80%", width="80%"), style="text-align: center;"))))), 
                
                
                column(width = 12, align = "center", uiOutput("get_volcano_box")
                       # box(width = 0, 
                       # height = 700,
                       #title = "Volcano plot",
                       #solidHeader = TRUE,
                       #status = "warning", 
                       #plotOutput( "volcano"),
                       #uiOutput("get_the_item"))
                       
                       
                ),
                column(width = 12, uiOutput("get_table_box"))  
              )
              
              
              
              
      )))
      

# combine UI elements
ui <- function(){
  addResourcePath("www", "www")
  tagList(
    shinydashboard::dashboardPage(useShinyjs(),
                                  header = header, sidebar = sidebar, body = body),
    
  )
  
  
}

# ======================================================================================================================
#                                                SERVER
#=======================================================================================================================


server <- function(input, output, session){
  
  # when an action link is clicked in the home tab, switch to the right menuItem
  
  observeEvent(input$link_to_tabitem_multiple_genes, {
    newvalue <- "multiple_genes"
    updateTabItems(session, "tabs", newvalue)
  })
  
  observeEvent(input$link_to_tabitem_multiple_splices, {
    newvalue <- "multiple_splices"
    updateTabItems(session, "tabs", newvalue)
  })
  
  observeEvent(input$link_to_tabitem_one_gene, {
    newvalue <- "one_gene"
    updateTabItems(session, "tabs", newvalue)
  })
  
  observeEvent(input$link_to_tabitem_expression_data, {
    newvalue <- "download_expr"
    updateTabItems(session, "tabs", newvalue)
  })
  
  observeEvent(input$link_to_tabitem_splicing_data, {
    newvalue <- "download_spl"
    updateTabItems(session, "tabs", newvalue)
  })
  
  
  #SINGLE GENE VIEW TAB ------------------------------------------------------
  updateSelectizeInput(session  = session,
                       inputId  = "Entry_gene",
                       server   = TRUE,
                       choices  = gene_names_vector,
                       selected = NULL,
                       options  = list(placeholder = 'eg. Agap2', onInitialize = I('function() { this.setValue(""); }')))
  
  
  
  updateSelectizeInput(session   = session,
                       inputId  = "Entry_ID",
                       server   = TRUE,
                       choices  = ensembl_IDs_vector,
                       selected = NULL,
                       options  = list(placeholder = 'eg. EMSMUS000...', onInitialize = I('function() { this.setValue(""); }')))
  
  #when search_gene input switches reset inputs for gene selection
  observeEvent(input$search_gene, {
    shinyjs::reset("Entry_gene")
    shinyjs::reset("Entry_ID")
  })
  
  #output$preview_table <- renderDataTable({
  #data_preview()
  #})
  
  Entry_variable <- reactiveVal()
  
  observeEvent(input$Entry_gene, {
    Entry_variable(input$Entry_gene)
  }) # manual single gene selection (symbol) #
  
  observeEvent(input$Entry_ID, {
    Entry_variable(input$Entry_ID)
  }) # manual single gene selection (ensembl) #
  
  
  # define a vector with organs selected based on switch inputs
  
  organ_vec <- reactive({
    v <- vector(mode = "character", length = 4)
    
    if(input$muscle == TRUE){
      v[1] <- "muscle"
    }
    
    
    if(input$brain == TRUE){
      v[2] <- "brain"
    }
    
    if(input$spleen == TRUE){
      v[3] <- "spleen"
    }
    
    if(input$blood == TRUE){
      v[4] <- "blood"
    }
    
    v
  })
  
  color_vec <- reactive({
    o <- organ_vec()
    col <- c('#fe4a49', '#2ab7ca', '#fed766', '#B464C1')
    names(col) <- c("muscle", "brain", "spleen", "blood")
    col <- col[which(o != "")]
    col
  })
  
  
  
  
  
  # ------- generates table for plotting  ---------------------
  
  single_gene_table <- reactive({
    o <- organ_vec()
    
    if(input$search_gene == 1) {
      
      a <- all_organs_data_list %>% dplyr::filter(gene_name == Entry_variable()) %>% filter(organ %in% o[which(o != "")])
      
    }else{
      
      a <- all_organs_data_list %>% dplyr::filter(Identifier == Entry_variable()) %>% filter(organ %in% o[which(o != "")])
      
    }
    
    a
    
  })
  
  #_____________________________________________________________
  
  
  vals <- reactiveValues()
  
  
  
  output$gene_in_organs  <- renderPlot({
    
    col <- color_vec()
    org <- organ_vec()
    t <- single_gene_table()
    
    if(nrow(t) != 0){
      
      p <- ggplot(data = t, aes(x = timepoint, y = log2.Ratio, color = organ, group = organ))+
        geom_point(size = 3) 
      
      
    }else{
      
      p <- ggplot(data = all_organs_data_list %>% dplyr::group_by(timepoint) %>% dplyr::sample_n(size = 1),
                  aes(x = timepoint, y = log2.Ratio))+
        ylim(-3,3)+
        geom_hline(yintercept = 0, linetype ="dashed", color = "#808B96", size = 0.5) +
        geom_blank()
    }
    
    
    
    p <- p + theme_light() +
      geom_line(show.legend = FALSE)+
      geom_point(data = t %>% filter(get(input$one_gene_signif_filter) <= as.numeric(input$FDR_thr)), 
                 mapping = aes(color = organ, group = organ),
                 shape = 2,
                 size = 6,
                 stroke = 2,
                 show.legend = FALSE) +
      scale_color_manual(values = col, breaks = org[which(org != "")])+
      geom_hline(yintercept = 0, linetype ="dashed", color = "#808B96", size = 0.5) +
      labs(x ="Time point", y = "log2 Fold Change", color = NULL) +
      ggtitle(paste("Selected gene:", Entry_variable())) +
      theme(title = element_text(size = 16),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            axis.text.x = element_text(size = 12),
            legend.text = element_text(size = 14)) +
      guides(color = guide_legend(override.aes = list(size = 10))) 
    
    
    vals$p <- p
    
    p
    
  })
  
  
  output$hover_info <- renderUI({
    
    t <- single_gene_table()
    
    if(nrow(t) == 0){
      hover <- ""
      hover
    }else{
      hover <- input$plot_hover
      point <- nearPoints(single_gene_table(), hover, threshold = 10, maxpoints = 1, addDist = TRUE)
      if(nrow(point) == 0) return(NULL)
      
      left_px <- hover$coords_css$x
      top_px <- hover$coords_css$y
      
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      wellPanel(
        style = style,
        p(HTML(paste0("<b> organ: </b>", point$organ, "<br/>",
                      "<b> timepoint: </b>", point$timepoint, "<br/>",
                      "<b> p-value (unadjusted): </b>", point$pvalue, "<br/>",
                      "<b> FDR: </b>", point$FDR, "<br/>",
                      "<b> L2FC: </b>", point$log2.Ratio, "<br/>")))
      )
    }
  })
  
  
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste(Entry_variable(), '.png', sep = '')},
    
    content = function(file){
      png(file, width = 1000, height = 500, units = "px")
      print(vals$p)
      dev.off()
    }
  )
  

  
  # update selectize input which allows user to select genes to annotate
  updateSelectizeInput(session  = session,
                       inputId  = "GenesToAnno",
                       server   = TRUE,
                       choices  = gene_names_vector,
                       selected = character(0),
                       options  = list(placeholder = 'eg. Agap2', onInitialize = I('function() { this.setValue(""); }')))
  
  # make a table for volcano plots based on selected FDR and L2FC thresholds
  
  volcano_table <- reactive({
    all_organs_data_list %>% mutate(gene_type = case_when(log2.Ratio >= input$absLFC_thr_global & get(input$signif_filter_global) <= as.numeric(input$FDR_thr_global) ~ "Upregulated",
                                                          log2.Ratio <= -input$absLFC_thr_global & get(input$signif_filter_global) <= as.numeric(input$FDR_thr_global) ~ "Downregulated",
                                                          TRUE ~ "Nonsignificant")) 
  })
  
  # table with all annotated genes:
  
  all_annotated <- reactive({
    anno_table <- volcano_table() %>%
      dplyr::filter(gene_name %in% input$GenesToAnno) %>% 
      dplyr::select(gene_name, Identifier, biotypes, description, pvalue, FDR, log2.Ratio, organ, timepoint, gene_type) 
    
    if(input$groupBy == "Organ"){
      anno_table <- anno_table %>% filter(organ == input$global_select_one_organ) %>%
        filter(timepoint %in% input$global_select_timepoints)
    }else{
      anno_table <- anno_table %>% filter(timepoint == input$global_select_one_tp) %>%
        filter(organ %in% input$global_select_organs)
      
    }
    anno_table
  })
  
  # vector with genes that have FDR = NA
  
  #no_fdr_genes <- reactive({
  # no_fdr_genes_vec <- all_annotated() %>% filter(is.na(FDR)) %>% select(gene_name) %>% flatten_chr()
  #  no_fdr_genes_vec
  # })
  
  # table with up-regulated annotated genes
  
  upregulated_anno <- reactive({
    up_anno_table <- all_annotated() %>% filter(gene_type == "Upregulated")
    up_anno_table
  })
  
  # table with down-regulated annotated genes
  
  downregulated_anno <- reactive({
    down_anno_table <- all_annotated() %>% filter(gene_type == "Downregulated")
    down_anno_table
  })
  
  # table with nonsignificant annotated genes
  
  nonsignificant_anno <- reactive({
    nons_anno_table <- all_annotated() %>% filter(gene_type == "Nonsignificant")
    nons_anno_table 
  })
  
  
  volcano_y_title <- reactive({
    if(input$signif_filter_global == "FDR"){
      y_axis_title_global <- "FDR"
    }else{
      y_axis_title_global <- "p-value"
    }
    y_axis_title_global
  })
  
  empty_volcano <- reactive({
    
    
    ggplot(data = all_organs_data_list %>% dplyr::sample_n(size = 1),
           aes(x = log2.Ratio, y = -log10(get(input$signif_filter_global))))+
      theme_light() +
      xlim(-5,5)+
      ylim(0,3) +
      geom_blank() +
      labs(x = expression("Log"[2]*" Fold Change"), y = bquote(-Log[10]*.(volcano_y_title()))) +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 12))
    
  })
  
  
  
  
  volcano_cols <- reactive({
    
    input$new_plot
    isolate({
      if(input$groupBy == "Time point"){
        if(is.null(input$global_select_one_tp)|is.null(input$global_select_organs)){
          return(1)
        }else{
          n_facets <- length(input$global_select_organs)
          return(n_facets)
        }
      }else{
        if(is.null(input$global_select_one_organ)|is.null(input$global_select_timepoints)){
          return(1)
        }else{
          n_facets <- length(input$global_select_timepoints)
          if(n_facets > 4){
            n_facets <- 4
          }
          return(n_facets)
        }
      }
    })
  })
  
  
  volcano_rows <- reactive({
    
    input$new_plot
    isolate({
      if(input$groupBy == "Time point"){
        return(1)
      }else{
        if(is.null(input$global_select_one_organ)|is.null(input$global_select_timepoints)){
          return(1)
        }else{
          n_rows <- length(input$global_select_timepoints)/4
          return(ceiling(n_rows))
        }
      }
    })
  })
  
  
  volcano_width_vec <- c(500, 900, 1300, 1400)
  
  volcano_height_vec <- c(500, 650)
  
  
  
  
  volcano_plot <- reactive({
    input$new_plot
    isolate({
      
      if(input$setScales == 1){
        s <- "fixed"
      }else{
        s <- "free"
      }
      
      if(input$groupBy == "Organ"){
        if(is.null(input$global_select_one_organ)|is.null(input$global_select_timepoints)){
          #display empty volcano plot
          print(empty_volcano())
        }else{
          
          v_table <- volcano_table() %>% filter(organ == input$global_select_one_organ, timepoint %in% input$global_select_timepoints) %>% drop_na(input$signif_filter_global)
          n_facet_cols <- length(input$global_select_timepoints)
          if(n_facet_cols > 4){
            n_facet_cols <- 4
          }
          
          vp <- ggplot(data = v_table, aes(x = log2.Ratio, y = -log10(get(input$signif_filter_global)))) +
            theme_light() +
            geom_point(aes(colour = gene_type),
                       alpha = 0.8,
                       shape = 16,
                       size = 1) +
            facet_wrap(~ timepoint, scales = s, ncol = volcano_cols()) +
            geom_hline(yintercept = -log10(as.numeric(input$FDR_thr_global)),
                       linetype = "dashed") + 
            geom_vline(xintercept = c(-input$absLFC_thr_global, input$absLFC_thr_global),
                       linetype = "dashed") +
            scale_colour_manual(values =  c("Upregulated" = "#ffad73", "Downregulated" = "#26b3ff", "Nonsignificant" = "grey")) + 
            guides(colour = guide_legend(override.aes = list(size= 4))) +
            labs(x = expression("Log"[2]*" Fold Change"),
                 y = bquote(-Log[10]*.(volcano_y_title())),
                 colour = NULL, 
                 title = paste("Selected organ: ", input$global_select_one_organ, sep = "")) +
            theme(strip.text = element_text(size = 14, colour = "black"),
                  legend.text = element_text(size = 14),
                  axis.title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  title = element_text(size = 16),
                  legend.position = "bottom",
                  legend.direction = "horizontal")
          
          # if checkbox Annotate genes is clicked and select input is not empty, add annotated gene labels to plot 
          if(input$ifAnno == TRUE & !is.null(input$GenesToAnno)){
            vp <- vp +  geom_point(data = downregulated_anno() %>% drop_na(input$signif_filter_global),
                                   shape = 21,
                                   size = 2,
                                   fill = "steelblue",
                                   colour = "black") +
              geom_point(data = upregulated_anno() %>% drop_na(input$signif_filter_global),
                         shape = 21,
                         size = 2,
                         fill = "firebrick",
                         colour = "black") +
              geom_point(data = nonsignificant_anno() %>% drop_na(input$signif_filter_global),
                         shape = 21,
                         size = 2,
                         fill = "grey34",
                         colour = "black")+
              geom_text_repel(data = all_annotated() %>% drop_na(input$signif_filter_global),
                              aes(label = gene_name, size = 10),
                              max.overlaps = Inf,
                              show.legend = FALSE)
          }
          vals$vp <- vp 
          return(vp)
        }
      }else if(input$groupBy == "Time point"){
        if(is.null(input$global_select_one_tp)|is.null(input$global_select_organs)){
          #display empty volcano plot
          print(empty_volcano())
        }else{
          selected_tp <- input$global_select_one_tp
          v_table <- volcano_table() %>% filter(timepoint == input$global_select_one_tp, organ %in% input$global_select_organs) %>% drop_na(input$signif_filter_global)
          n_facet_cols <- length(input$global_select_organs)
          
          vp <- ggplot(data = v_table, aes(x = log2.Ratio, y = -log10(get(input$signif_filter_global)))) +
            theme_light() +
            geom_point(aes(colour = gene_type),
                       alpha = 0.8,
                       shape = 16,
                       size = 1) +
            facet_wrap(~ organ, scales = s, ncol = volcano_cols() ) +
            geom_hline(yintercept = -log10(as.numeric(input$FDR_thr_global)),
                       linetype = "dashed") + 
            geom_vline(xintercept = c(-input$absLFC_thr_global, input$absLFC_thr_global),
                       linetype = "dashed") +
            scale_colour_manual(values =  c("Upregulated" = "#ffad73", "Downregulated" = "#26b3ff", "Nonsignificant" = "grey")) + 
            guides(colour = guide_legend(override.aes = list(size= 4))) +
            labs(x = expression("Log"[2]*" Fold Change"),
                 y = bquote(-Log[10]*.(volcano_y_title())),
                 colour = NULL, 
                 title = paste("Selected timepoint: ", input$global_select_one_tp, sep = ""))+
            theme(strip.text = element_text(size = 14, colour = "black"),
                  legend.text = element_text(size = 14),
                  axis.title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  title = element_text(size = 16),
                  
                  legend.position = "bottom",
                  legend.direction = "horizontal")
          
          if(input$ifAnno == TRUE & !is.null(input$GenesToAnno)){
            vp <- vp +  geom_point(data = downregulated_anno() %>% drop_na(input$signif_filter_global),
                                   shape = 21,
                                   size = 2,
                                   fill = "steelblue",
                                   colour = "black") +
              geom_point(data = upregulated_anno() %>% drop_na(input$signif_filter_global),
                         shape = 21,
                         size = 2,
                         fill = "firebrick",
                         colour = "black") +
              geom_point(data = nonsignificant_anno() %>% drop_na(input$signif_filter_global),
                         shape = 21,
                         size = 2,
                         fill = "grey34",
                         colour = "black")+
              geom_text_repel(data = all_annotated() %>% drop_na(input$signif_filter_global),
                              aes(label = gene_name, size = 10),
                              max.overlaps = Inf,
                              show.legend = FALSE)
          }
          vals$vp <- vp
          return(vp)
        }
      }
    })   
    
  })
  
  output$volcano <- renderPlot(
    
    {volcano_plot()},
    height = function(){volcano_height_vec[volcano_rows()]},
    width = function(){volcano_width_vec[volcano_cols()]}
    
  )
  
  
  
  # add d download button only when a volcano plot with points is displayed
  output$get_the_item <- renderUI({
    input$new_plot
    isolate({
      if(input$groupBy == "Organ"){
        if(!is.null(input$global_select_one_organ) & !is.null(input$global_select_timepoints)){
          downloadBttn('download_volcano', 'Download plot', style = "material-flat", color = "primary", size = "sm", icon = icon("download"))
        }
      }else{
        if(!is.null(input$global_select_one_tp) & !is.null(input$global_select_organs)){
          downloadBttn('download_volcano', 'Download plot', style = "material-flat", color = "primary", size = "sm", icon = icon("download"))
        }
      }
    })
  })
  
  
  volcano_plot_name <- reactive({
    input$new_plot
    isolate({
      if(input$groupBy == "Organ"){
        return(paste(input$global_select_one_organ, gsub(" ", "", paste(input$global_select_timepoints, sep = "", collapse = "_")), sep = "."))
      }else{
        return(paste(input$global_select_one_tp, gsub(" ", "", paste(input$global_select_organs, sep = "", collapse = "_")), sep = "." ))
      }
      
    })
  })
  
  output$download_volcano <- downloadHandler(
    filename = function(){paste(volcano_plot_name(), '.png', sep = "")},
    
    content = function(file){
      png(file, width = volcano_width_vec[volcano_cols()], height = volcano_height_vec[volcano_rows()], units = "px")
      print(vals$vp)
      dev.off()
    }
  )
  
  
  output$get_volcano_box <- renderUI({
    
    
    box(width = 0, 
        # height = 700,
        #title = "Volcano plot",
        #solidHeader = TRUE,
        #status = "warning", 
        plotOutput( "volcano", height = volcano_height_vec[volcano_rows()], width = volcano_width_vec[volcano_cols()]),
        uiOutput("get_the_item"))
    
  })
  
  
  # display a table with annotated genes when the "Annotate genes" checkbox is clicked
  
  output$get_table_box <- renderUI({
    input$new_plot
    isolate({
      if(input$groupBy == "Organ"){
        if(!is.null(input$global_select_one_organ)|!is.null(input$global_select_timepoints)){
          if(isTRUE(input$ifAnno)){
            box(width = 0,
                title = "Annotated genes table",
                solidHeader = TRUE,
                status = "warning",
                dataTableOutput("AnnoTable"))
          }
        }
      }else{
        if(!is.null(input$global_select_one_tp)|!is.null(input$global_select_organs)){
          if(isTRUE(input$ifAnno)){
            box(width = 0,
                title = "Annotated genes table",
                solidHeader = TRUE,
                status = "warning",
                dataTableOutput("AnnoTable"))
          }
        }
      }
    })
  })
  
  
  output$AnnoTable <- renderDataTable({
    input$new_plot
    isolate({
      t <- all_annotated()   
      DT::datatable(t, options = list(paging = TRUE, pageLength = 25, scrollX = TRUE), filter = list(position = "bottom", clear = FALSE), selection = "none")
      
    })
  })
  

  # DOWNLOAD TAB------------------------------------------------------------------------------------------------------------------
  #GENE EXPRESSION DATA DOWNLOAD------------------------------------------------------------------------------------------------
  
  # update selectize inputs
  
  updateSelectizeInput(session  = session,
                       inputId  = "Entry_gene_expr_down",
                       server   = TRUE,
                       choices  = gene_names_vector,
                       selected = NULL,
                       options  = list(placeholder = 'eg. Agap2', onInitialize = I('function() { this.setValue(""); }')))
  
  updateSelectizeInput(session   = session,
                       inputId  = "Entry_ID_expr_down",
                       server   = TRUE,
                       choices  = ensembl_IDs_vector,
                       selected = NULL,
                       options  = list(placeholder = 'eg. EMSMUS000...', onInitialize = I('function() { this.setValue(""); }')))
  
  #when search_gene_down_expr input switches reset inputs for gene filtering
  observeEvent(input$search_gene_expr_down, {
    shinyjs::reset("Entry_gene_expr_down")
    shinyjs::reset("Entry_ID_expr_down")
  })
  
  
  # generate table filtered for everything except gene_name and Identifier
  filtered_gene_down_table <- reactive({
    
    f <- all_organs_data_list %>% filter(timepoint %in% input$down_expr_time_filter,
                                         organ %in% input$down_exp_organs_filter,
                                         get(input$signif_filter_down_exp) <= as.numeric(input$FDR_thr_down_exp),
                                         abs_l2FC > input$absLFC_thr_down_exp) %>%
      dplyr::select(-abs_l2FC)
    
    f
    
    
  })
  
  
  # table to be displayed if preview data is checked
  
  data_preview <- reactive({
    if(input$expr_down_opts == "Download entire dataset"){ # if the option to download the entire dataset is selected only preview 20 rows of data 
      
      #first make a table will all data -abs_l2FC and put it into vals; this will be downloaded 
      t <- all_organs_data_list %>% dplyr::select(-abs_l2FC)  
      
      # if users selects xlsx as a filetype than split the dataframe based on organ (becaue entire df is too large) and write each df as a separate xlsx sheet
      if(input$expr_down_filetype == "xlsx"){
        l <- t %>% group_split(organ)
        l <- list(l[[1]], l[[2]], l[[3]], l[[4]])
        names(l) <- levels(t$organ)
        vals$expr_down_table <- l
      }else{
        vals$expr_down_table <- t
      }
      
      # only keep 5 top genes for each organ based on abs_l2FC for display
      t2 <- all_organs_data_list %>%
        group_by(organ) %>% 
        top_n(n = 5, wt = abs_l2FC ) %>%  
        dplyr::select(-abs_l2FC)
      
      
      
      return(t2)
    }else if(input$expr_down_opts == "Select filters" & input$search_gene_expr_down == 1){
      if(is.null(input$Entry_gene_expr_down)){
        t <- filtered_gene_down_table()
        vals$expr_down_table <- t #the same table is downloaded and displayed in preview
        return(t)
        
      }else{
        t <- filtered_gene_down_table() %>% filter(gene_name %in% input$Entry_gene_expr_down)
        vals$expr_down_table <- t
        return(t)
        
      }
      
    }else if(input$expr_down_opts == "Select filters" & input$search_gene_expr_down == 2){
      if(is.null(input$Entry_ID_expr_down)){
        t <- filtered_gene_down_table()
        vals$expr_down_table <- t #the same table is downloaded and displayed in preview
        return(t)
      }else{
        t <- filtered_gene_down_table() %>% filter(Identifier %in% input$Entry_ID_expr_down)
        vals$expr_down_table <- t
        return(t)
        
      }
      
    }
    
  })
  
  
  # display a box for data preview only if Preview data action button is clicked 
  output$get_box_for_preview <-  renderUI({
    
    
    if(input$expr_down_opts == "Download entire dataset" & input$see_total_expr != 0){
      
      
      box(width = 0, 
          title = "Data preview",
          solidHeader = TRUE,
          status = "warning", 
          dataTableOutput( "preview_table"))
      
      
    }else if(input$expr_down_opts == "Select filters" & input$see_filtered_expr != 0){
      
      box(width = 0, 
          title = "Data preview", 
          solidHeader = TRUE,
          
          status = "warning", 
          dataTableOutput( "preview_table"))
      
    }
    
    
    
  })
  
  
  # when Download option switches reset "Preview data" input and all inputs in Select filters
  
  observeEvent(input$expr_down_opts,{
    shinyjs::reset("see_total_expr")
    shinyjs::reset("see_filtered_expr")
    shinyjs::reset("down_exp_organs_filter")
    shinyjs::reset("down_expr_time_filter")
    shinyjs::reset("search_gene_expr_down")
    shinyjs::reset("Entry_gene_expr_down")
    shinyjs::reset("Entry_ID_expr_down")
    shinyjs::reset("FDR_thr_down_exp")
    shinyjs::reset("absLFC_thr_down_exp")
    
  })
  
  output$preview_table <- renderDataTable({
    d <- data_preview()
    DT::datatable(d, options = list(paging = TRUE, pageLength = 25, scrollX = TRUE), selection = "none")
    
  })
  
  
  
  # select the correct function for writing data based on user input
  table_down_fun <- reactive({
    if(input$expr_down_filetype == "csv"){
      "write_csv"
    }else if(input$expr_down_filetype == "xlsx"){
      "write.xlsx" # from the openxlsx library
    }else if(input$expr_down_filetype == "tsv"){
      "write_tsv"
    }else{
      "write_rds"
    }
  })
  # downloadHandler for full data download
  output$down_total_expr <- downloadHandler(
    filename = function(){paste("geneExpression_full_dataset_PrionDatabase", input$expr_down_filetype, sep = ".")},
    
    content = function(file){
      do.call(table_down_fun(), list(vals$expr_down_table, file))
      
    }
  )
  # downloadHandler for filtered data download
  output$down_filtered_expr <- downloadHandler(
    filename = function(){paste("geneExpression_filtered_dataset_PrionDatabase", input$expr_down_filetype, sep = ".")},
    
    content = function(file){
      do.call(table_down_fun(), list(vals$expr_down_table, file))
      
    }
  )
  
  
  # whenever another tab is clicked all inputs in all tabs should be reset
  
  observeEvent(input$tabs, {
    shinyjs::reset()
    click("new_plot")
    click("new_spl_plot") # automatically trigger the action button in the "Global View" tab to reset the plot output
  })
  
  
} 

# run shiny app
shinyApp(ui = ui, server = server)



