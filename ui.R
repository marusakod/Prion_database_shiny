
#==============================================
#        UI for the PRION DATABASE
#==============================================
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(org.Mm.eg.db) # not needed anymore
library(shinydashboard)
library(shinyjs)
library(fresh)
library(ggtips)
library(shinyBS)


#---- HEADER-----------------------------------------------------------------------------

header <- dashboardHeader(title = "Prion Database",
                          tags$li(a(href = 'https://doi.org/10.1371/journal.ppat.1008653',
                                    img(src = 'www/plos_pathogens.png',
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
    menuItem("Home", tabName = "description", icon = icon("home")),
    menuItem("Gene search", tabName = "genes", icon = icon("search"),
             menuSubItem("Global gene expression view", tabName = "multiple_genes"),
             menuSubItem("Global splicing view", tabName = "multiple_splices"),
             menuSubItem("Single gene view", tabName = "one_gene")),
    menuItem("Download", tabName = "download", icon = icon("download"),
             menuSubItem("Gene expression data", tabName = "download_expr"),
             menuSubItem("Splicing data", tabName = "download_spl"))
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
      # UI for the HOME tab item
     # tabItem(tabName="welcome",
              #h2("Welcome to the prion database"),
              #tags$style(HTML(".small-box {height: 150px}")),
             # fluidRow(
               # infoBox("",
                       # a("Learn more", onclick = "openTab('description')", href="#"),
                        #icon=icon("book"),
                        #color = "teal"),
                #infoBox("",
                        #a("Search for genes", onclick = "openTab('one_gene')", href="#"),
                        #icon=icon("search"),
                       # color = "teal"),
               # infoBox("",
                        #a("Download data", onclick = "openTab('download_expr')", href="#"),
                        #icon=icon("download"),
                       # color = "teal"),
            #  )
     # ),
      
   
      tabItem(tabName = "description",
              fluidPage(
                column(width = 12,
              div(h2(strong("Welcome to The Prion Database")), style = "text-align: center; color:#2F8BAE"),
              div(h3("-insert subtititle here-"), style = "text-align: center;"),
              br()),
                column(width = 6, 
              br(),
              div( img(src='www/experiment.png', height="100%", width="100%"), style="text-align: center;"),
              br()),
              column(width = 6,
                     p("Prion diseases (PrDs) are fatal neurodegenerative diseases caused by transmissible proteinaceous particles termed prions (PrPSc). Prions primarily consist of the misfolded form of cellular prion protein (PrPC) incorporated into fibrillary β-sheet-rich structures, which are termed ‘amyloids’. Although prions primarily accumulate in the central nervous system leading to severe pathophysiological brain related symptoms, they can be taken up by the digestive system, followed by accumulation in lymphoid tissue and neuroinvasion via peripheral nerves; furthermore, prions can be present in the blood, which represent an efficient route of infection. Here, we report a searchable RNA sequencing analyses on brain (hippocampus), blood, hindlimb skeletal muscle, and spleen during the entire life span of mice after intracerebral exposure to prions (4, 8, 12, 14, 16, 18 and 20 weeks-post-inoculation and terminal stage).", style="text-align: justify; font-size:16px;"),
                     br(),
                     p("In the", span(strong("Gene search tab")), span("user/s can select three sections:"), style = "font-size:16px;"), 
                     tags$div(
                       tags$ul(
                         tags$li(actionLink("link_to_tabitem_multiple_genes", "Global gene expression view tab"), "generates volcano plots (Log2 Fold change over -Log10FDR) in selected organs. Thresholding and significance parameters can be tuned in selected organs. Gene/s of interest can be individually displayed within the related plots.", style="text-align: justify; font-size:16px;"), 
                         tags$li(actionLink("link_to_tabitem_multiple_splices", "Global splicing view tab"), "generates bar plots (number of differentially used splice variants over timepoints) showing the number of significant splice variants in selected organs and timepoints. The colors in bar plots represent different proportions of splice variant types which are Alternative 3' splice site (proximal), Alternative 3' splice site (distal), Retained intron (exclusion), Retained intron (retention), Skipped exon (skipping), 5' splice site (proximal), Alternative last exon and other", style="text-align: justify; font-size:16px;"),
                         tags$li(actionLink("link_to_tabitem_one_gene", "Single gene view tab"), "generates line plot (Log2 Fold change over timepoints) showing how the expression of a selected gene changes over time. In case of any significant splice variants detected for the related gene, they are displayed in a table below: each row in the table represents one splice variant which can be visualized in a splice graph if you click on the row in the table", style="text-align: justify; font-size:16px;")
                       )
                     ), 
                     br(),
                     p("In the", span(strong("Download tab")), "user/s can download the post-processed lists of differentially express genes and/or alternative splicing in selected organs", style="text-align: justify; font-size:16px;")
              
            
      ))),
      tabItem(tabName = "one_gene", 
              fluidRow(
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
                column(width = 12, 
                       box(width = 0,
                           title = "Alternative splicing",
                           solidHeader = TRUE,
                           status = "warning",
                           uiOutput("get_splicing_out"))),
                column(width = 12, uiOutput("get_splice_graph"))
                
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
                                             
                                            
                                            # this has to somehow appear only when user selects xlsx as filetype 
                                            #bsPopover("label_down_total_expr",
                                                      #title="Entire dataset download",
                                                      #placement="bottom",
                                                      #options = list(container = "body"),
                                                      #content=".xlsx file with a separate sheet for each organ will be downloaded"))
                                            
                                          
                                            
                           
                          )),
                
                column(width = 9, uiOutput("get_box_for_preview")))),
      
      # SPLICING DATA DOWNLOAD TAB

      tabItem(tabName = "download_spl",
              fluidRow(
                column(width = 3,
                       box(width = 0,
                           awesomeRadio(inputId = "spl_down_opts",
                                        label = "Download options",
                                        choices = c("Download entire dataset", "Select filters"),
                                        selected = "Download entire dataset"), 
                          
                           conditionalPanel("input.spl_down_opts == 'Select filters'",
                                         
                                            pickerInput("down_spl_organs_filter","Select organs",
                                                        choices= levels(all_organs_data_list$organ),
                                                        options = list(`actions-box` = TRUE),
                                                        multiple = T,
                                                        width = "200px"),
                                            
                                            pickerInput("down_spl_time_filter", "Select timepoints",
                                                        choices = levels(all_organs_data_list$timepoint),
                                                        options = list(`actions-box` = TRUE),
                                                        multiple = TRUE,
                                                        width = "200px"),
                                            awesomeRadio(
                                              inputId = "search_gene_spl_down",
                                              label = div(style = "font-size: 14px", "Filter genes by:"),
                                              choices = c("Gene Symbol" = 1, "Ensembl ID" = 2),
                                              selected =  1,
                                              inline = FALSE
                                            ),
                                            conditionalPanel(
                                              "input.search_gene_spl_down == 1",
                                              selectizeInput(
                                                inputId = "Entry_gene_spl_down",
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
                                              "input.search_gene_spl_down == 2",
                                              selectizeInput(
                                                inputId = "Entry_ID_spl_down",
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
                                            )),
                                   
                                  conditionalPanel("input.spl_down_opts == 'Select filters'",
                                              
                                                pickerInput("variantType_selection",
                                                            "Splice Variant Type",
                                                            choices = unique(all_SGDEX_results_merged$variantTypeFullName_merged),
                                                            options = list(`actions-box` = TRUE),
                                                            multiple = TRUE,
                                                            width = "200px"),
                                            
                                            awesomeRadio(inputId = "signif_thr_type_down_spl",
                                                         label = "Significance filter",
                                                         choices = c("FDR" = "FDR" , "p-value (unadjusted)" = "pvalue"),
                                                         selected = "FDR",
                                                         inline = FALSE),
                                            
                                            awesomeRadio(inputId = "signif_thr_down_spl",
                                                         label = div(style = "font-size: 14px", "Significance threshold"),
                                                         choices = c("no filter" = 2, 
                                                                     "0.1" = 0.1,
                                                                     "0.05" = 0.05,
                                                                     "0.01" =  0.01,
                                                                     "0.001" = 0.001,
                                                                     "10-4" = 0.0001,
                                                                     "10-5" = 0.00001),
                                                         selected = 0.05),
                                            sliderInput(inputId = "absLFC_thr_down_spl",
                                                        label = div(style = "font-size: 14px", "Absolute Fold Change Threshold"),
                                                        value = 0,
                                                        min = 0,
                                                        max = 5,
                                                        step = 0.5,
                                                        ticks = TRUE,
                                                        width = "200px")),
                           pickerInput(inputId = "spl_down_filetype",
                                       label = "Select file type",
                                       choices = c("csv", "xlsx", "tsv", "rds"),
                                       selected = "csv",
                                       multiple = FALSE),
                           conditionalPanel("input.spl_down_opts == 'Select filters'",
                                            splitLayout(
                                              prettyCheckbox(inputId = "see_filtered_spl",
                                                             label = "PREVIEW DATA",
                                                             value = FALSE,
                                                             status = "warning",
                                                             icon = icon("eye"), 
                                                             plain = TRUE,
                                                             bigger = TRUE,
                                                             shape = "curve",
                                                             animation = "smooth",
                                                             outline = TRUE),
                                              downloadBttn(outputId = "down_filtered_spl",
                                                           label = "Download",
                                                           style = "material-flat",
                                                           color = "primary",
                                                           size = "sm",
                                                           icon = icon("download")))),
                           conditionalPanel(condition = "input.spl_down_opts == 'Download entire dataset'",
                                            splitLayout(
                                              prettyCheckbox(inputId = "see_total_spl",
                                                             label = "PREVIEW DATA",
                                                             value = FALSE,
                                                             status = "warning",
                                                             icon = icon("eye"), 
                                                             plain = TRUE,
                                                             bigger = TRUE, 
                                                             shape = "curve",
                                                             animation = "smooth",
                                                             outline = TRUE),
                                              
                                              
                                              downloadBttn(outputId = "down_total_spl",
                                                           label = tags$span(id = "label_down_total_spl", "Download"),
                                                           style = "material-flat",
                                                           color = "primary",
                                                           size = "sm",
                                                           icon = icon("download"))),
                                            bsPopover("see_total_spl", title="Entire dataset preview", placement="bottom", options = list(container = "body"),
                                                      content="The entire dataset contains # rows. Only 20 rows will be displayed in preview."))
    
                       )),
                column(width = 9, uiOutput("get_box_for_spl_preview")))), 
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
                                        inline = TRUE),
                          
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
                            
                           
                           
                           
              ),
      
      # GLOBAL SPLICING VIEW TAB
      
      tabItem(tabName = "multiple_splices",
              fluidRow(
                column(width = 7,
                       box(width = 0,
                           title = "Plot Parameters",
                           solidHeader = TRUE,
                           status = "warning",
                           fluidPage(
                             column( width= 4, 
                           awesomeRadio("SgroupBy",
                                        "Group by:", 
                                        choices = c("Organ" = "organ", "Time point" = "timepoint"),
                                        selected = "organ",
                                        inline = TRUE),
                           pickerInput("S_Organ_pick",
                                       "Select Organs",
                                       choices = levels(all_organs_data_list$organ),
                                       options = list(`actions-box` = TRUE),
                                       multiple = TRUE,
                                       width = "200px"),
                           pickerInput("S_Time_pick",
                                       "Select timepoints",
                                       choices = levels(all_organs_data_list$timepoint),
                                       options =  list(`actions-box` = TRUE),
                                       multiple = TRUE,
                                       width = "200px")
                           ),
                          column(width = 4,
                             awesomeRadio("signif_filter",
                                              "Significance filter", 
                                              choices = c("FDR" = "FDR", "p-value (unadjusted)" = "pvalue"),
                                              selected = "FDR",
                                              inline = FALSE),
                              awesomeRadio(inputId = "signif_thr",
                                           label = div(style = "font-size: 14px", "Significance threshold"),
                                           choices = c("0.1" = 0.1,
                                                       "0.05" = 0.05,
                                                       "0.01" = 0.01,
                                                       "0.001" = 0.001,
                                                       "10-4" = 0.0001,
                                                       "10-5" = 0.00001),
                                           selected = 0.05)
                              
                
                           ),
                          column(width = 4, 
                                 sliderInput(inputId = "absLFC_thr_splicing",
                                             label = div(style = "font-size: 14px", "Absolute Fold Change Threshold"),
                                             value = 0,
                                             min = 0,
                                             max = 5,
                                             step = 0.5,
                                             ticks = TRUE,
                                             width = "200px"),
                                 prettyCheckbox(inputId = "setScales_S",
                                                label = "Fixed scales",
                                                value = FALSE,
                                                shape = "curve",
                                                animation = "smooth",
                                                icon = icon("check"),
                                                status = "primary"),
                                 actionBttn("new_spl_plot", "Update plot", icon("redo"), size = "sm", style = "material-flat", block = FALSE, color = "primary"),
                                 
                                 bsPopover("new_spl_plot", title="Update splice variant count plot", placement="bottom", options = list(container = "body"),
                                           content="This will generate a new plot with speficied parameters")) 
                                
                       
                )
                       )
                
              ),
              column(width = 12, align = "center", uiOutput("get_splicingPlot_box")),
              column(width = 12, uiOutput("get_spl_table_box"))
              
              
              
      )
      
      
    )
    
    )

    
 
  )
    
  
footer <- tags$footer("This tool was developed by the Aguzzi lab at the University of Zürich", 
            br(), 
            "Code is available through Github",
            align = "center", 
            style = "position:fixed; bottom:0; width:100%; height:70px; color: black; padding: 10px; background-color: #D5DBDB; font-size:16px; text-align: center;"))




# combine UI elements
ui <- function(){
  addResourcePath("www", "www")
  tagList(
  shinydashboard::dashboardPage(useShinyjs(),
                header = header, sidebar = sidebar, body = body))

}


