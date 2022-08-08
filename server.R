
#==============================================
#        SERVER for the PRION DATABASE
#==============================================
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(ggrepel)
library(openxlsx)
library(org.Mm.eg.db)
library(DT)
library(SGSeq)


# read in data -------------------------------------
## gene expression data
all_organs_data_list <- readRDS("data/all_organs_data_list.rds")
all_organs_data_list$timepoint <- factor(all_organs_data_list$timepoint, levels = c("4 wpi", "8 wpi", "12 wpi", "14 wpi", "16 wpi", "18 wpi", "20 wpi", "terminal"))
all_organs_data_list$organ <- factor(all_organs_data_list$organ, levels = unique(all_organs_data_list$organ))
all_organs_data_list <- distinct(all_organs_data_list)
all_organs_data_list <- all_organs_data_list %>% filter(gene_name != "")


## splicing data ----------------------------------------------------

#all_JuncSeq_results_merged <- readRDS("all_JuncSeq_results_merged.rds")
all_SGDEX_results_merged <- readRDS("data/all_SGDEX_results_merged.rds")
all_SGDEX_results_merged$timepoint <- factor(all_SGDEX_results_merged$timepoint, levels = c("4 wpi", "8 wpi", "12 wpi", "14 wpi", "16 wpi", "18 wpi", "20 wpi", "terminal"))
all_SGDEX_results_merged$organ <- factor(all_SGDEX_results_merged$organ, levels = unique(all_SGDEX_results_merged$organ))

all_sg_variant_counts <- readRDS("data/all_vc_list.rds")
all_sg_feature_counts <- readRDS("data/all_fc_list.rds")
names(all_sg_feature_counts) <- gsub("Hippocampus", "brain", names(all_sg_feature_counts))
names(all_sg_variant_counts) <- gsub("Hippocampus", "brain", names(all_sg_variant_counts))
names(all_sg_feature_counts) <- gsub("term", "terminal", names(all_sg_feature_counts))
names(all_sg_variant_counts) <- gsub("term", "terminal", names(all_sg_variant_counts))

#--------------------------------------------------------------------


# extract gene symbols and Ensembl IDs for the dropdown selection

gene_names_vector <- unique(all_organs_data_list$gene_name)
ensembl_IDs_vector <- unique(all_organs_data_list$Identifier)
ensembl_IDs_vector <- ensembl_IDs_vector[!is.na(ensembl_IDs_vector)]

gene_names_vector2 <- unique(all_SGDEX_results_merged$geneSymbol)
gene_names_vector2 <- gene_names_vector2[!is.na(gene_names_vector2)]
ensembl_IDs_vector2 <- unique(all_SGDEX_results_merged$EnsemblID)
ensembl_IDs_vector2 <- ensembl_IDs_vector2[!is.na(ensembl_IDs_vector2)]


 
# DEFINE THE SEVER LOGIC ---------------------------------------------------

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
  
  # GENERATE A TABLE WITH SPLICING RESULTS

  
 # display a table when no gene is selected and when filtered dataset contains rows. Display text otherwise
  spl_table_filtered <- reactive({
    o <- organ_vec()
    s <- input$one_gene_signif_filter
    
    if(input$search_gene == 1) {
      
      a <- all_SGDEX_results_merged %>% dplyr::filter(geneSymbol == Entry_variable()) %>%
        filter(organ %in% o[which(o != "")]) %>%
        filter(get(s) <= as.numeric(input$FDR_thr))
      
    }else{
      
      a <- all_SGDEX_results_merged %>% dplyr::filter(EnsemblID == Entry_variable()) %>% 
        filter(organ %in% o[which(o != "")]) %>% 
        filter(get(s) <= as.numeric(input$FDR_thr))
      
    }
    
    a
    
  })
  
  # if no gene is selected or filtered table has rows render a datatable, otherwise render text
  
  output$get_splicing_out <- renderUI({
    a <- spl_table_filtered()
    if(input$Entry_gene == "" & input$Entry_ID == ""){
      dataTableOutput("single_gene_table_splicing")
    }else if(nrow(a) == 0){
      textOutput("no_spl_text")
    }else{
      dataTableOutput("single_gene_table_splicing")
    }
  })
  
  output$no_spl_text <- renderText({
    "No significant splice variants detected"
  })
  
  output$single_gene_table_splicing <- renderDataTable({
    
    org <- organ_vec()
    a <- spl_table_filtered()
    
    if(input$Entry_gene == "" & input$Entry_ID == ""){
      DT::datatable(a, options = list(pageLength = 25, scrollX = TRUE), selection = "none")
  
    }else{
      if(nrow(a) == 0){
        DT::datatable(a, options = list(scrollX = TRUE, language = list(zeroRecords = "No significant splice variants detected")))
    
      }else{
        col <- color_vec()
        # which are the unique organs that remain in a after filtering
        spl_org <- unique(a$organ)
        #subset_org 
        orgs <- org[org %in% spl_org]
        #subset_colors
        cols <- col[names(col) %in% spl_org]
        DT::datatable(a, options = list(pageLength = 25, scrollX = TRUE), selection = "single") %>%
          formatStyle(columns = "organ",
                      backgroundColor = styleEqual(orgs, cols)) %>%
          formatRound(columns = c('exonBaseMean',
                                  'dispersion',
                                  'stat',
                                  'pvalue',
                                  'log2.Ratio',
                                   'FDR',
                                   'NBH',
                                   'RML6'), digits = 3) 
    
      }
    }
  })
  
selected_row_data  <- reactive({
  
  selected_row_index <- input$single_gene_table_splicing_rows_selected 
 # extract selected row from filtered splicing table and print the eventID, variantID, entrezID, organ, timepoint
  spl_table_filtered()[selected_row_index, c("eventID", "variantID", "EntrezID", "organ", "timepoint")]
 
})

output$get_splice_graph <- renderUI({
  if(nrow(selected_row_data()) != 0){
    box(width = 0,
        height = 500,
        solidHeader = TRUE, 
        title = "Splice graph", 
        status = "warning", 
        fluidPage(
        column(width = 1,
        prettyCheckbox(inputId = "draw_to_scale",
                       label = "Scaled",
                       value = FALSE,
                       shape = "curve",
                       animation = "smooth",
                       icon = icon("check"),
                       status = "primary")),
        column(width = 1,
        prettyCheckbox(inputId = "annotateSgraph",
                       label = "Annotate",
                       value = FALSE,
                       shape = "curve",
                       animation = "smooth",
                       icon = icon("check"),
                       status = "primary")),
        column(width = 1,
        prettyCheckbox(inputId = "outlineExons",
                       label = "Exon borders",
                       value = FALSE,
                       shape = "curve",
                       animation = "smooth",
                       icon = icon("check"),
                       status = "primary"))),
        
        plotOutput("splice_graph")
       )
    
    
  }
})

# render a plot with colored selected splice variant-------------------------------------

output$splice_graph <- renderPlot({
  if(nrow(selected_row_data()) != 0){
  variant_row <- selected_row_data()
  
  #selected timepoint and organ
  to <- paste(as.character(variant_row$organ), "_", gsub(" wpi", "_", variant_row$timepoint), sep = "")

  # find correct sg feature counts and sg variant counts
  nfc <- names(all_sg_feature_counts)
  nvc <- names(all_sg_variant_counts)
  
  sgfc_pred <- all_sg_feature_counts[[nfc[grepl(to, nfc)]]]
  sgvc_pred <- all_sg_variant_counts[[nvc[grepl(to, nvc)]]]
  
  # which feature IDs constitute a specific variant
  x <- rowRanges(sgfc_pred)
  y <- as.data.frame(x)

s <- as.data.frame(mcols(sgvc_pred))
k <- s[which(s$variantID == as.integer(selected_row_data()$variantID)), ]$featureID
k <- as.integer(str_split(k, ",")[[1]])

c <- which(y$featureID %in% k)

color_vec <- rep("grey", nrow(y))
color_vec[c] <- "red"


mcols(sgfc_pred)= cbind(mcols(sgfc_pred),
                        DataFrame(color = color_vec))

if(input$draw_to_scale == 1){
  scale <- "gene"
}else{
  scale <- "none"
}

if(input$annotateSgraph == 1){
  annos <- "id"
}else{
  annos <- "none"
}

if(input$outlineExons == 1){
  outline <- "black"
}else{
  outline <- "fill"
}



plotSpliceGraph(rowRanges(sgfc_pred),
                geneName = as.integer(selected_row_data()$EntrezID),
                toscale = scale,
                border = outline,
                short_output = FALSE,
                label = annos)

  }else{ # create some plot when no rows from the splicing table are selected so there is no error
  
    ggplot(all_SGDEX_results_merged, aes(x = FDR, y = pvalue)) + geom_blank()
    
}

})

  # GLOBAL GENE EXPRESSION VIEW TAB ------------------------------------------------------------------
  
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
  
  
  
  # GLOBAL ALTERNATIVE SPLICING VIEW TAB ---------------------------------------------------------------------------------------------------------------
  

  # table with zero count for all organs/timepoints/splice variants
  zero_counts_table <- data.frame(organ = rep(levels(all_SGDEX_results_merged$organ), each = 8*11),
                                  timepoint = rep(rep(levels(all_SGDEX_results_merged$timepoint), each = 11), 4),
                                  variantTypeFullName_merged = rep(unique(all_SGDEX_results_merged$variantTypeFullName_merged), 32), 
                                  n = 0)
  
  zero_counts_table$timepoint <- factor(zero_counts_table$timepoint, levels = c("4 wpi", "8 wpi", "12 wpi", "14 wpi", "16 wpi", "18 wpi", "20 wpi", "terminal"))
  
  
  # make an empty plot that is shown when no time points or organs are in selected 
  empty_spl_plot <- reactive({
    
    if(input$SgroupBy == "organ"){
      xv <- "timepoint"
    }else{
      xv <- "organ"
    }
  
 
    p <- ggplot(zero_counts_table, aes(x = get(xv), y = n)) +
        geom_bar(stat = "identity") +
        ylim(0, 5) +
        theme_light() +
        labs(x = NULL, y = "Number of differentially used splice variants") +
        theme(axis.title = element_text(size = 16),
              axis.text = element_text(size = 12))
    
   p  
         
  })
    
  # make a filtered table with splicing results 
  spl_plot_full_table <- reactive({
    input$new_spl_plot
    isolate({
 
    t <- all_SGDEX_results_merged %>% filter(get(input$signif_filter) <=  as.numeric(input$signif_thr),
                                        organ %in% input$S_Organ_pick,
                                        timepoint %in% input$S_Time_pick,
                                        abs_l2fc > input$absLFC_thr_splicing)
                                 
    t
    
    })
  })
  
  # count the number of individual variant types per organ and time point 
    
  variantType_count_table <- reactive({
   
    ct <- spl_plot_full_table() %>% group_by(organ, timepoint, variantTypeFullName_merged) %>% summarise(n = n(), .groups = "keep")
    
    # filter zero counts table for selected organs and timepoints
    zct <- zero_counts_table %>% filter(organ %in% input$S_Organ_pick,
                                        timepoint %in% input$S_Time_pick)
    
    # find rows in the zero counts table that are not in the real count table (ct)
    
    zct_merged <- apply(zct[, 1:3] , 1 , paste , collapse = "-" )
    ct_merged <- apply(ct[, 1:3], 1, paste, collapse = "-")
    
    not_in_ct <- zct[which(!zct_merged %in% ct_merged), ]
    ct_full <- bind_rows(ct, not_in_ct)
    
    #set the correct factor levels - include just levels that are present in the dataframe
    
    ct_full$timepoint <- factor(ct_full$timepoint, levels = input$S_Time_pick)
    ct_full$organ <- factor(ct_full$organ, levels = input$S_Organ_pick)
    
    ct_full
    
  })
  
  
  # define optimal dimentions for splicing plot 
  splot_cols <- reactive({
    
    input$new_spl_plot
    isolate({
        if(is.null(input$S_Organ_pick)|is.null(input$S_Time_pick)){
          return(1)
        }else{
          if(input$SgroupBy == "timepoint"){
          n_facets <- length(input$S_Time_pick)
          if(n_facets > 4){
            n_facets <- 4
          }
          return(n_facets)
          }else{
          n_facets <- length(input$S_Organ_pick)
          if(length(input$S_Time_pick) %in% c(7,8)){
            n_facets <- 2
          }
          return(n_facets)
          }
        }
      
    })
  })

  
  splot_rows <- reactive({
    
   input$new_spl_plot
   isolate({
      if(input$SgroupBy == "organ"){
        if(length(input$S_Time_pick) %in% c(7,8)){
          return(2)
        }else{
        return(1)
        } 
      }else{
        if(ceiling(length(input$S_Time_pick)/4) == 2){
        return(2)
        }else{
         return(1)
       }
        #if(is.null(input$S_organ_pick)|is.null(input$S_Time_pick)){
        #  return(1)
       # }else{
         # n_rows <- length(input$S_Time_pick)/4
        #  return(ceiling(n_rows))
        #}
      }
     
   })   

  })
  
  sp_width_vec <- volcano_width_vec
  sp_height_vec <- c(500, 800)

  # define colors for specific splice variant types 
  fills_for_splice_vars <- c("#248137", '#fe4a49', '#2ab7ca', '#fed766', "#6AD75B", '#B464C1', "#645BD7", "#F59720", "#92E8E2", "#DF2599","#D6DBDF")
  sv_breaks <- unique(all_SGDEX_results_merged$variantTypeFullName_merged)[unique(all_SGDEX_results_merged$variantTypeFullName_merged) != "Other"]
  sv_breaks <- c(sv_breaks, "Other")
  names(fills_for_splice_vars) <- sv_breaks
  
  
  variantType_count_plot <- reactive({
    
    input$new_spl_plot
    isolate({
    
      if(is.null(input$S_Organ_pick)|is.null(input$S_Time_pick)){
        #display empty volcano plot
        print(empty_spl_plot())
        
      }else{
        
        if(input$SgroupBy == "organ"){
          xv <- "timepoint"
        }else{
          xv <- "organ"
        }
        
        if(input$SgroupBy == "organ"){
          fc <- "organ"
        }else{
          fc <- "timepoint"
        }
        
        
    t <- variantType_count_table()
    
    t$variantTypeFullName_merged <- factor(t$variantTypeFullName_merged, levels = sv_breaks, labels = sv_breaks)
    
    notz <- unique((t %>% filter(n != 0))$variantTypeFullName_merged)
    notz_ordered <- names(fills_for_splice_vars)[names(fills_for_splice_vars) %in% notz]
    
    z <- unique((t %>% filter(n == 0))$variantTypeFullName_merged)
    notz_z <- c(notz_ordered, z)
    
    fills_for_splice_vars_ordered <- fills_for_splice_vars[order(match(names(fills_for_splice_vars), notz_z))]
    
    if(input$setScales_S == 1){
      s <- "free_x"
    }else{
      s <- "free"
    }
    
    if(input$SgroupBy == "organ" & length(input$S_Organ_pick) == 1){
      lpos <- "right"
    }else if(input$SgroupBy == "timepoint" & length(input$S_Time_pick) == 1){
      lpos <- "right"
    }else{
      lpos <- "bottom"
    }
    
    if(input$SgroupBy == "organ" & length(input$S_Organ_pick) == 1){
      ldir <- "vertical"
    }else if(input$SgroupBy == "timepoint" & length(input$S_Time_pick) == 1){
      ldir <- "vertical"
    }else{
      ldir <- "horizontal"
    }
    
    sp <- ggplot(t, aes(x = get(xv), y =  n , fill = variantTypeFullName_merged)) +
    theme_light() +
    geom_bar(stat = "identity", position = "stack", width = 0.5) +
    facet_wrap(~ get(fc), ncol = splot_cols(), scales = s) + 
    labs(x = NULL, y = "Number of differentially used splice variants",
         fill = NULL) +
    #scale_y_log10() +
    theme(strip.text = element_text(size = 14, colour = "black"),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.position = lpos,
          legend.direction = ldir) +
    
      scale_fill_manual(values = fills_for_splice_vars_ordered, breaks = notz_ordered)
    
    vals$sp <- sp
    return(sp)
    
      }
    
    })
    
  })
  
  # if no timepoints OR no organs are selected display an empty plot
  
  output$splicing_plot_global <- renderPlot(
   
    {variantType_count_plot()},
    height = function(){sp_height_vec[splot_rows()]},
    width = function(){sp_width_vec[splot_cols()]}
    
  )
  
  
  # show a download button only when a plot with bars is displayed
  output$get_the_item_spl <- renderUI({
    input$new_spl_plot
    isolate({
        if(!is.null(input$S_Organ_pick) & !is.null(input$S_Time_pick)){
          downloadBttn('download_splot', 'Download plot', style = "material-flat", color = "primary", size = "sm", icon = icon("download"))
        }
      
    })
  })
  
  splicing_plot_name <- reactive({
    input$new_spl_plot
    isolate({
      if(input$SgroupBy == "organ"){
        return(paste(input$S_Organ_pick, collapse = "_"))
      }else{
        return(paste(input$S_Time_pick, collapse = "_"))
      }
    })
    
  })
  
  output$download_splot <- downloadHandler(
    filename = function(){paste(splicing_plot_name(), '.png', sep = "")},
    
    content = function(file){
      png(file, width = sp_width_vec[splot_cols()], height = sp_height_vec[splot_rows()], units = "px")
      print(vals$sp)
      dev.off()
      
    }
  )
  
  output$get_splicingPlot_box <- renderUI({

    box(width = 0,
        plotOutput("splicing_plot_global", click = "sp_click", 
                    height = sp_height_vec[splot_rows()],
                    width = sp_width_vec[splot_cols()]), 
        uiOutput("get_the_item_spl"))
      
    
  })
  
  #observeEvent(input$new_spl_plot,{
    #shinyjs::reset("sp_click")
 # }
  #)
  
  
  splot_click_x <- reactive({
    
    if(is.null(input$sp_click$x)){
      return(NA)
    }else{
      if(input$SgroupBy == "organ"){
        lvls <- levels(variantType_count_table()$timepoint)
      }else{
        lvls <- levels(variantType_count_table()$organ)
      }
      lvls[round(input$sp_click$x)]
    }
    
  })
  
  # retrieve facet 
  splot_facet <- reactive({
    
    if (is.null(input$sp_click$x)){
      return(NA)
    }else{
      panel = input$sp_click$panelvar1
      panel
    }
    
  })
  
  
  output$global_spl_table <- renderDataTable({
    
    input$sp_click
    isolate({
    if(input$SgroupBy == "organ"){
      xv <- "timepoint"
    }else{
      xv <- "organ"
    }
    
    if(input$SgroupBy == "organ"){
      fc <- "organ"
    }else{
      fc <- "timepoint"
    }
    
    t <- spl_plot_full_table() %>% filter(get(xv) == splot_click_x(), 
                                          get(fc) == splot_facet())
    
    # extract unique splice variant types from the table and assign correct colours
    #s_vars <- unique(t$variantTypeFullName_merged)
    #s_vars_cols <- fills_for_splice_vars[names(fills_for_splice_vars) %in% s_vars]
    
    DT::datatable(t, options = list(paging = TRUE, pageLength = 25, scrollX = TRUE), filter = "bottom", selection = "none") %>%
      #DT:: formatStyle(columns = "variantTypeFullName_merged",
                       #backgroundColor = styleEqual(names(s_vars_cols), s_vars_cols)) %>%
    
      DT::formatRound(columns = c('exonBaseMean',
                                  'dispersion',
                                  'stat',
                                  'pvalue',
                                  'log2.Ratio',
                                  'FDR',
                                  'NBH',
                                  'RML6'), digits = 3)  
    
    })
  })
  
  
  
  observeEvent(input$sp_click,{ # if plot click is not null than display a box with a table that contains information about the clicked bar
    
    output$get_spl_table_box <- renderUI({
      
      if(!is.na(splot_click_x())){
        box(width = 0,
            title = "Selected splice variants",
            solidHeader = TRUE,
            status = "warning",
            dataTableOutput("global_spl_table"))
      }
      
    })  
    
  }
  )
  
 
   
  
  
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
  
  
  # SPLICING DATA DOWNLOAD TAB-----------------------------------------------------------------------------------
  
  # update selectize inputs
  
  updateSelectizeInput(session  = session,
                       inputId  = "Entry_gene_spl_down",
                       server   = TRUE,
                       choices  = gene_names_vector2,
                       selected = NULL,
                       options  = list(placeholder = 'eg. Agap2', onInitialize = I('function() { this.setValue(""); }')))
  
  updateSelectizeInput(session   = session,
                       inputId  = "Entry_ID_spl_down",
                       server   = TRUE,
                       choices  = ensembl_IDs_vector2,
                       selected = NULL,
                       options  = list(placeholder = 'eg. EMSMUS000...', onInitialize = I('function() { this.setValue(""); }')))
  
  #when search_gene_spl_down input switches reset inputs for gene filtering
  observeEvent(input$search_gene_spl_down, {
    shinyjs::reset("Entry_gene_spl_down")
    shinyjs::reset("Entry_ID_spl_down")
  })
  
  
  # generate table filtered for everything except gene_name and Identifier and splice variant 
  filtered_spl_table <- reactive({
    
    f <- all_SGDEX_results_merged %>% filter(timepoint %in% input$down_spl_time_filter,
                                         organ %in% input$down_spl_organs_filter,
                                         get(input$signif_thr_type_down_spl) <= as.numeric(input$signif_thr_down_spl),
                                         abs_l2fc > input$absLFC_thr_down_spl) %>%
      dplyr::select(-abs_l2fc)
    
    f
    
    
  })
  
  
  # table to be displayed if preview data is checked
  
  data_preview_spl <- reactive({
    if(input$spl_down_opts == "Download entire dataset"){ # if the option to download the entire dataset is selected only preview 20 rows of data 
      
      #first make a table will all data and put it into vals; this will be downloaded 
      t <- all_SGDEX_results_merged %>% dplyr::select(-abs_l2fc, -variantTypeFullName_merged)  
      
      # if users selects xlsx as a filetype than split the dataframe based on organ (becaue entire df is too large) and write each df as a separate xlsx sheet
      if(input$spl_down_filetype == "xlsx"){
        l <- t %>% group_split(organ)
        l <- list(l[[1]], l[[2]], l[[3]], l[[4]])
        names(l) <- levels(t$organ)
        vals$spl_down_table <- l
      }else{
        vals$spl_down_table <- t
      }
      
      # only keep 5 top genes for each organ based on abs_l2FC for display
      t2 <- all_SGDEX_results_merged %>%
        group_by(organ) %>% 
        top_n(n = 5, wt = abs_l2fc) %>%  
        dplyr::select(-abs_l2fc, -variantTypeFullName_merged)
      
      
      
      return(t2)
      
      
    }else{
      if(input$spl_down_opts == "Select filters" & input$search_gene_spl_down == 1){
        if(is.null(input$Entry_gene_spl_down)){
        t <- filtered_spl_table()
       # vals$spl_down_table <- t #the same table is downloaded and displayed in preview
      #  return(t)
        
        }else{
        t <- filtered_spl_table() %>% filter(geneSymbol %in% input$Entry_gene_spl_down)
        #vals$spl_down_table <- t
       # return(t)
        
        }
      
      }else if(input$spl_down_opts == "Select filters" & input$search_gene_spl_down == 2){
      if(is.null(input$Entry_ID_spl_down)){
        t <- filtered_spl_table()
       # vals$spl_down_table <- t #the same table is downloaded and displayed in preview
       # return(t)
      }else{
        t <- filtered_spl_table() %>% filter(EnsemblId %in% input$Entry_ID_spl_down)
        #vals$spl_down_table <- t
        #return(t)
        
      }
      
      }
      
      if(is.null(input$variantType_selection)){
        return(t)
        vals$spl_down_table <- t
      }else
        t <- t %>% filter(variantTypeFullName_merged %in% input$variantType_selection)
        return(t)
        vals$spl_down_table <-t
    }
    
  })
  
  
  # display a box for data preview only if Preview data action button is clicked 
  output$get_box_for_spl_preview <-  renderUI({
    
    
    if(input$spl_down_opts == "Download entire dataset" & input$see_total_spl != 0){
      
      
      box(width = 0, 
          title = "Data preview",
          solidHeader = TRUE,
          status = "warning", 
          dataTableOutput( "preview_spl_table"))
      
      
    }else if(input$spl_down_opts == "Select filters" & input$see_filtered_spl != 0){
      
      box(width = 0, 
          title = "Data preview", 
          solidHeader = TRUE,
          
          status = "warning", 
          dataTableOutput( "preview_spl_table"))
      
    }
    
    
    
  })
  
  
  # when Download option switches reset "Preview data" input and all inputs in Select filters
  
  observeEvent(input$spl_down_opts,{
    shinyjs::reset("see_total_spl")
    shinyjs::reset("see_filtered_spl")
    shinyjs::reset("down_spl_organs_filter")
    shinyjs::reset("down_spl_time_filter")
    shinyjs::reset("search_gene_spl_down")
    shinyjs::reset("Entry_gene_spl_down")
    shinyjs::reset("Entry_ID_spl_down")
    shinyjs::reset("signif_thr_type_down_spl")
    shinyjs::reset("signif_thr_down_spl")
    shinyjs::reset("absLFC_thr_down_spl")
    shinyjs::reset("variantType_selection")
    
  })
  
  output$preview_spl_table <- renderDataTable({
  d <- data_preview_spl() 
  
      DT::datatable(d, options = list(paging = TRUE, pageLength = 25, scrollX = TRUE), selection = "none") %>%
      DT::formatRound(columns = c('exonBaseMean',
                                  'dispersion',
                                  'stat',
                                  'pvalue',
                                  'log2.Ratio',
                                  'FDR',
                                  'NBH',
                                  'RML6'), digits = 3)
  })
  
  
  
  # select the correct function for writing data based on user input
  table_down_fun_spl <- reactive({
    if(input$spl_down_filetype == "csv"){
      "write_csv"
    }else if(input$spl_down_filetype == "xlsx"){
      "write.xlsx" # from the openxlsx library
    }else if(input$spl_down_filetype == "tsv"){
      "write_tsv"
    }else{
      "write_rds"
    }
  })
  # downloadHandler for full data download
  output$down_total_spl <- downloadHandler(
    filename = function(){paste("AlternativeSplicing_full_dataset_PrionDatabase", input$spl_down_filetype, sep = ".")},
    
    content = function(file){
      do.call(table_down_fun_spl(), list(vals$spl_down_table, file))
      
    }
  )
  # downloadHandler for filtered data download
  output$down_filtered_spl <- downloadHandler(
    filename = function(){paste("AlternativeSplicing_filtered_dataset_PrionDatabase", input$spl_down_filetype, sep = ".")},
    
    content = function(file){
      do.call(table_down_fun_spl(), list(vals$spl_down_table, file))
      
    }
  )
  
#-----------------------------------------------------------------------------------------------  
    
  # whenever another tab is clicked all inputs in all tabs should be reset

  observeEvent(input$tabs, {
    shinyjs::reset()
    click("new_plot")
    click("new_spl_plot") # automatically trigger the action button in the "Global View" tab to reset the plot output
    })
    
    
} 

# run shiny app
shinyApp(ui = ui, server = server)

