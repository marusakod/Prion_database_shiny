
# read in data -------------------------------------
## gene expression data
all_organs_data_list <- readRDS("data/all_organs_data_list.rds")
all_organs_data_list$timepoint <- factor(all_organs_data_list$timepoint, levels = c("4 wpi", "8 wpi", "12 wpi", "14 wpi", "16 wpi", "18 wpi", "20 wpi", "terminal"))
all_organs_data_list$organ <- factor(all_organs_data_list$organ, levels = unique(all_organs_data_list$organ))
all_organs_data_list <- distinct(all_organs_data_list)
all_organs_data_list <- all_organs_data_list %>% filter(gene_name != "")

# extract gene symbols and Ensembl IDs for the dropdown selection

gene_names_vector <- unique(all_organs_data_list$gene_name)
ensembl_IDs_vector <- unique(all_organs_data_list$Identifier)
ensembl_IDs_vector <- ensembl_IDs_vector[!is.na(ensembl_IDs_vector)]

