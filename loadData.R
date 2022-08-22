
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


all_vc_files <- paste0("data/", list.files(path = "data", pattern = "sg_variant_counts.*.rds"))
all_fc_files <- paste0("data/", list.files(path = "data", pattern = "sg_feature_counts.*.rds"))

all_sg_variant_counts <- sapply(all_vc_files, FUN = readRDS, simplify = FALSE, USE.NAMES = FALSE)
all_sg_feature_counts  <- sapply(all_fc_files, FUN = readRDS, simplify = FALSE, USE.NAMES = FALSE)

names(all_sg_variant_counts)  <- gsub(".rds", "", all_vc_files)
names(all_sg_feature_counts) <- gsub(".rds", "", all_fc_files)

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
