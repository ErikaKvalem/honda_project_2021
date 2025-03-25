library(dplyr)
library(conflicted)

samplesheet = readr::read_tsv("/data/projects/2021/MicrobialMetabolites/bacterial-supernatant/10_rnaseq_pipeline/pipeline_info/samplesheet_group_B2.valid.tsv")
count_matrix = readr::read_tsv("/data/projects/2021/MicrobialMetabolites/bacterial-supernatant/10_rnaseq_pipeline/star_salmon/salmon.merged.gene_counts.tsv")
dea<- readr::read_tsv("/data/projects/2021/MicrobialMetabolites/bacterial-supernatant/20_deseq2icbi/paired_grp/deseq2_11mix_vs_10mix/redo_deseq2_11102024/_IHWsigFCgenes_1.50003898928582_fold.tsv")
deg_genes <- dea$gene_name

gene_entrez <- bitr(deg_genes, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Mm.eg.db")
entrez_ids <- gene_entrez$ENTREZID
go_results <- enrichGO(gene          = entrez_ids,
                       OrgDb         = org.Mm.eg.db,
                       keyType       = "ENTREZID",
                       ont           = "BP",
                       pAdjustMethod = "BH",
                       pvalueCutoff  = 0.05,
                       qvalueCutoff  = 0.05)

# Plot results
dotplot(go_results, showCategory=15) + ggtitle("GO Biological Process Enrichment")

kegg_results <- enrichKEGG(gene          = entrez_ids,
                           organism      = "mmu",   # Mouse
                           pvalueCutoff  = 0.05)


  

# Select immune genes
immune_genes <- c("Ifng", "Il6", "Tnf", "Cd40", "Cxcl10", "Il1b")  # Example immune genes
heatmap_data <- count_matrix[rownames(count_matrix) %in% immune_genes, ]

# Normalize counts (log2 transformation)
normalized_counts <- log2(heatmap_data + 1)

# Create heatmap
pheatmap(normalized_counts, scale = "row", clustering_distance_rows = "correlation",
         clustering_distance_cols = "euclidean", clustering_method = "complete",
         color = colorRampPalette(c("blue", "white", "red"))(50),
         main = "Immune-Related DEGs Heatmap")
