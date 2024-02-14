# Load necessary libraries
library(igraph)
library(dplyr)  # Load the dplyr package

# Assuming your data is already loaded and processed, and you have the tidygraph object 'tg'

# Load the Excel spreadsheet
file_path <- "~/Desktop/R-Files/GO_Cellular_Component_2023_table.xlsx"
df <- read_excel(file_path, sheet = "GO Relationships")

# Coerce Genes column to character
df$Genes <- as.character(df$Genes)

# Split genes in column C and count occurrences
gene_counts <- table(unlist(strsplit(as.character(df$Genes), ";")))

# Filter genes that appear in multiple rows
multi_gene_names <- names(gene_counts[gene_counts > 1])

# Create a data frame for genes that appear in multiple rows
multi_gene_df <- data.frame(Gene = as.character(multi_gene_names))

# Initialize an empty list to store gene information
gene_info_list <- list()

# Iterate through each gene in multi_gene_names
for (gene in multi_gene_names) {
  # Get the row numbers where the gene appears
  row_numbers <- which(grepl(gene, df$Genes))
  
  # Store gene and row numbers in the list
  gene_info_list[[gene]] <- list(gene = gene, row_numbers = row_numbers)
}

# Create a new data frame with correct column names
gene_info_df <- data.frame(
  gene = unname(unlist(sapply(gene_info_list, function(x) rep(x$gene, length(x$row_numbers))))),
  row_numbers = unlist(sapply(gene_info_list, function(x) x$row_numbers)),
  stringsAsFactors = FALSE
)

# Get unique values in column 'Relation Group'
unique_values <- unique(df$`Relation Group`)

# Initialize an empty list to store group information
group_info_list <- list()

# Iterate through each unique value
for (value in unique_values) {
  # Get the row numbers where the value appears
  row_numbers <- which(df$`Relation Group` == value)
  
  # Store value and row numbers in the list
  group_info_list[[as.character(value)]] <- list(group = paste("Group", value), row_numbers = row_numbers)
}

# Assuming group_info_df is available or creating it from the previous steps
group_info_df <- data.frame(Relation_Group = df$`Relation Group`, Row = 1:nrow(df))

# Split the 'rows' column into a list of numeric vectors
gene_info_df$rows <- lapply(gene_info_df$row_numbers, function(x) as.numeric(strsplit(paste(x, collapse = ', '), ",")[[1]]))

# Rest of the script to create gene_group_overlaps

# Initialize an empty list to store gene overlaps for each group
gene_overlaps_list <- list()

# Iterate through each group
for (group in unique(group_info_df$Relation_Group)) {
  group_rows <- as.numeric(group_info_df$Row[group_info_df$Relation_Group == group])
  genes_for_group <- character(0)
  
  # Iterate through each gene
  for (gene_row in 1:nrow(gene_info_df)) {
    gene_rows <- gene_info_df$rows[gene_row]
    
    # Check for overlap with this group
    if (any(gene_rows %in% group_rows)) {
      genes_for_group <- c(genes_for_group, gene_info_df$gene[gene_row])
    }
  }
  
  # Remove duplicates within the group
  genes_for_group <- unique(genes_for_group)
  
  # Store group and associated genes in the list
  gene_overlaps_list[[paste("Group", group)]] <- list(Genes = paste(genes_for_group, collapse = ', '))
}

# Convert the list to a data frame
gene_group_overlaps <- do.call(rbind, lapply(gene_overlaps_list, function(x) data.frame(Group = x[[1]], Genes = x$Genes)))

# Display the gene_group_overlaps data frame
print(gene_group_overlaps)

# Create a graph with nodes for each gene
g <- graph_from_data_frame(df, directed = FALSE)

# Set the vertex names to match the genes
V(g)$name <- unique(unlist(strsplit(as.character(df$Genes), ";")))

# Define colors for each group
unique_groups <- unique(gene_group_overlaps$Group)
group_colors <- rainbow(length(unique_groups))

# Map colors to groups
group_color_mapping <- setNames(group_colors, unique_groups)

# Set node attributes including color based on group
V(graph)$color <- group_color_mapping[gene_group_overlaps$Group]

# Helper function to find common genes
common_genes <- function(genes1, genes2) {
  genes1 <- strsplit(genes1, ", ")[[1]]
  genes2 <- strsplit(genes2, ", ")[[1]]
  intersect(genes1, genes2)
}

# Iterate through each pair of genes and add edges based on overlap
for (i in 1:(nrow(gene_group_overlaps) - 1)) {
  for (j in (i + 1):nrow(gene_group_overlaps)) {
    common <- common_genes(gene_group_overlaps$Genes[i], gene_group_overlaps$Genes[j])
    if (length(common) > 0) {
      graph <- add_edges(graph, edges = c(i, j))
    }
  }
}

# Plot the network
set.seed(12345)
ggraph(graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(color = factor(gene_group_overlaps$Group)), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines"), colour = "gray10") +
  scale_color_manual(values = group_color_mapping) +
  theme_graph(background = "white") +
  theme(legend.position = "top")