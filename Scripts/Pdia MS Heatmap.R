# Load necessary R libraries
library(readxl)
library(ggplot2)

# Set the file path and sheet name
file_path <- "~/Desktop/R-Files/Exp01-LB_Hipp_PDIA3_Buck_Result.xlsx"
sheet_name <- "VolcanoPlot_Discoveries"

# Read data from the specified Excel file and sheet
data <- read_excel(path = file_path, sheet = sheet_name)

# Remove the first row (labels for the axes)
data <- data[-1,]

# Convert relevant columns to numeric and remove rows with NA
data$Abundance <- as.numeric(data$`Abundance Ratio (log2): (HET) / (WT)`)
data$PValue <- 10^(-as.numeric(data$`(-)log10(adj. p-value)`))

# Determine if the gene is in the top 100
data$Top100 <- ifelse(rank(data$PValue) <= 100, TRUE, FALSE)

# Define color scale for heatmap
color_scale <- scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# Filter top 20 upregulated genes
top_upregulated <- data[data$Abundance > 0,]
top_upregulated <- top_upregulated[order(-top_upregulated$Abundance),][1:25,]

# Filter top 20 downregulated genes
top_downregulated <- data[data$Abundance < 0,]
top_downregulated <- top_downregulated[order(top_downregulated$Abundance),][1:25,]

# Combine the top genes
top_genes <- rbind(top_upregulated, top_downregulated)

# Create the heatmap with color based on abundance ratio
heatmap_plot <- ggplot(top_genes, aes(x = reorder(`Gene Symbol`, -Abundance), y = " ", fill = Abundance)) +
  geom_tile(aes(width = 2)) +
  color_scale +
  theme_minimal() +
  labs(x = "Gene Symbol", y = "", title = "Top 25 Upregulated and Downregulated Genes") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1.1))

# Display the heatmap
print(heatmap_plot)
