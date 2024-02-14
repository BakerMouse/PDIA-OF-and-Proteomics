# Load necessary libraries
library(readxl)
library(ComplexHeatmap)
library(dplyr)
library(tidyr) # For the 'spread' function

# Assuming 'data' is your dataframe
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

# Step 1: Filter significant genes
significant_genes <- data %>%
  filter(`(-)log10(adj. p-value)` > 1.301)

# Step 2: Sort and select the top 25 upregulated and downregulated genes
top_genes <- significant_genes %>%
  arrange(desc(`Abundance Ratio (log2): (HET) / (WT)`)) %>%
  top_n(25, `Abundance Ratio (log2): (HET) / (WT)`) %>%
  bind_rows(
    significant_genes %>%
      arrange(`Abundance Ratio (log2): (HET) / (WT)`) %>%
      top_n(25, desc(`Abundance Ratio (log2): (HET) / (WT)`))
  )

# Prepare the data for the heatmap (only select the relevant columns for the heatmap)
heatmap_data <- top_genes %>%
  select(`Gene Symbol`, `Abundance Ratio (log2): (HET) / (WT)`) %>%
  spread(`Gene Symbol`, `Abundance Ratio (log2): (HET) / (WT)`)

# Transpose to get Vertical orientation
heatmap_data <- t(top_genes %>%
                    select(`Gene Symbol`, `Abundance Ratio (log2): (HET) / (WT)`) %>%
                    spread(`Gene Symbol`, `Abundance Ratio (log2): (HET) / (WT)`))

# Step 3: Create the heatmap
Heatmap(heatmap_data)
