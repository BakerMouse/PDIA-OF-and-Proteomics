# Load necessary R libraries
library(readxl)
library(ggrepel)

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

# Set the y-axis intersection to 0
y_axis_intersection <- 0

# Set the font family to Arial in the plot window, this opens the plot in quartz which is lame, don't use
#quartz(family = "Arial")

# Plot the volcano plot with labels for the top 10 genes
plot(data$Abundance, -log10(data$PValue), 
     col = ifelse(data$Top100 & data$Abundance > 0, "blue", 
                  ifelse(data$Top100 & data$Abundance < 0, "red", "black")),
     pch = 16,
     xlab = "Abundance Ratio (log2: HET/WT)",
     ylab = "-log10(Adjusted p-value)",
     main = "Volcano Plot",
     xlim = range(data$Abundance) * c(1.1, 1.1),  # Extend x-axis range
     ylim = range(-log10(data$PValue)) * c(1.1, 1.1),  # Extend y-axis range
     axes = FALSE)  # Turn off automatic axes

# Find the row of the node of interest for "Pdia3"
node_row <- which(data$`Gene Symbol` == "Pdia3")

# Extract the coordinates for "Pdia3"
x_coord_Pdia3 <- data$Abundance[node_row]
y_coord_Pdia3 <- -log10(data$PValue[node_row])

# Manually adjust the position of the label for "Pdia3" to a new y-coordinate
text(x = x_coord_Pdia3, y = y_coord_Pdia3 - 0, labels = "Pdia3", pos = 4, col = "red", cex = 1)

# Find the row of the node of interest for "Pa2g4"
node_row <- which(data$`Gene Symbol` == "Pa2g4")

# Extract the coordinates for "Pa2g4"
x_coord_Pa2g4 <- data$Abundance[node_row]
y_coord_Pa2g4 <- -log10(data$PValue[node_row])

# Manually adjust the position of the label for "Pa2g4" to a new y-coordinate
text(x = x_coord_Pa2g4, y = y_coord_Pa2g4 - 0, labels = "Pa2g4", pos = 1, col = "black", cex = 1)

# Find the row of the node of interest for "Tubb1"
node_row <- which(data$`Gene Symbol` == "Tubb1")

# Extract the coordinates for "Tubb1"
x_coord_Tubb1 <- data$Abundance[node_row]
y_coord_Tubb1 <- -log10(data$PValue[node_row])

# Manually adjust the position of the label for "Tubb1" to a new y-coordinate
text(x = x_coord_Tubb1, y = y_coord_Tubb1 - 0, labels = "Tubb1", pos = 4, col = "black", cex = 1)

# Find the row of the node of interest for "Slc6a11"
node_row <- which(data$`Gene Symbol` == "Slc6a11")

# Extract the coordinates for "Slc6a11"
x_coord_Slc6a11 <- data$Abundance[node_row]
y_coord_Slc6a11 <- -log10(data$PValue[node_row])

# Manually adjust the position of the label for "Slc6a11" to a new y-coordinate
text(x = x_coord_Slc6a11, y = y_coord_Slc6a11 - 0, labels = "Slc6a11", pos = 2, col = "black", cex = 1)

# Find the row of the nodes of interest
nodes_of_interest <- c("Pa2g4", "Tubb1", "Slc6a11")
node_rows <- which(data$`Gene Symbol` %in% nodes_of_interest)

# Exclude "Pa2g4", "Tubb1", and "Slc6a11" from the top 10 genes
top_10_genes <- data[order(data$PValue),][1:10,]
top_10_genes <- top_10_genes[!top_10_genes$`Gene Symbol` %in% nodes_of_interest,]

# Loop through the top 10 genes to add labels
for (i in 1:nrow(top_10_genes)) {
  text(top_10_genes$Abundance[i], -log10(top_10_genes$PValue[i]), 
       labels = top_10_genes$`Gene Symbol`[i], pos = 3, col = "black", cex = 1)
}


# Add custom x-axis with increased tick marks
axis(1, at = seq(-1, 1, by = 0.25), pos = y_axis_intersection, tick = TRUE)

# Add custom y-axis with increased tick marks and labels
axis(2, at = seq(0, max(-log10(data$PValue)) + 0.5, by = 3), las = 1)

# Add legend at the bottom right
legend("bottomright", legend = c("Top 100 Upregulated", "Top 100 Downregulated", "n.s."), 
       col = c("blue", "red", "black"), pch = 16, cex = 1, inset = c(0, 0.05), xpd = TRUE, text.width = .3)

# Export as landscape 5"x10"