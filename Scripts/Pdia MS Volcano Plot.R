# Load necessary R libraries
library(readxl)

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

# Add custom x-axis with increased tick marks
axis(1, at = seq(-1, 1, by = 0.25), pos = y_axis_intersection, tick = TRUE)

# Add custom y-axis with increased tick marks and labels
axis(2, at = seq(0, max(-log10(data$PValue)) + 0.5, by = 3), las = 1)

# Add labels for the top 10 genes
top_10_genes <- data[order(data$PValue),][1:10,]
text(top_10_genes$Abundance, -log10(top_10_genes$PValue), 
     labels = top_10_genes$`Gene Symbol`, pos = 3, col = "black", cex = 0.7)  # Adjust label size (cex)

# Add legend at the bottom right
legend("bottomright", legend = c("Top100 Upregulated", "Top100 Downregulated", "n.s."), 
       col = c("blue", "red", "black"), pch = 16, cex = 0.7, inset = c(0, 0.05), xpd = TRUE)
