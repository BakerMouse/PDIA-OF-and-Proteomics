# Load the necessary packages
library(dplyr)  # Data manipulation package
library(openxlsx)  # Package for reading and writing Excel files

# Read the .E99 files and combine the data into a data frame
files <- list.files("~/Desktop/R-Files/Doxo Open Field", pattern = ".E99", full.names = TRUE)
first <- TRUE
for (i in files) {
  file <- read.table(i, sep = " ", fill = TRUE, stringsAsFactors = FALSE)
  if (first) {
    ID <- file[1, 1]
    names(ID) <- "ID"
    data <- file[13, 2:45]
    df <- data.frame(c(ID, data))
    first <- FALSE
  } else {
    ID <- file[1, 1]
    names(ID) <- "ID"
    data <- file[13, 2:45]
    newrow <- data.frame(c(ID, data))
    df <- bind_rows(df, newrow) 
  }
}

# Define column names for the data frame
colnams <- c("ID", "FP_MOVES", "FP_MOVE_TIME", "FP_REST_TIME", "FP_DISTANCE", "FP_HOME_BASE", "HOME_BASE_TIME", "STPY-1_MOVES", "FP_VELOCITY", "C-P_TURNS(CCW)", "C-P_TURNS(CW)", "VP_ENTRIES", "VP_TIME", "FP_JUMPS", "STPY-1_EPISODES", "STPY-1_TIME", "AMBL_MOVE_TIME", "AMBL_DISTANCE", "AMBL_VELOCITY", "STPY-2_MOVES", "STPY-2_EPISODES", "STPY-2_TIME", "AMBL_MRGN_DIST", "AMBL_CNTR_DIST", "FP_AVG_DIST/MVT", "FP_LATENCY_MVT", "LATENCY_AMB_MV", "VP-MOVES", "VP_DISTANCE", "VP_STPY-1_MOVES", "VP_LH_TIME", "VP_LH_ENTRIES", "VP_RH_TIME", "VP_RH_ENTRIES", "VP_STPY-1_EPS", "VP_STPY-1_TIME", "vp_STPY-2_MOVES", "VP_STPY-2_EPS", "VP_STPY-2_TIME", "VP_LH_MOVES", "VP_LH_MV_TIME", "VP_LH_REST", "VP_RH_MOVES", "VP_RH_MV_TIME", "VP_RH_REST")
colnames(df) <- colnams

# Write the data frame to a CSV file
write.csv(df,"~/Desktop/R-Files/Doxo Open Field/FILE1.csv")

# Convert the CSV file to an Excel (XLSX) file using openxlsx
write.xlsx(df, file = "~/Desktop/R-Files/Doxo Open Field/FILE1.xlsx")
