
#Date: 2025_04_28
#Note: Calculate the average % inhibition score for 3 techincial replciates and
#output the correct format for use with the syngeryfinder+ tool. 

#load libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(writexl)


# Set working directory
setwd("~/OneDrive - University of Edinburgh/Brien Lab/Anto/Project_Data/EP300_Project/CTG_Work/CTG_20250428_Third_Round_Synergy/")

# Read in viability data for Drug 1
Full_Plate <- read_excel("FUJI.xlsx", 
                         sheet = "Sheet2", 
                         range = "A24:W38") %>%
  as.data.frame()

# Set the values in column "<>" as row names
rownames(Full_Plate) <- Full_Plate[["<>"]]

# Remove the "<>" column now that it's used as row names
Full_Plate <- Full_Plate[, -1]

# Optionally convert to a numeric matrix
Full_Plate_matrix <- as.matrix(sapply(Full_Plate, as.numeric))

print(Full_Plate)

replicate1 <- Full_Plate[rownames(Full_Plate) %in% LETTERS[2:8], as.character(2:8)]

replicate2 <- Full_Plate[rownames(Full_Plate) %in% LETTERS[2:8], as.character(10:16)]

replicate3 <- Full_Plate[rownames(Full_Plate) %in% LETTERS[2:8], as.character(17:23)]

# Proper dimension checks
dim(replicate1)
dim(replicate2)
dim(replicate3)

# Compare dimensions pairwise
all.equal(dim(replicate1), dim(replicate2))
all.equal(dim(replicate1), dim(replicate3))

# Calculate the mean across the three replicates
replicate_mean <- (replicate1 + replicate2 + replicate3) / 3

# Calculate the standard deviation
replicate_sd <- sqrt(((replicate1 - replicate_mean)^2 + 
                        (replicate2 - replicate_mean)^2 + 
                        (replicate3 - replicate_mean)^2) / 2)


# Extract the DMSO sample value from replicate_mean
dmso_sample <- replicate_mean["B", 1]  # Get the DMSO sample value

# Normalize replicate_mean to the DMSO sample and express as a percentage
normalized_replicate_mean <- (replicate_mean / dmso_sample) * 100

# Print normalized values
print(normalized_replicate_mean)

#Calculate Percent inhibtion
percent_inhibiton <- (100-normalized_replicate_mean)

# Assuming you already have your percentage_inhibition DataFrame
percent_inhibition_df <- as.data.frame(percent_inhibiton)

# 
# # Set the row names to represent concentrations
# rownames(percent_inhibition_df) <- c(0, 0.1, 0.2, 0.4, 0.5, 0.79, 1)
# colnames(percent_inhibition_df) <- c(0, 0.01, 0.02, 0.03, 0.06, 0.08, 0.1)

# Set the row names to represent concentrations
rownames(percent_inhibition_df) <- c(0, 0.1, 0.2, 0.4, 0.5, 0.79, 1)
colnames(percent_inhibition_df) <- c(0, 0.001, 0.002, 0.005, 0.01, 0.015, 0.02)

print(percent_inhibition_df)

# Define the path for the output file
output_file <- "percent_inhibition_data.txt"

# Open a connection to the file
file_connection <- file(output_file, open = "wt")

# Write the header lines
writeLines(c("Drug1:\tACBI-1",
             "Drug2:\tdCBP",
             "Cell:\tHSSYII",
             "ConcUnit:\tuM"), con = file_connection)

# Write the dataframe to the file, ensuring row names are included
write.table(percent_inhibition_df,
            file = file_connection,
            sep = "\t",
            col.names = NA,
            quote = FALSE)

# Close the file connection
close(file_connection)

# Inform the user
cat("Data has been written to", output_file, "\n")

#Data is not in the correct format to use with the below tool:

#https://www.bioconductor.org/packages/release/bioc/vignettes/synergyfinder/inst/doc/User_tutorual_of_the_SynergyFinder_plus.html 



