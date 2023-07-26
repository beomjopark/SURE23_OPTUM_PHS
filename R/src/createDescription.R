# Script to create variable description
#
# The original data has two rows:
# 1. the long description and 2. the encoded name
# Output is 1 row dataframe of desc (row1) with column names (row2)
library(tidyverse)
desc = colnames(read_csv("data/raw/analytic_data2023_0.csv", n_max=1))
enc = colnames(read_csv("data/raw/analytic_data2023_0.csv", skip=1, n_max=1))

descriptions = data.frame(t(desc))
colnames(descriptions) = enc
row.names(descriptions) = "description"
saveRDS(descriptions, "data/processed/colDesc_analysis2023.rds")

# Use case
descriptions %>% select(starts_with("v005_rawvalue"))
#                                   v005_rawvalue
#description Preventable Hospital Stays raw value
