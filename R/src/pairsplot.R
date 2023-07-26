## Pairsplot
data_chrs23 <- readRDS("./data/processed/data_core_long_30p.rds")
desc = readRDS("./data/processed/colDesc_analysis2023.rds")

race_vars = data_chrs23 %>% select(starts_with("v"), -ends_with("_rawvalue"))  %>% colnames 
desc[sub("_race", "_rawvalue", race_vars)]
sub("_race", "_rawvalue", race_vars) %in% colnames(data_chrs23) # all race subgroup var has rawvalue

# All subgroup variable
data_processed = data_chrs23 %>% select(-sub("_race", "_rawvalue", race_vars), -county) %>%
  mutate(across(where(is.character), as.factor))

library(GGally)
queryVariables = paste0(c("v143", "v144", "v145"), "_rawvalue")
plotData = data_processed %>%
  select(queryVariables, "race") #%>%
#  mutate(v005_race = log(v005_race))

colnames(plotData) = c(
  sub(" raw value", "",
      desc[colnames(plotData[-ncol(plotData)])]), "Race")
#                       sub(" raw value", "", desc["v005_rawvalue"]))
ggpairs(plotData,  mapping = aes(color = Race, alpha=0.001),
        columns = 1:3, 
        lower = list(continuous = wrap("points", alpha = 0.1), 
                     combo = wrap("box", alpha = 0.3), 
                     discrete = wrap("facetbar", alpha = 0.3)))
