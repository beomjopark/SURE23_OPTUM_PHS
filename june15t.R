data_interim$v005_race_aian + data_interim$v005_race_asian + data_interim$v005_race_black + data_interim$v005_race_hispanic + data_interim$v005_race_white

changeNAtozZero = function(x) ifelse(is.na(x), 0, x)
plot(
data_interim %>%
  select(127:131) %>%
  lapply(changeNAtozZero) %>% as.tibble %>%
  rowSums,
data_interim$v005_rawvalue)
abline(a=0, b=1, col="red")