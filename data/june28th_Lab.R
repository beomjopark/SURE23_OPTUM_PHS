
analytic_data2023_0 %>% select(starts_with("v005_race"))

racial_proportion = 
analytic_data2023_0 %>% select(v055_rawvalue, v081_rawvalue, v054_rawvalue,v056_rawvalue, v126_rawvalue)

plot(
  (analytic_data2023_0 %>% select(starts_with("v005_race")) %>% 
sapply(.,
       function(x) ifelse(is.na(x), 0, x)) * as.matrix(racial_proportion) ) %>% rowSums,
analytic_data2023_0 %>% pull("v005_rawvalue"))
abline(a=0, b=1, col="red")


#PHS_RATIO = sum of all PHS / # pop =  sum(PHS_R) / #pop_R

analytic_data2023_0 %>% mutate(pop_count = v080_denominator) %>%
  select(statecode, countycode, county, pop_count)

analytic_data2023_0 %>% select(v080_denominator, v081_denominator, v054_denominator,
                               v056_denominator, v126_denominator)
