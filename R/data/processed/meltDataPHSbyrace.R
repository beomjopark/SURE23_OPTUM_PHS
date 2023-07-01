#melt data 
# reshape for unemployment variables 
data_income = data_analytic_interim[ , c("state","county","v005_race_asian","v005_race_black","v005_race_hispanic", "v005_race_white")]

#melted data for preventable stays
ndimdf_preventable = melt(data_income, id = c("state", "county"),variable.name='race') #this melts the county and state instead of the others

#you may plot but not very useful 
meltplot <- ggplot(ndimdf_preventable, aes_string(x='state', y='county'))+
  geom_point(alpha = 0.2)+
  labs(x='state', y ='county')+
  theme_bw()
print(meltplot)

