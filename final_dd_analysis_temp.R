library(dplyr)
library(ggplot2)
final_dd <- readxl::read_xlsx(path = '~/Git/pendentes_farmac/final_dd.xlsx', col_names = TRUE,na = "")



a = final_dd %>% group_by(unidade_sanitaria, status ) %>% summarise(n = n())



ggplot(data = final_dd) + 
  geom_bar(mapping = aes(x = unidade_sanitaria, fill =  status), position = "dodge")
