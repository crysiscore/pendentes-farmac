fina_dd <- readxl::read_xlsx(path = '~/Git/pendentes_farmac/final_dd.xlsx',col_names = TRUE,na = ".")

library(ggplot2)
library(dplyr)

ggplot(fina_dd, 
       aes(x = unidade_sanitaria, 
           fill = status)) + 
  geom_bar(position = position_dodge(preserve = "single"))


a <- fina_dd %>% group_by(unidade_sanitaria,status) %>% summarise(n=n())

writexl::write_xlsx(x = a,path = '~/Git/pendentes_farmac/dd_result.xlsx',col_names = TRUE,format_headers = TRUE)
