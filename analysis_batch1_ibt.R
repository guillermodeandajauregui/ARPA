library(tidyverse)
source("data_local/Archivos 7500 ibt/corrida_46.R")
arpa_results_v1 <- vroom::vroom("results_local/validation/resultados_rendimiento_ibt.txt")
manu_results_v1 <- vroom::vroom("data_local/Archivos 7500 ibt/resultados_manuales.tsv.csv")
#manu_results_v1 <- vroom::vroom("data_local/Nuevos_archivos_inmegen/laufix/IBT_batch1_simple.tsv", col_names = F)
#manu_results_v1 <- vroom::vroom("data_local/Nuevos_archivos_inmegen/", col_names = F)

manu_results_v1 %>%
  mutate(Folio= as.character(Folio)) %>%
  left_join(arpa_results_v1, by = c("Folio"="sample")) %>%
  filter(!(Folio%in%corrida_46)) %>% 
  rename(resultado_manual = Resultado,
         resultado_arpa   = classification1) %>%
  mutate(resultado_manual = str_to_lower(resultado_manual),
         resultado_arpa   = str_to_lower(resultado_arpa)) %>%
  mutate(resultado_manual = ifelse(resultado_manual=="posiivo", "positivo", resultado_manual)) %>% 
  group_by(resultado_manual, resultado_arpa) %>%
  tally() 

# 
# 
# 
# arpa_results_v1 <-
#   arpa_results_v1 %>% 
#   mutate(corrida_fecha = str_split(string = corrida, pattern = "corrida")) %>% 
#   #mutate(corrida_ska = str_extract(corrida, "corrida[0-9]*")) %>% 
#   mutate(corrida_fecha = map_chr(corrida_fecha, function(i){i[[1]]})) %>% 
#   #mutate(id = glue::glue("{corrida_ska}_{sample}"))
#   mutate(id = glue::glue("{corrida_fecha}_{sample}"))
# 
# manu_results_v1 <-
#   manu_results_v1 %>% 
#   #mutate(X1 = str_replace(X1, " ", replacement = "")) %>% 
#   mutate(X1 = str_sub(X1, end = 8L)) %>% 
#   mutate(id = glue::glue("{X1}_{X2}"))
# 
# right_join(arpa_results_v1, manu_results_v1) %>% 
#    rename(clasificacion_manual = X3) %>% 
#    select(id, classification1, clasificacion_manual) %>% 
#    drop_na() #%>% 
#    group_by(clasificacion_manual, classification1) %>% 
#    tally() #%>% 
# #   drop_na()
# 
# manu_results_v1  %>% 
#   select(X1, X2, id)
# arpa_results_v1 %>% 
#   select(corrida, sample, id)
# 
# 
# intersect((arpa_results_v1$sample %>% sort), (manu_results_v1$X2 %>% sort))
# 
