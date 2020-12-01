library(tidyverse)

arpa_results <- vroom::vroom("results_local/validation/resultados_rendimiento_ibt_v2.txt")
manu_results <- vroom::vroom("data_local/Nuevos_archivos_inmegen/IBT_batch2_simple.tsv", col_names = F)

arpa_results <-
arpa_results %>% 
  mutate(corrida_fecha = str_split(string = corrida, pattern = " ")) %>% 
  mutate(corrida_fecha = map_chr(corrida_fecha, function(i){i[[1]]})) %>% 
  mutate(id = glue::glue("{corrida_fecha}_{sample}"))

manu_results <-
manu_results %>% 
  mutate(id = glue::glue("{X1}_{X2}"))

left_join(arpa_results, manu_results) %>% 
  rename(clasificacion_manual = X3) %>% 
  select(id, classification1, clasificacion_manual) %>% 
  group_by(clasificacion_manual, classification1) %>% 
  tally() %>% 
  drop_na()

manu_results %>% 
  mutate(Folio= as.character(Folio)) %>% 
  left_join(arpa_results, by = c("Folio"="sample")) %>% 
  rename(resultado_manual = Resultado,
         resultado_arpa   = classification1) %>% 
  mutate(resultado_manual = str_to_lower(resultado_manual),
         resultado_arpa   = str_to_lower(resultado_arpa)) %>% 
  group_by(resultado_manual, resultado_arpa) %>% 
  tally()
