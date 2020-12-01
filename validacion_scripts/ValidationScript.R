#source("src/functions.R")
#source("src/functions_adjustment.R")
#source("src/plots.R")
#source("validacion_scripts/Berlin_Validation_IBT.R")
source("extract_analysis/funcion_berlin_noReport.R")

#los_eds <- list.files(path = "data_local/Nuevos_archivos_inmegen/", full.names = T, pattern = ".eds")
names(los_eds) <- sapply(los_eds, basename)

names(los_eds) <- str_remove(string = names(los_eds), pattern = ".eds")

#los_eds <- los_eds[grep(pattern = "^P-", x = names(los_eds))]

berlin_probes <- c("Gen E", "Gen RNasaP")

names(berlin_probes) <- berlin_probes


tempus <- Sys.time()
mis_resultados <-
lapply(los_eds, function(my_eds){
  
  # message("We're starting the analysis of: ")
  # print(my_eds)
  # 
  # #read data 
  # 
   my_deltaRN <- try(tidy_deltaRN(my_eds)) #read deltaRN from EDS file 
  # 
  # if(class(my_deltaRN)=="try-error"){
  #   message("ERROR at parsing")
  #   return("cochinero")
  # }
  # 
  # 
  # 
  # 
  # #make qc
  #  qc_results <- try(plate_qc.berlin(tdrn = my_deltaRN, 
  #                         all_probes = berlin_probes))
  # # 
  # # 
  # # if(class(qc_results)=="try-error"){
  # #   message("ERROR at QC")
  # #   return("cochinero")
  # # }
  # 
  # #analyse results
   test.results <- try(test.plate(tdrn = my_deltaRN, probes = berlin_probes) %>% janitor::clean_names())
  # 
  # if(class(test.results)=="try-error"){
  #   message("ERROR at plate testing")
  #   return("cochinero")
  # }
  
  
   test.results <- 
     test.results %>% 
     mutate(classification1 = case_when(gen_e <= 38 & gen_r_nasa_p <= 35 ~ "Positivo",
                                        gen_e > 38 & gen_r_nasa_p <= 35 ~ "Negativo",
                                        gen_r_nasa_p > 35 ~ "Repetir"
     )) %>% 
     mutate(classification2 = case_when(gen_e <= 38  ~ "positive",
                                        gen_e < 99 ~ "edge_positive",
                                        gen_e  == Inf  ~ "inconclusive_LowAmp",
                                        gen_e == 99 & gen_r_nasa_p <= 35 ~ "negative",
                                        gen_e == 99 & gen_r_nasa_p < 99  ~ "edge_negative",
                                        gen_e == 99 & gen_r_nasa_p >= 99 ~ "invalid"
     )) 
  #  test_diagnosis <- 
  #    data.frame(plate    = plate,
  #               ntc.pass = qc_results$ntc.pass, 
  #               ptc.pass = qc_results$ptc.pass, 
  #               ec.pass = qc_results$ec.pass,
  #               qc      = qc_results$QC,
  #               test_diagnosis 
  #    ) %>% as.tbl() %>% select(sample, everything())
  # # 
  #  return(test_diagnosis)
  
  
  
})

mis_resultados %>% bind_rows(.id = "corrida") %>% 
  #vroom::vroom_write(path = "results_local/validation/resultados_rendimiento_ibt.txt")
  vroom::vroom_write(path = "results_local/validation/resultados_rendimiento_ibt_v2.txt")

# tempus <- tempus - Sys.time()
# tempus
# saveRDS(object = mis_resultados, file = "results/rendimiento/resultados_rendimiento_ibt.rds")
# 
# mis_resultados[sapply(mis_resultados, is.data.frame)] %>% 
#   lapply(function(i){i %>% mutate(ec.pass = as.factor(ec.pass))}) %>% 
#   bind_rows() #%>% 
#   vroom::vroom_write(path = "results/rendimiento/resultados_rendimiento_ibt.txt")
# 
# 
