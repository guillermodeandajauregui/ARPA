library(tidyverse)
library(vroom)
library(janitor)
source("src/functions.R")
source("src/functions_adjustment.R")
source("src/functions_sanitizing.R")
source("src/getVolumes.R")
source("src/plots.R")
source("src/reports.R")

funcion_berlin_fromManualResults <- function(input_eds, 
                                             input_txt, 
                                             output){
  
  
  #####################
  #Sanity checks 
  #####################
  
  ### check eds has results
  
  has_results <- CheckResultsEDS(eds = input_eds)
  if(has_results == FALSE){
    return("no_results")
  }
  
  ### check sample names in eds have no special characters (only AZaz09 and -)
  
  #has_CleanNames <- CheckNamesEDS(eds = input)
  #if(has_CleanNames == FALSE){
  #  return("special_characters_in_names")
  #}
  
  ########
  #define probes
  ########
  
  berlin_probes <- c("Gen E", "Gen Rd.Rp", "Gen Rp")
  names(berlin_probes) <- berlin_probes
  
  ### check that all samples have all probes
  
  has_allProbes <- CheckProbesEDS(eds = input_eds, my_probes = berlin_probes)
  if(has_allProbes == FALSE){
    warning("some_samples_are_missing_probes")
  #  return("some_samples_are_missing_probes")
  }
  
  ##############################################################################
  #define QC names 
  #this is hard code for now, until we get feedback on actual names and roles
  #used by analytics team
  
  #CURRENT REQUESTS OMIT DOING QC CHECK
  
  ##############################################################################
  
  #qc_names = c("NTC", "PTC", "CRE")
  
  
  #read eds 
  
  my_deltaRN <- tidy_deltaRN(input_eds) #read deltaRN from EDS file 
  
  
  #read txt 
  
  my_results <- vroom::vroom(input_txt, skip = 8, delim = "\t") 
  my_results <- janitor::clean_names(my_results)
  ###may come with some NAs, so lets drop them
  
  my_results <- my_results %>% 
    filter(!is.na(sample_name)) %>% 
    #and let's make the cts numeric, so that we can do logics
    mutate(ct = ifelse(ct == "Undetermined", Inf, ct)) %>% 
    mutate(ct = as.numeric(ct)) 
  
  
  #what is a sample? 
  #samples start with "CoV-"
  
  my_samples = grep(pattern = "^CoV-", x = my_results$sample_name, value = T)
  
  my_results <-
  my_results %>% 
    filter(sample_name%in%my_samples)
  
  #we wont carry the QC samples because FQ uses a different approach to QC
  
  #manipulate so that each row is a sample
  
  results.pivoted <- 
    my_results %>% 
    select(sample_name, target_name, ct, ct_threshold) %>% 
    pivot_wider(id_cols = sample_name,
                names_from = target_name, 
                values_from = ct) #%>% 
    #janitor::clean_names()
  
  test.samples = results.pivoted
  
  # #break results in samples and qc
  # 
  # 
  # 
  # 
  # test.samples <-
  #   results.pivoted %>% 
  #   filter(sample_name%in%my_samples) #%>% 
  #   #pull(sample_name) %>% .[!is.na(.)]
  # 
  # qc.samples <-
  #   results.pivoted %>% 
  #   filter(!(sample_name%in%my_samples)) #%>% 
  #   #pull(sample_name) %>% .[!is.na(.)]
  
  ##do results for qc
  ##############################################################################
  #this is a temporal working version
  #we need to get 
  #standard names for controls
  #standard names for genes/probes
  #acceptance status for each probe
  ##############################################################################
  
  ####NO QC DONE
  
  # qc.results <- 
  # qc.samples %>% 
  # mutate(classification = case_when(sample_name == "NTC" & gen_e > 38 & gen_r_nasa_p > 38~ "PASS", 
  #                                   sample_name == "CRE" & gen_e > 38 & gen_r_nasa_p > 38~ "PASS", 
  #                                   sample_name == "PTC" & gen_e <= 38 & gen_r_nasa_p <= 38 ~ "PASS", 
  #                                   TRUE ~ as.character("FAIL")
  #                                   )
  #        )
  # 
  # 
  # ntc.pass = (qc.results %>% filter(sample_name=="NTC") %>% pull(classification))
  # ptc.pass = (qc.results %>% filter(sample_name=="PTC") %>% pull(classification))
  # ec.pass  = (qc.results %>% filter(sample_name=="PTC") %>% pull(classification))
  # 
  # qc.assess = all(sapply(list(ntc.pass, ptc.pass, ec.pass), function(i){i=="PASS"}))
  # qc.assess = ifelse(qc.assess, "PASS", "FAIL")
  # 
  # qc_results <- list(qc.values = qc.results,
  #                      ntc.pass = ntc.pass,
  #                      ptc.pass = ptc.pass,
  #                      ec.pass  = ec.pass,
  #                      QC = qc.assess)
  # 
  fq_message = "FQ does not asks for QC"
  qc_results <- list(qc.values = fq_message,
                     ntc.pass = fq_message,
                     ptc.pass = fq_message,
                     ec.pass  = fq_message,
                     QC = fq_message)
  
  #do results for samples 
  
  test_results <- 
    test.samples %>% 
    #mutate(classification = ifelse(gen_e <= 38, "positive", "negative" ))
    mutate(classification = case_when((`Gen E` <= 38 & `Gen Rd.Rp` <= 38 &  `Gen Rp` <= 38) ~ "positivo",
                                      (`Gen E` > 38 & `Gen Rd.Rp` > 38 & `Gen Rp` <= 38) ~ "negativo", 
                                      `Gen Rp` > 38 ~ "invalido", 
                                      TRUE ~ "indeterminado"
                                      
                                      )
           )
  
  ################################################################################
  #Plot preparation
  ################################################################################
  
  threshold_list = 
    my_results %>% 
    select(target_name, ct_threshold) %>% 
    unique() %>% 
    pull(ct_threshold) %>% 
    as.list()
  
  names(threshold_list) = 
    my_results %>% 
    select(target_name, ct_threshold) %>% 
    unique() %>% 
    pull(target_name) 
    
  
  
  #############
  #list of single figure plots
  #############
  
  all_samples <- my_results$sample_name %>% unique
  names(all_samples) <- all_samples
  
  single_plots <- lapply(all_samples, FUN = function(i){
    sample_curve.berlin.manual(tdrn = my_deltaRN, 
                               sample_id = i, 
                               th_list = threshold_list,
                               probes = berlin_probes
    )
  })
    
    
  ########
  #This functions need to be fixed to take either
  #A) the QC samples used by analytical team (HARDCODED)
  #or 
  #B) Arbitrary QC samples defined by user 
  ########
  # plots.qc <- plot.curves(tdrn = my_deltaRN, 
  #                         probes = berlin_probes, 
  #                         threshold_list = threshold_list,
  #                         qc = T)
  
  # plots.samples <- plot.curves(tdrn = my_deltaRN, 
  #                              probes = berlin_probes, 
  #                              threshold_list = threshold_list,
  #                              qc = F)
  
  
  ################################################################################
  #Create list output
  ################################################################################
  
  results_list <- list(
    test_results = test_results,
    qc_results = qc_results, 
    single_plots = single_plots
  )
  
  return(results_list)
  
}
