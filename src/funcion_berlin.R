library(tidyverse)
library(vroom)
library(janitor)
source("src/functions.R")
source("src/plots.R")
source("src/reports.R")

source("src/functions_adjustment.R")
source("src/functions_sanitizing.R")
source("src/getVolumes.R")

funcion_berlin <- function(input_eds, 
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
  
  berlin_probes <- c("Gen E", "Gen RNasaP")
  names(berlin_probes) <- berlin_probes
  
  ### check that all samples have all probes
  
  has_allProbes <- CheckProbesEDS(eds = input_eds, my_probes = berlin_probes)
  if(has_allProbes == FALSE){
    warning("some_samples_are_missing_probes")
  }
  
  ##############################################################################
  #define QC names 
  #this is hard code for now, until we get feedback on actual names and roles
  #used by analytics team
  ##############################################################################
  
  qc_names = c("NTC", "PTC", "CRE")
  
  
  #read eds 
  
  my_deltaRN <- tidy_deltaRN(input_eds) #read deltaRN from EDS file 
  
  # analyze curves, determine sample ct
  test.samples <- test.plate(tdrn = my_deltaRN, probes = berlin_probes)
  names(test.samples) <- c("sample_name", "gen_e", "gen_r_nasa_p")
  
  #curve plot
  plate_curves <- 
    pivot_deltaRN(my_deltaRN) %>% 
    plot_deltaRN.long()
  
  #qc results
  qc_results <- plate_qc.berlin(tdrn = my_deltaRN, 
                                all_probes = berlin_probes)
  

 #do results for samples 
  
  test.results <- 
    test.samples %>% 
    mutate(classification = case_when(gen_e <= 38 & gen_r_nasa_p <= 35 ~ "Positivo",
                                      gen_e > 38 & gen_r_nasa_p <= 35 ~ "Negativo",
                                      gen_r_nasa_p > 35 ~ "Repetir"
    )) 

  #############
  #list of single figure plots
  #############
  
  all_samples <- c(test.results$sample_name %>% unique, qc_names)
  names(all_samples) <- c(all_samples)
  
  single_plots <- lapply(all_samples, FUN = function(i){
    sample_curve.berlin.manual(tdrn = my_deltaRN, 
                               sample_id = i, 
                               probes = berlin_probes
    )
  })
  
  make_reports(plot_list = single_plots, 
              result_table = qc_results$qc.values[,1:3], 
              input = input_eds,
              outdir = output, 
              qc_results = qc_results$QC,
              qc = F)
  
  make_reports(plot_list = single_plots, 
              result_table = qc_results$qc.values[,1:3], 
              input = input_eds,
              outdir = output, 
              qc_results = qc_results$QC,
              qc = T)
  
  ################################################################################
  #Create list output
  ################################################################################
  
  results_list <- list(
    test_results = test.results,
    qc_results = qc_results, 
    single_plots = single_plots
  )
  
  return(results_list)
  
}
