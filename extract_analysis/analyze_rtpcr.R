library(shiny)
library(shinythemes)
library(shinyFiles)
library(DT)
library(janitor)
library(cowplot)
library(ggpubr)
library(growthrates)
library(pdftools)
library(kableExtra)

source("extract_analysis/funcion_berlin.R")

input_eds <- "/Users/laura/Desktop/data7500/15052020 corrida28 ibt unam insabi.eds" 
#input_txt <- "/Users/laura/Desktop/data7500/15052020 corrida28 ibt unam insabi_data.txt"
output <- "/Users/laura/Desktop/data7500/test/"

all_results <- funcion_berlin(
  input_eds = input_eds, 
  output = paste(output, "/", sep=""))

