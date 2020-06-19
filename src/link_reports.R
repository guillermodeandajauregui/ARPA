
individual_reports <-function(single_plots, qc_results, input_eds, output ){
  ################################################################################
  #Write individual reports
  ################################################################################

  make_reports(plot_list = single_plots, 
             result_table = test.results, 
             input = input_eds,
             outdir = output)

}


# booklet_report <- function(test_results, qc_results, input_eds, output){
  
#  ################################################################################ 
#  #Get the plate name
#  ################################################################################

#  plate <- stringr::str_remove(string = basename(input_eds), pattern = ".eds")

#  ################################################################################
#  #Write plate Booklet
#  ################################################################################

#  test_diagnosis <- cbind(plate, test_results)

#  plateBooklet(results = test_diagnosis,
#              qc_results = qc_results,
#              outdir = output)
# }