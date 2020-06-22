
individual_reports <-function(results_list, output){
  ################################################################################
  #Write individual reports
  ################################################################################

	make_reports(result_table = results_list$test_results, 
				outdir = output,
				sample = TRUE)

}

booklet_report <- function(results_list, input_eds, output){

 ################################################################################
 #Write plate Booklet
 ################################################################################

	make_reports(plot_list = results_list$single_plots, 
				result_table = results_list$test_results, 
				input = input_eds,
				outdir = output,
				sample = FALSE)
}