
individual_reports <-function(result_table, output){
  ################################################################################
  #Write individual reports
  ################################################################################

	make_reports(result_table = result_table, 
				outdir = output,
				sample = TRUE)

}

booklet_report <- function(plot_list, result_table, input_eds, output){

 ################################################################################
 #Write plate Booklet
 ################################################################################

	make_reports(plot_list = plot_list, 
				result_table = result_table, 
				input = input_eds,
				outdir = output,
				sample = FALSE)
}