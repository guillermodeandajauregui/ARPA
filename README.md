# ARPA
Automatic RT-PCR Analysis; A tool for massive detection of COVID-19

## MOTIVATION
ARPA is designed to help the specialized staff working on the detection of COVID-19 thorugh the technique of RT-PCR. ARPA's main goal is to automatize some laborious steps of the process to speed up the analysis and to decrease the response time.

## FUNCTIONALITIES

ARPA has been designed in a modular setting. It allows the automatic analysis of the amplification curves, the visual inspection of the results via a friendly web-interface and the generation of pdf reports with the results from the experiment.

### ATUOMATIC ASSESMENT OF THE EXPERIMENT'S QUALITY AND OF THE AMPLIFICATION CURVES 

The values of the fluorescence per cycle are retrieved from the files exportes by the RT-PCR equipment. The curves are evaluated. A correct amplification should behave as a sigmoid curve and cross a fluroescence threshold before a chosen cycle.

All control samples are evaluated to determine the quality of the experiment.


The curves for each sample are assesed to determine the status of each sample


### FRIENDLY WEB-INTERFACE TO LOOK AT THE AMPLIFICATION CURVES AND THE RESULTS OF THE EXPERIMENT



### AUTOMATIC GENERATION OF PDF REPORTS PER SAMPLE AND A SUMMARY REPORT PER EXPERIMENT



ARPA has several functionalities:

 - It generates and analyzes the amplification curves from fluorescence measured in a RT-PCR experiment
 - It evaluates the control curves to assess the quality of the experiment
 - It evalutes the sample curves to determine the status of each sample
 - It provides a friendly web-interface to look at each amplification curve
 - It provides a friendly web-interface to look at a summary table with the results of the experiment
 - It generates pdf reports with the amplification curves and the status per each sample
 - It generates a pdf report with a summary report with the results of the experiment
  
 A summary video can be found at:
 https://drive.google.com/file/d/1SuLN44O7MULU4wht42cfPOu3PWVCWRRK/view?usp=sharing
 
 
