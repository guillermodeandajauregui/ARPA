# ARPA
Automatic RT-PCR Analysis; A tool for massive detection of COVID-19

## MOTIVATION
ARPA is designed to help the specialized staff working on the detection of COVID-19 thorugh the technique of RT-PCR. ARPA's main goal is to automatize some laborious steps of the process to speed up the analysis and to decrease the response time.

## FUNCTIONALITIES

ARPA has been designed in a modular setting. It allows the automatic analysis of the amplification curves, the visual inspection of the results via a friendly web-interface and the generation of pdf reports with the results from the experiment.

ARPA has several functionalities:

 - It generates and analyzes the amplification curves from fluorescence measured in a RT-PCR experiment
 - It evaluates the control curves to assess the quality of the experiment
 - It evalutes the sample curves to determine the status of each sample
 - It provides a friendly web-interface to look at each amplification curve
 - It provides a friendly web-interface to look at a summary table with the results of the experiment
 - It generates pdf reports with the amplification curves and the status per each sample
 - It generates a pdf report with a summary report with the results of the experiment

### ATUOMATIC ASSESMENT OF THE EXPERIMENT'S QUALITY AND OF THE AMPLIFICATION CURVES 

The values of the fluorescence per cycle are retrieved from the files exportes by the RT-PCR equipment. The curves are evaluated. A correct amplification should behave as a sigmoid curve and cross a fluroescence threshold before a chosen cycle.

![alt text](https://github.com/guillermodeandajauregui/ARPA/blob/master/images/Presentacion_20200522.png?raw=true)

- All control samples are evaluated to determine the quality of the experiment.

- The curves for each sample are assesed to determine the status of each sample

### FRIENDLY WEB-INTERFACE TO LOOK AT THE AMPLIFICATION CURVES AND THE RESULTS OF THE EXPERIMENT

**The web interface allow the user to load the results file generated by the RT-PCR machine**

![alt text](https://github.com/guillermodeandajauregui/ARPA/blob/master/images/web-initial.png?raw=true)


**It shows the summary table with the status of each sample of the experiment**

![alt text](https://github.com/guillermodeandajauregui/ARPA/blob/master/images/web-summary.jpeg?raw=true)


**It also allow the visual inspection of the amplification curves per sample**

![alt text](https://github.com/guillermodeandajauregui/ARPA/blob/master/images/web-sample.png?raw=true)


**Finally, the user can visualize the QC amplification curves**

![alt text](https://github.com/guillermodeandajauregui/ARPA/blob/master/images/web-QC.png?raw=true)





### AUTOMATIC GENERATION OF PDF REPORTS PER SAMPLE AND A SUMMARY REPORT PER EXPERIMENT

A summary report (in PDF format) with the classification per sample and the metadata of the institute of origin is generated.

![plateBooklet](https://github.com/guillermodeandajauregui/ARPA/blob/master/images/report-plateBooklet.png?raw=true)

An individual report (in PDF format) per sample is generated. It contains the amplification curves and plate QC result.

![sample](https://github.com/guillermodeandajauregui/ARPA/blob/master/images/report-sample.png?raw=true)


A quality report of the plate with the quality assesment and the control amplification curves is generated

![qcplate](https://github.com/guillermodeandajauregui/ARPA/blob/master/images/report-qcplate.png?raw=true)


 
*For more information, a summary video can be found at:*

 https://drive.google.com/file/d/1SuLN44O7MULU4wht42cfPOu3PWVCWRRK/view?usp=sharing
 
 
