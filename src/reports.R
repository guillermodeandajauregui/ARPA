################################################################################
#
# R code for generat reports - COVID19 eds app/COVID binnacle
# by: INMEGEN Computational Genomics Dept
# Hugo Tovar hatovar@inmegen.gob.mx and
# 
################################################################################




################################################################################
#required libs
################################################################################
library("rmarkdown")


######################################################################################
######################################################################################
#                                    s     r    c                                    #
######################################################################################
######################################################################################

######make_reports

make_reports <- function(plot_list,
                        result_table,
                        input,
                        outdir,
                        sample = TRUE){
  if(sample == TRUE){
      lapply(seq_along(result_table$sample_name), function(i){
        the_sample_is <- result_table$sample_name[i]
        my_table <- data.frame(gen=c("E", "RdRp", "Rnasa P"), cycle = (t(result_table[i,])[2:4]))
        my_table[1,2] = ifelse(my_table[1,2] == Inf, "S/A", my_table[1,2])
        my_table[2,2] = ifelse(my_table[2,2] == Inf, "S/A", my_table[2,2])
        my_table[3,2] = ifelse(my_table[3,2] == Inf, paste("\\cellcolor{red!50}{S/A}", sep = ""), my_table[3,2])
        classification <- result_table[i,"classification"]
        outpath_inf <- paste0(outdir, "/", the_sample_is, "_",Sys.Date(), "_informe.pdf")
        render("template_inf.Rmd", output_file = outpath_inf)
      })
  }else{
    my_table <- cbind(result_table, obs = rep(x = "---", times = dim(result_table)[1]))
    my_table$classification[which(my_table$classification %in% "negativo")] <- "NEGATIVO"
    my_table$classification[which(my_table$classification %in% "positivo")] <- "\\textbf{POSITIVO}"
    for(i in which(my_table$classification %in% "indeterminado")){
        if(my_table$"Gen E"[i] != Inf & my_table$"Gen Rd.Rp"[i] != Inf){
            my_table$obs[i] <- "Amplificación tardía en ambos genes marcadorers"
      }else{my_table$obs[i] <- "Amplificación tardía de solo un gen marcador"}
        }
    my_table$classification[which(my_table$classification %in% "indeterminado")] <- "\\textbf{DUDOSO}"
    my_table$"Gen E"[my_table$"Gen E" == Inf] <- "S/A"
    my_table$"Gen Rd.Rp"[my_table$"Gen Rd.Rp" == Inf] <- "S/A"
    pname <- paste(sort(result_table$sample_name)[1], "a", sort(result_table$sample_name)[length(results_list$test_results$sample_name)], sep = " ")
    classification <- "Ver tabla de resultados"
    plate <- stringr::str_remove(string = basename(input), pattern = ".eds")
    outpath_plate <- paste0(outdir, "/", plate, "_",Sys.Date(), "_inf_placa.pdf")
    render("template_plate.Rmd", output_file = outpath_plate)
  }
}




# make_reports <- function(plot_list, 
#                          result_table,
#                          input,
#                          outdir, 
#                          qc_results,
#                          qc = F){
#   plate <- stringr::str_remove(string = basename(input), pattern = ".eds")
# #  qcplate <- ifelse(qc_results == "PASS", "true", "")
#   #qcs <- which(names(plot_list) %in% c("NTC", "PTC", "CRE"))
#   #smpls <- which(!(names(plot_list) %in% c("NTC", "PTC", "CRE")))
#   #smplsPlots <- plot_list[smpls]
#   #makes reports from a list of plots and some result table
#   if(qc==F){
#   lapply(seq_along(smplsPlots), function(i){
    
#     the_sample_is <- names(smplsPlots)[i]    
#     my_name <- names(smplsPlots)[i]
#     mea_plote <- smplsPlots[i]
#     classification <- ifelse(result_table[i,"classification"] == "negative", "true", "")
    
#     outpath <- paste0(outdir, "/", Sys.Date(), "_", my_name, ".pdf")
#     outpath_inf <- paste0(outdir, "/", Sys.Date(), "_", my_name, "_results.pdf")
#     render("template_inf.Rmd", output_file = outpath_inf)
#     render("template_smpl.Rmd",output_file = outpath)})
#   }else{
#     my_r <- as.matrix(result_table)[,c("sample_name", "gen_e", "gen_r_nasa_p")]
#     my_r[grep("Inf", my_r)] <- "45+"
#     ntc <- grep(pattern = "NTC", x = names(plot_list))
#   ptc <- grep(pattern = "PTC", x = names(plot_list))
#   exc <- grep(pattern = "CRE", x = names(plot_list))
#     outpath <- paste0(outdir, "/", Sys.Date(), "_", plate, ".pdf")
#     render("template_qc.Rmd",output_file = outpath)
#   }
# }

## Extracts the plaque processing date from the sample INMEGEN ID
getDecade <- function(idinmegen){
  sapply(sapply(idinmegen, 
            function(x){strsplit(x, split = "-")}), 
    function(x){x[2]})
}

getMonth <- function(idinmegen){
  tail <- sapply(sapply(idinmegen, 
            function(x){strsplit(x, split = "-")}), 
    function(x){x[4]})
  monthID <- substr(tail, start = 3, stop = 3)
  month <- sapply(idinmegen, function(x){which(c("E", "F", "Z", "A", "Y", "J", "L", "G", "S", "O", "N", "D") %in% monthID)})
  return(month)
}

getDay <- function(idinmegen){
  tail <- sapply(sapply(idinmegen, 
            function(x){strsplit(x, split = "-")}), 
    function(x){x[4]})
  day <- substr(tail, start = 1, stop = 2)
  return(day)
}

getDate <- function(idinmegen){
  myDate <- as.Date(paste(paste("20", getDecade(idinmegen), sep = ""), 
                        getMonth(idinmegen), 
                        getDay(idinmegen), sep ="-"))
  return(myDate)
}

## Extract the client ID from the sample INMEGEN ID
getClientID <- function(idinmegen){
  sapply(sapply(idinmegen, 
            function(x){strsplit(x, split = "-")}), 
    function(x){x[3]})
}

# ######################################################################################
# ######################################################################################
# #                                   Inform Lab                                       #
# ######################################################################################
# ######################################################################################


# Print the plate report for the lab
plateBooklet <- function(results, outdir, qc_results){
    qcdf <- data.frame(controType = c("Positivo", "Negativo", "Extracción"), 
                    externalName = c("PTC", "NTC", "CRE"), 
                    used = c("Falla sustancial del reactivo, incluida la integridad del primer y la sonda.", "Contaminación de reactivos y/o ambiente.", "Falla en el procedimiento de lisis y extracción, contaminación potencial durante la extracción."))
    qc_table <- data.frame(qc_results$qc.values[c(3, 1, 2),c(2, 3)])
    my_qc <- cbind(qcdf,qc_table)
    rownames(my_qc) <- c("PTC", "NTC", "CRE")
    my_qc["PTC", c("gen_e", "gen_r_nasa_p")] = ifelse(qc_results$ptc.pass == "PASS", paste("\\color{teal}{", my_qc["PTC", c("gen_e", "gen_r_nasa_p")], "}", sep = "") , paste("\\cellcolor{red!50}{", my_qc["PTC", c("gen_e", "gen_r_nasa_p")], "}", sep = "") )
    my_qc["NTC", c("gen_e", "gen_r_nasa_p")] = ifelse(qc_results$ntc.pass == "PASS", paste("\\color{teal}{", "+45", "}", sep = ""), paste("\\cellcolor{red!50}{", my_qc["NTC", c("gen_e", "gen_r_nasa_p")], "}", sep = "") )
    my_qc["CRE", c("gen_e", "gen_r_nasa_p")] = ifelse(qc_results$ec.pass== "PASS", paste("\\color{teal}{", "+45", "}", sep = ""), paste("\\cellcolor{red!50}{", my_qc["CRE", c("gen_e", "gen_r_nasa_p")], "}", sep = ""))

    my_r <- results[,c("plate", "sample_name", "gen_e", "gen_r_nasa_p", "classification")]
      my_r <- cbind(nsample = sub('(^[0-9]$)','0\\1', 1:length(my_r[,1])), my_r)
      my_r[,"gen_e"] = ifelse(my_r[,"gen_e"] == "Inf", "Indeterminado", paste("\\cellcolor{red!50}{", my_r[,"gen_e"], "}", sep = ""))
      my_r[,"classification"] = ifelse(my_r[,"classification"] == "positive", "\\cellcolor{red!50}{positivo}", 
                                      ifelse(my_r[,"classification"] == "negative", "\\cellcolor{yellow!50}{negativo}", 
                                            ifelse(my_r[,"classification"] == "inconclusive", "\\cellcolor{cyan!50}{inconclusive}", 
                                                  ifelse(my_r[,"classification"] == "invalid", "\\cellcolor{cyan!50}{invalid}", 
                                                        ifelse(my_r[,"classification"] == "edge_positive", "\\cellcolor{red!25}{positivo marginal}", 
                                                          my_r[,"classification"])))))
    processingdate <- Sys.Date() #unique(getDate(my_r$sample))
    plate <- unique(results$plate)
    outpath <- paste0(outdir, "/", Sys.Date(), "_", plate, "_", "plateBooklet.pdf") 
    render("plateBooklet.Rmd",output_file = outpath)
}




# ######################################################################################
# ######################################################################################
# #                                 Inform Client                                      #
# ######################################################################################
# ######################################################################################

## Print the results report for all clients
resultsReport <- function(results, outdir){
    the_samples <- which(results$qc == "PASS" 
        & (results$classification == "positive" 
          | results$classification == "negative"))
    my_r <- results[the_samples, c("sample", "classification")]
            my_r <- cbind(nsample = sub('(^[0-9]$)','0\\1', 1:length(my_r[,1])), my_r)
            my_r$classification = ifelse(my_r$classification == "positive", paste("\\cellcolor{red!50}{", "positive", "}", sep = ""),"negative")
    client <- getClientID(my_r$sample)
    n_samples <- length(rownames(my_r))
    allsamples <- length(rownames(results))
    processingdate <- unique(getDate(my_r$sample))
    client_file <- paste0(gsub("[^[:alnum:]]", "_", unique(client[order(client)])), collapse = "-")# Claimant name institution safe for file name
    outpath <- paste0(outdir, "/", Sys.Date(), "_", client_file, "_", "report.pdf") 
    render("resultsReport.Rmd",output_file = outpath)
  }

## Print the results report by clients

makeReports <- function(table_diagnosis, # data with columns like simulated data
                         outdir # The report output directory will be defined either by the user or by the DDT
                         ){
    results <- cbind(table_diagnosis, id_client = getClientID(table_diagnosis$sample))
    lapply(split(results,results$id_client), resultsReport, outdir = outdir)
  }



