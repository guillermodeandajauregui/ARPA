
#######################################################
##### THIS SCRIPT WILL INSTALL ALL R DEPENDENCIES
#######################################################


##### VERIFY THE EXISTENCE OF COMMAND RSCRIPT
command -v Rscript >/dev/null 2>&1 || { echo >&2 "I require Rscript but it's not installed.  Aborting."; exit 1; }


##### INSTALLING DEPENDENCIES
sudo Rscript -e 'install.packages("shiny")'
sudo Rscript -e 'install.packages("shinythemes")'
sudo Rscript -e 'install.packages("shinyFiles")'
sudo Rscript -e 'install.packages("DT")'
sudo Rscript -e 'install.packages("tidyverse")'
sudo Rscript -e 'install.packages("rmarkdown")'
sudo Rscript -e 'install.packages("cowplot")'
sudo Rscript -e 'install.packages("ggpubr")'
sudo Rscript -e 'install.packages("kableExtra")'
sudo Rscript -e 'install.packages("growthrates")'
sudo Rscript -e 'install.packages("fs")'
sudo Rscript -e 'install.packages("janitor")'


##### VERIFY THE EXISTENCE OF COMMAND RSCRIPT
command -v apt-get >/dev/null 2>&1 || { echo >&2 "Latex update could not be done. apt-get required.  Aborting."; exit 1; }

sudo apt-get install texlive-full
