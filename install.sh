
#######################################################
##### THIS SCRIPT WILL INSTALL ALL R DEPENDENCIES
#######################################################


##### VERIFY THE EXISTENCE OF COMMAND RSCRIPT
command -v Rscript >/dev/null 2>&1 || { echo >&2 "I require Rscript but it's not installed.  Aborting."; exit 1; }


##### INSTALLING DEPENDENCIES
sudo Rscript -e 'install.packages("shiny", repos="http://cran.us.r-project.org")'
sudo Rscript -e 'install.packages("shinythemes", repos="http://cran.us.r-project.org")'
sudo Rscript -e 'install.packages("shinyFiles", repos="http://cran.us.r-project.org")'
sudo Rscript -e 'install.packages("DT", repos="http://cran.us.r-project.org")'
sudo Rscript -e 'install.packages("tidyverse", repos="http://cran.us.r-project.org")'
sudo Rscript -e 'install.packages("rmarkdown", repos="http://cran.us.r-project.org")'
sudo Rscript -e 'install.packages("cowplot", repos="http://cran.us.r-project.org")'
sudo Rscript -e 'install.packages("ggpubr", repos="http://cran.us.r-project.org")'
sudo Rscript -e 'install.packages("kableExtra", repos="http://cran.us.r-project.org")'
sudo Rscript -e 'install.packages("growthrates", repos="http://cran.us.r-project.org")'
sudo Rscript -e 'install.packages("fs", repos="http://cran.us.r-project.org")'
sudo Rscript -e 'install.packages("janitor", repos="http://cran.us.r-project.org")'


##### VERIFY THE EXISTENCE OF COMMAND RSCRIPT
command -v apt-get >/dev/null 2>&1 || { echo >&2 "LATEX UPDATE COULD NOT BE DONE. apt-get REQUIRED. IF YOU ARE ON MacOSX YOU SHOULD UPDATE YOUR LATEX PACKAGES IN LIVE-MAC UTILITY."; exit 1; }

apt-get install texlive-full
