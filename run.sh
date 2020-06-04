#######################################################
##### THIS SCRIPT WILL LAUNCH THE SHINY APP
#######################################################


##### VERIFY THE EXISTENCE OF COMMAND RSCRIPT
command -v Rscript >/dev/null 2>&1 || { echo >&2 "I require Rscript but it's not installed.  Aborting."; exit 1; }

##### WARNING MESSAGE
echo "YOU MUST RUN THIS SOFTWARE FROM THE APPLICATION DIRECTORY"

R -e "shiny::runApp('.', launch.browser = TRUE)"