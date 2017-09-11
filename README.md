This is a Dockerfile repository to build a shinyproxy image for the LBI shiny app
the LBI app was developed by Scott Large and is hosted at 
https://github.com/ices-tools-dev/LBI_shiny

The docker image is automaticaly built if this repository is changed and hosted at 
https://hub.docker.com/r/nathanvaughan/shinyproxy-lbi/

Additional shiny proxy information
    
name: LBI_shiny

display-name: LBI shiny app

docker-cmd: ["R", "-e shiny::runApp('/root/LBI_shiny')"]

docker-image: nathanvaughan/shinyproxy-lbi
