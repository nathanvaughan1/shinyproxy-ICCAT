This is a Dockerfile repository to build a shinyproxy image for the ICCAT shiny app
the ICCAT app is hosted at 
https://github.com/TahaImz/Shiny_App_chat_ICCAT_v0_OnLineApp

The docker image hosted at 
https://hub.docker.com/r/nathanvaughan/shinyproxy-iccat/

it is automaticaly built from the shiny app repository hosted at  
https://github.com/nathanvaughan1/shinyproxy-ICCAT

and will be updated if this repository is edited.

Additional shiny proxy information
    
name: ICCAT

display-name: ICCAT shiny app

docker-cmd: ["R", "-e shiny::runApp('/root/ICCAT')"]

docker-image: nathanvaughan/shinyproxy-ICCAT
