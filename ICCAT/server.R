## server.r
library(shiny)
library(stringr)
require(rCharts)
library(plyr)
library(reshape)
library(ggplot2)
library(ncdf4)

dirMain     <- paste(getwd(),"/taha/",sep="")
# dirMain     <-  "/home/taha/Projets/Taha_VPA_orig1/"

dirRScripts <- paste(dirMain, "Rscripts/", sep="")
dirData     <- paste(dirMain, "data/", sep="")
dirRData    <- paste(dirMain, "Rdata/", sep="")


###########
##########
dir_main <- dirMain
dir_R_scripts <- dirRScripts
dir_data     <- dirData
# setwd("/home/taha/Projets/VPA_StepByStep/Shiny_App_chat_ICCAT_v0_OnLine")
dirMain     <- paste(getwd(),"/taha/",sep="")
dirRScripts <- paste(dirMain, "Rscripts/", sep="")
dir_R_scripts <- dirRScripts
# dirRreport <- paste(dirMain, 'reports/', sep="")
dirData <- paste(dirMain, 'data/', sep="")
dir_data <- dirData
dirRData <- paste(dirMain, 'Rdata/', sep="")
# source(paste(dirRScripts, 'plot_output_VPA_function.R', sep=""))
# source(paste(dirRScripts, 'plot_outputs_VPA.R', sep=""))
# source(paste(dirRScripts, 'OutputsVPA2NetCDF.R', sep=""))



# Globally define a place where all users can share some reactive data.
vars <- reactiveValues(chat=NULL, users=NULL)

# Restore the chat log from the last session.
if (file.exists("chat.Rds")){
  vars$chat <- readRDS("chat.Rds")
} else {
  vars$chat <- "Welcome !"
}



#' Get the prefix for the line to be added to the chat window. Usually a newline
#' character unless it's the first line.
linePrefix <- function(){
  if (is.null(isolate(vars$chat))){
    return("")
  }
  return("<br />")
}

shinyServer(function(input, output, session) {
  
  # Create a spot for reactive variables specific to this particular session
  sessionVars <- reactiveValues(username = "")
  
  # Track whether or not this session has been initialized. We'll use this to
  # assign a username to unininitialized sessions.
  init <- FALSE
  
  # When a session is ended, remove the user and note that they left the room. 
  session$onSessionEnded(function() {
    isolate({
      vars$users <- vars$users[vars$users != sessionVars$username]
      vars$chat <- c(vars$chat, paste0(linePrefix(),
                                       tags$span(class="user-exit",
                                                 sessionVars$username,
                                                 "left the room.")))
    })
  })
  
  # Observer to handle changes to the username
  observe({
    # We want a reactive dependency on this variable, so we'll just list it here.
    input$user
    
    if (!init){
      # Seed initial username
      sessionVars$username <- paste0("User", round(runif(1, 10000, 99999)))
      isolate({
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                          tags$span(class="user-enter",
                                                    sessionVars$username,
                                                    "entered the room.")))
      })
      init <<- TRUE
    } else{
      # A previous username was already given
      isolate({
        if (input$user == sessionVars$username || input$user == ""){
          # No change. Just return.
          return()
        }
        
        # Updating username      
        # First, remove the old one
        vars$users <- vars$users[vars$users != sessionVars$username]
        
        # Note the change in the chat log
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                          tags$span(class="user-change",
                                                    paste0("\"", sessionVars$username, "\""),
                                                    " -> ",
                                                    paste0("\"", input$user, "\""))))
        
        # Now update with the new one
        sessionVars$username <- input$user
      })
    }
    # Add this user to the global list of users
    isolate(vars$users <- c(vars$users, sessionVars$username))
  })
  
  # Keep the username updated with whatever sanitized/assigned username we have
  observe({
    updateTextInput(session, "user", 
                    value=sessionVars$username)    
  })
  
  # Keep the list of connected users updated
  output$userList <- renderUI({
    tagList(tags$ul( lapply(vars$users, function(user){
      return(tags$li(user))
    })))
  })
  
  # Listen for input$send changes (i.e. when the button is clicked)
  observe({
    if(input$send < 1){
      # The code must be initializing, b/c the button hasn't been clicked yet.
      return()
    }
    isolate({
      # Add the current entry to the chat log.
      vars$chat <<- c(vars$chat, 
                      paste0(linePrefix(),
                             tags$span(class="username",
                                       tags$abbr(title=Sys.time(), sessionVars$username)
                             ),
                             ": ",
                             tagList(input$entry)))
    })
    # Clear out the text entry field.
    updateTextInput(session, "entry", value="")
  })
  
  # Dynamically create the UI for the chat window.
  output$chat <- renderUI({
    if (length(vars$chat) > 500){
      # Too long, use only the most recent 500 lines
      vars$chat <- vars$chat[(length(vars$chat)-500):(length(vars$chat))]
    }
    # Save the chat object so we can restore it later if needed.
    saveRDS(vars$chat, "chat.Rds")
    
    # Pass the chat log through as HTML
    HTML(vars$chat)
  })
  
  
  dir_save  <- dirRData
  
  # to do : read netcdf instead of Rdata
  read_data_nc <- function(RS,dir_save){
    RS <- gsub(' SEED:','_',RS)
    for(nrun in 1:length(RS)){
      ncName <- paste("data_retro_",RS[nrun],".nc",sep="")
      data_nc <- NetCDFtoRdata(nc_name=ncName, nc_path=dir_save)
      data_nc <- data_nc$main
      data_nc$Run_seed <- RS[nrun]
      if(nrun==1 ){
        data_nc1 <- data_nc
      } else {data_nc1 <- rbind(data_nc1,data_nc)}
    }
    return(data_nc1)
    
  }
  
  dir_save  <- "http://mdst-macroes.ird.fr:8080/thredds/dodsC/AWA/Ocean_Model_Output/ICCAT_BFT-E_Stock_Assessment_model/taha_04_02_2016/"
  # RUN_SEED <- reactive({unlist(strsplit(input$run, "_(?=[^_]+$)", perl=TRUE))})
  # data_plot <- read_data_nc(RUN_SEED[1], RUN_SEED[2], dir_save=dir_save)
  
  data_plot <- reactive({read_data_nc(input$run, dir_save=dir_save)})
  
  
  #data_runs1 <- data_runs()
  
  output$plots1_2 <- renderUI({
    plot_output_list <- lapply(1:2, function(i) {                               
      plotname <- paste("plot", i, sep="")
      chartOutput(plotname, "highcharts")
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  output$plots3_4 <- renderUI({
    plot_output_list <- lapply(3:4, function(i) {                               
      plotname <- paste("plot", i, sep="")
      chartOutput(plotname, "highcharts")
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  for (i in 1:4) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      
      
      
      output[[plotname]] <- renderChart2({
        
        data_plot1 <- data_plot()      
        un <- unique(data_plot1$variable)
        data_sub <- data_plot1[data_plot1$variable==un[my_i] & data_plot1$Retros==0,]
        h1 <- hPlot(value ~ Year , data = data_sub, type = "line", group = "Run_seed", title=as.character(un[my_i]))
        h1$chart(zoomType = "xy")
        h1$exporting(enabled = T)
        h1$params$width <- 500
        h1$params$height <- 300
        #h1$save(paste("titi", my_i, ".html", sep=""))
        #print(str(h1))
        return(h1)
      })
    })
  }
})
