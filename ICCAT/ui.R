


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

## ui.R
library(shiny)
library(plyr)
require(rCharts)
library(reshape)
library(ggplot2)

dirMain     <- paste(getwd(),"/taha/",sep="")
dirRScripts <- paste(dirMain, "Rscripts/", sep="")
# dirRreport <- paste(dirMain, 'reports/', sep="")
dirData <- paste(dirMain, 'data/', sep="")
dirRData <- paste(dirMain, 'Rdata/', sep="")
source(paste(dirRScripts, 'plot_output_VPA_function.R', sep=""))
source(paste(dirRScripts, 'plot_outputs_VPA.R', sep=""))
source(paste(dirRScripts, 'OutputsVPA2NetCDF.R', sep=""))
# run_parameters <- read.csv(paste(dirData, "run_spec.csv", sep=""))
### load the seed number
load(paste(dirRData, "seed_number.Rdata", sep=""))
seed_nb <- seeds


####################
##########################"
###############################


## choose the runs existing on the repository
dir_save  <- "http://mdst-macroes.ird.fr:8080/thredds/dodsC/AWA/Ocean_Model_Output/ICCAT_BFT-E_Stock_Assessment_model/taha_04_02_2016/"
##
require(RCurl)
require(XML)
webpage <- getURL("http://mdst-macroes.ird.fr:8080/thredds/dodsC/AWA/Ocean_Model_Output/ICCAT_BFT-E_Stock_Assessment_model/taha_04_02_2016/")
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
# parse the tree by tables
x <- xpathSApply(pagetree, "//*/table", xmlValue)  
# do some clean up with regular expressions
x <- unlist(strsplit(x, "\n"))
x <- gsub("\t","",x)
x <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
x <- x[!(x %in% c("", "|"))]
####

ind <- grep(pattern ='data_retro',x)
###
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
##
ind2 <- which(substrRight(x[ind], 2) != 'nc')
listRun <- x[ind[-ind2]]
listRun2 <- strtrim(listRun, nchar(listRun)-3)
tabl <- data.frame(list_run=listRun2)
toto <- unlist(strsplit(listRun2, "_(?=[^_]+$)", perl=TRUE))
toto1 <- toto[seq(2,length(toto),2)]
tabl$seed <- toto1
toto2 <- toto[seq(1,length(toto),2)]
toto2 <- gsub('data_retro_','',toto2,perl=T)
tabl$run <- toto2

run_seed <- paste(tabl$run,tabl$seed,sep="_")
runSEED <- paste(tabl$run,tabl$seed,sep=' SEED:')
#####
######
##########
#############



shinyUI(pageWithSidebar(
  # We'll add some custom CSS styling -- totally optional
  includeCSS("www/styling1.css"),
  
  # And custom JavaScript -- just to send a message when a user hits "enter"
  # and automatically scroll the chat window for us. Totally optional.
  includeScript("www/sendOnEnter.js"),
  
  div(
    # Setup custom Bootstrap elements here to define a new layout
    class = "container-fluid", 
    div(class = "row-fluid",
        # Set the page title
        tags$head(tags$title("East/Med Bluefin tuna stock assessment")),
        img(src="bluefin-tuna.png", height = 150, width = 380),
        tags$style(".span12 {background-color: black;}"),
        # Create the header
        div(class="span6", style="padding: 10px 0px;",
            h1("East/Med Bluefin tuna stock assessment"),
            # h1("Chat members "),
            # h4("East/Med Bluefin tuna stock assessment"),
            h6(format(Sys.Date(), format="%B %d %Y"))
        ), div(class="span6", id=as.character(Sys.info()["nodename"]),
               "IP Addresses are logged... be a decent human being."
        )
        
        
    ),
    # The main panel
    div(class = "row-fluid",
        
        
        
        # The right sidebar
        sidebarPanel(
          
          uiOutput("chat"),
          # Let the user define his/her own ID
          textInput("user", "Your User ID:", value=""),
          tags$hr(),
          # Create the bottom bar to allow users to chat.
          fluidRow(
            div(class="span10",
                textInput("entry", " Comment message:")
            ),
            div(class="span2 center",
                actionButton("send", "Send")
            )
          ),
          h5("Connected Users"),
          # Create a spot for a dynamic UI containing the list of users.
          uiOutput("userList"),
          tags$hr(),
          helpText(HTML("<p><a href = \"mailto:?=taha.imzilen@ird.fr,Julien.Barde@ird.fr \">admins contacts</a>.")),
          tags$style(type="text/css", "select { width: 10px; }"), # defines width of dropdown panel
          tags$style(type='text/css', ".span4 { max-width: 10px; }") # defines width of panel containing dropdown panel
          
          
        ),
        div(sidebarPanel(
          selectInput(inputId = "mode",
                      label = "                   Plot one specific run or several runs",
                      choices = c( 'Several'),
                      selected = "Several"
          ),
          
          # conditionalPanel(condition  = "input.mode == 'One run'",
          #                  selectizeInput(inputId  = 'run',
          #                                 label    = 'Choose the run(s) to plot',
          #                                 choices  = runSEED,
          #                                 selected = runSEED[1],
          #                                 multiple = FALSE
          #                  )
          #                  
          # ),
          
          conditionalPanel(condition  = "input.mode == 'Several'",
                           selectizeInput(inputId  = 'run',
                                          label    = 'Choose the run(s) to plot',
                                          choices  = runSEED,
                                          selected = runSEED[1],
                                          multiple = TRUE
                           )
                           
          )
        ),
        
        #   sidebarPanel(
        #     selectInput(inputId = "mode",
        #                 label = "Plot one specific run or several runs",
        #                 choices = c('One run', 'Several'),
        #                 selected = "Several"
        #     ),
        #     
        #     conditionalPanel(condition  = "input.mode == 'Several'",
        #                      selectizeInput(inputId  = 'run',
        #                                     label    = 'Choose the run(s) to plot',
        #                                     choices  = as.character(run_parameters$Run),
        #                                     selected = c(as.character(run_parameters$Run)[run_parameters$Run=="Run_1"]),
        #                                     multiple = TRUE
        #                      ),
        #                      selectizeInput(inputId  = 'seed_nb',
        #                                     label    = 'Choose the seed number(s)',
        #                                     choices  = seed_nb ,
        #                                     selected = seed_nb[1],
        #                                     multiple = TRUE
        #                      )
        #                      
        #     ),
        #     
        #     conditionalPanel(condition  = "input.mode == 'One run'",
        #                      selectInput(   inputId  = 'run',
        #                                     label    = 'Choose the run to plot',
        #                                     choices  = c(as.character(run_parameters$Run)[run_parameters$Run=="Run_1"]),
        #                                     selected = as.character(run_parameters$Run)[1]
        #                      ),
        #                      selectInput(   inputId  = 'seed_nb',
        #                                     label    = 'Choose the seed number',
        #                                     choices  = seed_nb ,
        #                                     selected = seed_nb[1]
        #                      )
        #     )
        #   ),
        
        # mainPanel(height = 5, 
        
        #TextOutput('ex_out1'),
        #plotOutput("myplot")
        #verbatimTextOutput('ex_out')
        # uiOutput("plots"),
        #showOutput("SSB", "highcharts")
        #showOutput("Recruits", "highcharts"),
        #showOutput("F25", "highcharts"),
        #showOutput("F10", "highcharts")
        # Create a spot for a dynamic UI containing the chat contents.
        
        # uiOutput("chat"), 
        
        
        #       sidebarPanel(
        #         selectInput(inputId = "mode",
        #                     label = "Plot one specific run or several runs",
        #                     choices = c('One run', 'Several'),
        #                     selected = "Several"
        #         ),
        #         
        #         conditionalPanel(condition  = "input.mode == 'Several'",
        #                          selectizeInput(inputId  = 'run',
        #                                         label    = 'Choose the run(s) to plot',
        #                                         choices  = as.character(run_parameters$Run),
        #                                         selected = c(as.character(run_parameters$Run)[run_parameters$Run=="Run_1"]),
        #                                         multiple = TRUE
        #                          ),
        #                          selectizeInput(inputId  = 'seed_nb',
        #                                         label    = 'Choose the seed number(s)',
        #                                         choices  = seed_nb ,
        #                                         selected = seed_nb[1],
        #                                         multiple = TRUE
        #                          )
        #                          
        #         ),
        #         
        #         conditionalPanel(condition  = "input.mode == 'One run'",
        #                          selectInput(   inputId  = 'run',
        #                                         label    = 'Choose the run to plot',
        #                                         choices  = c(as.character(run_parameters$Run)[run_parameters$Run=="Run_1"]),
        #                                         selected = as.character(run_parameters$Run)[1]
        #                          ),
        #                          selectInput(   inputId  = 'seed_nb',
        #                                         label    = 'Choose the seed number',
        #                                         choices  = seed_nb ,
        #                                         selected = seed_nb[1]
        #                          )
        #         )
        #       ),
        bootstrapPage(mainPanel(
          
          div(class='row',
              div(
                fluidRow(
                  column(6,div(uiOutput("plots1_2"))),
                  column(6,div( uiOutput("plots3_4")))
                ))
              
              #       uiOutput("plots1_2"),
              #     uiOutput("plots3_4"))
          ))
        )
        )
    )
  )
)
)