##########################################################################
######   Script to make the different plots of the VPA output files   ####
##########################################################################

##############################################################
###  Source the different scripts to read the VPA outputs  ###
###  These scripts should be fine#                         ###
##############################################################

# add it for shiny app 21/09/2016
dirMain     <- paste(getwd(),"/taha/",sep="")
dirRScripts <- paste(dirMain, "Rscripts/", sep="")
dir_R_scripts <- dirRScripts
dirData     <- paste(dirMain, "data/", sep="")
dir_data <- dirData
###

source(paste(dir_R_scripts, "io.funcs.R", sep=""))
source(paste(dir_R_scripts, "io.VPA2Box.R", sep=""))
source(paste(dir_R_scripts, "io.Retros.R", sep=""))
source(paste(dir_R_scripts, "kobe-2box.R", sep=""))

#################################################################
### Read the secifications of the differents run proposed    ####
#################################################################
run_parameters <- read.table(paste(dir_data, "run_spec.csv", sep=""), sep=",", header=T, fill=T)
#or From netcdf spec version
# source(paste(dirRScripts,"run_specToNetCDF.R",sep=""))
# runParameters <- specNCDF2DataFrame(file_name = 'run_spec.nc',directory = dirNCData)

###################################################################
### Here are a set of functions to read and plot the outputs    ###
### get_run_data :  get the data from a run using the           ###
###                 directory of the run (dir_in)               ###
### get_retros_data :  get the data from a retrospective analysis##
###                    using the  directory of the run (dir_in) ###
###                 directory of the run (dir_in)               ###
### plot_runs_fig1:  plot F2-5, Fageplus, recruitment,          ###
###                 and SSB from a simple run or several runs   ###
###                 using its/their directory                   ###
### plot_runs_faa:  plot fishing mortality at age               ###
###                 from a simple run or several runs           ###
###                 using its/their directory                   ###
### plot_residuals: plot the residuals of the abundance indices ###
###                 from a simple run or several runs           ###
###                 using its/their directory                   ###
### plot_retros_fig1:  plot F2-5, Fageplus, recruitment,        ###
###                 and SSB from a retrospective analysis       ###
###                 using its/their directory                   ###
###################################################################

#################################################################
### Basic function to read the VPA output of a retrospective  ###
### analysis                                                  ###
#################################################################
get_proj_data <- function(dir_in=dir_proj, scen= c("low", "med", "high"), proxy="f0.1"){
  library(kobe)
  dir_proj1 <- paste(dir_in, scen, sep="")
  reported_output_proj <- NULL
  for (i in 1:length(dir_proj1)){
    tempproj             <- readKobe2box(dir=dir_proj1[i], proxy=proxy)
    reported_output_proj <- rbind(reported_output_proj, cbind("Scenario"=scen[i],tempproj))
  }
}


#####################################################
### Plot F2-5, Fageplus, recruitment and SSB      ###
### from a single run or several runs             ###
#####################################################
# plot_runs_fig1(c("Run_2_2012","Run_1"), c(911, dir_bests$seed_nb[dir_bests$run=="Run_1"]))
plot_runs_fig1 <- function(run_number="Run_1", seed_nb=911, age_plus=10, point=FALSE){
  #comb_run  <- expand.grid(run_number=run_number, seed_nb=seed_nb)
  pltstot <- NULL
  for (i in 1:length(run_number)){
    # load(paste(dir_outputs, "data_retro_", run_number[i], "_", seed_nb[i], ".Rdata", sep="" ))
    source(paste(dirRScripts,"OutputsVPA2NetCDF.R", sep=""))
    outputs_VPA <- NetCDFtoRdata(nc_name =paste("data_retro_",run_number[i],"_",seed_nb[i],".nc",sep=""),nc_path = dirNCData )
    pltstot <- rbind(pltstot,data.frame(Run= paste(run_number[i], seed_nb[i]), outputs_VPA$main[outputs_VPA$main$Retros==0,] ))
#     if (sum(is.na(pltstot$variable)==T)>0){
#     pltstot$variable[is.na(pltstot$variable)==T] <- "F10."}
    pltstot$variable <- revalue(pltstot$variable, c("Fplusgroup"="F10."))
  }
  pltstot$variable <- factor(pltstot$variable, levels=c("F2.5", "F10.", "Recruits", "SSB"))
  p1 <- ggplot(pltstot, aes(x=Year, y=value, colour=Run))+
        geom_line()+
        facet_wrap(~variable, scales="free")
  if (point==TRUE){p1 <- p1+geom_point()}
  print(p1)
}

plot_runs_fig12 <- function(run_number="Run_7", seed_nb=11, age_plus=10, point=FALSE){
  #comb_run  <- expand.grid(run_number=run_number, seed_nb=seed_nb)
  pltstot <- NULL
  for (i in 1:length(run_number)){
    # load(paste(dir_outputs, "data_retro_", run_number[i], "_", seed_nb[i], ".Rdata", sep="" ))
    source(paste(dirRScripts,"OutputsVPA2NetCDF.R", sep=""))
    outputs_VPA <- NetCDFtoRdata(nc_name =paste("data_retro_",run_number[i],"_",seed_nb[i],".nc",sep=""),nc_path = dirNCData )
    pltstot <- rbind(pltstot,data.frame(Run= paste(run_number[i]), outputs_VPA$main[outputs_VPA$main$Retros==0,] ))
#     if (sum(is.na(pltstot$variable)==T)>0){
#       pltstot$variable[is.na(pltstot$variable)==T] <- "F10."}
    pltstot$variable <- revalue(pltstot$variable, c("Fplusgroup"="F10."))
  }
  pltstot$variable <- factor(pltstot$variable, levels=c("F2.5", "F10.", "Recruits", "SSB"))
  pltstot$Run <- factor(pltstot$Run, levels=c("Run_3", "Run_4", "Run_5", "Run_6", "Run_7","Run_17", "Run_14", "Run_15", "Run_16"))
  p1 <- ggplot(pltstot, aes(x=Year, y=value, colour=Run))+
    geom_line()+
    facet_wrap(~variable, scales="free")
  if (point==TRUE){p1 <- p1+geom_point()}
  print(p1)
}

#####################################################
### Plot fishing mortality at age                 ###
### from a single run or several runs             ###
#####################################################
plot_runs_faa <- function(run_number, seed_number, age_plus=10, point=FALSE){
  #comb_run  <- expand.grid(run_number=run_number, seed_nb=seed_nb)
  pltstot <- NULL
  for (i in 1:length(run_number)){
    # load(paste(dir_outputs, "data_retro_", run_number[i], "_", seed_number[i], ".Rdata", sep="" ))
    source(paste(dirRScripts,"OutputsVPA2NetCDF.R", sep=""))
    outputs_VPA <- NetCDFtoRdata(nc_name =paste("data_retro_",run_number[i],"_",seed_number[i],".nc",sep=""),nc_path = dirNCData )
     # load(paste(dirRData,"data_retro_",run_number[i],"_",seed_number[i],".Rdata",sep=""))
        pltstot <- rbind(pltstot,data.frame(Run= paste(run_number[i], seed_number[i]), melt(outputs_VPA$faa) ))
  }
  names(pltstot) <- c("Run","age","year","value")
  pltstot$age <- gsub("^\\s+|\\s+$", "", pltstot$age)
  pltstot$age <- factor(pltstot$age, levels=c(paste("Age ", 1:(age_plus-1), sep=""), paste("Age ", age_plus,"plus", sep="")))
  pltstot$age <- revalue(pltstot$age, c("Age 10plus"="Age 10+"))
  p1 <- ggplot(pltstot, aes(x=year, y=value, colour=Run))+
        geom_line()+
        facet_wrap(~age)
  if (point==TRUE){p1 <- p1+geom_point()}
  print(p1)
}

#####################################################
### Plot fit of abundance index                   ###
### from a single run or several runs             ###
#####################################################
## Fit abundance index
plot_residuals_ts <- function(run_number, seed_number){
  # load(paste(dir_outputs, "data_retro_",run_number, "_",seed_number, ".Rdata", sep="" ))
  source(paste(dirRScripts,"OutputsVPA2NetCDF.R", sep=""))
  outputs_VPA <- NetCDFtoRdata(nc_name =paste("data_retro_",run_number,"_",seed_number,".nc",sep=""),nc_path = dirNCData )
  fit_tot <- outputs_VPA$residuals
        
    p1 <- ggplot(fit_tot, aes(x=Year, y=residuals))+
          geom_point()+
          stat_smooth(se=F, span=1)+
            xlab("Year")+
            ylab("value")+
            geom_hline(yintercept = 0, aes(size=2))+
            facet_wrap(~CPUE, scales="free")
    print(p1)
}

#####################################################
### Plot fit of abundance index                   ###
### from a single run or several runs             ###
#####################################################
## Fit abundance index
plot_residuals <- function(run_number, seed_number){
  # load(paste(dir_outputs, "data_retro_",run_number, "_",seed_number, ".Rdata", sep="" ))
  source(paste(dirRScripts,"OutputsVPA2NetCDF.R", sep=""))
  outputs_VPA <- NetCDFtoRdata(nc_name =paste("data_retro_",run_number,"_",seed_number,".nc",sep=""),nc_path = dirNCData )
  fit_tot <- outputs_VPA$residuals
  
  p1 <- ggplot(fit_tot, aes(x=observed, y=predicted))+
    geom_point()+
    geom_smooth(method="lm", se=F)+
    xlab("Observed")+
    ylab("Predicted")+
    geom_abline(intercept = 0, slope=1)+
    facet_wrap(~CPUE, scales="free")
  print(p1)
}
#####################################################
### Plot F2-5, Fageplus, recruitment and SSB      ###
### from a retrospective analysis                 ###
#####################################################
plot_retros_fig1 <- function(run_number, seed_number, nRet=6, point=FALSE){
  # load(paste(dir_outputs, "data_retro_",run_number, "_",seed_number, ".Rdata", sep="" ))
  source(paste(dirRScripts,"OutputsVPA2NetCDF.R", sep=""))
  outputs_VPA <- NetCDFtoRdata(nc_name =paste("data_retro_",run_number,"_",seed_number,".nc",sep=""),nc_path = dirNCData )
#   if (sum(is.na(outputs_VPA$main$variable)==T)>0){
#     outputs_VPA$main$variable[is.na(outputs_VPA$main$variable)==T] <- "F10."}
  outputs_VPA$main$variable <- revalue(outputs_VPA$main$variable, c("Fplusgroup"="F10."))
  outputs_VPA$main$variable <- factor(outputs_VPA$main$variable, levels=c("F2.5", "F10.", "Recruits", "SSB"))
  outputs_VPA$main <- subset(outputs_VPA$main,outputs_VPA$main$Retros %in% unique(outputs_VPA$main$Retros)[1:nRet])
  p1    <- ggplot(outputs_VPA$main, aes(x=Year, y=value, colour=as.factor(Retros)))+
           geom_line()+
           facet_wrap(~variable, scales="free")
  if (point==TRUE){p1 <- p1+geom_point()}
  print(p1)
}

#####################################################
### Kobe Plot for the projections                 ###
### TO BE FIXED BECAUSE doesn't work when putting ###
### year as a parameter of the function           ###
#####################################################
kobe_VPA_output <- function(year1,tac1){
  color_pie <- brewer.pal(9, "Set1")[c(3,6,1)]
  path_proj <- ddply(output_proj, .(Scenario, year), function(x) data.frame(harvest= median(x$harvest),
                                                                                stock=median(x$stock),
                                                                                ssb0.1=median(x$ssb0.1),
                                                                                f0.1=median(x$f0.1)))
  
  last3_years <- path_proj[path_proj$year%in%c((year1-2):(year1)),]
  kp  <- kobePhase(subset(output_proj, year==year1 & tac==tac1), ylim=c(0,max(last3_years$harvest)+0.05)) +
    geom_point(aes(stock,harvest, colour=Scenario))
  kp1 <- kp+ geom_line(data=last3_years, aes(x=stock, y=harvest, colour=Scenario), alpha=0.75 , size=2)
  kp1 <- kp1+ geom_point(data=last3_years, aes(x=stock, y=harvest), size=3)
  print(kp1)
}



kobe_VPA_output2 <- function(year1, year2,tac1){
  color_pie <- brewer.pal(9, "Set1")[c(3,6,1)]
  path_proj <- ddply(output_proj, .(Scenario, year), function(x) data.frame(harvest= median(x$harvest),
                                                                            stock=median(x$stock),
                                                                            ssb0.1=median(x$ssb0.1),
                                                                            f0.1=median(x$f0.1)))
  path_proj$year <- as.numeric(as.character(path_proj$year))
  output_proj$year <- as.numeric(as.character(output_proj$year))
  
  #last3_years <- path_proj[path_proj$year%in%c((year1-2):(year1)),]
  kp  <- kobePhase(subset(output_proj, year>=year1 &year<=year2 & tac==tac1)) +
    geom_point(aes(stock,harvest, colour=Scenario))
  #kp1 <- kp+ geom_line(data=last3_years, aes(x=stock, y=harvest, colour=Scenario), alpha=0.75 , size=2)
  #kp1 <- kp1+ geom_point(data=last3_years, aes(x=stock, y=harvest), size=3)
  print(kp)
}

pie_kobe <- function(year1=1950, tac1="13500"){
  titi <- ddply(output_proj, .(Scenario, year, tac), function(x) data.frame(green = sum(x$harvest<=1 & x$stock>=1)/length(x$harvest)*100,
                                                                       yellow= sum((x$harvest>1 & x$stock>1)|(x$harvest<1 & x$stock<1))/length(x$harvest)*100,
                                                                       red   = sum(x$harvest>=1 & x$stock<=1)/length(x$harvest)*100
                                                                                ))
  color_pie <- brewer.pal(9, "Set1")[c(3,6,1)]
  titi$year <- as.numeric(as.character(titi$year))+1949
  titi$tac <- quot[as.numeric(as.character(titi$tac))]
  sub_proj <- titi[titi$year==year1 &titi$tac==tac1,]
  sub_proj <- melt(sub_proj, measure.vars = c("green", "yellow", "red"))
  colnames(sub_proj)[4] <- c("Stock_status")
  p = ggplot(data=sub_proj, aes(x=factor(1), y=value, fill = Stock_status))
  p=p + geom_bar(width = 1, stat="identity") +facet_grid(~Scenario)
  p = p + coord_polar(theta="y") +scale_fill_manual(values=color_pie) + xlab("") + ylab("") #+ scale_y_discrete(breaks=NULL)
  print(p)
}

pie_kobe_2022 <- function(year1=64, year2=72, tac1="13500"){
  titi <- ddply(output_proj, .(Scenario, year, tac), function(x) data.frame(green = sum(x$harvest<=1 & x$stock>=1)/length(x$harvest)*100,
                                                                       yellow= sum((x$harvest>1 & x$stock>1)|(x$harvest<1 & x$stock<1))/length(x$harvest)*100,
                                                                       red   = sum(x$harvest>=1 & x$stock<=1)/length(x$harvest)*100
  ))
  titi$year <- as.numeric(as.character(titi$year))+1949
  titi$tac <- quot[as.numeric(as.character(titi$tac))]
  color_pie <- brewer.pal(9, "Set1")[c(3,6,1)]
  sub_proj <- titi[titi$year>=year1 & titi$year<=year2 & titi$tac==tac1,]
  sub_proj <- melt(sub_proj, measure.vars = c("green", "yellow", "red"))
  colnames(sub_proj)[4] <- c("Stock_status")
  p = ggplot(data=sub_proj, aes(x=factor(1), y=value, fill = Stock_status))
  p=p + geom_bar(width = 1, stat="identity") +facet_grid(Scenario~year)
  p = p + coord_polar(theta="y") +scale_fill_manual(values=color_pie) + xlab("") + ylab("") #+ scale_y_discrete(breaks=NULL)
  print(p)
}

proj_curve <- function(){
  pupu <- ddply(output_proj, .(year, tac, Scenario), function(x) data.frame(stock= median(x$stock), harvest=median(x$harvest)))
  pupu$year <- as.numeric(as.character(pupu$year))+1949
  pupu <- pupu[pupu$tac%in%c(1:18),]
  pupu$tac  <- factor(quot[as.numeric(as.character(pupu$tac))], levels=unique(quot[as.numeric(as.character(pupu$tac))]))
  pipi <- melt(pupu, id=c("year", "tac", "Scenario"))
  p <- ggplot(pipi, aes(x=year, y= value, group=factor(tac),colour=factor(tac)))+geom_line()+facet_wrap(Scenario~variable, ncol=2, scales="free")
  print(p)
}

pie_kobe_all <- function(year1=64){
  titi <- ddply(output_proj, .(Scenario, year), function(x) data.frame(green = sum(x$harvest<=1 & x$stock>=1)/length(x$harvest)*100,
                                                                            yellow= sum((x$harvest>1 & x$stock>1)|(x$harvest<1 & x$stock<1))/length(x$harvest)*100,
                                                                            red   = sum(x$harvest>=1 & x$stock<=1)/length(x$harvest)*100
  ))
  color_pie <- brewer.pal(9, "Set1")[c(3,6,1)]
  titi$year <- as.numeric(as.character(titi$year))+1949
  sub_proj <- titi[titi$year==year1,]
  sub_proj <- melt(sub_proj, measure.vars = c("green", "yellow", "red"))
  colnames(sub_proj)[3] <- c("Stock_status")
  p = ggplot(data=sub_proj, aes(x=factor(1), y=value, fill = Stock_status))
  p=p + geom_bar(width = 1, stat="identity") +facet_grid(~Scenario)
  p = p + coord_polar(theta="y") +scale_fill_manual(values=color_pie) + xlab("") + ylab("") #+ scale_y_discrete(breaks=NULL)
  print(p)
}

pie_kobe_2022_all <- function(year1=64, year2=72){
  titi <- ddply(output_proj, .(Scenario, year), function(x) data.frame(green = sum(x$harvest<=1 & x$stock>=1)/length(x$harvest)*100,
                                                                            yellow= sum((x$harvest>1 & x$stock>1)|(x$harvest<1 & x$stock<1))/length(x$harvest)*100,
                                                                            red   = sum(x$harvest>=1 & x$stock<=1)/length(x$harvest)*100
  ))
  titi$year <- as.numeric(as.character(titi$year))+1949
  color_pie <- brewer.pal(9, "Set1")[c(3,6,1)]
  sub_proj <- titi[titi$year>=year1 & titi$year<=year2,]
  sub_proj <- melt(sub_proj, measure.vars = c("green", "yellow", "red"))
  colnames(sub_proj)[3] <- c("Stock_status")
  p = ggplot(data=sub_proj, aes(x=factor(1), y=value, fill = Stock_status))
  p=p + geom_bar(width = 1, stat="identity") +facet_grid(Scenario~year)
  p = p + coord_polar(theta="y") +scale_fill_manual(values=color_pie) + xlab("") + ylab("") #+ scale_y_discrete(breaks=NULL)
  print(p)
}

