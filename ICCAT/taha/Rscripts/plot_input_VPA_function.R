### source diffrent scripts to read VPA outputs

# add it for shiny app 21/09/2016
dirMain     <- paste(getwd(),"/taha/",sep="")
dirRScripts <- paste(dirMain, "Rscripts/", sep="")
dir_R_scripts <- dirRScripts
###

source(paste(dir_R_scripts, "io.funcs.R", sep=""))
source(paste(dir_R_scripts, "io.VPA2Box.R", sep=""))
source(paste(dir_R_scripts, "io.Retros.R", sep=""))
# run_parameters <- read.table(paste(dir_data, "run_spec.csv", sep=""), sep=",", header=T, fill=T)

#or From netcdf spec version
source(paste(dirRScripts,"run_specToNetCDF.R",sep=""))
runParameters <- specNCDF2DataFrame(file_name = 'run_spec.nc',directory = dirNCData)

#####################################################
### COMPARISON between previous and updated CAA   ###
#####################################################


### Define your path
### Read the two CAA
plot_CAA_comparison <- function(update="CAA.csv", last="CAA2012.csv"){
    CAA                 <- read.table(paste(dir_data, 'CAA.csv', sep=""), header=F, sep=";")
    colnames(CAA)       <- c("Year", paste("Age",1:9), "Age 10+")
    CAA2012             <- read.table(paste(dir_data, 'CAA2012.csv', sep=""), header=F, sep=";")
    colnames(CAA2012)   <- c("Year", paste("Age",1:9), "Age 10+")
    ### Rearrange CAAs for ggplot
    CAA1                <- cbind(period="update", melt(CAA, measure.vars= c(paste("Age",1:9), "Age 10+")))
    colnames(CAA1)      <- c("Period", "Year", "Age", "value")
    CAA1_2012           <- cbind(period="2012", melt(CAA2012, measure.vars= c(paste("Age",1:9), "Age 10+")))
    colnames(CAA1_2012) <- c("Period", "Year", "Age", "value")
    ### Merge the CAAs
    CAA_comparison      <- rbind(CAA1, CAA1_2012)
    p                   <- ggplot(CAA_comparison, aes(x=Year, y=value, colour=Period))
    p                   <- p + geom_line() + facet_wrap(~Age, scales="free")
    print(p)
}    

diff_CAA_bubble <- function(update="CAA.csv", last="CAA2012.csv"){
  CAA                 <- read.table(paste(dir_data, update, sep=""), header=F, sep=";")
  if (last=="CAA2012.csv"){CAA <- CAA[1:62,]}
  colnames(CAA)       <- c("Year", paste("Age",1:9), "Age 10+")
  CAA2012             <- read.table(paste(dir_data, last, sep=""), header=F, sep=";")
  colnames(CAA2012)   <- c("Year", paste("Age",1:9), "Age 10+")
  CAA_diff            <- cbind(Year=CAA[,1], CAA[,2:11]-CAA2012[,2:11])
  ### Rearrange CAAs for ggplot
  CAA_diff1           <- melt(CAA_diff, measure.vars= c(paste("Age",1:9), "Age 10+"))
  colnames(CAA_diff1) <- c("Year", "Age", "value")
  CAA_diff1$value     <- round(CAA_diff1$value)
  ### Merge the CAAs
  p                   <- ggplot(CAA_diff1, aes(x=Year, y=Age, size=abs(value), colour=factor(sign(value))))
  p                   <- p + geom_point()
  p                   <- p + scale_colour_discrete(name="Sign",
                                           breaks=c("-1", "1"),
                                           labels=c("Negative", "Positive"))
  print(p)  
}

diff_CAA_bubble_percent <- function(update="CAA.csv", last="CAA2012.csv"){
  CAA                 <- read.table(paste(dir_data, update, sep=""), header=F, sep=";")
  if (last=="CAA2012.csv"){CAA <- CAA[1:62,]}
  colnames(CAA)       <- c("Year", paste("Age",1:9), "Age 10+")
  CAA2012             <- read.table(paste(dir_data, last, sep=""), header=F, sep=";")
  colnames(CAA2012)   <- c("Year", paste("Age",1:9), "Age 10+")
  CAA_diff            <- cbind(Year=CAA[,1], (CAA[,2:11]-CAA2012[,2:11])/CAA2012[,2:11]*100)
  ### Rearrange CAAs for ggplot
  CAA_diff1           <- melt(CAA_diff, measure.vars= c(paste("Age",1:9), "Age 10+"))
  colnames(CAA_diff1) <- c("Year", "Age", "value")
  CAA_diff1$value     <- round(CAA_diff1$value)
  ### Merge the CAAs
  p                   <- ggplot(CAA_diff1, aes(x=Year, y=Age, size=abs(value), colour=factor(sign(value))))
  p                   <- p + geom_point()
  p                   <- p + scale_colour_discrete(name="Sign",
                                                   breaks=c("-1", "1"),
                                                   labels=c("Negative", "Positive"))
  print(p)  
}

barplot_CAA <- function(CAA="CAA.csv"){
    CAA                 <- read.table(paste(dir_data, CAA, sep=""), header=F, sep=";")
    colnames(CAA)       <- c("Year", paste("Age",1:9), "Age 10+")
    CAA1                <- cbind(period="update", melt(CAA, measure.vars= c(paste("Age",1:9),"Age 10+")))
    colnames(CAA1)      <- c("Period", "Year", "Age", "value")
    p                   <- ggplot(CAA1, aes(x=Year, y=value, fill=Age))
    p                   <- p + geom_bar(stat="identity") + scale_fill_manual(values = brewer.pal(10, 'Paired'))
    print(p)
}

barplot_perc_CAA <- function(CAA="CAA.csv"){
  CAA                 <- read.table(paste(dir_data, CAA, sep=""), header=F, sep=";")
  colnames(CAA)       <- c("Year", paste("Age",1:9), "Age 10+")
  CAA1                <- melt(CAA, measure.vars= c(paste("Age",1:9),"Age 10+"))
  colnames(CAA1)      <- c("Year", "Age", "value")
  CAA2                <- ddply(CAA1, .(Year), function(x) data.frame(Age=x$Age, value=x$value/sum(x$value)*100) )
  p                   <- ggplot(CAA2, aes(x=Year, y=value, fill=Age))
  p                   <- p + geom_bar(stat="identity") + scale_fill_manual(values = brewer.pal(10, 'Paired'))
  print(p)
}

plot_CPUE <- function(update='CPUE.csv', last='CPUE2012.csv'){
  index_name <- c("Mor Span Traps", "JPLL E_MED", "Nor PS", "JPLL NEATL", "SP BB1", "SP BB2", "SP BB3")
  CPUE     <- read.table(paste(dir_data, update, sep=""), header=F, sep=";")
  CPUE     <- CPUE[,1:4] 
  colnames(CPUE) <- c("Index", "Year", "value", "CV") 
  CPUE2012     <- read.table(paste(dir_data, last, sep=""), header=F, sep=";")
  CPUE2012     <- CPUE2012[,1:4] 
  colnames(CPUE2012) <- c("Index", "Year", "value", "CV") 
  CPUE_comparison <- rbind(cbind(Period="update", CPUE), cbind(Period="2012",CPUE2012))
  for (i in 1:length(index_name)){
    CPUE_comparison$Index[CPUE_comparison$Index==i] <- index_name[i]
  }
  p <- ggplot(CPUE_comparison, aes(x=Year, y=value, colour=Period))
  p+geom_line()+facet_wrap(~Index, scales="free")
}

plot_WAA_comparison <- function(update='fecaa.csv', last='fecaa2012.csv'){
  WAA     <- read.table(paste(dir_data, update, sep=""), header=F, sep=";")
  colnames(WAA) <- c("Year", paste("Age",1:9), "Age 10+")
  WAA2012 <- read.table(paste(dir_data, last, sep=""), header=F, sep=";")
  colnames(WAA2012) <- c("Year", paste("Age",1:9), "Age 10+")
  WAA1 <- cbind(period="update", melt(WAA, measure.vars= c(paste("Age",1:9), "Age 10+")))
  colnames(WAA1) <- c("Period", "Year", "Age", "value")
  WAA1_2012 <- cbind(period="2012", melt(WAA2012, measure.vars= c(paste("Age",1:9), "Age 10+")))
  colnames(WAA1_2012) <- c("Period", "Year", "Age", "value")
  WAA_comparison <- rbind(WAA1, WAA1_2012)
  p <- ggplot(WAA_comparison, aes(x=Year, y=value, colour=Period))
  p+geom_line()+facet_wrap(~Age, scales="free")
}

diff_WAA_bubble <- function(update="fecaa.csv", last="fecaa2012.csv"){
  WAA                 <- read.table(paste(dir_data, update, sep=""), header=F, sep=";")
  if (last=="fecaa2012.csv"){WAA <- WAA[1:62,]}
  colnames(WAA)       <- c("Year", paste("Age",1:9), "Age 10+")
  WAA2012             <- read.table(paste(dir_data, last, sep=""), header=F, sep=";")
  colnames(WAA2012)   <- c("Year", paste("Age",1:9), "Age 10+")
  WAA_diff            <- cbind(Year=WAA[,1], WAA[,2:11]-WAA2012[,2:11])
  ### Rearrange CAAs for ggplot
  WAA_diff1           <- melt(WAA_diff, measure.vars= c(paste("Age",1:9), "Age 10+"))
  colnames(WAA_diff1) <- c("Year", "Age", "value")
  WAA_diff1$value     <- round(WAA_diff1$value)
  ### Merge the CAAs
  p                   <- ggplot(WAA_diff1, aes(x=Year, y=Age, size=abs(value), colour=factor(sign(value))))
  p                   <- p + geom_point()
  p                   <- p + scale_colour_discrete(name="Sign",
                                                   breaks=c("-1", "1"),
                                                   labels=c("Negative", "Positive"))
  print(p)  
}

diff_WAA_bubble_perc <- function(update="fecaa.csv", last="fecaa2012.csv"){
  WAA                 <- read.table(paste(dir_data, update, sep=""), header=F, sep=";")
  if (last=="fecaa2012.csv"){WAA <- WAA[1:62,]}
  colnames(WAA)       <- c("Year", paste("Age",1:9), "Age 10+")
  WAA2012             <- read.table(paste(dir_data, last, sep=""), header=F, sep=";")
  colnames(WAA2012)   <- c("Year", paste("Age",1:9), "Age 10+")
  WAA_diff            <- cbind(Year=WAA[,1], (WAA[,2:11]-WAA2012[,2:11])/WAA2012[,2:11]*100)
  ### Rearrange CAAs for ggplot
  WAA_diff1           <- melt(WAA_diff, measure.vars= c(paste("Age",1:9), "Age 10+"))
  colnames(WAA_diff1) <- c("Year", "Age", "value")
  WAA_diff1$value     <- round(WAA_diff1$value)
  ### Merge the CAAs
  p                   <- ggplot(WAA_diff1, aes(x=Year, y=Age, size=abs(value), colour=factor(sign(value))))
  p                   <- p + geom_point()
  p                   <- p + scale_colour_discrete(name="Sign",
                                                   breaks=c("-1", "1"),
                                                   labels=c("Negative", "Positive"))
  print(p)  
}

plot_PCAA <- function(pcaa_number){ 
    index_name <- c("Mor Span Traps", "JPLL E_MED",  "JPLL NEATL", "SP BB1", "SP BB2", "SP BB3")
  
    PCAA     <- read.table(paste(dir_data, 'PCAA.csv', sep=""), header=F, sep=";")
    colnames(PCAA) <- c("Index","Year", paste("Age",1:10))
    
    PCAA2012 <- read.table(paste(dir_data, 'PCAA2012.csv', sep=""), header=F, sep=";")
    colnames(PCAA2012) <- c("Index","Year", paste("Age",1:10))
    
    for (i in 1:length(index_name)){
    PCAA$Index[PCAA$Index==i] <- index_name[i]
    PCAA2012$Index[PCAA2012$Index==i] <- index_name[i]
    }
    
    PCAA1 <- cbind(period="update", melt(PCAA, measure.vars= paste("Age",1:10)))
    colnames(PCAA1) <- c("Period", "Index", "Year", "Age", "value")
    PCAA1_2012 <- cbind(period="2012", melt(PCAA2012, measure.vars= paste("Age",1:10)))
    colnames(PCAA1_2012) <- c("Period", "Index", "Year", "Age", "value")
    PCAA_comparison <- rbind(PCAA1, PCAA1_2012)
    un <- unique(PCAA_comparison$Index)
    
    PCAA_index <- subset(PCAA_comparison, subset=c(PCAA_comparison$Index==un[pcaa_number]))
    p <- ggplot(PCAA_index, aes(x=Year, y=value, colour=Period))
    p+geom_line()+facet_wrap(~Age, scales="free")
    #ggsave(filename=paste(dir_figs, "PCAA_", un[i],".pdf", sep=""))
}

plot_CAA_diff <- function(year){
  ### Read the two CAA
  CAA     <- read.table(paste(dir_data, 'CAA.csv', sep=""), header=F, sep=";")
  colnames(CAA) <- c("Year", paste("Age",1:9), "Age 10+")
  CAA2012 <- read.table(paste(dir_data, 'CAA2012.csv', sep=""), header=F, sep=";")
  colnames(CAA2012) <- c("Year", paste("Age",1:9), "Age 10+")
  ### Rearrange CAAs for ggplot
  CAA1 <- cbind(period="update", melt(CAA, measure.vars= c(paste("Age",1:9), "Age 10+")))
  colnames(CAA1) <- c("Period", "Year", "Age", "value")
  CAA1_2012 <- cbind(period="2012", melt(CAA2012, measure.vars= c(paste("Age",1:9), "Age 10+")))
  colnames(CAA1_2012) <- c("Period", "Year", "Age", "value")
  ### Merge the CAAs
  CAA_comparison <- rbind(CAA1, CAA1_2012)
  CAA_diff <- subset(CAA_comparison, subset=c(CAA_comparison$Year==year))
  CAA_diff$Period <- as.factor(CAA_diff$Period)
  CAA_diff$Age <- as.character(CAA_diff$Age) 
  p <- ggplot(CAA_diff, aes(x=Age, y=value))
  p+geom_point(aes(colour=Period))
    
}

