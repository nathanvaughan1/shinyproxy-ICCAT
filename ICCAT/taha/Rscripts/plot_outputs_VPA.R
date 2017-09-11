
for_data <- function(x, dir_save){
  load(paste(dir_save,x, sep=""))
  data_plot <- outputs_VPA$main
  data_plot$variable <- as.character(data_plot$variable)
  iwh2 <- which(is.na(data_plot$variable))
  if (length(iwh2)>0){data_plot$variable[iwh2] <- "Fplusgroup"} ## small patch due to data import for first get_runs_data function
  return(data_plot)
}

read_data <- function(run, seed_nb, dir_save){
  comb_run  <- expand.grid(run=run, seed_nb=seed_nb)
  data_runs <- cbind(File=paste("data_retro_", comb_run$run, "_", comb_run$seed_nb, ".Rdata", sep=""), comb_run)
  run_name  <- paste(run)
  seed_nb   <- paste(seed_nb)
  Run_seed  <- paste(run, seed_nb)
  data_plot <- ddply(data_runs, .(File), function(x) data.frame(Run_seed=paste(x$run, x$seed_nb),ldply(lapply(x$File, for_data, dir_save))))[,-1]
  return(data_plot)
}

read_data2 <- function(run, criteria, dir_save){
  comb_run <- NULL
  for (i in 1:length(run)){
    for (j in 1:length(criteria)){
    comb_run <- rbind(comb_run, cbind(run=run[i], criteria=criteria[j], seed_nb=dir_bests_tot$seed_nb[dir_bests_tot$run==run[i] & as.character(dir_bests_tot$criteria)==criteria[j]]))
    }
  }
  comb_run <- as.data.frame(comb_run)
  data_runs <- cbind(File=paste("data_retro_", comb_run$run, "_", comb_run$seed_nb, ".Rdata", sep=""), comb_run)
  run_name  <- paste(run)
  seed_nb   <- paste(seed_nb)
  Run_seed  <- paste(run, seed_nb)
  data_plot <- ddply(data_runs, .(File), function(x) data.frame(Run_seed=paste(x$run, x$seed_nb),ldply(lapply(x$File, for_data, dir_save))))[,-1]
  return(data_plot)
}

plot_output_VPA <- function(data_runs, vari){

  data_sub <- data_runs[data_runs$variable==vari,]
  #paste(runs_id$path)
  h1 <- hPlot(value ~ Year , data = data_sub, type = "line", group = "criteria")
  h1$addParams(dom = vari)
  h1$chart(zoomType = "xy")
  #h1$save('essai.html', standalone=TRUE)
  return(h1)
}