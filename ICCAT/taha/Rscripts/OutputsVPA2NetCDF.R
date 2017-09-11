######## create netcdf version 4
##################################
### from Rdata to NetCDF
####################################
#####################################
OutputsVPA2NetCDF <- function(dirOut_nc,run_name,nbr_seed,outputs_VPA){
  #--- DEBUT
  # rm(list=ls())
  
  # options(digits=10)
  # Parameters:
  #   rdata_name <-"data_retro_Run_1_911.Rdata"
  #   rdata_name <-"data_retro_Run_0_880.Rdata"
  #   rdata_name <-"data_retro_Run_0_635.Rdata"
  #   rdata_name <-"data_retro_Run_0_163.Rdata"
  # rdata_repository <- "/home/taha/Projets/VPA/Rdata"
  
  # load("/home/taha/Projets/VPA/Rdata/seed_number.Rdata")
  
  
  # packages
  library(ncdf4)
  #--- DEBUT
  # rm(list=ls())
  # load(paste(rdata_repository,rdata_name,sep="/"))
  
  
  ####Create netCDF dimensions
  ############################
  # make Run and Seed as two first dimentions
#   run_seed <- unique(na.omit(as.numeric(unlist(strsplit(unlist(rdata_name), "[^0-9]+")))))
#   Run <-   run_seed[1]
#   Seed <- run_seed[2]
  
  Run <- run_name
  Seed <- nbr_seed
  
  unit=""
  for(k in 1:length(Run)){ unit <- paste(unit,paste(k,":",Run[k],sep=""),sep=" ")}
  
  #unlim T or F ??
  dimRun <- ncdim_def( name="RUN", units= unit, vals=1:length(Run),unlim = F)
  dimSeed <- ncdim_def( name="SEED", units= "num of seed", vals=Seed,unlim=F)
  
  
  ##for year dimension 
  YEAR=sort(unique(c(unique(outputs_VPA$residuals$Year),unique(outputs_VPA$main$Year),as.numeric(colnames(outputs_VPA$faa)))))
  # dimYEAR <- dim.def.ncdf("year", "year_date", YEAR)
  
  dimYEAR <- ncdim_def( name="year", units= "year_date", vals=YEAR)
  
  ##### for obj outputs vpa dimension
  objdim <-names(outputs_VPA$obj)
  OBJDIM <- 1:length(objdim)
  
  objdim1 <- objdim
  for(p in objdim){
    ind <- match(p,objdim1)
    objdim1[ind]<- paste(ind,objdim1[ind],sep=":")
  }
  objdim1 <- paste(objdim1, collapse = ' ')
  
  
  dimObj <- ncdim_def("dim_obj", objdim1, OBJDIM)
  
 
  ##### for main outputs vpa dimension
  
  retros <- sort(unique(outputs_VPA$main$Retros))
  variable <- as.character(sort(unique(outputs_VPA$main$variable)))
  VARIABLE <- 1:length(variable)
  
  variable1 <- variable
  for(p in variable){
    ind <- match(p,variable1)
    variable1[ind]<- paste(ind,variable1[ind],sep=":")
  }
  variable1 <- paste(variable1, collapse = ' ')
  
  dimRetros <- ncdim_def("Retros", "num", retros)
  dimvariable <- ncdim_def("variable",variable1, VARIABLE)
  
 
  ##### for residual outputs vpa dimension
  
  cpue <- as.character(sort(unique(outputs_VPA$residuals$CPUE)))
  CPUE <- 1:length(cpue)
  cpue1 <- cpue
  for(p in cpue){
    ind <- match(p,cpue1)
    cpue1[ind]<- paste(ind,cpue1[ind],sep=":")
  }
  cpue1 <- paste(cpue1, collapse = ' ')
  dimCPUE <- ncdim_def("CPUE", cpue1, CPUE)
  
 
  ##### for faa outputs vpa dimension
  
  age <- rownames(outputs_VPA$faa)  
  AGE <- 1:length(age)
  age1 <- age
  for(p in age){
    ind <- match(p,age1)
    age1[ind]<- paste(ind,age1[ind],sep=":")
  }
  age1 <- paste(age1, collapse = ' ')
  dimage <- ncdim_def("age", age1, AGE)
  
  
  
  ##define variables of NetCDF
  nonAvailable <- NA
  OBJ <- ncvar_def(name="obj", units="", dim=list(dimRun,dimSeed,dimObj), missval=nonAvailable, prec="float")
  MAIN <- ncvar_def(name="main", units="", dim=list(dimRun,dimSeed,dimYEAR,dimvariable, dimRetros), missval=nonAvailable, prec="float")
  RESID1 <- ncvar_def(name="residuals", units="", dim=list(dimRun,dimSeed,dimYEAR, dimCPUE), missval=nonAvailable, prec="float")
  RESID2 <- ncvar_def(name="residuals_obseved", units="", dim=list(dimRun,dimSeed,dimYEAR, dimCPUE), missval=nonAvailable, prec="float")
  RESID3 <- ncvar_def(name="residuals_predicted", units="", dim=list(dimRun,dimSeed,dimYEAR, dimCPUE), missval=nonAvailable, prec="float")
  FAA <- ncvar_def(name="faa", units="Y⁻¹", dim=list(dimRun,dimSeed,dimYEAR,dimage), missval=nonAvailable, prec="float")
  
  
  # Create netCDF file (the same name of .Rdata)
  # ncName <- gsub("Rdata","nc",rdata_name)
  ncName <- paste(dirOut_nc, "data_retro_", run_name, "_", nbr_seed,".nc", sep="" )
  nc <- nc_create( ncName, list(OBJ,MAIN,RESID1,RESID2,RESID3,FAA),force_v4 = F)
  
  ##Put Variables
  data <- outputs_VPA$obj
  for( i in OBJDIM){
    ncvar_put(nc=nc, varid=OBJ, vals=as.character(data)[i], start = c(1,1,i),  count = c(1,1,1))
    
  }
  
  data <- outputs_VPA$main
  vr <- variable
  for (i in 1:length(retros)) {
    res1 <- subset(data,data$Retros==retros[i])
    for(k in 1:length(VARIABLE)){
      res2 <- subset(res1,res1$variable == vr[k] )
      
      for(j in 1:length(YEAR)){
        res <- subset(res2,res2$Year==YEAR[j])
        paste(i,j,k,sep=" ")
        if(nrow(res)==0){ 
          ncvar_put(nc=nc, varid=MAIN, vals=NA, start = c(1,1,j,k,i),  count = c(1,1,1,1,1))
          
        } else {
          ncvar_put(nc=nc, varid=MAIN, vals=res$value, start = c(1,1,j,k,i),  count = c(1,1,1,1,1))
          
        }
      }
    }
  }
  
  data <- outputs_VPA$residuals
  for (i in 1:length(CPUE)) {
    res1 <- subset(data,data$CPUE==cpue[i])
    for(j in 1:length(YEAR)){
      res <- subset(res1,res1$Year==YEAR[j])
      if(nrow(res)==0){ 
        
        ncvar_put(nc=nc, varid=RESID1, vals=NA, start = c(1,1,j,i),  count = c(1,1,1, 1))
        ncvar_put(nc=nc, varid=RESID2, vals=NA, start = c(1,1,j,i),  count = c(1,1,1, 1))
        ncvar_put(nc=nc, varid=RESID3, vals=NA, start = c(1,1,j,i),  count = c(1,1,1, 1))
      } else {
        
        ncvar_put(nc=nc, varid=RESID1, vals=res$residuals, start = c(1,1,j,i),  count = c(1,1,1, 1))
        ncvar_put(nc=nc, varid=RESID2, vals=res$observed, start = c(1,1,j,i),  count = c(1,1,1, 1))
        ncvar_put(nc=nc, varid=RESID3, vals=res$predicted, start = c(1,1,j,i),  count = c(1,1,1, 1))
      }
    }
  }
  
  data <- outputs_VPA$faa
  for (i in 1:length(age)) {
    res1 <- data[i,]
    for(j in 1:length(YEAR)){
      res <- subset(res1,as.numeric(names(res1))==YEAR[j])
      if(length(res)==0){ 
        ncvar_put(nc=nc, varid=FAA, vals=NA, start = c(1,1,j,i),  count = c(1,1,1, 1))
        
      } else {
        ncvar_put(nc=nc, varid=FAA, vals=res, start = c(1,1,j,i),  count = c(1,1,1, 1))
        
      }
    }
  }
  
  ## set outputs VPA object as global attributes
  
  # for(ob in names(outputs_VPA$obj)){ncatt_put(nc,0 ,ob,as.character(outputs_VPA$obj[[ob]]),prec=NA, verbose=FALSE)}
  ncatt_put(nc,"dim_obj","flag_values",1:length(objdim))
  ncatt_put(nc,"dim_obj","flag_masks",paste(names(outputs_VPA$obj), collapse = ' '),prec = 'text')
  ncatt_put(nc,"dim_obj","flag_meanings","Value_of_the_objective_function Value_of_the_objective_function_with_cte Number_of_parameters_estimated_by_the_model 
            Number_of_data_used_by_the_model Akaike_Information_Criteria Akaike_Information_Criteria_corrected Bayesian_Information_Criteria",prec = 'text')
  
  ncatt_put(nc,"variable","flag_values",1:length(variable))
  ncatt_put(nc,"variable","flag_meanings",paste(as.character(sort(unique(outputs_VPA$main$variable))), collapse = ' '),prec = 'text')
  
  ncatt_put(nc,"CPUE","long_name","Catch Per unit Effort",prec = 'text')
  ncatt_put(nc,"CPUE","standard_name","Catch_Per_unit_Effort",prec = 'text')
  ncatt_put(nc,"CPUE","flag_values",1:length(cpue))
  ncatt_put(nc,"CPUE","flag_meanings",paste(gsub(' ','_',as.character(sort(unique(outputs_VPA$residuals$CPUE)))), collapse = ' '),prec = 'text')
  
  ncatt_put(nc,"age","flag_values",1:length(age))
  ncatt_put(nc,"age","flag_meanings",paste(gsub(' ','_',rownames(outputs_VPA$faa)), collapse = ' '),prec = 'text')
  
  ncatt_put(nc,"residuals","long_name","residuals in catch",prec = 'text')
  ncatt_put(nc,"residuals","standard_name","residuals_catch",prec = 'text')

  ncatt_put(nc,"residuals_obseved","long_name","obseved residuals in catch",prec = 'text')
  ncatt_put(nc,"residuals_obseved","standard_name","obseved_residuals_catch",prec = 'text')
  
  ncatt_put(nc,"residuals_predicted","long_name","predicted residuals in catch",prec = 'text')
  ncatt_put(nc,"residuals_predicted","standard_name","predicted_residuals_catch",prec = 'text')
  
  ncatt_put(nc,"faa","long_name","fishing mortality by age",prec = 'text')
  ncatt_put(nc,"faa","standard_name","fishing_mortality_by_age",prec = 'text')
  
  ncatt_put(nc,0,"title","Retrospective anlysis outputs of the Bluefin tuna east stock assessment",prec = 'text')
  ncatt_put(nc,0,"summary","This file is considered as a standard data format to store Stock Assessment data",prec = 'text')
  ncatt_put(nc,0,"creator_name","Taha Imzilen, Julien Barde",prec = 'text')
  ncatt_put(nc,0,"creator_email","taha.imzilen@ird.fr, julien.barde@ird.fr",prec = 'text')
  ncatt_put(nc,0,"creator_url","https://i-marine.d4science.org",prec = 'text')
  ncatt_put(nc,0,"contributor","Sylvain Bonhommeau, Tristan Rouyer",prec = 'text')
  ncatt_put(nc,0,"contributor_role","Spiritual leader",prec = 'text')
  ncatt_put(nc,0,"institution","IRD-IFREMER, SETE",prec = 'text')
  ncatt_put(nc,0,"source","ICCAT Scripts, R fortran",prec = 'text')
  ncatt_put(nc,0,"project","BlueBridge H2020 Project")
  ncatt_put(nc,0,"keywords","BlueBridge, bluefin, tuna, iccat, Retros, stock assessment, Retrospective analysis, BFT-E",prec = 'text')
  ncatt_put(nc,0,"Version","v0.1",prec = 'text')
  ncatt_put(nc,0,"date_created",as.character(Sys.Date()),prec = 'text')
  ncatt_put(nc,0,"Conventions","CF-1.6",prec = 'text')
  
  
  
  nc_close(nc)
  
}

NetCDFtoRdata <- function(nc_name,nc_path="/home/taha/R1/monR-2013/esp_travail"){
  
  # nc_name = "data_retro_Run_1_911_v4.nc"
  library(reshape)
  ##obj
  # obj <- ncatt_get(nc,0)
  nc <- nc_open(paste(nc_path,nc_name,sep=""))
  obj <- ncvar_get(nc,"obj")
  cname <- nc$var[["obj"]]$dim[[1+2]]$unit
  indelet <-  as.vector(gregexpr(pattern =':',nc$var[["obj"]]$dim[[1+2]]$unit)[[1]])
  for(r in indelet){
    if(r==2){cname <- substr(cname, 3, nchar(cname)) 
    }else{
      cname <- paste(substr(cname, 1, r-3-2), substr(cname, r+1-2, nchar(cname)), sep='///')}
  }
  cname <- strsplit(cname,"///")[[1]]
  
  names(obj) <- cname
  obj <- as.list(obj)
  ##residuals
  RES <-  list()
  rr=1
  for( resi in c("residuals","residuals_obseved","residuals_predicted")){
    residuals <- ncvar_get(nc,resi)
    
    rownames(residuals) <- nc$var[[resi]]$dim[[1+2]]$vals
    cname <- nc$var[[resi]]$dim[[2+2]]$unit
    indelet <-  as.vector(gregexpr(pattern =':',nc$var[[resi]]$dim[[2+2]]$unit)[[1]])
    for(r in indelet){
      if(r==2){cname <- substr(cname, 3, nchar(cname)) 
      }else{
        cname <- paste(substr(cname, 1, r-3-2), substr(cname, r+1-2, nchar(cname)), sep='///')}
    }
    cname <- strsplit(cname,"///")[[1]]
    colnames(residuals) <- cname
    residuals <- melt(residuals)
    resi <- strsplit(resi,"_")[[1]][length(strsplit(resi,"_")[[1]])]
    colnames(residuals) <- c("Year","CPUE",resi)
    RES[[rr]] <- residuals
    rr <- rr+1
  }
  residuals <- na.omit(cbind(RES[[1]],RES[[2]],RES[[3]]))
  residuals <- residuals[, unique(colnames(residuals))]
  # residuals <- na.omit(Reduce(merge,RES))
  # residuals <- residuals[with(residuals, order(CPUE)), ]
  
  ##main
  main <- ncvar_get(nc,"main")
  cname <- nc$var[["main"]]$dim[[2+2]]$unit
  indelet <-  as.vector(gregexpr(pattern =':',nc$var[["main"]]$dim[[2+2]]$unit)[[1]])
  for(r in indelet){
    if(r==2){cname <- substr(cname, 3, nchar(cname)) 
    }else{
      cname <- paste(substr(cname, 1, r-3-2), substr(cname, r+1-2, nchar(cname)), sep='///')}
  }
  cname <- strsplit(cname,"///")[[1]]
  for(i in 1:dim(main)[3]){
    MAIN <- main[,,i]
    colnames(MAIN) <- cname
    rownames(MAIN)<- nc$var[["main"]]$dim[[1+2]]$vals
    MAIN <- MAIN[rowSums(MAIN,na.rm = T)!=0, ]
    MAIN <- melt(MAIN);colnames(MAIN)[1:2]<- c("Year","variable")
    MAIN$Retros <- nc$var[["main"]]$dim[[3+2]]$vals[i]
    MAIN <- MAIN[,c("Retros","Year","variable","value")]
    if(i==1){main1 <- MAIN}
    if(i>1){main1 <- rbind(main1,MAIN)}
  }
  main <- main1
  
  
  ##faa
  faa <- ncvar_get(nc,"faa")
  faa <- t(faa)
  cname <- nc$var[["faa"]]$dim[[2+2]]$unit
  indelet <-  as.vector(gregexpr(pattern =':',nc$var[["faa"]]$dim[[2+2]]$unit)[[1]])
  for(r in indelet){
    if(r==2){cname <- substr(cname, 3, nchar(cname)) 
    }else{
      cname <- paste(substr(cname, 1, r-3-2), substr(cname, r+1-2, nchar(cname)), sep='///')}
  }
  cname <- strsplit(cname,"///")[[1]]
  colnames(faa) <- nc$var[["faa"]]$dim[[1+2]]$vals
  rownames(faa) <- cname
  
  ###creat output VPA object from netcdf
  
  outputs_VPA_nc <- list(obj=obj,residuals=residuals,main=main,faa=faa)
  return(outputs_VPA_nc)
}


