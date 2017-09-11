#### Utility functions
## read in from VPA2Box output "R" files
posFile<-function(i,filename,char="-"){
   while (TRUE){
     firstChar<-substr(scan(filename, skip = i, nlines = 1, what = ("character"), quiet = TRUE)[1], 1, 1)

     if (!is.na(firstChar))
        if (firstChar == char) break

     i<-i+1}

   return(i)}

getFLQ<-function(filename,pos1, pos2)
    {
    nyrs <-pos2-pos1-1
    t.   <-scan(filename, skip = pos1+1, nlines=nyrs, quiet = TRUE)
    nages<-length(t.)/nyrs
    t.   <-array(t.,c(nages,nyrs))

    yrs <-array(t.,c(nages,nyrs))[1,]
    ages<-scan(filename, skip = pos1-1, nlines=1, quiet = TRUE)

    flq<-FLQuant(t.[-1,],dimnames=list(age=ages,year=yrs))

    return(flq)}

## create retro stocks
getRetros<-function(vpa.dir,stk,nRet,stks=FLStocks(stk))
    {
    for (iRetro in nRet[-1]) {
       ## Start reading file

       filename<-paste(vpa.dir,"MINUS",iRetro,".R",sep="")
       
       ## get Retro estimates
       i<-0
       pos1             <-posFile(i,filename)
       pos2             <-posFile(pos1,filename,char="=")
       harvest          <-getFLQ(filename,pos1, pos2)

       pos1             <-posFile(pos2,filename)
       pos2             <-posFile(pos1,filename,char="=")
       stock.n          <-getFLQ(filename,pos1, pos2-1)

       pos1             <-posFile(pos2,filename)
       pos2             <-posFile(pos1,filename,char="=")
       catch.n          <-getFLQ(filename,pos1, pos2)

       stks[[iRetro+1]]<-window(stk,end=dims(harvest)$maxyear)

       harvest(   stks[[iRetro+1]])<-harvest
       stock.n(   stks[[iRetro+1]])<-stock.n
       catch.n(   stks[[iRetro+1]])<-catch.n
       landings.n(stks[[iRetro+1]])<-catch.n
       discards.n(stks[[iRetro+1]])[]<-0
       units(harvest(stks[[iRetro+1]]))<-"f"

       catch(   stks[[iRetro+1]])<-computeCatch(   stks[[iRetro+1]],'all')
       landings(stks[[iRetro+1]])<-computeLandings(stks[[iRetro+1]])
       discards(stks[[iRetro+1]])<-computeDiscards(stks[[iRetro+1]])}

    names(stks)=nRet
    return(stks)}

