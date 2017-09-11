getDir <- function(file) {
  if (!grepl(.Platform$file.sep,file))
    res <- getwd()
  else
    res <- substr(file,1,max(gregexpr(.Platform$file.sep,file)[[1]])-1)
  return(res)} 

getFile <- function(file) {
  if (substr(file,1,1)=="\\"){
      dr=getDir(file)
      res=substr(file,nchar(dr)+2,nchar(file))}
  else
    res=file
  
  return(res)} 

getExt <- function(file) {
  res=getFile(file)
  
  res=substr(res,gregexpr("\\.",res)[[1]][1]+1,nchar(res))
  return(res)} 

skip.hash <- function(i,file) {
    i <- i+1
    while (substr(scan(file,skip=i,nlines=1,what=("character"),quiet=TRUE)[1],1,1)=="#")
        i <- i+1

    return(i)}

skip.until.hash <- function(i,file) {
    i <- i+1
    while (substr(scan(file,skip=i,nlines=1,what=("character"),quiet=TRUE)[1],1,1)!="#")
        i <- i+1

    return(i)} 

skip.until.minus.1<-function(i,x) {
        i<-i+1
        while (scan(x,skip=i,nlines=1,what=("character"),quiet=TRUE)[1]!="-1")
            i<-i+1
        return(i)}



