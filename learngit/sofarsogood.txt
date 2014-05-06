#Small improvement based on Mac is made.
##Only use library(plyr) shows error in my Mac
##So I replace it to install.packages and get the released version.
##In Mac, it may appear”tar: Failed to set default locale”
##This error can be solved by writing the following in terminal: defaults write org.R-project.R force.LANG en_US.UTF-8
##And restart R

install.packages("plyr") 

install.packages("gnumeric")

install.packages("gdata")

install.packages("foreign")



#download data from FERC website
downloadFERCdata1995 <- function(base.url="http://www.ferc.gov/docs-filing/forms/form-2/data",
                          years=1991:1995,
                          out.dir="./fercForm2Data") {
   dir.create(out.dir)
   for(y in years) {
     for(letters in c("A-M","N-Z")) {
       name <- sprintf("F2Y%d%s.zip",y-1900,letters)
       file.name <- paste(out.dir,name,sep="/")
       if (file.exists(file.name)) {
         warning(sprintf("%s already exists, not redownloading\n",file.name))
         return
       } else {
         url <- paste(base.url,y,name,sep="/")
         download.file(url,file.name,mode="wb")
       }
     }
   }
 }


#Small changes are made(e.g. replace ”dbf” to “dat”)
 loadYears <- function(name, years=1991:1995, base.dir=.){
 require(gnumeric)
 appendedData <- NULL;
 Tem.dir <- tempdir()
 dir.create(tem.dir)
 for (years in years){
 zip.name <-
 paste(base.dir,"/F2Y%d%s",as.character(year),".zip",sep="")
 zl <- unzip(zip.name,list=TRUE)$Name
 archive.name <- zl[grepl(name,x=zl,ignore.case=TRUE)]
 unzip(zip.name,giles=archive.name,exdir=tem.dir)
 dat.file <- paste(tmp.dir,archive.name,sep="/")
 yrData <- read.gnumeric.sheet(dat.faile,head=TRUE)
 if (is.null(appendedData)) {
       appendedData <- yrData
     }
     else {
       appendedData <- rbind.fill(appendedData,
                                  yrData)
     }
 unlink(dat.file)
 print(paste("Finished",name,year))  }
 names(appendedData) <- tolower(names(appendedData))
 
   appendedData <- rename(appendedData,
                          c(respondent="respondent_id",
                            responden2="respondent_name"))
 
   return(appendedData)
 }


#make variable names all lower case
#Codes remain unchanged
getRows <- function(data, rowNums, rowNames, vars=c("bal_end_yr","additions","retirement"),
                    years=NULL,periods=12) {
  for (i in 1:length(rowNums)) {
    temp <- data[c("report_yr","respondent_id","report_prd",vars)][data$row_num==rowNums[i],]
    for (v in vars) {
      names(temp)[names(temp)==v]=paste(rowNames[i],v,sep="_")
    }
    if (i==1) {
      output <- temp
    } else {
      output <- merge(output,temp,by=c("report_yr","respondent_id","report_prd"),all=TRUE)
    }
  }
  if (!is.null(years)) {
    output <- output[is.element(output$report_yr,years),]
  }
  if (!is.null(periods)) {
    output <- output[is.element(output$report_prd,periods),]
  }
  return(output)
}

## Addition, treating NA + x = x instead of usual NA + x = NA
plusNA0 <- function(x,y) {
  z <- x+y
  z[is.na(x)] = y[is.na(x)]
  z[is.na(y)] = x[is.na(y)]
  return(z)
}

