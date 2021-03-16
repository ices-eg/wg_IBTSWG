rm()
library(icesDatras)

OutPath<-"n:/projecten/Marine litter IBTS/2021/figures/"

os.type <- function (type = c("unix", "windows", "other"))
{
  type <- match.arg(type)
  if (type %in% c("windows", "unix")) {
    .Platform$OS.type == type
  } else {
    TRUE
  }
}

readDatras <- function(url) {
  # try downloading first:
  # create file name
  tmp <- tempfile()
  on.exit(unlink(tmp))
  
  # download file
  ret <-
    if (os.type("windows")) {
      download.file(url, destfile = tmp, quiet = TRUE)
    } else if (os.type("unix") & Sys.which("wget") != "") {
      download.file(url, destfile = tmp, quiet = TRUE, method = "wget")
    } else if (os.type("unix") & Sys.which("curl") != "") {
      download.file(url, destfile = tmp, quiet = TRUE, method = "curl")
    } else {
      127
    }
  
  # check return value
  if (ret == 0) {
    # scan lines
    scan(tmp, what = "", sep = "\n", quiet = TRUE)
  } else {
    message("Unable to download file so using slower method url().\n",
            "Try setting an appropriate value via\n\t",
            "options(download.file.method = ...)\n",
            "see ?download.file for more information.")
    # connect to url
    con <- url(url)
    on.exit(close(con))
    
    # scan lines
    scan(con, what = "", sep = "\n", quiet = TRUE)
  }
}

parseDatras <- function(x) {
  # parse using line and column separators
  type <- gsub(" *<ArrayOf(.*?) .*", "\\1", x[2])
  starts <- grep(paste0("<", type, ">"), x)
  ends <- grep(paste0("</", type, ">"), x)
  ncol <- unique(ends[1] - starts[1]) - 1
  # drop everything we don't need
  x <- x[-c(1, 2, starts, ends, length(x))]
  
  # exit if no data is being returned
  if (length(x) == 0) return(NULL)
  
  # match content of first <tag>
  names_x <- gsub(" *<(.*?)>.*", "\\1", x[1:ncol])
  
  # delete all <tags>
  x <- gsub(" *<.*?>", "", x)
  # trim white space
  x <- trimws(x)
  
  # convert to data frame
  dim(x) <- c(ncol, length(x)/ncol)
  row.names(x) <- names_x
  x <- as.data.frame(t(x), stringsAsFactors = FALSE)
  
  # return data frame now if empty
  if (nrow(x) == 0) return(x)
  
  # DATRAS uses -9 and "" to indicate NA
  x[x == -9] <- NA
  x[x == ""] <- NA
  
  # simplify all columns except StatRec (so "45e6" does not become 45000000)
  x[names(x) != "StatRec"] <- simplify(x[names(x) != "StatRec"])
  
  x
}

simplify <- function(x) {
  # from Arni's toolbox
  # coerce object to simplest storage mode: factor > character > numeric > integer
  owarn <- options(warn = -1)
  on.exit(options(owarn))
  # list or data.frame
  if (is.list(x)) {
    for (i in seq_len(length(x)))
      x[[i]] <- simplify(x[[i]])
  }
  # matrix
  else if (is.matrix(x))
  {
    if (is.character(x) && sum(is.na(as.numeric(x))) == sum(is.na(x)))
      mode(x) <- "numeric"
    if (is.numeric(x))
    {
      y <- as.integer(x)
      if (sum(is.na(x)) == sum(is.na(y)) && all(x == y, na.rm = TRUE))
        mode(x) <- "integer"
    }
  }
  # vector
  else
  {
    if (is.factor(x))
      x <- as.character(x)
    if (is.character(x))
    {
      y <- as.numeric(x)
      if (sum(is.na(y)) == sum(is.na(x)))
        x <- y
    }
    if (is.numeric(x))
    {
      y <- as.integer(x)
      if (sum(is.na(x)) == sum(is.na(y)) && all(x == y, na.rm = TRUE))
        x <- y
    }
  }
  x
}



getLTdata<-function (survey, year, quarter) 
{
  if (!checkSurveyOK(survey)) 
    return(FALSE)
  if (!checkSurveyYearOK(survey, year, checksurvey = FALSE)) 
    return(FALSE)
  if (!checkSurveyYearQuarterOK(survey, year, quarter, checksurvey = FALSE, 
                                checkyear = FALSE)) 
    return(FALSE)
  url <- sprintf("https://datras.ices.dk/WebServices/DATRASWebService.asmx/getLitterAssessmentOutput?survey=%s&year=%i&quarter=%i",
                 survey, year, quarter)
  out <- readDatras(url)
  out <- parseDatras(out)
  out
}

##################################################reading the LitterAssessment data gives errounous names differing by year#######

Newnames<-c("Survey","Quarter","Year","Ship","Gear","Country","StNo","HaulNo","ShootLat","ShootLong","HaulLat","HaulLong","OSPARArea",
"MSFDArea",                       "BottomDepth",                    "Distance",                      
"DoorSpread",                     "WingSpread",  "LTREF"                        ,  "PARAM",                         
"LTSZC",                          "UnitWgt",                        "LT_Weight",                      "UnitItem",                      
"LT_Items",                       "LTSRC",                          "TYPPL",                          "LTPRP",                         
"SweepLngt",                      "GearExp",                        "DoorType",                       "Month",                         
"Day",                            "TimeShot",                       "HaulDur",                        "StatRec",                       
"Depth",                          "HaulVal",                        "DataType",                       "Netopening",                    
"Rigging",                        "Tickler",     "Warplngt",                       "Warpdia",                       
"WarpDen",                        "DoorSurface",                    "DoorWgt",                        "TowDir",     
"GroundSpeed",                    "SpeedWater",  "WindDir",                        "WindSpeed",                     
"SwellDir",    "SwellHeight",    "EEZ",                            "NMArea",                      
"DateofCalculation") 

##########################Reading the LitterAssessment data
for(z in c(2012:2021)){
LT<-getLTdata("NS-IBTS",z,1)
names(LT)<-Newnames
if(z==2012){LT.tot<-LT}
else{LT.tot<-rbind(LT.tot,LT)}
}


####### Litter it reported but no count registered
checkNA<-LT.tot[is.na(LT.tot$LT_Items)==T,]
dim(checkNA)
#[1] 470  57
### 47 - items om 2012,2013 and 2014. All From France. Item was weighed but not counted. 
###### all the items counted as 1. 
LT.tot[is.na(LT.tot$LT_Items)==T,]$LT_Items<-1


############Not all countries provide zero hauls specific data to the litter part, merging with HH is preffered####
for(z in c(2012:2021)){
  LT<-getHHdata("NS-IBTS",z,1)
  if(z==2012){HH.tot<-LT}
  else{HH.tot<-rbind(HH.tot,LT)}
}

LT1<-merge(HH.tot,LT.tot, by=c("Survey","Year","Quarter","Country","Ship","Gear","StNo","StatRec","HaulNo", "ShootLat", "ShootLong","HaulLong","HaulLat","HaulVal","GearExp"),all=T)

###aggregate by PARAM (e.g. litter_type)
a1<-aggregate(LT1$LT_Items, by=list(Quarter=LT1$Quarter,Year=LT1$Year,Ship=LT1$Ship,Country=LT1$Country,StNo=LT1$StNo,PARAM=LT1$PARAM),FUN=sum)

##create boxplots to show alleged differences in counting items over the years and between countries. 

PARAMs<-unique(a1$PARAM)
PDFfigures = paste0(OutPath, "Overview_of_numbers_per_PARAM_per_year.pdf")
pdf(file=PDFfigures) 

for (x in unique(a1$PARAM)){
a3<-a1[a1$PARAM==x,]

for(i in unique(a1$Country)){
a2<-a3[a3$Country==i,]
if(sum(a2$x)>1){
boxplot(a2$x~a2$Year, xlab="year", ylab="number", main=paste0(i," ",x))       
}}
}
dev.off() 






