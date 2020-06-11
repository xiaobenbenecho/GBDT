library(here)
library(data.table)
library(snow)
library(fasttime)
library(fst)
library(lubridate)
library(zoo)
library(plotROC)
library(RcppRoll)



files <- sort(list.files(here::here("RawData")))
files <- files[grepl(".csv",files)]



for(iter in 1:length(subset)){ 
  # file reading
  cl <- makeCluster(getOption("cl.cores", 8))
  readfiles <- files[subset[[iter]]]
  mylist <- parLapply(cl,here::here("RawData",readfiles), fread)
  stopCluster(cl = cl)
  alldata <- rbindlist( mylist )
  
  # erase observations further than 3 secs
  alldata<-alldata[groupdif<3]
  
  # Add tool names
  toolnames <- fread(here::here("AuxiliaryTables","ToolNames.csv"))
    alldata <- toolnames[alldata,on="Cnc_ToolNumber"]
  alldata[is.na(ToolName),ToolName:="Unknown"]
  alldata[,ToolFactor:=as.double(as.factor(ToolName))]
  
  # Convert time to POSIXct
  alldata[,Time:=fastPOSIXct(Time)]
  # Add Date (optional)
  alldata[,Date:=date(Time)]
  # Safety ordering
  setorder(alldata,"Time")
  
  # Add manual mode, interruptions are also considered
  alldata[,isMan:=(opmode==0|Cnc_OperationMode==1)]
  # Auxiliar block for stop marking
  alldata[,isManBlock:=cumsum(isMan!=shift(isMan,fill=FALSE))]
  
  # Indicate inRun criteria
  alldata[,inRun:=opmode==2&Cnc_OperationMode==3&
            grepl("OP",Cnc_ProgramName)&
            !ToolName%in%c("Touch probe","Unknown")&Cnc_Override_Axis>0]
  
  # Stops will at least contain 10 observations after stopping
  # Can be done with roll_max() and rev() but this seems clearer
  alldata[,stop:=!inRun&shift(inRun)&
            !shift(inRun,type="lead")&
            !shift(inRun,n=2L,type="lead")&
            !shift(inRun,n=3L,type="lead")&
            !shift(inRun,n=4L,type="lead")&
            !shift(inRun,n=5L,type="lead")&
            !shift(inRun,n=6L,type="lead")&
            !shift(inRun,n=7L,type="lead")&
            !shift(inRun,n=8L,type="lead")&
            !shift(inRun,n=9L,type="lead")&
            !shift(inRun,n=10L,type="lead")]
  alldata[is.na(stop),stop:=FALSE]
  
  
  # Variables to erase program/day begin and end stops
  alldata[,sbefore:=cumsum(inRun),by=c("Date","Cnc_ProgramName","execution")]
  alldata[,safter:=rev(cumsum(rev(inRun))),by=c("Date","Cnc_ProgramName","execution")]
  # Last/First 5 minutes of the day not considered stops
  k=300
  alldata[,stop2:=stop&sbefore>k&safter>k]
  # erase safter (not predictable unknown in advance)
  alldata[,safter:=NULL]
  
  # stopintervals groups of observations with one stop at the beginning
  alldata[,stopintervals:=cumsum(as.integer(stop2))]
  
  
  # Intervals of time containing JOGS this is calculated
  # to obtain the time to the next JOG usage
  alldata[,isJog:=as.integer(opmode==0)]
  alldata[,jogBlock:=rev(cumsum(rev(isJog)))]
  alldata[,ttojog:=max(Time)-Time,jogBlock]
  
  # Same as the jogs but with tool change
  alldata[,ischan:=as.integer(Cnc_Program_BlockNumber_EC==-2)]
  alldata[,chanBlock:=rev(cumsum(rev(ischan)))]
  alldata[,ttochan:=max(Time)-Time,chanBlock]
  
  # Filter some stops because of time
  # stopsecs observations in stop needed so the stop is considered in the next
  # stopseclen observations
  stopsecs <- 120
  stopsecslen <- 300
  # stopcoef marks the run time in the next 5 mins
  alldata[,stopRunaux:=rev(cumsum(rev(!inRun)))]
  alldata[,stopRunauxfut:=shift(stopRunaux,stopsecslen,type="lead")]
  alldata[,stopcoef:=stopRunaux-stopRunauxfut]
  
  # If there is a small time to a tool change or
  # the stop is short or the JOG is not activated after five minutes
  # the stops will be discarded
  ttojogsafe <- 300
  ttochanerase <- 120
  
  alldata[stopcoef<=stopsecs|(ttojogsafe<ttojog|ttochanerase>ttochan),stop2:=FALSE]
  # Recalculate stopintervals for the chosen stops
  alldata[,stopintervals:=cumsum(as.integer(stop2))]
  
  # Variables to measure the movements in Z and the time with the operator door open
  # the latter isn't used
  ManMoves <- alldata[isMan==TRUE,
                      list(Zrangeman=abs(diff(range(Axis_Z_positionActualMCS_mm))),
                           OperatorTime=sum(Is_OperatorDoorOpen)),
                      by=c("isManBlock","stopintervals")]
  
  # Only the stops with manual moves in Z are being analysed
  stopin <- ManMoves[Zrangeman>10,stopintervals]
  alldata[,stop2:=stop2&stopintervals%in%stopin]
  
  # Recalculate stopintervals for the chosen stops
  alldata[,stopintervals:=cumsum(as.integer(stop2))]
  
  # Obtain max time for each stopinterval
  alldata[,dtime:=as.double(max(Time))-as.double(Time),stopintervals]
  # The prior 60 secs are marked
  intertime <- 60
  alldata[,stopvalid:=as.integer(dtime<intertime)]
  
  # marked data is generated
  outname <- paste0("marked",names(subset)[[iter]],".fst")
  write.fst(alldata,here::here("MarkedData",outname))

  
  # Keep important variables and collect garbage
  rm(list=setdiff(ls(),c("subset","iter","files","cl")))
  for(i in 1:20)print(gc())
  
}
