library(png)
days<-read.csv("daysfile.csv")
#days<-Data$Days_Opened[1:(length(Data$Days_Opened)-1)]
#days<-c(2,3,42,3,4,23,4,2,34,6,6,4,3,2,4,2,42,4,3,24,3)
daysInInter<-read.csv("DaysInInter.csv")
LocsTimes<-read.csv('Parameters.csv')
Locs<-LocsTimes[1,'Locations']
TimeSlices<-LocsTimes[1,'TimeSlices']
extension<-paste(Locs,"L",TimeSlices,"T.csv",sep="")
WazeFilename=paste("WazeData",extension,sep="")
OfficialDataFilename=paste("OfficialData",extension,sep="")
ofdata<-read.csv("OfficialData100L168T.csv")
wzdata<-read.csv("WazeData100L168T.csv")
ofLocs<-ofdata[,"Location"]
ofInitialTime<-ofdata[,"InitialTimeSlot"]
ofFinalTime<-ofdata[,"FinalTimeSlot"]
ofDaysLength<-ofdata[,"TotalDaysPotholeWasOpen"]
wzLocs<-wzdata[,"Location"]
wzTime<-wzdata[,"TimeSlice"]
wzPointX<-wzdata[,"locx"]
wzPointY<-wzdata[,"locy"]
ofPointX<-ofdata[,"LONGITUDE"]
ofPointY<-ofdata[,"LATITUDE"]
n_official_points<-length(ofPointX)
xmin<-min(min(ofPointX),min(wzPointX))
ymin<-min(min(ofPointY),min(wzPointY))
xmax<-max(max(ofPointX),max(wzPointX))
ymax<-max(max(ofPointY),max(wzPointY))
mycols=c("blue","red","forestgreen","blueviolet")
mypch=c(22,20,24,7)
mygrid=c(1,10,15,20,25,30,35,40,80,120,160,200,240,400)
maxtimeslice<-max(ofFinalTime)

  
shinyServer(function(input, output) {
output$bottomText<-renderText("(1,1)<-bottom left")
output$main_plot <- renderPlot({
  if(input$potdata=="All Potholes"){
      if(input$Granularity=="days"){mydata<-days}
      if(input$Granularity=="hours"){mydata<-days*24}
      if(input$Granularity=="minutes"){mydata<-days*1440}
  }
  else{
    if(input$Granularity=="days"){mydata<-ofDaysLength}
    if(input$Granularity=="hours"){mydata<-ofDaysLength*24}
    if(input$Granularity=="minutes"){mydata<-ofDaysLength*1440}
  }
  
  

  limited_max<-mydata[mydata<input$minmax[2]]
  limited_data<-limited_max[limited_max>input$minmax[1]]
  
  hist(limited_data,
      probability = TRUE,
      breaks = as.numeric(input$n_breaks),
      xlab = "Duration of Open Potholes",
      main = "Open Potholes")

    output$text1 <- renderText({summary(limited_data)})
    output$text0 <- renderText({names(summary(limited_data))})
  
  
  
  
  })


determineLocation<-function(points){
     my_gridsize<-mygrid[as.numeric(input$gridsize)]
     stepsizex<-(xmax-xmin)/my_gridsize
     stepsizey<-(ymax-ymin)/my_gridsize
     lx<-ceiling((points[,1]-xmin)/stepsizex)
     ly<-ceiling((points[,2]-ymin)/stepsizey)
     
     return(my_gridsize*(ly-1)+lx)
}



fixMyReports<-function(waze,official){
     wzLocs<-determineLocation(matrix(c(wzPointX[waze],wzPointY[waze]),ncol=2))
     offLocs<-determineLocation(matrix(c(ofPointX[official],ofPointY[official]),ncol=2))
     return(waze[wzLocs %in% offLocs])
}


addCoordinates<-function(locations){
     my_gridsize<-mygrid[as.numeric(input$gridsize)]
     stepsizex<-(xmax-xmin)/my_gridsize
     stepsizey<-(ymax-ymin)/my_gridsize
     locx<-xmin+(locations%%my_gridsize)*stepsizex-stepsizex/2
     locx<-replace(locx,which(locx==0),my_gridsize)
     locy<-ymin+(floor(locations/my_gridsize)+1)*stepsizey-stepsizey/2
     ofPointX<-c(ofPointX,locx)
     ofPointY<-c(ofPointY,locy)
     return(matrix(c(locx,locy),ncol=2))
     
}



fixAddPotholes<-function(waze,official){
     wzLocs<-determineLocation(matrix(c(wzPointX[waze],wzPointY[waze]),ncol=2))
     offLocs<-determineLocation(matrix(c(ofPointX[official],ofPointY[official]),ncol=2))
     
     A<-as.data.frame(table(wzLocs))
     B<-as.numeric(as.character(A$wzLocs[A$Freq>=input$crazyThreshold]))
     C<-B[!(B %in% offLocs)]
     if(length(C)>0){newCoords<-addCoordinates(C)}
     else{newCoords<-c()}
     
     
     return(list(v=c(official,C),newCoords=newCoords))
     
}

output$scatterPlot<-renderPlot({
     ct<-input$CurrentTime;
     tss<-as.numeric(input$timeSliceSize);
     lb<-(tss*(ct-1)+1) 
     ub<-ct*tss
    wztime<-which(wzTime>=lb & wzTime<=ub)
    validIndex<-which(ofInitialTime<ub & ofFinalTime>lb)
    newPotholeIndex<-which(ofInitialTime>= lb & ofInitialTime<=ub)
    closedPotholeIndex<-which(ofFinalTime>=lb & ofFinalTime<=ub)
    
    if(input$fixReports){    
         wztime<-fixMyReports(wztime,validIndex)         
    }
   
    if(input$fixPotholes){
          A<-fixAddPotholes(wztime,validIndex)
          validIndex<-A$v; ofPointX<-c(ofPointX,A$newCoords[,1]); ofPointY<-c(ofPointY,A$newCoords[,2])
          par(new=TRUE)
          plot(ofPointX[n_official_points:length(ofPointX)],ofPointY[n_official_points:length(ofPointX)],col="blue",xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="LONGITUDE",ylab="LATITUDE",pch=22)
    
          
    }
    else{
          ofPointX<-ofPointX[1:n_official_points]
          ofPointY<-ofPointY[1:n_official_points]
    }
 
    
    par(new=TRUE)
    m_title<-paste("Potholes and Reports from",as.character(sort(wzdata$inject_date[wztime])[1]),"to",as.character(sort(wzdata$inject_date[wztime])[length(wztime)]))
    if(length(wztime)==0){m_title<-"No Reports at this time"}
    if(input$showmap){ima <- readPNG("bostonmap.png");lim<-par();rasterImage(ima, lim$usr[1]-0.25, lim$usr[3]+0.12, lim$usr[2]+0.125, lim$usr[4]-0.075,interpolate=FALSE);par(new=TRUE);par(new=TRUE);}
    plot(wzPointX[wztime],wzPointY[wztime],type="p",col="red",xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="LONGITUDE",ylab="LATITUDE",main=m_title,pch=20)
    par(new=TRUE)
    plot(ofPointX[validIndex],ofPointY[validIndex],col="blue",xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="LONGITUDE",ylab="LATITUDE",main=m_title,pch=22)
    par(new=TRUE)
    plot(ofPointX[newPotholeIndex],ofPointY[newPotholeIndex],col="forestgreen",xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="LONGITUDE",ylab="LATITUDE",main=m_title,pch=24)
    par(new=TRUE)
    plot(ofPointX[closedPotholeIndex],ofPointY[closedPotholeIndex],col="blueviolet",xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab="LONGITUDE",ylab="LATITUDE",main=m_title,pch=7)  
    
    
    
    
    #ADDING GRID
    par(new=TRUE)
    grid(nx=mygrid[as.numeric(input$gridsize)],ny=mygrid[as.numeric(input$gridsize)],col="black")    
    
    
    legend("bottomright",c("Open potholes","Reports","New potholes","Closed potholes"),col=mycols,pch=mypch)       
    
    
    
    if(input$showsquare){
     i<-as.numeric(input$LocX)
     j<-as.numeric(input$LocY)
     my_gridsize<-mygrid[as.numeric(input$gridsize)]
     stepsizex<-(xmax-xmin)/my_gridsize
     stepsizey<-(ymax-ymin)/my_gridsize
     boundsx<-c(xmin+(stepsizex)*(i-1),xmin+(stepsizex)*i)
     boundsy<-c(ymin+(stepsizey)*(j-1),ymin+(stepsizey)*j)
     rect(boundsx[1],boundsy[1],boundsx[2],boundsy[2],border="azure3",density=15)
    }
    })


output$miniHist<-renderPlot({
     ct<-input$CurrentTime;
     tss<-as.numeric(input$timeSliceSize);
     lb<-(tss*(ct-1)+1) 
     ub<-ct*tss
    wztime<-which(wzTime>=lb & wzTime<=ub)
    validIndex<-which(ofInitialTime<ub & ofFinalTime>lb)
    newPotholeIndex<-which(ofInitialTime>= lb & ofInitialTime<=ub)
    closedPotholeIndex<-which(ofFinalTime>=lb & ofFinalTime<=ub)
    if(input$fixReports){wztime<-fixMyReports(wztime,validIndex)}
    if(input$fixPotholes){
          A<-fixAddPotholes(wztime,validIndex)
          validIndex<-A$v; ofPointX<-c(ofPointX,A$newCoords[,1]); ofPointY<-c(ofPointY,A$newCoords[,2])          
    }
    else{
          ofPointX<-ofPointX[1:n_official_points]
          ofPointY<-ofPointY[1:n_official_points]
    }
     
     
     
    #wztime<-which(wzTime==input$CurrentTime)
    #validIndex<-which(ofInitialTime<input$CurrentTime & input$CurrentTime<ofFinalTime)
    #newPotholeIndex<-which(ofInitialTime==input$CurrentTime)
    #closedPotholeIndex<-which(ofFinalTime==input$CurrentTime)
 
    Duration_in_Hours<-c(ofDaysLength[validIndex],ofDaysLength[newPotholeIndex],ofDaysLength[closedPotholeIndex])
    hist(Duration_in_Hours[Duration_in_Hours<input$setMax],300,xlab="Hours",main="Histogram of the duration in hours of potholes in this period")
     output$text3 <- renderText({summary(Duration_in_Hours)})
    output$text2 <- renderText({names(summary(Duration_in_Hours))})
     output$gridSummary<-renderText({"Overall Statistics:"})
     output$openPotholeText <- renderText({paste("Open potholes (were open before this period and closed afterwards): ",length(validIndex))})
     output$newPotholeText<-renderText({paste("New potholes (were open in this period): ", length(newPotholeIndex))})
     output$closedPotholeText<-renderText({paste("Closed potholes (were closed in this period): ",length(closedPotholeIndex))})
     output$N_reports<-renderText({paste("Number of reports in this period: ",length(wztime))})
      print(wzTime[wztime])
    })

output$timeLine<-renderPlot({
     
     tss<-as.numeric(input$timeSliceSize);
     
     lenReports<-c()
     lenOpenPot<-c()
     lenNewPot<-c()
     lenClosedPot<-c()
     for(CurrentTime in seq(2,(maxtimeslice/tss-1),1)){
          ct<-CurrentTime
          lb<-(tss*(ct-1)+1) 
          ub<-ct*tss
     
          wztime<-which(wzTime>=lb & wzTime<=ub);
          validIndex<-which(ofInitialTime<ub & ofFinalTime>lb);
          newPotholeIndex<-which(ofInitialTime>= lb & ofInitialTime<=ub);
          closedPotholeIndex<-which(ofFinalTime>=lb & ofFinalTime<=ub);          
          if(input$fixReports){wztime<-fixMyReports(wztime,validIndex)}
          if(input$fixPotholes){A<-fixAddPotholes(wztime,validIndex);validIndex<-A$v; ofPointX<-c(ofPointX,A$newCoords[,1]); ofPointY<-c(ofPointY,A$newCoords[,2])}
                else{ofPointX<-ofPointX[1:n_official_points];ofPointY<-ofPointY[1:n_official_points]}
          
          
          lenReports<-c(lenReports,length(wztime))
          lenOpenPot<-c(lenOpenPot,length(validIndex));          
          lenNewPot<-c(lenNewPot,length(newPotholeIndex))
          
          lenClosedPot<-c(lenClosedPot,length(closedPotholeIndex));     
     }
     
     
     
     A<-cbind(lenOpenPot,lenNewPot,lenClosedPot,lenReports)
     #xmaxtime<-max(ofFinalTime)-1
     
     
     choiceConverter<-c("Open potholes","New potholes","Closed potholes","Reports")
     conversion<-c()
     for(i in 1:length(input$plotvar)){conversion<-c(conversion,which(choiceConverter==input$plotvar[i]))}
     ymaxtime<-max(A[,conversion])
     
     plot(0,0,xlim = c(0,dim(A)[1]),ylim = c(0,ymaxtime+1),type = "n",xlab="Time Period",ylab="Count",main="Potholes and Reports time series")
     axis(1, xaxp=c(10, 200, 19), las=1)
     mypch2<-c(mypch[1],mypch[3],mypch[4],mypch[2])
     mycols2<-c(mycols[1],mycols[3],mycols[4],mycols[2])
     for(i in conversion){
          lines(2:(dim(A)[1]),A[2:dim(A)[1],i],type = 'b',pch=mypch2[i],col=mycols2[i])
     }
     legend("right", inset=c(0,0),c("Open potholes","Reports","New potholes","Closed potholes"),col=mycols,pch=mypch)       
      
    
  
     
     
     })

output$locationsTable<-renderTable({
  if(input$actionTable){
  my_gridsize<-mygrid[as.numeric(input$gridsize)]  
  totalTimeSlices<-(maxtimeslice/as.numeric(input$timeSliceSize))
  tss<-as.numeric(input$timeSliceSize);
  stepsizex<-(xmax-xmin)/my_gridsize
  stepsizey<-(ymax-ymin)/my_gridsize
  
  
  Results<-matrix(0,my_gridsize*my_gridsize,7)
  
  for(i in 1:my_gridsize){for(j in 1:my_gridsize){
    countReports<-c()
    countOpenPotholes<-c()
    countNewPotholes<-c()
    countClosedPotholes<-c()
    
    
  my_gridsize<-mygrid[as.numeric(input$gridsize)]    
  boundsx<-c(xmin+(stepsizex)*(i-1),xmin+(stepsizex)*i)
  boundsy<-c(ymin+(stepsizey)*(j-1),ymin+(stepsizey)*j)
  
  
  for(ct in 1:totalTimeSlices){
    
    lb<-(tss*(ct-1)+1) 
    ub<-ct*tss
    wztime<-which(wzTime>=lb & wzTime<=ub)
    validIndex<-which(ofInitialTime<ub & ofFinalTime>lb)
    newPotholeIndex<-which(ofInitialTime>= lb & ofInitialTime<=ub)
    closedPotholeIndex<-which(ofFinalTime>=lb & ofFinalTime<=ub)
    
    if(input$fixReports){wztime<-fixMyReports(wztime,validIndex)}
       if(input$fixPotholes){A<-fixAddPotholes(wztime,validIndex);validIndex<-A$v; ofPointX<-c(ofPointX,A$newCoords[,1]); ofPointY<-c(ofPointY,A$newCoords[,2])}
                else{ofPointX<-ofPointX[1:n_official_points];ofPointY<-ofPointY[1:n_official_points]}
       
    
    inrow<-which(wzdata$locx[wztime]>=boundsx[1] & wzdata$locx[wztime]<boundsx[2])
    countReports<-c(countReports,length(which(wzdata$locy[wztime[inrow]]>=boundsy[1] & wzdata$locy[wztime[inrow]]<boundsy[2])))
    
    inrow<-which(ofdata$LONGITUDE[validIndex]>=boundsx[1] & ofdata$LONGITUDE[validIndex]<boundsx[2])
    countOpenPotholes<-c(countOpenPotholes,length(which(ofdata$LATITUDE[validIndex[inrow]]>=boundsy[1] & ofdata$LATITUDE[validIndex[inrow]]<boundsy[2])))
    
    inrow<-which(ofdata$LONGITUDE[newPotholeIndex]>=boundsx[1] & ofdata$LONGITUDE[newPotholeIndex]<boundsx[2])
    countNewPotholes<-c(countNewPotholes,length(which(ofdata$LATITUDE[newPotholeIndex[inrow]]>=boundsy[1] & ofdata$LATITUDE[newPotholeIndex[inrow]]<boundsy[2])))
    
    inrow<-which(ofdata$LONGITUDE[closedPotholeIndex]>=boundsx[1] & ofdata$LONGITUDE[closedPotholeIndex]<boundsx[2])
    countClosedPotholes<-c(countClosedPotholes,length(which(ofdata$LATITUDE[closedPotholeIndex[inrow]]>=boundsy[1] & ofdata$LATITUDE[closedPotholeIndex[inrow]]<boundsy[2])))          
    
  } #closes ct for loop     
  
  Results[my_gridsize*(i-1)+j,1]<-i
  Results[my_gridsize*(i-1)+j,2]<-j
  Results[my_gridsize*(i-1)+j,3]<-sum(countOpenPotholes)
  Results[my_gridsize*(i-1)+j,4]<-sum(countReports)
  Results[my_gridsize*(i-1)+j,5]<-sum(countNewPotholes)
  Results[my_gridsize*(i-1)+j,6]<-sum(countClosedPotholes)
  Results[my_gridsize*(i-1)+j,7]<-sum(countReports)/(sum(c(countOpenPotholes,countNewPotholes,countClosedPotholes)))
  
  
  
  }}#closes i, j for loops
  entries<-my_gridsize**2
   infs<-sum(is.infinite(Results[,7]))
  zeros<-length(which(Results[,7]==0))
  lessThan1<-length(which(Results[,7]<1))-zeros
  moreThan1<-length(which(Results[,7]>1))-infs
  emptyLocs<-sum(is.nan(Results[,7]))
  output$matrixSummary<-renderText({paste("Entries:",entries,"Locations with reports but no potholes (inf):", infs, "(",infs/entries*100,"%) Locations with potholes but no reports (0's):",zeros,"(",zeros/entries*100,"%). Less than 1: ", lessThan1, "(",lessThan1/entries*100,"%). More than 1:",moreThan1,"(",moreThan1/entries*100,"%). Empty Locations: ",emptyLocs,"(",emptyLocs/entries*100,"%)")})#
  Results
  }
}
  )

output$LocationTimeLineGrid<-renderPlot({
     
     if(input$showgrid){
     par(mar=c(1,1,1,1))     
     par(mfrow=c(3,3))
     for(g1 in -1:1){
          for(g2 in -1:1){
               #if(as.numeric(input$LocX)+g1<=0 || as.numeric(input$LocX)+g1>input$gridsize){next}
               #if(as.numeric(input$LocY)+g2<=0 || as.numeric(input$LocY)+g2>input$gridsize){next}
     #-----------------------------------------------------
     
     
     
     tss<-as.numeric(input$timeSliceSize);
     
     countReports<-c()
    countOpenPotholes<-c()
    countNewPotholes<-c()
    countClosedPotholes<-c()
    my_gridsize<-mygrid[as.numeric(input$gridsize)]  
     totalTimeSlices<-(maxtimeslice/as.numeric(input$timeSliceSize))
     i<-as.numeric(input$LocX)+g1
     j<-as.numeric(input$LocY)+g2
     stepsizex<-(xmax-xmin)/my_gridsize
     stepsizey<-(ymax-ymin)/my_gridsize
   
     boundsx<-c(xmin+(stepsizex)*(i-1),xmin+(stepsizex)*i)
     boundsy<-c(ymin+(stepsizey)*(j-1),ymin+(stepsizey)*j)
  
   
     for(ct in 1:totalTimeSlices){
     
     lb<-(tss*(ct-1)+1) 
     ub<-ct*tss
    wztime<-which(wzTime>=lb & wzTime<=ub)
    validIndex<-which(ofInitialTime<ub & ofFinalTime>lb)
    newPotholeIndex<-which(ofInitialTime>= lb & ofInitialTime<=ub)
    closedPotholeIndex<-which(ofFinalTime>=lb & ofFinalTime<=ub)
    
    if(input$fixReports){wztime<-fixMyReports(wztime,validIndex)}
       if(input$fixPotholes){A<-fixAddPotholes(wztime,validIndex);validIndex<-A$v; ofPointX<-c(ofPointX,A$newCoords[,1]); ofPointY<-c(ofPointY,A$newCoords[,2])}
                else{ofPointX<-ofPointX[1:n_official_points];ofPointY<-ofPointY[1:n_official_points]}
       
          inrow<-which(wzdata$locx[wztime]>=boundsx[1] & wzdata$locx[wztime]<boundsx[2])
          countReports<-c(countReports,length(which(wzdata$locy[wztime[inrow]]>=boundsy[1] & wzdata$locy[wztime[inrow]]<boundsy[2])))
          
          inrow<-which(ofdata$LONGITUDE[validIndex]>=boundsx[1] & ofdata$LONGITUDE[validIndex]<boundsx[2])
          countOpenPotholes<-c(countOpenPotholes,length(which(ofdata$LATITUDE[validIndex[inrow]]>=boundsy[1] & ofdata$LATITUDE[validIndex[inrow]]<boundsy[2])))
          
          inrow<-which(ofdata$LONGITUDE[newPotholeIndex]>=boundsx[1] & ofdata$LONGITUDE[newPotholeIndex]<boundsx[2])
          countNewPotholes<-c(countNewPotholes,length(which(ofdata$LATITUDE[newPotholeIndex[inrow]]>=boundsy[1] & ofdata$LATITUDE[newPotholeIndex[inrow]]<boundsy[2])))

          inrow<-which(ofdata$LONGITUDE[closedPotholeIndex]>=boundsx[1] & ofdata$LONGITUDE[closedPotholeIndex]<boundsx[2])
          countClosedPotholes<-c(countClosedPotholes,length(which(ofdata$LATITUDE[closedPotholeIndex[inrow]]>=boundsy[1] & ofdata$LATITUDE[closedPotholeIndex[inrow]]<boundsy[2])))          
     
     }      
    
    choiceConverter<-c("Open potholes","Reports","New potholes","Closed potholes")
    conversion<-c()
    for(i in 1:length(input$plotvar)){conversion<-c(conversion,which(choiceConverter==input$plotvar[i]))}
    
    
     maintitle<-paste("Time series by location (",as.numeric(input$LocX)+g1,",",as.numeric(input$LocY)+g2,")")
     
     
     topgraphlimit<-max(countReports,countOpenPotholes)
     if("Reports" %in% input$plotvar){plot(1:totalTimeSlices,countReports,"b",col="red",main=maintitle,ylim=c(-1,topgraphlimit+1),xlab="Time",ylab="Count",pch=mypch[2])}
     par(new=T) 
     if("Open potholes" %in% input$plotvar){plot(1:totalTimeSlices,countOpenPotholes,"b",col="blue",main=maintitle,ylim=c(-1,topgraphlimit+1),xlab="Time",ylab="Count",pch=mypch[1])
     par(new=T)}
     if("New potholes" %in% input$plotvar){plot(1:totalTimeSlices,countNewPotholes,"b",col="green",main=maintitle,ylim=c(-1,topgraphlimit+1),xlab="Time",ylab="Count",pch=mypch[3])
     par(new=T)}
     if("Closed potholes" %in% input$plotvar){plot(1:totalTimeSlices,countClosedPotholes,"b",col="purple",main=maintitle,ylim=c(-1,topgraphlimit+1),xlab="Time",ylab="Count",pch=mypch[4])
     par(new=T)}
     axis(1, at=seq(0,totalTimeSlices,5))
     
     
   #-----------------------------------------------------
}}#closes for g1, g2  
}#closes if showgrid
     
          
     
     })



output$LocationTimeLine<-renderPlot({
     
     tss<-as.numeric(input$timeSliceSize);
     
     countReports<-c()
    countOpenPotholes<-c()
    countNewPotholes<-c()
    countClosedPotholes<-c()
    my_gridsize<-mygrid[as.numeric(input$gridsize)]  
     totalTimeSlices<-(maxtimeslice/as.numeric(input$timeSliceSize))
     i<-as.numeric(input$LocX)
     j<-as.numeric(input$LocY)
     stepsizex<-(xmax-xmin)/my_gridsize
     stepsizey<-(ymax-ymin)/my_gridsize
   
     boundsx<-c(xmin+(stepsizex)*(i-1),xmin+(stepsizex)*i)
     boundsy<-c(ymin+(stepsizey)*(j-1),ymin+(stepsizey)*j)
  
   
     for(ct in 1:totalTimeSlices){
     
     lb<-(tss*(ct-1)+1) 
     ub<-ct*tss
    wztime<-which(wzTime>=lb & wzTime<=ub)
    validIndex<-which(ofInitialTime<ub & ofFinalTime>lb)
    newPotholeIndex<-which(ofInitialTime>= lb & ofInitialTime<=ub)
    closedPotholeIndex<-which(ofFinalTime>=lb & ofFinalTime<=ub)
    
    if(input$fixReports){wztime<-fixMyReports(wztime,validIndex)}
       if(input$fixPotholes){A<-fixAddPotholes(wztime,validIndex);validIndex<-A$v; ofPointX<-c(ofPointX,A$newCoords[,1]); ofPointY<-c(ofPointY,A$newCoords[,2])}
                else{ofPointX<-ofPointX[1:n_official_points];ofPointY<-ofPointY[1:n_official_points]}
       
    
          inrow<-which(wzdata$locx[wztime]>=boundsx[1] & wzdata$locx[wztime]<boundsx[2])
          countReports<-c(countReports,length(which(wzdata$locy[wztime[inrow]]>=boundsy[1] & wzdata$locy[wztime[inrow]]<boundsy[2])))
          
          inrow<-which(ofdata$LONGITUDE[validIndex]>=boundsx[1] & ofdata$LONGITUDE[validIndex]<boundsx[2])
          countOpenPotholes<-c(countOpenPotholes,length(which(ofdata$LATITUDE[validIndex[inrow]]>=boundsy[1] & ofdata$LATITUDE[validIndex[inrow]]<boundsy[2])))
          
          inrow<-which(ofdata$LONGITUDE[newPotholeIndex]>=boundsx[1] & ofdata$LONGITUDE[newPotholeIndex]<boundsx[2])
          countNewPotholes<-c(countNewPotholes,length(which(ofdata$LATITUDE[newPotholeIndex[inrow]]>=boundsy[1] & ofdata$LATITUDE[newPotholeIndex[inrow]]<boundsy[2])))

          inrow<-which(ofdata$LONGITUDE[closedPotholeIndex]>=boundsx[1] & ofdata$LONGITUDE[closedPotholeIndex]<boundsx[2])
          countClosedPotholes<-c(countClosedPotholes,length(which(ofdata$LATITUDE[closedPotholeIndex[inrow]]>=boundsy[1] & ofdata$LATITUDE[closedPotholeIndex[inrow]]<boundsy[2])))          
     
     }      
    
    choiceConverter<-c("Open potholes","Reports","New potholes","Closed potholes")
    conversion<-c()
    for(i in 1:length(input$plotvar)){conversion<-c(conversion,which(choiceConverter==input$plotvar[i]))}
    
    
     maintitle<-paste("Time series by location ((",boundsx[1],",",boundsy[1],") to (",boundsx[2],",",boundsy[2],"))")
     
     
     topgraphlimit<-max(countReports,countOpenPotholes)
     if("Reports" %in% input$plotvar){plot(1:totalTimeSlices,countReports,"b",col="red",main=maintitle,ylim=c(-1,topgraphlimit+1),xlab="Time",ylab="Count",pch=mypch[2],ps=0.2)}
     par(new=T) 
     if("Open potholes" %in% input$plotvar){plot(1:totalTimeSlices,countOpenPotholes,"b",col="blue",main=maintitle,ylim=c(-1,topgraphlimit+1),xlab="Time",ylab="Count",pch=mypch[1])
     par(new=T)}
     if("New potholes" %in% input$plotvar){plot(1:totalTimeSlices,countNewPotholes,"b",col="green",main=maintitle,ylim=c(-1,topgraphlimit+1),xlab="Time",ylab="Count",pch=mypch[3])
     par(new=T)}
     if("Closed potholes" %in% input$plotvar){plot(1:totalTimeSlices,countClosedPotholes,"b",col="purple",main=maintitle,ylim=c(-1,topgraphlimit+1),xlab="Time",ylab="Count",pch=mypch[4])
     par(new=T)}
     axis(1, at=seq(0,totalTimeSlices,5))
     legend("right",c("Open potholes","Reports","New potholes","Closed potholes"),col=mycols,pch=mypch)
     
     output$locationSummary <- renderUI({
       str1 <- paste("Total open potholes: ", sum(countOpenPotholes))
       str2 <- paste("Total reports: ",sum(countReports))
       str3<-paste("Total new potholes",sum(countNewPotholes))
       str4<-paste("Total closed potholes",sum(countClosedPotholes))
       str5<-paste("This location's ratio (reports/potholes):", sum(countReports)/(sum(c(countOpenPotholes,countNewPotholes,countClosedPotholes))))
       HTML(paste(str1, str2, str3,str4,str5,sep = '<br/>'))
       
     })
     
     
     
     })








output$barPlot<-renderPlot({
     ct<-input$CurrentTime;
     tss<-as.numeric(input$timeSliceSize);
     lb<-(tss*(ct-1)+1) 
     ub<-ct*tss
    wztime<-which(wzTime>=lb & wzTime<=ub)
    validIndex<-which(ofInitialTime<ub & ofFinalTime>lb)
    newPotholeIndex<-which(ofInitialTime>= lb & ofInitialTime<=ub)
    closedPotholeIndex<-which(ofFinalTime>=lb & ofFinalTime<=ub)
    
    if(input$fixReports){wztime<-fixMyReports(wztime,validIndex)}
       if(input$fixPotholes){A<-fixAddPotholes(wztime,validIndex);validIndex<-A$v; ofPointX<-c(ofPointX,A$newCoords[,1]); ofPointY<-c(ofPointY,A$newCoords[,2])}
                else{ofPointX<-ofPointX[1:n_official_points];ofPointY<-ofPointY[1:n_official_points]}
       
    my_gridsize<-mygrid[as.numeric(input$gridsize)]
    countReports<-matrix(0,my_gridsize,my_gridsize)
    countOpenPotholes<-matrix(0,my_gridsize,my_gridsize)
    countNewPotholes<-matrix(0,my_gridsize,my_gridsize)
    countClosedPotholes<-matrix(0,my_gridsize,my_gridsize)
    
    stepsizex<-(xmax-xmin)/my_gridsize
    stepsizey<-(ymax-ymin)/my_gridsize
    
    for(i in 1:my_gridsize){
     for(j in 1:my_gridsize){
          boundsx<-c(xmin+(stepsizex)*(i-1),xmin+(stepsizex)*i)
          boundsy<-c(ymin+(stepsizey)*(j-1),ymin+(stepsizey)*j)
          
          inrow<-which(wzdata$locx[wztime]>=boundsx[1] & wzdata$locx[wztime]<boundsx[2])
          countReports[i,j]<-length(which(wzdata$locy[wztime[inrow]]>=boundsy[1] & wzdata$locy[wztime[inrow]]<boundsy[2]))
          
          inrow<-which(ofdata$LONGITUDE[validIndex]>=boundsx[1] & ofdata$LONGITUDE[validIndex]<boundsx[2])
          countOpenPotholes[i,j]<-length(which(ofdata$LATITUDE[validIndex[inrow]]>=boundsy[1] & ofdata$LATITUDE[validIndex[inrow]]<boundsy[2]))
          
          inrow<-which(ofdata$LONGITUDE[newPotholeIndex]>=boundsx[1] & ofdata$LONGITUDE[newPotholeIndex]<boundsx[2])
          countNewPotholes[i,j]<-length(which(ofdata$LATITUDE[newPotholeIndex[inrow]]>=boundsy[1] & ofdata$LATITUDE[newPotholeIndex[inrow]]<boundsy[2]))

          inrow<-which(ofdata$LONGITUDE[closedPotholeIndex]>=boundsx[1] & ofdata$LONGITUDE[closedPotholeIndex]<boundsx[2])
          countClosedPotholes[i,j]<-length(which(ofdata$LATITUDE[closedPotholeIndex[inrow]]>=boundsy[1] & ofdata$LATITUDE[closedPotholeIndex[inrow]]<boundsy[2]))          
          
     }
    }
    choiceConverter<-c("Open potholes","Reports","New potholes","Closed potholes")
    conversion<-c()
    for(i in 1:length(input$plotvar)){conversion<-c(conversion,which(choiceConverter==input$plotvar[i]))}
    
    
    
    cr<-c();cop<-c();cnp<-c();ccp<-c()
    for(i in input$LocChooser[1]:(input$LocChooser[2]-1)){
         cr<-c(cr,  length(which(countReports==i)));
         cop<-c(cop,length(which(countOpenPotholes==i)));
         cnp<-c(cnp,length(which(countNewPotholes==i)));
         ccp<-c(ccp,length(which(countClosedPotholes==i)))
    }
    i<-input$LocChooser[2]
    cr<-c(cr,  length(which(countReports>=i)));
    cop<-c(cop,length(which(countOpenPotholes>=i)));
    cnp<-c(cnp,length(which(countNewPotholes>=i)));
    ccp<-c(ccp,length(which(countClosedPotholes>=i)))
    
    
    K<-rbind(cop,cr,cnp,ccp) 
    xaxisnames<-c(as.character(input$LocChooser[1]:(input$LocChooser[2]-1)),paste(">=",as.character(input$LocChooser[2])))
    barplot(K[conversion,],beside=TRUE,col=mycols[conversion],names.arg=xaxisnames,main="Amount of locations with x number of Potholes or Reports")
    #----------------------------------------------------------------------#    
    
    
    
    #----------------------------------------------------------------------#    
    
    output$explainBar <- renderText({paste("Example on how to read the bar graph: There were",K[conversion[1]],"squares (y-axis) with ",input$LocChooser[1],choiceConverter[conversion[1]])})
     })

     output$gridWarning<-renderText({"(Grid in map is approximate)"})
     
     output$gridAnalysis<-renderPlot({
          data<-matrix(c(26,8,43,5,18,26,8,39,9,18,26,8,32,15,18,21.33,10.22,32,4,32.44,21.33,10.22,27.55,8.44,32.44,21.33,10.22,25.33,10.66,32.44,22.75,10.75,23.75,3.5,38.75,22.75,10.75,22,5.75,38.75,22.75,10.75,19.75,7.25,38.75,21.76,10.4,17.12,3.68,46.88,21.76,10.4,15.52,5.12,46.88,21.76,10.4,13.44,6.72,46.88,22.55,10.11,12.77,2.33,52.1,22.55,10.11,11.44,3.44,52.1,22.55,10.11,10.22,4.22,52.1,20.4,9.71,9.46,2.36,57.55,20.4,9.71,10.04,1.95,57.55,20.4,9.71,9.46,2.36,57.55,18.87,8.125,8.06,1.43,63.375,18.87,8.125,7.56,1.75,63.375,18.87,8.125,6.68,2.5,63.375),nrow=21,byrow=T)
          colnames(data)<-c("Infs","0s","<1",">=1","Empty")
          rownames(data)<-c("10x10,1hr","10x10,4hr","10x10,8hr","15x15,1hr","15x15,4hr","15x15,8hr","20x20,1hr","20x20,4hr","20x20,8hr","25x25,1hr","25x25,4hr","25x25,8hr","30x30,1hr","30x30,4hr","30x30,8hr","35x35,1hr","35x35,4hr","35x35,8hr","40x40,1hr","40x40,4hr","40x40,8hr")
          cg<-as.numeric(input$compareGrids)
          cc<-c()
          for(j in 1:length(input$compareCells)){
               cc<-c(cc,which(colnames(data)==input$compareCells[j]))
          }
          par(mfrow=c(length(cg),1),mar=c(1,1,1,1),xpd=TRUE)
          mycolors<-c("blue","red","darkgreen","yellow","purple")
          for(i in 1:length(cg)){
               x<-cg[i]/5+(cg[i]/5-3)*2+1
               barplot(t(data[x:(x+2),cc]/rowSums(data[x:(x+2),cc])),col=mycolors[cc],xlab="1hr  4hrs  8hrs",beside=input$singleBar)
          }
          legend("topright",  legend=colnames(data),fill=mycolors)
          })


})




