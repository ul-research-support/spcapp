## api.func uses api key entered to pull the full data set (it also converts the date to a date)

## Users in user groups who use their API key will only pull data relevant to their usergroup

api.func<-function(x){

  rcon<-redcapConnection(url="https://informatics.ctrsu.org/redcap_v6.15.11/API/",token=x)

  labels<-exportMetaData(rcon)$field_name

  dat<-data.frame(exportRecords(rcon,factors=T,dag=F,fields=labels))

  return(dat)

}



## hosp.func returns a list of unique hospital names from the REDCap dataset

## This is important if the user is affiliated with multiple hospitals

hosp.func<-function(x){

  hosp<-grep("hosp_id",colnames(x))

  x<-x[,hosp]

  hosp.factor<-unname(unlist(apply(x,2,unique)))

  hosp.factor<-hosp.factor[!is.na(hosp.factor)]

  return(hosp.factor)

}



## item.func requires the value of hosp.func (as y) and returns a list of items that have observations

## in our REDCap database we have each user group set up with certain forms depending on their hospitals'

## items of interest, however data they pull will include data from *ALL* forms, so this is necessary

item.func<-function(y,x){

  hosp<-grep("hosp_id",colnames(x))

  x<-x[unname(unlist(apply(x[,hosp],2,function(x) grep(y,x)))),]

  x<-x[, colSums(is.na(x)) != nrow(x)]

  colnames(x)<-gsub("Number of ","",unlist(lapply(x,attr,which="label")))

  n.labels<-colnames(x)

  n.labels<-n.labels[grep("Cases|Compliant Observations",n.labels,fixed=F)]

  n.labels<-gsub(" Cases| Compliant Observations","",n.labels,fixed=F)

  return(n.labels)

}



## trim.func creates a new dataset based on hospital and item selected.

trim.func<-function(x,y,z){

  hosp<-grep("hosp_id",colnames(x))

  x<-x[unname(unlist(apply(x[,hosp],2,function(x) grep(y,x)))),]

  x<-x[, colSums(is.na(x)) != nrow(x)]

  colnames(x)<-gsub("Number of ","",unlist(lapply(x,attr,which="label")))

  names<-colnames(x)

  a<-grep(z,names,fixed=F)

  s<-x[,c(a,a+1)]

  t<-x[,"Date"]

  q<-data.frame(as.Date(t,format="%Y-%m-%d"),s)

  q<-subset(q,complete.cases(q)=="TRUE")

  colnames(q)<-c("moyr","num","denom")

  return(q)

}



## date.func uses the data set from trim.func to return end, start, and minimum dates as defaults

## default start and end are to be the most recent 36 months

date.func<-function(x){

  enddate<-as.Date(x[nrow(x),1])

  startdate<-as.Date(ifelse(nrow(x)<36,as.Date(x[1,1]),enddate-months(35)))

  mindate<-as.Date(x[1,1])

  return(list(enddate,startdate,mindate))

}



## date.trim.func trims down the data set to the selected dates

## start is first selected date, end is second selected date

date.trim.func<-function(start,end,x){

  s<-floor_date(start,unit="month")

  e<-floor_date(end,unit="month")

  st<-which(x[,1]==s)

  en<-which(x[,1]==e)

  data<-x[(st:en),]

  return(data)

}



## yaxe forms an axis title based on the selected item and standardization

## because of funny subsetting, data is pulled from the data in api.func

yaxe.func<-function(data,item,standardization){

  q<-grep(item,unlist(lapply(data,attr,which="label")),fixed=F)

  num<-gsub("Number of ","",attr(data[,q],which="label"))

  denom<-gsub("Number of ","",attr(data[,q+1],which="label"))

  title<-paste(num,"\n","per",as.integer(standardization),denom,sep=" ")

  return(list(title,num,denom))

}



## this makes the chart

spc.chart<-function(data, standardization, chart.type, y.axis.title, hline.location){

  

  ### Building data frame

  obj3 <- qcc(data$num, data$denom, ylab="", labels = data$moyr, type=chart.type, nsigmas=3, plot=FALSE)

  new.df<-data.frame(date=data[,1],stat=obj3$statistics*standardization,center=rep(obj3$center*standardization,nrow(data)))

  three.sigma<-data.frame(obj3$limits)*standardization

  new.df$UCL<-three.sigma$UCL

  new.df$LCL<-three.sigma$LCL

  obj2 <- qcc(data$num, data$denom, type=chart.type, nsigmas=2, plot=FALSE)

  two.sigma<-data.frame(obj2$limits)*standardization

  new.df$U2S<-two.sigma$UCL

  new.df$L2S<-two.sigma$LCL

  new.df$L2S<-ifelse(new.df$L2S<0,0,new.df$L2S)

  obj1 <- qcc(data$num, data$denom, type=chart.type, nsigmas=1, plot=FALSE)

  one.sigma<-data.frame(obj1$limits)*standardization

  new.df$U1S<-one.sigma$UCL

  new.df$L1S<-one.sigma$LCL

  new.df$L1S<-ifelse(new.df$L1S<0,0,new.df$L1S)

  

  ### runs of 8 or more

  new.df$diff<-new.df$stat-new.df$center

  new.df$diff[new.df$diff > 0] <- 1

  new.df$diff[new.df$diff < 0] <- -1

  q<-new.df$diff

  attr(q,"names")<-rownames(new.df)

  runs <- rle(q)

  run.df<-data.frame(runs=runs$lengths,date=(as.Date(names(runs$lengths),format="%Y-%m-%d",origin="1970-01-01")-months(1)))

  run.df[nrow(run.df),2]<-data[nrow(data),1]

  rownames(run.df)<-run.df$date

  data3<-merge(new.df,run.df,by="row.names",all.x=T)

  data3<-data3[order(data3$date.x,decreasing=T),]

  data3$runs<-na.locf(data3$runs)

  data3<-data3[order(data3$date.x),-c(1,ncol(data3))]

  

  ### finding above for rule 3 (2 above out of 3 2sig)

  data3$UP2<-data3$stat - data3$U2S

  data3$UP2[data3$UP2 > 0] <- 1

  data3$UP2[data3$UP2 < 0] <- 0

  data3$UP2R<-NA

  for(j in 1:(nrow(data3)-2)){

    data3$UP2R[j]<-data3$UP2[j]+data3$UP2[j+1]+data3$UP2[j+2]

  }

  data3$UP2R2<-0

  for(j in 1:(nrow(data3)-2)){

    if(data3$UP2R[j]==2){

      data3$UP2R2[j]<-1

      data3$UP2R2[j+1]<-1

      data3$UP2R2[j+2]<-1

    }

  }

  

  ### finding above for rule 4 (4 above out of 5 1sig)

  data3$UP1<-data3$stat - data3$U1S

  data3$UP1[data3$UP1 > 0] <- 1

  data3$UP1[data3$UP1 < 0] <- 0

  data3$UP1R<-NA

  for(j in 1:(nrow(data3)-4)){

    data3$UP1R[j]<-data3$UP1[j]+data3$UP1[j+1]+data3$UP1[j+2]+data3$UP1[j+3]+data3$UP1[j+4]

  }

  data3$UP1R2<-0

  for(j in 1:(nrow(data3)-4)){

    if(data3$UP1R[j]==4){

      data3$UP1R2[j]<-1

      data3$UP1R2[j+1]<-1

      data3$UP1R2[j+2]<-1

      data3$UP1R2[j+3]<-1

      data3$UP1R2[j+4]<-1

    }

  }

  

  ### finding below for rule 3 (2 below out of 3 2sig)

  data3$LW2<-data3$L2S - data3$stat

  data3$LW2[data3$LW2 > 0] <- 1

  data3$LW2[data3$LW2 < 0] <- 0

  data3$LW2R<-NA

  for(j in 1:(nrow(data3)-2)){

    data3$LW2R[j]<-data3$LW2[j]+data3$LW2[j+1]+data3$LW2[j+2]

  }

  data3$LW2R2<-0

  for(j in 1:(nrow(data3)-2)){

    if(data3$LW2R[j]==2){

      data3$LW2R2[j]<-1

      data3$LW2R2[j+1]<-1

      data3$LW2R2[j+2]<-1

    }

  }

  

  ### finding below for rule 4 (4 below out of 5 1sig)

  data3$LW1<- data3$L1S - data3$stat

  data3$LW1[data3$LW1 > 0] <- 1

  data3$LW1[data3$LW1 < 0] <- 0

  data3$LW1R<-NA

  for(j in 1:(nrow(data3)-4)){

    data3$LW1R[j]<-data3$LW1[j]+data3$LW1[j+1]+data3$LW1[j+2]+data3$LW1[j+3]+data3$LW1[j+4]

  }

  data3$LW1R2<-0

  for(j in 1:(nrow(data3)-4)){

    if(data3$LW1R[j]==4){

      data3$LW1R2[j]<-1

      data3$LW1R2[j+1]<-1

      data3$LW1R2[j+2]<-1

      data3$LW1R2[j+3]<-1

      data3$LW1R2[j+4]<-1

    }

  }

  

  

  ### Establishing rule 1: 1 above/below +3/-3 sigmas

  data3$rule<-ifelse(data3$stat > data3$UCL

                     | data3$stat < data3$LCL

                     | data3$runs >=8

                     | data3$UP2R2 == 1

                     | data3$UP1R2 == 1

                     | data3$LW2R2 == 1

                     | data3$LW1R2 == 1

                     ,1,0)

  

  range<-c(ifelse(min(data3$stat)<min(data3$LCL),

                  ifelse((min(data3$stat) - 0.05*min(data3$stat))<=0,

                         0,

                         (min(data3$stat) - 0.05*min(data3$stat))),

                  ifelse((min(data3$LCL) - 0.05*min(data3$LCL))<=0,

                         0,

                         (min(data3$LCL) - 0.05*min(data3$LCL)))),

           ifelse(max(data3$stat)>max(data3$UCL),

                  ifelse(max(data3$stat)<5,

                         (max(data3$stat) + 0.05*max(data3$stat)),

                         (max(data3$stat) + (2 - max(data3$stat) %% 2))),

                  ifelse(max(data3$UCL)<5,

                         (max(data3$UCL) + 0.05*max(data3$UCL)),

                         (max(data3$UCL) + (2 - max(data3$UCL) %% 2)))))

  

  par(cex.axis = 1.5, cex.lab=1.5, lab = c(5,10,7), mar=c(7,7,1,1),mgp=c(3,1,0),oma=c(0,0,0,0))

  plot(data3$stat, xlab = "", ylab=y.axis.title, cex.lab=1.5, xaxt="n", las=2,

       ylim=range,type="l",lwd=2,cex=2,col="black")

  points(data3$stat,pch=19,cex=2,col=ifelse(data3$rule == 1,"red","black"))

  lines(data3$LCL, lwd=2, lty=2, col="firebrick3")

  lines(data3$UCL, lwd=2, lty=2, col="firebrick3")

  lines(data3$L1S, lwd=2, lty=2, col="gold")

  lines(data3$L2S, lwd=2, lty=2, col="darkorange")

  lines(data3$U1S, lwd=2, lty=2, col="gold")

  lines(data3$U2S, lwd=2, lty=2, col="darkorange")

  if(hline.location > 0 & !is.na(hline.location)){

    abline(a=hline.location,b=0,lwd=2,col="purple")

  }

  abline(h=obj3$center*standardization, lwd=3, lty=2, col="limegreen")

  tck<-axis(side=1, at=1:nrow(data3), labels=F)

  text(tck, par("usr")[3], labels=as.yearmon(data$moyr,"%b-%Y"), srt=90,

       xpd=TRUE, adj=c(1.2,.5), cex=1.5)

}



## this makes a rolling 12-month table (or all data if there's less than 12 months)

rolling.table<-function(x,standardization,type.num,type.denom){

  x<-if(nrow(x)<12) x else(x[(nrow(x)-11):nrow(x),])

  x$moyr<-as.yearmon(x$moyr)

  x$num<-as.numeric(x$num)

  x$denom<-as.numeric(x$denom)

  c<-as.numeric(standardization)

  rate<-round(x$num/x$denom*c,1)

  x<-cbind(x, rate)

  datatable<-data.frame(t(x))

  r2<-" ";  r3<-" ";  r4<-" "

  if(grepl("Cases",type.num)){r2<-"Cases"} else

    if(grepl("Observations",type.num)){r2<-"Compliances"} else{r2<-" "}

  if(grepl("Patient",type.denom)){r3<-"Patient Days"} else

    if(grepl("Device",type.denom)){r3<-"Device Days"} else

      if(grepl("Observations",type.denom)){r3<-"Observations"} else {r3<-" "}

  ifelse(c==100,{r4<-"Percent"},{r4<-"Rate"})

  rownames(datatable)<-c("Month",r2,r3,r4)

  return(xtable(datatable))

}



## this makes the YTD table with comparisons - rates for each year

ytd.table<-function(x,standardization){

  x$moyr<-year(x$moyr)

  x$num<-as.numeric(x$num)

  x$denom<-as.numeric(x$denom)

  c<-as.numeric(standardization)

  num<-rev(tapply(x$num,x$moyr,function(x) sum(x)))

  denom<-rev(tapply(x$denom,x$moyr,function(x) sum(x)))

  rate<-num/denom*as.numeric(standardization)

  pt<-data.frame(num,denom)

  p<-NA

  for (i in 1:(nrow(pt)-1)){

    p[i]<-ormidp.test(pt[i,1],pt[i,2],pt[i+1,1],pt[i+1,2])[[2]]

  }

  p<-c(NA,p)  

  ytd.df<-data.frame(rate,p)

  colnames(ytd.df)<-c("Rate","p-value")

  return(xtable(ytd.df))

}





