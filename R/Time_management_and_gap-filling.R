# This script create continuos time series without any gaps and skipped scans

Time_management<-function(data,X){

# Load the original timestamp
	
	T<-as.POSIXlt(as.character(paste(data$V5,data$V6,sep=" ")),origin = "1970-01-01 00:00:00.1",,tz="UTC")
	Date<-format(T,'%Y-%m-%d')
	Hour<-format(T,'%H')
	Minute<-format(T,'%M')
	Seconds<-as.numeric(str_sub(gsub(":", ".", as.character(paste(data$V5,data$V6,sep=" "))), start= -6))
	Seconds<-round(as.numeric(Seconds)/dSampling)*dSampling
	Date<-paste(Date,' ',Hour,':',Minute,':',Seconds,sep='')
	options(digits.secs=3)
	date<-as.POSIXlt(strftime(Date,'%Y-%m-%d %H:%M:%OS2'),tz="UTC")

# Create continuous timestamp
	length<-1/dSampling*60*Time_step

	hour=str_sub(paste(0,as.numeric(format(date[1],"%H")),sep=""),-2,-1)

	minute=as.numeric(format(date[1],"%M"))

	if(minute<Time_step){
		M=0
		}else{
		M=Time_step
	}

	Ini=paste(as.POSIXlt(as.character(Date[1]),origin='1970-01-01 00:00:00.0',tz='UTC',format='%Y-%m-%d'),
	' ',hour,':',M,':','00.0',sep='')

	Time<-rep(seq((as.POSIXlt(as.character(Ini),tz="UTC")),length.out=Time_step*60,by=1),f)

	Time<-Time[order(Time)]

	Time<-as.POSIXlt(Time,tz="UTC")

	Date<-format(Time,"%Y-%m-%d")
	Hour<-as.numeric(format(Time,"%H"))
	Minute<-as.numeric(format(Time,"%M"))
	Seconds<-as.numeric(format(Time,"%S"))
	seconds<-rep(seq(0,60-dSampling,by=dSampling),Time_step)
	Date<-paste(Date,' ',Hour,':',Minute,':',seconds,sep='')

	Time<-as.POSIXlt(strftime(Date,'%Y-%m-%d %H:%M:%OS2'),tz="UTC")

# Merge the original data with the new continuos NA series
	Original<-data.frame(as.character(date),X)
	names(Original)[1:2]<-c('Time','X')
	Data<-data.frame(as.character(Time),NA,check.names=FALSE)
	names(Data)[1:2]<-c('Time','X')
	merged<-rbind(Original,Data)

# Remove the duplicates
	filtered<-subset(merged,!duplicated(merged[,1]))

# Order the data frame accroding to continuous timestamp
	Final_order=filtered[order(as.POSIXlt(filtered[,1])),]

# First and last non-NA index
	First=min(which(!is.na(X)))
	Last=max(which(!is.na(X)))

# Use first and last non-NA values from the original time series as the first and last values of the final series
	Y=Final_order$X
	Y[1]=X[First]
	Y[length(Y)]=X[Last]

# Misssing data are linearly interpolated
	Y_interpol<-na.approx(Y)

out<-list(Time=Time,Y_interpol=Y_interpol)
}