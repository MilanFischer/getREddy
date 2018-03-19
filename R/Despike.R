# Despike.r :     removes spikes from a 1-D data vector and fill the gaps (including skipped scans) by the linear interpolation
#            
#        X        = 1-D data vector  
#                    
#        usage: Y = Despike(Data,X)

Despike<-function(Data,X,plausibility_threshold,gapfill){

# Run 2 is moved behind the run 1 by one half of the window from run 1 - as a result, run 2 has one window less as compared to run 1 
Despike_windows<-function(X,number_of_windows,run)
{

X_despiked=numeric()

for(i in 1:number_of_windows)
{
if(run==1)	
{
x=X[((i-1)*Time_step*60*f/number_of_windows+1):(i*Time_step*60*f/number_of_windows)]
}else{
	if(i==number_of_windows)
	{
	x=numeric()
	}else{
	x=X[(((i-1)*Time_step*60*f/number_of_windows+1)+Time_step*60*f/number_of_windows/2):((i*Time_step*60*f/number_of_windows)+Time_step*60*f/number_of_windows/2)]
	}
}


# Loop is ensuring that the test is repeated if needed (Foken 2008, p. 109) - it is important because first spikes can result in large standard deviation and as a consequence smaller spikes could remain
fulfilled='no'
while(fulfilled %in% 'no')
{
	xm=mean(x,na.rm=TRUE)
	stda=sd(x,na.rm=TRUE)

# The plausibility_threshold determine the number of standard deviations for data removal
	Fc1=-plausibility_threshold*stda+xm
	Fc2= plausibility_threshold*stda+xm

	# Values that are outside given limits or that are missing are assigned as NA
	x[x<Fc1|x>Fc2|x %in% NA]=NA
	
	M=mean(x,na.rm=TRUE)
	SD=sd(x,na.rm=TRUE)
	Low=-plausibility_threshold*SD+M
	Up = plausibility_threshold*SD+M
		if(any(x<Low|x>Up,na.rm=TRUE)==TRUE)
		{
			fulfilled='no'
			}else{
			fulfilled='yes'
		}

}
		X_despiked=c(X_despiked,x)
}
	return(X_despiked)
}

No_of_NA=length(which(X %in% NA))

# Windows starting from the beginning
Run1=Despike_windows(X,number_of_windows,run=1)
Index1=which(Run1 %in% NA)

# Windows starting from the half of the first window (to ensure that the window is moving by half)
Run2=Despike_windows(X,number_of_windows,run=2)
Index2=which(Run2 %in% NA)+(Time_step*60*f/number_of_windows/2)

X_despiked=X

Index=c(Index1,Index2)
Index=Index[!duplicated(Index)]
Index=Index[order(Index)]

# A few consecutive values with the same sign are not necessarily spikes but untypical physical phenomena
if(length(Index)>=number_of_consecutive_points)
{

	# Padding by zeroes
	Index=c(rep(0,number_of_consecutive_points-1),Index,rep(0,number_of_consecutive_points-1))
	Index_new=Index

	if(length(Index)>number_of_consecutive_points)
	{
	for(i in number_of_consecutive_points:(length(Index)-number_of_consecutive_points+1))
		{
		if(abs(mean(sign(X[Index[(i-floor(number_of_consecutive_points/2)):(i+floor(number_of_consecutive_points/2))]]-mean(X,na.rm=TRUE))))==1&
		max(diff(Index[(i-floor(number_of_consecutive_points/2)):(i+floor(number_of_consecutive_points/2))]))==1|
		abs(mean(sign(X[Index[(i-number_of_consecutive_points+1):i]]-mean(X,na.rm=TRUE))))==1&
		max(diff(Index[(i-number_of_consecutive_points+1):i]))==1|
		abs(mean(sign(X[Index[i:(i+number_of_consecutive_points-1)]]-mean(X,na.rm=TRUE))))==1&
		max(diff(Index[i:(i+number_of_consecutive_points-1)]))==1)
		{
		Index_new[i]=Index_new[i]
		}else{
		Index_new[i]=NA
		}
	}
	}

	# Back to original vector length
	Index=Index[number_of_consecutive_points:(length(Index)-number_of_consecutive_points+1)]
	Index_new=Index_new[number_of_consecutive_points:(length(Index_new)-number_of_consecutive_points+1)]
}else{
	Index_new=rep(NA,length(Index))
}


# Clean the Index vector
Index=Index[which(Index_new %in% NA)]

# Despike the scalar vector 
X_despiked[Index]=NA

	No_of_spikes=length(which(X_despiked %in% NA))-No_of_NA
	
	if(gapfill %in% TRUE)
	{
	out=Time_management(Data,X_despiked)
	Time=out$Time
	Y=out$Y_interpol
	}else{
	Time=NA
	Y=X_despiked
	}

	out<-list(Time=Time,Y=Y,No_of_spikes=No_of_spikes)
}
