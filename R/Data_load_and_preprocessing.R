Load<-function(file,variable)
{
	# Load input data
	Unzipped_file<-unzip(file)[1]
	L=integer(); n=1
	while(length(L)<1)
	{
	L=which(grepl('DATAH',readLines(Unzipped_file,n)))
	n=n+1
	}	

	Data<-read.table(unzip(file)[1],skip=L)
	Info<-readLines(Unzipped_file,n=L-1)
	Header<-readLines(Unzipped_file,n=L)[L]
	file.remove(unzip(file))
	Names<-unlist(strsplit(Header,"\t"))
	names(Data)<-Names

	if(length(grep('LI-7500A',Info))==1){Analyser='LI-7500A'}else
	if(length(grep('LI-7500',Info))==1){Analyser='LI-7500'}else
	if(length(grep('LI-7200',Info))==1){Analyser='LI-7200'}
	
	# Timestamp
	timestamp=as.character(Timestamp(Data$"Date",Data$"Time"))

	# Air humidity (mmol/mol)
	if(Analyser=='LI-7500'|Analyser=='LI-7500A')
	{
	Q<-Data$"H2O (mmol/mol)"
	}else{
	Q<-Data$"H2O dry(mmol/mol)"
	}

	# CO2 concentration (µmol/mol)
	if(Analyser=='LI-7500'|Analyser=='LI-7500A')
	{
	C<-Data$"CO2 (umol/mol)"
	}else{
	C<-Data$"CO2 dry(umol/mol)"
	}

	# Sonic temperature (°C)
	ts<-Data$"Aux 4 - Ts (C)" 

	# Vertical velocity (m/s)
	u<-Data$"Aux 1 - U (m/s)" 
	v<-Data$"Aux 2 - V (m/s)"
	w<-Data$"Aux 3 - W (m/s)"


	###############################
	### Despike the time series ###
	###############################

	u_de=Despike(Data,u,plausibility_threshold$u,gapfill=TRUE)$Y
	v_de=Despike(Data,v,plausibility_threshold$v,gapfill=TRUE)$Y
	w_de=Despike(Data,w,plausibility_threshold$w,gapfill=TRUE)$Y

	if(any(variable=='ts')){
	out_despike=Despike(Data,ts,plausibility_threshold$ts,gapfill=TRUE)
	Tde=out_despike$Y
	No_of_spikes_ts=out_despike$No_of_spikes}
	if(any(variable=='Q')){
	out_despike=Despike(Data,Q,plausibility_threshold$Q,gapfill=TRUE)
	Qde=out_despike$Y
	No_of_spikes_Q=out_despike$No_of_spikes}
	if(any(variable=='C')){
	out_despike=Despike(Data,C,plausibility_threshold$C,gapfill=TRUE)
	Cde=out_despike$Y
	No_of_spikes_C=out_despike$No_of_spikes}


	#############################
	### Rotate the anemometer ###
	#############################

	Anemometer_rotation=rotation2D(u_de,v_de,w_de)
	u_de_rot=Anemometer_rotation$uRot
	v_de_rot=Anemometer_rotation$vRot
	w_de_rot=Anemometer_rotation$wRot

	# Define the variable(s) to be analyzed

	Vde=as.data.frame(matrix(NA,nrow=length(w_de),ncol=length(variable)))
	V=as.data.frame(matrix(NA,nrow=length(w_de),,ncol=length(which(variable %in% c('ts','Q','C')))))

	if(any(variable=='u')){Vde[,min(which(is.na(Vde[1,])))]=u_de_rot; names(Vde)[max(which(!is.na(Vde[1,])))]='u'}
	if(any(variable=='w')){Vde[,min(which(is.na(Vde[1,])))]=w_de_rot; names(Vde)[max(which(!is.na(Vde[1,])))]='w'}
	if(any(variable=='ts')){Vde[,min(which(is.na(Vde[1,])))]=Tde; names(Vde)[max(which(!is.na(Vde[1,])))]='ts'}
	if(any(variable=='Q')){Vde[,min(which(is.na(Vde[1,])))]=Qde; names(Vde)[max(which(!is.na(Vde[1,])))]='Q'}
	if(any(variable=='C')){Vde[,min(which(is.na(Vde[1,])))]=Cde; names(Vde)[max(which(!is.na(Vde[1,])))]='C'}

	if(any(variable=='ts')){V[,min(which(is.na(V[1,])))]=ts; names(V)[max(which(!is.na(V[1,])))]='ts'}
	if(any(variable=='Q')){V[,min(which(is.na(V[1,])))]=Q; names(V)[max(which(!is.na(V[1,])))]='Q'}
	if(any(variable=='C')){V[,min(which(is.na(V[1,])))]=C; names(V)[max(which(!is.na(V[1,])))]='C'}


	########################
	### Plot time-series ###
	########################

	if(any(variable=='u')){
	png(paste(Main_WD,'/Outputs/u/',format(as.POSIXct(timestamp),'%Y-%m-%d_%H%M'),'_','u','_a) time-series.png',sep=''),type='cairo',width=4*480,height=480,units='px')
	plot(Vde$u,type='l',ylab='u',xlab='n')
	dev.off()}

	if(any(variable=='w')){
	png(paste(Main_WD,'/Outputs/w/',format(as.POSIXct(timestamp),'%Y-%m-%d_%H%M'),'_','w','_a) time-series.png',sep=''),type='cairo',width=4*480,height=480,units='px')
	plot(Vde$w,type='l',ylab='w',xlab='n')
	dev.off()}

	if(any(variable=='ts')){
	png(paste(Main_WD,'/Outputs/ts/',format(as.POSIXct(timestamp),'%Y-%m-%d_%H%M'),'_','ts','_a) time-series.png',sep=''),type='cairo',width=4*480,height=480,units='px')
	plot(V$ts,type='l',col='red',ylab='ts',xlab='n')
	lines(Vde$ts)
	dev.off()}

	if(any(variable=='Q')){
	png(paste(Main_WD,'/Outputs/Q/',format(as.POSIXct(timestamp),'%Y-%m-%d_%H%M'),'_','Q','_a) time-series.png',sep=''),type='cairo',width=4*480,height=480,units='px')
	plot(V$Q,type='l',col='red',ylab='Q',xlab='n')
	lines(Vde$Q)
	dev.off()}

	if(any(variable=='C')){
	png(paste(Main_WD,'/Outputs/C/',format(as.POSIXct(timestamp),'%Y-%m-%d_%H%M'),'_','C','_a) time-series.png',sep=''),type='cairo',width=4*480,height=480,units='px')
	plot(V$C,type='l',col='red',ylab='C',xlab='n')
	lines(Vde$C)
	dev.off()}


	#####################
	### Trend removal ###
	#####################
	
	Vde_det=Vde

	for(n in 1:length(Vde))
	{
	if(trend_removal=='block_average'){
	Vde_det[,n]=Vde[,n]-mean(Vde[,n])
	}else if(trend_removal=='linear_detrend'){
	Vde_det[,n]=Detrend(Vde[,n])$Detrended}
	}

	####################
	### Plot spectra ###
	####################

	for(n in 1:length(Vde_det))
	{
	plot_spectra(Data,Vde_det[,n],names(Vde_det)[n],timestamp)
	}
	
	######################
	### Plot cospectra ###
	######################

	for(n in 1:length(Vde_det))
	{
	if(!names(Vde_det)[n]=='w')
	plot_cospectra(Data,w_de_rot,Vde_det[,n],names(Vde_det)[n],timestamp)
	}


out<-list(timestamp=as.character(timestamp))
}
