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

	#Vertical velcoity (m/s)
	u<-Data$"Aux 1 - U (m/s)" 
	v<-Data$"Aux 2 - V (m/s)"
	w<-Data$"Aux 3 - W (m/s)"


	###############################
	### Despike the time series ###
	###############################

	u_de=Despike(Data,u,gapfill=TRUE)$Y
	v_de=Despike(Data,v,gapfill=TRUE)$Y
	w_de=Despike(Data,w,gapfill=TRUE)$Y

	if(variable=='ts'){
	out_despike=Despike(Data,ts,gapfill=TRUE)
	Tde=out_despike$Y
	No_of_spikes_ts=out_despike$No_of_spikes
	}else if(variable=='Q'){
	out_despike=Despike(Data,Q,gapfill=TRUE)
	Qde=out_despike$Y
	No_of_spikes_Q=out_despike$No_of_spikes
	}else if(variable=='C'){
	out_despike=Despike(Data,C,gapfill=TRUE)
	Cde=out_despike$Y
	No_of_spikes_C=out_despike$No_of_spikes
	}

	#############################
	### Rotate the anemometer ###
	#############################

	Anemometer_rotation=rotation2D(u_de,v_de,w_de)
	u_de_rot=Anemometer_rotation$uRot
	v_de_rot=Anemometer_rotation$vRot
	w_de_rot=Anemometer_rotation$wRot

	# Define the variable to be analyzed
	if(variable=='u'){
	Vde=u_de_rot
	}else if(variable=='w'){
	Vde=w_de_rot
	}else if(variable=='ts'){
	V=ts; Vde=Tde
	}else if(variable=='Q'){
	V=Q; Vde=Qde
	}else if(variable=='C'){
	V=C; Vde=Cde
	}

	########################
	### Plot time-series ###
	########################

	if(variable %in% c('u','w'))
	{
	png(paste(Main_WD,'/Outputs/',variable,'/',format(as.POSIXct(timestamp),'%Y-%m-%d_%H%M'),'_',variable,'_a) time-series.png',sep=''),type='cairo',width=4*480,height=480,units='px')
	plot(Vde,type='l',ylab=variable,xlab='n')
	dev.off()
	}else{
	png(paste(Main_WD,'/Outputs/',variable,'/',format(as.POSIXct(timestamp),'%Y-%m-%d_%H%M'),'_',variable,'_a) time-series.png',sep=''),type='cairo',width=4*480,height=480,units='px')
	plot(V,type='l',col='red',ylab=variable,xlab='n')
	lines(Vde)
	dev.off()
	}

	#####################
	### Trend removal ###
	#####################
	
	if(trend_removal=='block_average'){
	Vde_det=Vde-mean(Vde)
	}else if(trend_removal=='linear_detrend'){
	Vde_det=Detrend(Vde)$Detrended}

	####################
	### Plot spectra ###
	####################

	plot_spectra(Data,Vde_det,variable,timestamp)
	
	######################
	### Plot cospectra ###
	######################

	if(variable %in% c('u','ts','Q','C'))
	{
	plot_cospectra(Data,w_de_rot,Vde_det,variable,timestamp)
	}

out<-list(timestamp=as.character(timestamp))
}
