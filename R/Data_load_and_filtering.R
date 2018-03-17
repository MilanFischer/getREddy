Load<-function(file,variable)
{
	# Load input data
	Data<-read.table(unzip(file)[1],skip=8)
	file.remove(unzip(file))

	# Timestamp
	timestamp=as.character(Timestamp(Data$V5,Data$V6))

	# Air humidity (mmol/mol)
	Q<-Data$V17

	# CO2 concentration (µmol/mol)
	C<-Data$V16

	#Air temperature (°C)
	ts<-Data$V15

	#Vertical velcoity (m/s)
	u<-Data$V12
	v<-Data$V13
	w<-Data$V14

	if(variable=='ts')
	{
	plot_spectra(Data,ts,variable,timestamp)
	}else if(variable=='Q'){
	plot_spectra(Data,Q,variable,timestamp)
	}else if(variable=='C'){
	plot_spectra(Data,C,variable,timestamp)
	}else if(variable=='w'){
	plot_spectra(Data,w_rot,variable,timestamp)
	}

	if(variable=='ts')
	{
	plot_cospectra(Data,u,v,w,ts,variable,timestamp)
	}else if(variable=='Q'){
	plot_spectra(Data,u,v,w,Q,variable,timestamp)
	}else if(variable=='C'){
	plot_spectra(Data,u,v,w,C,variable,timestamp)
	}


out<-list(timestamp=as.character(timestamp))
}