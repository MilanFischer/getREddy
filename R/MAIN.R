##############################################################################################
#                                                                                            #
#      -------------------------------------------------------                               #
#       Calculation of fluxes using surface renewal analysis                                 #
#      -------------------------------------------------------                               #
#                                                                                            #
#       Author: Milan Fischer                                                                #
#                                                                                            #
##############################################################################################

	rm(list=ls())
	library(stringr)
	library(zoo)

	#Functions to be used
	source('2D_rotation.r')
	source('Data_load_and_filtering.r')
	source('Despike.r')
	source('Ensemble_averages.r')
	source('FFT.r')
	source('Linear_detrend.r')
	source('Plot_spectrum.r')
	source('Plot_cospectrum.r')
	source('Spectral_density.r')
	source('Cospectral_density.r')
	source('Taper.r')
	source('Timestamp.r')
	source('Time_management_and_gap-filling.r')


	# Move one directory up
	setwd('..')
	# Move to directory with raw data
	setwd(paste(getwd(),'/above_canopy',sep=''))

	#Create a list of raw data files
	file<-list.files(path=getwd(),pattern='\\.ghg$')

	#Air density (kg m-3)
	rhoAir=1.225

	#Specific heat of air (J K-1kg-1),
	CpAir=1004.67

	#Frequency (Hz)
	f=20

	#the sampling period (s)
	dSampling=1/f

	#Time step (minutes)
	Time_step=30

	# Number of standard deviation used for despikin
	plausibility_threshold=4
	number_of_windows=6
	
	# Define the variable to analyse (ts,Q,C,w)
	variable='ts'

	pdf(paste('(co)spectrum_',variable,'.pdf',sep=''))
	for(i in 1:length(file))
	{
	Load(file[i],variable)
	}
	dev.off()
	file.remove(unzip(file[i]))



