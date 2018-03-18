###########################################################################
#                                                                         #
#      --------------------------------------------------------------     #
#       getREddy - initial exploring of the raw eddy covariance data      #
#      --------------------------------------------------------------     #
#                                                                         #
#       Author: Milan Fischer                                             #
#       email: fischer.milan@gmail.com                                    #
#                                                                         #
###########################################################################

	rm(list=ls())
	library(stringr)
	library(zoo)

	#Functions to be used
	source('2D_rotation.r')
	source('Data_load_and_preprocessing.r')
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


	# Save the path to the directory with the R codes
	R_codes_WD<-getwd()

	# Move one directory up
	setwd('..')
	Main_WD<-getwd()

	# Create the ouput directory
	dir.create(paste(Main_WD,'/Outputs',sep=''),showWarnings=FALSE)

	# Move to directory with raw data
	setwd(paste(getwd(),'/Inputs',sep=''))

	# Create a list of raw data files
	file<-list.files(path=getwd(),pattern='\\.ghg$')

	# Air density (kg m-3)
	rhoAir=1.225

	# Specific heat of air (J K-1kg-1),
	CpAir=1004.67

	# Frequency (Hz)
	f=20

	# The sampling period (s)
	dSampling=1/f

	# Time step (minutes)
	Time_step=30

	# Number of standard deviation used for despiking
	plausibility_threshold=4
	number_of_windows=6

	# Select the trend removal approach (either 'block_average' or 'linear_detrend')
	trend_removal='linear_detrend'
	
	# Define the variable to analyse (ts,Q,C,u,v,w)
	variable='ts'

	# Cretae output folder
	dir.create(paste(Main_WD,'/Outputs/',variable,sep=''),showWarnings=FALSE)
	
	for(i in 1:length(file))
	{
	graphics.off()
	print(Load(file[i],variable)$timestamp)
	flush.console()
	}
	graphics.off()
	file.remove(unzip(file[i]))
	setwd(R_codes_WD)