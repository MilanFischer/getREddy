# Compute and plot cospectrum

plot_cospectra<-function(Data,u,v,w,ts,variable,timestamp)
{

#################################################################### 
### Despike the tempearture time series                            #
#################################################################### 

out_despike=Despike(Data,ts,gapfill=TRUE)
Tde=out_despike$Y

No_of_spikes_ts=out_despike$No_of_spikes
rm(out_despike)

out_despike=Despike(Data,u,gapfill=TRUE)
u_de=out_despike$Y

out_despike=Despike(Data,v,gapfill=TRUE)
v_de=out_despike$Y

out_despike=Despike(Data,w,gapfill=TRUE)
w_de=out_despike$Y

w_rot_de=rotation2D(u_de,v_de,w_de)$wRot


#################################################################### 
### Trend removal                                                  #
#################################################################### 

	Tde_dlm=Detrend(Tde)$Detrended

#################################################################### 
### Windowing                                                      #
#################################################################### 

	Tde_dlmW=Taper(Tde_dlm,100/12/2)$Tapered
	w_rot_deW=Taper(w_rot_de,100/12/2)$Tapered

####################################################################
### Fourier transformation                                         #
#################################################################### 

	out=FFT(Tde_dlmW)
	even_or_odd=out$even_or_odd
	f_Hz=out$f_Hz
	Nyquist=out$Nyquist
	Fts_T=fft(Tde_dlmW)/length(Tde_dlmW)
	rm(out)

	out=FFT(w_rot_deW)
	even_or_odd=out$even_or_odd
	f_Hz=out$f_Hz
	Nyquist=out$Nyquist
	Fts_w=fft(w_rot_deW)/length(w_rot_deW)
	rm(out)



####################################################################
### Cospectral density (Stull 1988, p. 312-331)                    #
#################################################################### 

	df_Hz=f_Hz[2]-f_Hz[1]	

	Far=Re(Fts_T)
	Fbr=Re(Fts_w)
	Fai=Im(Fts_T)
	Fbi=Im(Fts_w)

	Gab=Far*Fbr+1i*Far*Fbi-1i*Fai*Fbr-1i^2*Fai*Fbi

	# Cospectrum
	Co=Far*Fbr+Fai*Fbi

# Parseval's theorem (energy conservation-the cospectrum intergral is equal to scalars' covariance)

	sum(Co)
	cov(Tde_dlmW,w_rot_deW,use='pairwise.complete.obs')

	# Cospetcral density
	CD=Cospectral_density(even_or_odd,f_Hz,Nyquist,Co)
	sum(CD*df_Hz)

####################################################################
### Ensemble averages                                              #
#################################################################### 

	out=Ensemble(N=20,f_Hz,Nyquist,CD)
	F=out$F
	S=out$S
	rm(out)

################################
### Plotting                 ###
################################

	Intertial_range=(10^F)^(-7/3)		# Not multiplied by frequency
	#Intertial_range=(10^F)^(-4/3)	# Premultiplied by frequency

	#Log-log presentation of cospectra

	par(mfrow=c(1,1),oma=c(3.5,3.5,0,0)+1,mar=c(0,0,0,0),xpd=NA)
	plot(log10(f_Hz[2:(which(f_Hz==Nyquist))]),log10(CD[2:(which(f_Hz==Nyquist))]),type='l',xaxt = 'n',xlab='Frequency (Hz)',
	yaxt = 'n',ylab='Spectral density',xlim=c(log10(f_Hz[2]),log10(Nyquist)),ylim=c(-12,2),cex=1.2)
	axis(1, at=c(log10(0.001),log10(0.01),log10(0.1),log10(1),log10(10)), labels=c(0.001,0.01,0.1,1,10))
	axis(2, at=c(-12,-10,-8,-6,-4,-2,0,2), labels=c(10^-12,10^-10,10^-8,10^-6,10^-4,10^-2,10^0,10^2))
	clip(x1=log10(f_Hz[2])-(log10(Nyquist)+log10(f_Hz[2]))*0.04, x2=log10(Nyquist)+(log10(Nyquist)-log10(f_Hz[2]))*0.04, y1=-14-14*0.04,y2=2+14*0.04)
	lines(F,S,col='blue',lwd=3)


	text(x=-1.051317,y=1.75,labels='-7/3',col='red')
	text(x=0.1,y=1.9,labels=timestamp,cex = 1.2)

	Fig_offset=0.1
	fit=lm((log10(Intertial_range[2:20])-Fig_offset)~F[2:20])
	abline(fit,col='red',lwd=2,lty=2)

}
