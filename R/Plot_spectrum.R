# Compute and plot spectrum

plot_spectra<-function(Data,ts,variable,timestamp)
{


#################################################################### 
### Despike the tempearture time series                            #
#################################################################### 

out_despike=Despike(Data,ts,gapfill=TRUE)
Tde=out_despike$Y

No_of_spikes=out_despike$No_of_spikes

plot(ts,type='l',col='red',ylab=variable)
lines(Tde)

#H=CpAir*rhoAir*cov(Tde,w_rot,use='pairwise.complete.obs')
#H

#################################################################### 
### Trend removal                                                  #
#################################################################### 

	T_dlm=Detrend(Tde)$Detrended

#################################################################### 
### Windowing                                                      #
#################################################################### 

	T_dlmW=Taper(T_dlm,100/12/2)$Tapered

####################################################################
### Fourier transformation                                         #
#################################################################### 

	out=FFT(T_dlmW)
	even_or_odd=out$even_or_odd
	f_Hz=out$f_Hz
	Nyquist=out$Nyquist
	Fts=fft(T_dlmW)/length(T_dlmW)
	rm(out)

####################################################################
### Spectral density (Stull 1988, p. 312-313)                      #
#################################################################### 

	SD=Spectral_density(even_or_odd,f_Hz,Nyquist,Fts)

# Parseval's theorem (energy conservation-the spectrum intergral is equal to scalar variance)
	
	df_Hz=f_Hz[2]-f_Hz[1]	

	sum((T_dlmW-mean(T_dlmW))^2)/length(T_dlmW)	# Biased variance
	sum((abs(Fts))^2)
	sum(SD*df_Hz)

####################################################################
### Ensemble averages                                              #
#################################################################### 

	out=Ensemble(N=20,f_Hz,Nyquist,SD)
	F=out$F
	S=out$S
	rm(out)

################################
### Plotting                 ###
################################

	Intertial_range=(10^F)^(-5/3)

	#Log-log presentation of power spectra

	par(mfrow=c(1,1),oma=c(3.5,3.5,0,0)+1,mar=c(0,0,0,0),xpd=NA)
	plot(log10(f_Hz[2:(which(f_Hz==Nyquist))]),log10(SD[2:(which(f_Hz==Nyquist))]),type='l',xaxt = 'n',xlab='Frequency (Hz)',
	yaxt = 'n',ylab='Spectral density (K*K)',xlim=c(log10(f_Hz[2]),log10(Nyquist)),ylim=c(-12,2),cex=1.2)
	axis(1, at=c(log10(0.001),log10(0.01),log10(0.1),log10(1),log10(10)), labels=c(0.001,0.01,0.1,1,10))
	axis(2, at=c(-12,-10,-8,-6,-4,-2,0,2), labels=c(10^-12,10^-10,10^-8,10^-6,10^-4,10^-2,10^0,10^2))
	clip(x1=log10(f_Hz[2])-(log10(Nyquist)+log10(f_Hz[2]))*0.04, x2=log10(Nyquist)+(log10(Nyquist)-log10(f_Hz[2]))*0.04, y1=-14-14*0.04,y2=2+14*0.04)
	lines(F,S,col='blue',lwd=3)


	text(x=-1.051317,y=1.75,labels='-5/3',col='red')
	text(x=0.1,y=1.9,labels=timestamp,cex = 1.2)

	Fig_offset=-0.5
	fit=lm((log10(Intertial_range[2:20])-Fig_offset)~F[2:20])
	abline(fit,col='red',lwd=1,lty=2)

}
