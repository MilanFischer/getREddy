plot_cospectra<-function(Data,w_de_rot,Vde_det,variable,timestamp)
{

####################################################################
### Fourier transformation                                       ###
#################################################################### 

	out=FFT(Vde_det)
	even_or_odd=out$even_or_odd
	f_Hz=out$f_Hz
	Nyquist=out$Nyquist
	Fts_V=fft(Vde_det)/length(Vde_det)
	rm(out)

	out=FFT(w_de_rot)
	even_or_odd=out$even_or_odd
	f_Hz=out$f_Hz
	Nyquist=out$Nyquist
	Fts_w=fft(w_de_rot)/length(w_de_rot)
	rm(out)

####################################################################
### Cospectral density (Stull 1988, p. 312-331)                  ###
#################################################################### 

	df_Hz=f_Hz[2]-f_Hz[1]	

	Far=Re(Fts_V)
	Fbr=Re(Fts_w)
	Fai=Im(Fts_V)
	Fbi=Im(Fts_w)

	Gab=Far*Fbr+1i*Far*Fbi-1i*Fai*Fbr-1i^2*Fai*Fbi

	# Cospectrum
	Co=Far*Fbr+Fai*Fbi

	# Parseval's theorem (energy conservation-the cospectrum intergral is equal to scalars' covariance)

	sum(Co)
	cov(Vde_det,w_de_rot,use='pairwise.complete.obs')

	# Cospetcral density
	CD=Cospectral_density(even_or_odd,f_Hz,Nyquist,Co)
	sum(CD*df_Hz)

	covariance=sum(Co)			# For further normalization by covariance

############################
### Ensemble averages    ###
############################ 

	out=Ensemble(N=20,f_Hz,Nyquist,CD/covariance)
	F=out$F
	S=out$S
	rm(out)

############################
### Plotting             ###
############################


	png(paste(Main_WD,'/Outputs/',variable,'/',format(as.POSIXct(timestamp),'%Y-%m-%d_%H%M'),'_',variable,'_c) co-spectrum.png',sep=''),type='cairo')	

	Intertial_range=(10^F)^(-7/3)		# Not multiplied by frequency
	# Intertial_range=(10^F)^(-4/3)		# Premultiplied by frequency

	# Log-log presentation of cospectra

	par(mfrow=c(1,1),oma=c(3.5,3.5,0,0)+1,mar=c(0,0,0,0),xpd=NA)
	plot(log10(f_Hz[2:(which(f_Hz==Nyquist))]),log10(CD[2:(which(f_Hz==Nyquist))]/covariance),type='l',xaxt = 'n',xlab='Frequency (Hz)',
	yaxt = 'n',ylab='Normlaized cospectral density',xlim=c(log10(f_Hz[2]),log10(Nyquist)),ylim=c(-12,4),cex=1.2)
	axis(1, at=c(log10(0.001),log10(0.01),log10(0.1),log10(1),log10(10)), labels=c(0.001,0.01,0.1,1,10))
	axis(2, at=c(-12,-10,-8,-6,-4,-2,0,2,3,4), labels=c(10^-12,10^-10,10^-8,10^-6,10^-4,10^-2,10^0,10^2,10^3,10^4))
	clip(x1=log10(f_Hz[2])-(log10(Nyquist)+log10(f_Hz[2]))*0.04, x2=log10(Nyquist)+(log10(Nyquist)-log10(f_Hz[2]))*0.04, y1=-16-16*0.04,y2=4+16*0.04)
	lines(F,S,col='blue',lwd=3)


	text(x=-1.051317,y=3.75,labels='-7/3',col='red')
	text(x=0.1,y=3.9,labels=timestamp,cex = 1.2)

	Fig_offset=-2
	fit=lm((log10(Intertial_range[2:20])-Fig_offset)~F[2:20])
	abline(fit,col='red',lwd=1,lty=2)
	box()

	dev.off()
}
