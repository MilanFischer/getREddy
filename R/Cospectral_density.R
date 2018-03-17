Cospectral_density<-function(even_or_odd,f_Hz,Nyquist,Co)
{	
	df_Hz<-f_Hz[2]-f_Hz[1]
	if(even_or_odd=='even'){
	E=c(2*Co[1:(which(f_Hz==Nyquist)-1)],Co[which(f_Hz==Nyquist)])
	}else{
	E=2*Co
	}


	CD=E/df_Hz

	return(CD)
}