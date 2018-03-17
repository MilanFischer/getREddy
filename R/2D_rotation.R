rotation2D<-function(u,v,w)
{

	# calculate the wind component means
	uBar = mean(u, na.rm = TRUE)
	vBar = mean(v, na.rm = TRUE)
	wBar = mean(w, na.rm = TRUE)

	# determine the rotation angles
	ST = wBar / sqrt(uBar^2 + vBar^2 + wBar^2)
	CT = sqrt(uBar^2 + vBar^2) / sqrt(uBar^2 + vBar^2 + wBar^2)
	CE = uBar / sqrt(uBar^2 + vBar^2)
	SE = vBar / sqrt(uBar^2 + vBar^2)

	# rotate each wind component
	uRot = (u * CT * CE) + (v * CT * SE) + (w * ST) 
	vRot = (v * CE) - (u * SE)
	wRot = (w * CT) - (u * ST * CE) - (v * ST * SE)

	data.frame(uRot, vRot, wRot)
}