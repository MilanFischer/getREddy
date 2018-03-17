# Timestamp (determined as the the end of the interval)

Timestamp<-function(x,y)
{
	t<-paste(as.character(x[length(x)]),as.character(y[length(y)]),sep=" ")
	tx<-as.POSIXlt(as.character(t),origin = "1970-01-01 00:00:00.1",tz="UTC")
	timestamp=tx+1

	return(timestamp)
}