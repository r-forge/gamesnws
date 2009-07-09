#####################################
# Funtion for running text-bar
# waits for oparation in nws
######################################
.txtProgressBarNWS <- function(ws, variable, steps=10, width=20)
{
	# look for variable
	tmp <- nwsFindTry(ws, variable)
	# initialize txtProgressBar
	pb <- txtProgressBar(min=0, max=steps, style=1, width=width)
	i <- run <- 0
	# if variable not available, run progress bar
	while( is.null(tmp) ){
		setTxtProgressBar(pb, i)
		if(run==0) i <- i+1
		else i <- i-1
		if(i==steps) run<-1
		if(i==0) run<-0
		# look for variable
		tmp <- nwsFindTry(ws, variable)
		Sys.sleep(0.1) #to reduce requests to NWS
	}
	# close txtProgressBar
	close(pb)
	
	return( tmp )	
}
