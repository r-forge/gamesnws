source("adminUno.R")
ws <- createUnoGame("test")#, serverHost="138.245.80.17")
startUnoGame(ws, log=T, cardsStart=10)

########################################

source("playUno.R")
source("adminUno.R")
playUno("test", user="Markus",computerPlayer=T)#, serverHost="138.245.80.17")

source("playUno.R")
source("adminUno.R")
playUno("test", user="Manuel", computerPlayer=T)#, serverHost="138.245.80.17")

source("playUno.R")
source("adminUno.R")
playUno("test", user="Flo", computerPlayer=T)#, serverHost="138.245.80.17")


#########################################

source("playUno.R")
source("adminUno.R")
playUno("test", user="Markus",computerPlayer=F)#, serverHost="138.245.80.17")

source("playUno.R")
source("adminUno.R")
playUno("test", user="Manuel", computerPlayer=F)#, serverHost="138.245.80.17")

############################################
library(nws)
s <- sleigh(workerCount=10)
result = eachElem(s, function(x){
			source("playUno.R")
			source("adminUno.R")
			playUno("test", user=paste("user", x, sep=""),computerPlayer=T)#, serverHost="138.245.80.17")
		}, 1:length(s@nodeList))
stopSleigh(s)