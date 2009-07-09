library("gamesNws")
#source("adminUno.R")
ws <- createUnoGame("test")#, serverHost="138.245.80.17")
startUnoGame(ws, log=T)

########################################

#library("gamesNws")
source("zzz.R")
source("playUno.R")
source("adminUno.R")
playUno("test", user="Markus")#,serverHost="138.245.80.17")

source("zzz.R")
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
#library(playnws)
s <- sleigh(workerCount=6)
result = eachElem(s, function(x){
			source("zzz.R")
			source("playUno.R")
			source("adminUno.R")
			playUno("test", user=paste("user", x, sep=""),computerPlayer=T)#, serverHost="138.245.80.17")
		}, 1:length(s@nodeList))
stopSleigh(s)