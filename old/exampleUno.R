library("gamesNws")
ws <- createUnoGame("test", serverHost="138.245.80.17")
startUnoGame(ws, log=T)

########################################

library("gamesNws")
playUno("test", user="Markus", computerPlayer=T, serverHost="138.245.80.17")

library("gamesNws")
playUno("test", user="Manuel", computerPlayer=T, serverHost="138.245.80.17")

library("gamesNws")
playUno("test", user="Flo", computerPlayer=T)#, serverHost="138.245.80.17")


#########################################

library("gamesNws")
playUno("test", user="Markus",computerPlayer=F, serverHost="138.245.80.17")

library("gamesNws")
playUno("test", user="Manuel", computerPlayer=F, serverHost="138.245.80.17")

############################################
library(nws)
library("gamesNws")
s <- sleigh(workerCount=3)
result = eachElem(s, function(x){
			library("gamesNws")
			playUno("test", user=paste("user", x, sep=""),computerPlayer=T, serverHost="138.245.80.17")
		}, 1:length(s@nodeList))
stopSleigh(s)