source("adminUno.R")
ws <- createUnoGame("test")#, serverHost="138.245.80.17")
startUnoGame(ws, log=T)

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
