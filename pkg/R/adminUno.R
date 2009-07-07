#####################################
# Function to create Uno Game in NWS
#####################################
createUnoGame <- function(wsName, ...)
{
	require(nws)

	# create nws
	ws <- netWorkSpace(wsName, ...)

	# declate variables in nws
	# TODO alles declarieren
	nwsDeclare(ws, 'players', 'fifo')
	nwsDeclare(ws, 'played', 'lifo') #lifo
	nwsDeclare(ws, 'cards', 'lifo')
	nwsDeclare(ws, 'startplayers', 'single')
	nwsDeclare(ws, 'winner', 'single')
	# user declares own variable for his hand-cards in .playUnoMaster()

	# initialize cards as described in wikipedia
	# and store in nws
	cards <- c( paste("red", 1:9, sep="-"), paste("red", 1:9, sep="-"), "red-0",
#			"red-BREAK", "red-BACK", "red-2+", "red-BREAK", "red-BACK", "red-2+",
			paste("yellow", 1:9, sep="-"), paste("yellow", 1:9, sep="-"), "yellow-0",
#			"yellow-BREAK", "yellow-BACK", "yellow-2+", "yellow-BREAK", "yellow-BACK", "yellow-2+",
			paste("blue", 1:9, sep="-"), paste("blue", 1:9, sep="-"), "blue-0",
#			"blue-BREAK", "blue-BACK", "blue-2+", "blue-BREAK", "blue-BACK", "blue-2+",
			paste("green", 1:9, sep="-"), paste("green", 1:9, sep="-"), "green-0",
#			"green-BREAK", "green-BACK", "green-2+", "green-BREAK", "green-BACK", "green-2+",
			rep("rybg-0", 4)
#			rep("rybg-4+",4) 
)
	for( i in 1:length(cards))
		nwsStore(ws, 'cards', cards[i])

	cat("Send the name and the server address to your other players!\n")
	cat("Start the game with 'startUnoGame(ws)'\n")

	return(ws)
}

##########################################################
# Function to start the uno game
# especially to get commands from master-user
# and to wait for other players
############################################################
startUnoGame <- function(ws, cardsStart=7, minPlayers=2, maxPlayers=10, log=FALSE, logfile=NULL)
{

	readCommand <- ""
	while(readCommand != "e"){

		# Ask for command from master user
		readCommand <- readline("Players online [o], Start Game [s], End Game [e]")
		# get players from new
		mat <- as.matrix( nwsListVars(ws, showDataFrame=T) )
		id <- which( mat[,1] == "players")
		nplayers <- unlist(mat[id,2])
		if(nplayers!=0){
			players <- vector()
			for(i in 1:nplayers){
				players[i] <- nwsFetchTry(ws, 'players')
				nwsStore(ws, "players", players[i])
			}
		} else
			players <- ""

		# Commands for actions depending on master-user Input
		if(readCommand=="s" && nplayers>=minPlayers && nplayers<=maxPlayers){
			# Start game	
			.playUnoMaster(ws, players, cardsStart, log=log, logfile=logfile)
			#TODO, replay!
			# readCommand <- readline("End Game [e]")
			readCommand <- "e"
		} else if( ! (nplayers>=minPlayers) &&  readCommand=="s"){
			cat("You need more than ", nplayers," player!\n")
		} else if( ! (nplayers<=maxPlayers) &&  readCommand=="s"){
			cat("You can only play with less than ", nplayers," players!\n")
		} else if(readCommand=="o") 
			cat("Players:", players, "\n")
	}

	# At the end close / delete game and nws Server
	nwsDeleteWs(ws@server, ws@wsName)
	nwsClose(ws)
	cat(" GAME OVER \n")
}


###############################################################
# Internal function to playUno
# * distribute cards
# * open first card
# * monitor game
##############################################################
.playUnoMaster <- function(ws, players, cardsStart, log=FALSE, logfile=NULL){

	#Store all players in one vector
	nwsStore(ws, "startplayers", players)

	# Get all cards
	cards <- vector()
	i <- 1
	while (!is.null(tmp <- nwsFetchTry(ws, 'cards'))) {
		cards[i] <- tmp
		i <- i + 1
	}

	# create stock at table (nws)
	cards <- cards[(1+length(players)*cardsStart):length(cards)]
	for( i in 1:length(cards))
		nwsStore(ws, 'cards', cards[i])

	#Distribute cards to player
	cat("\tGive Cards\n")
	cards_players <- split(cards[1:(length(players)*cardsStart)], sample(rep(1:length(players), cardsStart)) )
	for( p in 1:length(players)){
		nwsDeclare(ws, players[p], 'lifo')
		nwsStore(ws, players[p], unlist(cards_players[p])) 
	}

	# open one card to table
	cat("\tOpen first Card\n")
	nwsStore(ws, 'played', nwsFetch(ws, 'cards'))

	cat("\tGame is running:\n")
	if(log==TRUE){
		winner <- watchUnoGame(ws, logfile=logfile)
	} else {
		winner <- .txtProgressBarNWS(ws, 'winner') 	
	}

	#Return winner
	# TODO: calculate Points for winner
	cat("Winner:", winner, "\n")
	
}

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
	}
	# close txtProgressBar
	close(pb)

	return( tmp )	
}

###########################################
# Function to log the game
##########################################
watchUnoGame <- function(ws, ..., logfile=NULL)
{	
	# connect to nws
	require(nws)
	if( class(ws) != "netWorkSpace" ){
		# connect to nws
		ws <- netWorkSpace(ws, ...)
	}
	
	# read played cards
	card_played_tmp <- ""
	user <- ""
	while( is.null(winner <- nwsFindTry(ws, 'winner')) ){
		card_played <- nwsFindTry(ws, 'played') 
		if( card_played != card_played_tmp ){
			cat(user, ": ", card_played, "\n", sep="")
			user <- nwsFindTry(ws, 'players')
			card_played_tmp <- card_played		
		}
	}
	
	#return winner
	return(winner)
}
