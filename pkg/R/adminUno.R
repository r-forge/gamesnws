#####################################
# Function to create Uno Game in NWS
#####################################
createUnoGame <- function(wsName, ...)
{
	require(nws)

	# create nws
	ws <- netWorkSpace(wsName, ...)

	# declate variables in nws
	nwsDeclare(ws, 'players', 'fifo')
	nwsDeclare(ws, 'played', 'lifo') 
	nwsDeclare(ws, 'cards', 'fifo')
	nwsDeclare(ws, 'players_logedin', 'single')
	nwsDeclare(ws, 'player_in_action', 'single')
	nwsDeclare(ws, 'winner', 'single')
	# user declares own variable for his hand-cards in .playUnoMaster()

	# initialize cards as described in wikipedia
	# mischen and store in nws
	cards <- c( paste("red", 1:9, sep="-"), paste("red", 1:9, sep="-"), "red-0",
			rep("red-BREAK",2), 
			rep("red-2+",2), 
			rep("red-BACK",2),
			paste("yellow", 1:9, sep="-"), paste("yellow", 1:9, sep="-"), "yellow-0",
			rep("yellow-BACK",2), 
			rep("yellow-2+"), 
			rep("yellow-BREAK",2),
			paste("blue", 1:9, sep="-"), paste("blue", 1:9, sep="-"), "blue-0",
			rep("blue-BACK",2), 
			rep("blue-2+",2), 
			rep("blue-BREAK",2),
			paste("green", 1:9, sep="-"), paste("green", 1:9, sep="-"), "green-0",
			rep("green-BREAK",2),
			rep("green-2+",2),
			rep("green-BACK",2),
			rep("rybg-0", 4), rep("rybg-4+",4)
	)
	cards <- sample(cards)
	for( i in 1:length(cards))
		nwsStore(ws, 'cards', cards[i])
	
	nwsStore(ws, 'players_logedin', 'master')

	cat("Send the name and the server address to your other players!\n")
	cat("Start the game with 'startUnoGame(ws)'\n")

	return(ws)
}

##########################################################
# Function to start the uno game
# especially to get commands from master-user
# and to wait for other players
############################################################
startUnoGame <- function(ws, cardsStart=7, 
		minPlayers=2, maxPlayers=10, 
		log=FALSE, logfile=NULL)
{

	readCommand <- ""
	while(readCommand != "e"){

		# Ask for command from master user
		readCommand <- readline("Players online [o], Start Game [s], End Game [e]?")
	
		# get players
		players <- nwsFindTry(ws, 'players_logedin')
		if( any(players=="master"))
			players <- players[ -which( players == "master")]
		nplayers <- length(players)

		# Commands for actions depending on master-user Input
		if(readCommand=="s" && nplayers>=minPlayers && nplayers<=maxPlayers){
			# Start game	
			.playUnoMaster(ws, players, cardsStart, log=log, logfile=logfile)
			cat("For replay you have to reset the Game: createUnoGame()\n")
			readCommand <- readline("End Game [e]?")
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

	#Store all players in one vector and in players variable
	if( any(players=="master"))
		players <- players[ -which( players == "master")]
	nwsStore(ws, "players_logedin", players)
	for( i in 1:length(players))
		nwsStore(ws, "players", players[i])
	
	# Get all cards
	cards <- vector()
	i <- 1
	while (!is.null(tmp <- nwsFetchTry(ws, 'cards'))) {
		cards[i] <- tmp
		i <- i + 1
	}

	# create stock at table (nws)
	cards_stock <- cards[(1+length(players)*cardsStart):length(cards)]
	for( i in 1:length(cards_stock))
		nwsStore(ws, 'cards', cards_stock[i])

	#Distribute cards to player
	cat("\tGive Cards\n")
	cards_players <- split(cards[1:(length(players)*cardsStart)], sample(rep(1:length(players), cardsStart)) )
	for( p in 1:length(players))
		nwsStore(ws, players[p], unlist(cards_players[p])) 

	# open one card to table
	cat("\tOpen first Card\n")
	first_card <- nwsFetch(ws, 'cards')
	first_card_color <- strsplit(unlist(first_card), "-")[[1]][1]
	first_card_number <- strsplit(unlist(first_card), "-")[[1]][2]
	if( first_card_color =="rybg")
		first_card <- sample(c("red-rybg", "yellow-rybg", "blue-rybg", "green-rybg"),1)
	if( first_card_number == "4+")
		first_card <- paste(first_card, "4+", sep="")
	nwsStore(ws, 'played', first_card)
	
	# set player_in_action and start game
	nwsStore(ws, 'player_in_action', players[length(players)])

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
	card_played <- card_played_tmp <- ""
	user <- user_tmp <- ""
	while( is.null(winner <- nwsFindTry(ws, 'winner'))
		|| card_played != card_played_tmp 
		|| user != user_tmp
		){
		card_played <- nwsFindTry(ws, 'played') 

		#if cards change: output
		if( card_played != card_played_tmp ){
			cat(user, ": ", card_played, " (",length(nwsFindTry(ws,user)),")\n", sep="")
			user <- nwsFindTry(ws, 'player_in_action')
			user_tmp <- user # save player 
			card_played_tmp <- card_played # save card
		# if user change and no card: output
		}else if( user != user_tmp){
			cat(user_tmp, ": NO (",length(nwsFindTry(ws,user_tmp)),")\n", sep="")
			#user <- nwsFindTry(ws, 'players')
			user_tmp <- user
		}
		user <- nwsFindTry(ws, 'player_in_action')
		Sys.sleep(0.1)
	}
		
	#return winner
	return(winner)
}
