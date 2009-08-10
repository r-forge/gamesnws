#####################################
# Function to create Uno Game in NWS
#####################################
createUnoGame <- function(wsName, ...)
{
	require(nws)

	# create nws
	ws <- netWorkSpace(wsName, ...)

	# declare variables in nws
	nwsDeclare(ws, 'players', 'fifo') 	# list of players for player rotation
	nwsDeclare(ws, 'played', 'lifo') 	# played card
	nwsDeclare(ws, 'cards', 'fifo') 	# stack of cards
	nwsDeclare(ws, 'players_logedin', 'single') # vector of loged-in players
	nwsDeclare(ws, 'player_in_action', 'single') # player in action
	nwsDeclare(ws, 'winner', 'single') 	# name of winner
	nwsDeclare(ws, 'penalty', 'single')	#if one player got penalty but can not play a card, next player should not get penalty too
	nwsDeclare(ws, 'debug' , 'single') 	# boolean for debug-mode
	nwsDeclare(ws, 'points', 'single') 	# vector of points, in order of players_logedin
	nwsDeclare(ws, 'uno' , 'single')  #vector of uno-booleans, in order of players_logedin 
	
  # user declares own variable for his hand-cards in .playUnoMaster()

	# initialize cards as described in wikipedia
	# mix and store in nws
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
	
	# initialize master player, will be removed later
	nwsStore(ws, 'players_logedin', 'master')

	#Some output
	cat("Send the NWS-name and the server address to your other players and\n")
	cat("start the game with the command 'startUnoGame(ws)'\n\n")

	return(ws)
}

##########################################################
# Function to start the uno game
# especially to get commands from master-user
# and to wait for other players
############################################################
startUnoGame <- function(ws, cardsStart=7, 
		minPlayers=2, maxPlayers=10, 
		log=FALSE, logfile=NULL, debug=FALSE)
{	
	require(nws)

	readCommand <- ""
	while(readCommand != "e"){

		# Ask for command from master user
		readCommand <- readline("Players online [o], Start Game [s], End Game [e], Start Game in Debugmode [d]?")
	
		# get players (remove master, it is not a player!)
		players <- nwsFindTry(ws, 'players_logedin')
		if( any(players=="master"))
			players <- players[ -which( players == "master")]
		nplayers <- length(players)

		# Commands for actions depending on master-user Input
		if(readCommand=="s" && nplayers>=minPlayers && nplayers<=maxPlayers){
			# Start UNO game	
			.playUnoMaster(ws, players, cardsStart, log=log, logfile=logfile, debug)
			cat("For replay you have to reset the Game: createUnoGame()\n")
			readCommand <- readline("End Game [e]?")
		} else if(readCommand=="d" && nplayers>=minPlayers && nplayers<=maxPlayers){
			# Start UNO game	
			.playUnoMaster(ws, players, cardsStart, log=log, logfile=logfile, debug=TRUE)
			cat("For replay you have to reset the Game: createUnoGame()\n")
			readCommand <- readline("End Game [e]?")
		} else if( !(nplayers>=minPlayers) &&  readCommand=="s"){
			cat("You need more than ", nplayers," player!\n")
		} else if( !(nplayers<=maxPlayers) &&  readCommand=="s"){
			cat("You can only play with less than ", nplayers," players!\n")
		} else if(readCommand=="o") 
			cat("Players:", players, "\n")
	}

	# At the end close nws connection / delete game and nws Server
	nwsDeleteWs(ws@server, ws@wsName)
	nwsClose(ws)
	cat("GAME OVER \n")
}


###############################################################
# Internal function to playUno
# * distribute cards
# * open first card
# * monitor game
##############################################################
.playUnoMaster <- function(ws, players, 
		cardsStart, log=FALSE, logfile=NULL, debug)
{
	require(nws)

	#Store all players in one vector and in players variable for player rotation
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

	# create stock at table(=nws)
	cards_stock <- cards[(1+length(players)*cardsStart):length(cards)]
	for( i in 1:length(cards_stock))
		nwsStore(ws, 'cards', cards_stock[i])

	#Distribute cards to player
	cat("\tGive cards\n")
	cards_players <- split(cards[1:(length(players)*cardsStart)], sample(rep(1:length(players), cardsStart)) )
	for( p in 1:length(players))
		nwsStore(ws, players[p], unlist(cards_players[p])) 

	# open one card to table
	cat("\tOpen first card\n")
	first_card <- nwsFetch(ws, 'cards')
	# special operations if fist card is a special card
	first_card_color <- strsplit(unlist(first_card), "-")[[1]][1]
	first_card_number <- strsplit(unlist(first_card), "-")[[1]][2]
	if( first_card_color =="rybg")
		first_card <- sample(c("red-rybg", "yellow-rybg", "blue-rybg", "green-rybg"),1)
	if( first_card_number == "4+")
		first_card <- paste(first_card, "4+", sep="")
	nwsStore(ws, 'played', first_card)
	
  	#Set startvalue for variable penalty
  	#FALSE = penalty not allready given, TRUE = penalty has been given to a player -> not again!
 	 nwsStore(ws, 'penalty', TRUE)
 	 
 	 #set startvalues for uno-vector
 	 uno<-vector()
 	 for(i in 1:length(nwsFindTry(ws, 'players_logedin', c(1:10)))){
 	 uno<-c(uno,FALSE)
 	 }
 	 nwsStore(ws, 'uno',uno)
 	 
	# set player_in_action and start game
	nwsStore(ws, 'player_in_action', players[length(players)])
	
	#Debug information
	if(debug){
		nwsStore(ws,'debug',TRUE)
		cat("\tDebugmode is activated!\n")
	} else{
		nwsStore(ws,'debug',FALSE)
		cat("\tDebugmode is not activated!\n")
	}
	

	#Operation during running game
	cat("\tGame is running:\n")
	if(log==TRUE){
		winner <- watchUnoGame(ws, logfile=logfile)
	} else {
		winner <- .txtProgressBarNWS(ws, 'winner') 	
	}

	#Return winner
	cat("Winner:", winner, "\n")
	
}

###########################################
# Function to log the game
##########################################
watchUnoGame <- function(ws, ..., logfile=NULL)
{	
	require(nws)
	
	#TODO: write to logfile
	#TODO: log hand cards of players for statistics
	
	# connect to nws server
	if( class(ws) != "netWorkSpace" ){
		# connect to nws
		ws <- netWorkSpace(ws, ...)
	} #else nothing to do, ws is netWorkSpace object
	
	
	# read played cards
	card_played <- card_played_tmp <- ""
	user <- user_tmp <- ""
	while( is.null(winner <- nwsFindTry(ws, 'winner'))
		|| card_played != card_played_tmp 
		|| user != user_tmp
		){
		card_played <- nwsFindTry(ws, 'played') 

		#if played card has changed: output
		if( card_played != card_played_tmp ){
			cat(user, ": ", card_played, " (",length(nwsFindTry(ws,user)),")\n", sep="")
			user <- nwsFindTry(ws, 'player_in_action')
			user_tmp <- user # save player 
			card_played_tmp <- card_played # save card
		# if user has changed and no card: output
		}else if( user != user_tmp){
			cat(user_tmp, ": NO (",length(nwsFindTry(ws,user_tmp)),")\n", sep="")
			#user <- nwsFindTry(ws, 'players')
			user_tmp <- user
		}
		user <- nwsFindTry(ws, 'player_in_action')
		#code runs to fast for nws
		Sys.sleep(0.15)
	}
		
	#return winner
	return(winner)
}
