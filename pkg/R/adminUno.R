#####################################
# Function to create Uno Game in NWS
#####################################
.createUnoGame <- function(wsName, ...)
{
	require(nws)
	nwss<-nwsServer(...)
  serverlist<-nwsListWss(nwss, showDataFrame=TRUE)
  servernamebool<-TRUE
  for(i in 1:length(serverlist$Name)){
    if(wsName==serverlist$Name[[i]]){
      servernamebool<-FALSE
    }
  }
  if(servernamebool){
	# create nws
	ws <- netWorkSpace(wsName, ...)

	# declare variables in nws
	#ToDo usernames not valid: players, played...
  	nwsDeclare(ws, 'players', 'fifo') 		# list of players for player rotation
	nwsDeclare(ws, 'played', 'lifo') 	# played card
	nwsDeclare(ws, 'cards', 'fifo') 	# stack of cards
	nwsDeclare(ws, 'players_logedin', 'single') # vector of loged-in players
	nwsDeclare(ws, 'player_in_action', 'single') # player in action
	nwsDeclare(ws, 'winner', 'single') 	# name of winner
	nwsDeclare(ws, 'penalty', 'single')	# value of penalty
	nwsDeclare(ws, 'debug' , 'single') 	# boolean for debug-mode
	nwsDeclare(ws, 'points', 'single') 	# vector of points, in order of players_logedin
	nwsDeclare(ws, 'uno' , 'single')  	#vector of uno-booleans, in order of players_logedin
	nwsDeclare(ws, 'rules' , 'single') 	#vector of the rules
  	nwsDeclare(ws, 'rulesbools', 'single')	#vector of booleanvalues in order of rules 
	nwsDeclare(ws,	'graphics', 'single')	#boolean if graphic device is shown
	nwsDeclare(ws,	'cpu', 'single')	#vector of cpu-booleans, in order of players_logedin
	#nwsDeclare(ws, 'buttonlist','single')	#list of buttons
	#nwsDeclare(ws, 'cardcoord','single')	#list of coordinates of the cards
	
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
			rep("rybg-00", 4), rep("rybg-4+",4)
	)
	cards <- sample(cards)
	for( i in 1:length(cards))
		nwsStore(ws, 'cards', cards[i])
	
	# initialize master player, will be removed later
	nwsStore(ws, 'players_logedin', 'master')

	#Some output
	#cat("Send the NWS-name and the server address to your other players and\n")
	#cat("start the game with the command 'startUnoGame(ws)'\n\n")

	return(ws)
 }
 else{    #if workspacename allready exists
 #cat("Workspacename allready exists, please try another name\n")
 return("notaws")
 }
}

##########################################################
# Function to start the uno game
# especially to get commands from master-user
# and to wait for other players
############################################################
startUnoGame <- function(wsName, cardsStart=7, 
		minPlayers=2, maxPlayers=10, 
		log=0, logfile=NULL, debug=FALSE, config=NULL,graphics=TRUE,...)
{	
	require(nws)
	ws<-.createUnoGame(wsName, ...)
  wsclass<-class(ws)
  if(wsclass[1]=="netWorkSpace"){
	readCommand <- ""
	while(readCommand != "e"){
		nwsStore(ws,'rulesbools', config)
		nwsStore(ws,'graphics', graphics)

		# Ask for command from master user
		readCommand <- readline("Players online [o], Start Game [s], End Game [e], Start Game in Debugmode [d]?")
	
		# get players (remove master, it is not a player!)
		players <- nwsFindTry(ws, 'players_logedin')
		if( any(players=="master"))
			players <- players[ -which( players == "master")]
		nplayers <- length(players)

		# Commands for actions depending on master-user Input
		if(readCommand=="s" && nplayers>=minPlayers && nplayers<=maxPlayers){
		  	if(is.null(config)){
				.askForRules(ws)
			}
      # Start UNO game	
			.playUnoMaster(ws, players, cardsStart, log=log, logfile=logfile, debug)
			cat("For replay you have to reset the Game: createUnoGame()\n")
			readCommand <- readline("End Game [e]?")
		}else if(readCommand=="d" && nplayers>=minPlayers && nplayers<=maxPlayers){
		  	if(is.null(config)){
				.askForRules(ws)
			}
      # Start UNO game	
			.playUnoMaster(ws, players, cardsStart, log=log, logfile=logfile, debug=TRUE)
			cat("For replay you have to reset the Game: createUnoGame()\n")
			readCommand <- readline("End Game [e]?")
		} else if( !(nplayers>=minPlayers) &&  (readCommand=="s" || readCommand=="d")){
			cat("You need more than ", nplayers," player!\n")
		} else if( !(nplayers<=maxPlayers) &&  (readCommand=="s" || readCommand=="d")){
			cat("You can only play with less than ", nplayers," players!\n")
		} else if(readCommand=="o") 
			cat("Players:", players, "\n")
	}

	# At the end close nws connection / delete game and nws Server
	nwsDeleteWs(ws@server, ws@wsName)
	nwsClose(ws)
	cat("GAME OVER \n")
	}else{
    cat("wsName is no valid networkspacename, please try another one")
    	nwsDeleteWs(ws@server, ws@wsName)
      nwsClose(ws)
  }
}


###############################################################
# Internal function to playUno
# * distribute cards
# * open first card
# * monitor game
##############################################################
.playUnoMaster <- function(ws, players, 
		cardsStart, log=0, logfile=NULL, debug)
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
      
    #set values of rulesvector
    rules<-c("wc","bb","pc","dc")
    nwsStore(ws, 'rules', rules)
    
    #Set startvalue for variable penalty
  	#FALSE = penalty not allready given, TRUE = penalty has been given to a player -> not again!
 	 nwsStore(ws, 'penalty', 0)
 	 
 	 #set startvalues for uno-vector
 	 uno<-vector()
 	 for(i in 1:length(nwsFindTry(ws, 'players_logedin', c(1:10)))){
 	 uno<-c(uno,FALSE)
 	 }
 	 nwsStore(ws, 'uno',uno)
 	 
	# set player_in_action and start game
	nwsStore(ws, 'player_in_action', players[length(players)])
	
	#set startvalue for cpu
	cpu<-rep(FALSE,10)
	nwsStore(ws,'cpu',cpu)
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
	if(log!=0){
		winner <- .watchUnoGame(ws, logfile=logfile)
	} else {
		winner <- .txtProgressBarNWS(ws, 'winner') 	
	}

	#Return winner
	cat("Winner:", winner, "\n")
	
}

###########################################
# Function to log the game
# Log default = 0 -> no log
#               1 -> names, winner , points
#               2 -> 1, played cards
#               3 -> 2, handcards
#               4 -> 3, date, time...
##########################################
.watchUnoGame <- function(ws, ..., logfile=NULL)
{	
	require(nws)
	
	
	#TODO: log hand cards of players for statistics
	
	# connect to nws server
	if( class(ws) != "netWorkSpace" ){
		# connect to nws
		ws <- netWorkSpace(ws, ...)
	} #else nothing to do, ws is netWorkSpace object
	
	
	# read played cards
	card_played <- card_played_tmp <- ""
	user <- user_tmp <- "startcard"
	output<-character()
	while( is.null(winner <- nwsFindTry(ws, 'winner'))
		|| card_played != card_played_tmp 
		|| user != user_tmp
		){
		card_played <- nwsFindTry(ws, 'played') 

		#if played card has changed: output
		if( card_played != card_played_tmp ){
			cat(user, ": ", card_played, " (",length(nwsFindTry(ws,user)),")\n", sep="")
			output<-paste(output,user, ";", card_played, ";",length(nwsFindTry(ws,user)),"\n", sep="")
      user <- nwsFindTry(ws, 'player_in_action')
			user_tmp <- user # save player 
			card_played_tmp <- card_played # save card
		# if user has changed and no card: output
		}else if( user != user_tmp){
			cat(user_tmp, ": NO (",length(nwsFindTry(ws,user_tmp)),")\n", sep="")
			output<-paste(output,user_tmp, ";NO;",length(nwsFindTry(ws,user_tmp)),"\n", sep="")
			#user <- nwsFindTry(ws, 'players')
			user_tmp <- user
		}
		user <- nwsFindTry(ws, 'player_in_action')
		#code runs to fast for nws
		Sys.sleep(0.15)
	}
	# to get the last card into logfile
	#if played card has changed: output
		if( card_played != card_played_tmp ){
			cat(user, ": ", card_played, " (",length(nwsFindTry(ws,user)),")\n", sep="")
			output<-paste(output,user, ";", card_played, ";",length(nwsFindTry(ws,user)),"\n", sep="")
      user <- nwsFindTry(ws, 'player_in_action')
			user_tmp <- user # save player 
			card_played_tmp <- card_played # save card
		# if user has changed and no card: output
		}else if( user != user_tmp){
			cat(user_tmp, ": NO (",length(nwsFindTry(ws,user_tmp)),")\n", sep="")
			output<-paste(output,user_tmp, ";NO;",length(nwsFindTry(ws,user_tmp)),"\n", sep="")
			#user <- nwsFindTry(ws, 'players')
			user_tmp <- user
		}
		user <- nwsFindTry(ws, 'player_in_action')
		#code runs to fast for nws
		Sys.sleep(0.15)
	write(output,file = 'logfile.txt')	
	#return winner
	return(winner)
}
###############################################
#Function to ask what rules should be used
###############################################
.askForRules<-function(ws)
{
  require(nws)
  rules<-nwsFindTry(ws, 'rules')
 	readWCRule<-""
  while(readWCRule != "y" && readWCRule != "n"){
    readWCRule <- readline("Wildcards must not be played as uno-card[y],else[n]")
    if(readWCRule=="y"){
      wc<-TRUE
    }
    else if(readWCRule=="n"){
      wc<-FALSE
    }   
  }
  readBBRule<-""
  while(readBBRule != "y" && readBBRule != "n"){
    readBBRule <- readline("Wildcards must not be laid on other wild cards[y],else[n]")
    if(readBBRule=="y"){
      bb<-TRUE
    }
    else if(readBBRule=="n"){
      bb<-FALSE
    }   
  }
  readPCRule<-""
  while(readPCRule != "y" && readPCRule != "n"){
    readPCRule <- readline("Penalties could be concatenated[y],else[n]")
    if(readPCRule=="y"){
      pc<-TRUE
    }
    else if(readPCRule=="n"){
      pc<-FALSE
    }   
  }
  readDCRule<-""
  while(readDCRule != "y" && readDCRule != "n"){
    readDCRule <- readline("Cards, you have twice can be played as one[y],else[n]")
    if(readDCRule=="y"){
      dc<-TRUE
    }
    else if(readDCRule=="n"){
      dc<-FALSE
    }   
  }
  rulesbools<-c(wc,bb,pc,dc)
  nwsStore(ws,'rulesbools',rulesbools)
}
