###########################################################
# Main Function to play UNO for user
######################################################
playUno <- function(name, 
		user=Sys.info()["user"], 
		computerPlayer=FALSE, computerPlayerFunction=computerPlayerUNO, 
		...)
{
	require(nws)
	# Login to nws
	ws <- netWorkSpace(name, ...)
	  
	# set user to nws: log in
	while ( is.null(players_logedin <- nwsFetchTry(ws, 'players_logedin')) )
	{ }# if players_logedin is empty, wait while other user is registering
	if( any(players_logedin == user)){
		nwsStore(ws, 'players_logedin', players_logedin)
		stop("User already exists, please change user name!")
	}else if( !any(players_logedin == 'master')){
		nwsStore(ws, 'players_logedin', players_logedin)
		stop("Game is already running, no more login possible!")
	}else
		nwsStore(ws, 'players_logedin', c(players_logedin, user))

	# Wait for other players
	# if the player (=user) got cards, go to game start
	# additional get cards for player (=user)
	# and declare card-varable for user
	nwsDeclare(ws, user, 'lifo')
	cat("Wait for other players and game start:\n")
	cards_hand <- .txtProgressBarNWS(ws, user) 

  
  # GAME START
	# play the game, as long as there is no winner
	while(is.null(nwsFindTry(ws, 'winner')) ){
		# look for player in Action
		# if you are user in action, there is no winner,
		# than play your cards
		pb <- txtProgressBar(min=0, max=10, style=1, width=20)
		i <- run <- 0
		while( nwsFindTry(ws, 'player_in_action')!=user && is.null(nwsFindTry(ws, 'winner')) ){
			setTxtProgressBar(pb, i)
			if(run==0)
				i <- i+1
			else i <- i-1
			if(i==10) run<-1
			if(i==0) run<-0
			Sys.sleep(0.1) #to reduce requests to NWS
		}
		close(pb)
		playerInAction <- nwsFindTry(ws, 'player_in_action')
		cards_hand <- nwsFindTry(ws, user)
		
		# Check card stack for enough cards
		.check_card_stack(ws, "played", "cards")
		
		# SOME OUTPUT
		# all players and their number of cards in the hand + points
		players <- nwsFindTry(ws, 'players_logedin')
		
		cat("Players: ")
		for(p in players){
			cat(">>",p, " - (",length(nwsFindTry(ws,p))," card(s)<< ", sep="")
		}
    cat("\n")
		
		#Play Card if there is no winner
		# no card played and user in action
		card_play <- ""
		NO <- 0  

		
		while(card_play=="" && is.null(nwsFindTry(ws, "winner")) 
				&& playerInAction==user )
		{
		
			# SOME OUTPUT
			#Debuginformation
			if(nwsFindTry(ws, 'debug')){
        #calculate points
		    .calculate(ws)
				cat("\nDebuginformation:\n")			
				cat("	Players in Game: ",length(nwsFindTry(ws,'players_logedin')),"\n")
				cat("	Active Player: ",nwsFindTry(ws,'player_in_action'),"\n")
				cat("	Status of penalty-var: ",nwsFindTry(ws,'penalty'),"\n")
				#cat("	Handkarten:",cards_hand,"\n")
				for(p in players){
			  points <- nwsFindTry(ws,'points')
		    cat("	>>",p, " - (",length(nwsFindTry(ws,p))," card(s), ",points[p]," point(s))<< ", sep="")
		    }
		    cat("\n")
		  }

			# get played card
			played <- nwsFindTry(ws, 'played')
			#split for color and number
			played_color <- strsplit(unlist(played), "-")[[1]][1]
			played_number <- strsplit(unlist(played), "-")[[1]][2]
			cat("Table:", played,"\n")
			
			# PENALTY
			# TODO change for reaction to penalty cards
			if( played_number =='2+'&& nwsFind(ws, 'penalty') == FALSE){
				cat("You got 2 penalty cards\n")
				cards_hand <- c(cards_hand, nwsFetchTry(ws,'cards'), nwsFetchTry(ws,'cards'))
				nwsStore(ws, user, cards_hand)
        		nwsStore(ws, 'penalty', TRUE) 
			}

			if( played_number =='rygb4+' && nwsFindTry(ws, 'penalty') == FALSE){
				cat("You got 4 penalty cards\n")
				cards_hand <- c(cards_hand, nwsFetchTry(ws,'cards'), nwsFetchTry(ws,'cards'),nwsFetchTry(ws,'cards'), nwsFetchTry(ws,'cards'))
				nwsStore(ws, user, cards_hand)
				nwsStore(ws, 'penalty', TRUE)
			}
			cards_hand <- nwsFindTry(ws, user)
			cat("Hand:", sort(unlist(cards_hand)), "\n") #sorted output
			
			# PLAY CARD
			tmp <- .playUnoCard(cards_hand, played, 
					computerPlayer=computerPlayer, computerPlayerFunction=computerPlayerFunction)
			card_play <- tmp[[1]]
			card_play_save <- tmp[[2]]
			
			#ACTION DEPENDING ON CARD TYPE
			tmp <- .playUnoAction(ws, card_play, card_play_save, user, cards_hand, played, NO)
			card_play<- tmp[[1]]
			NO <- tmp[[2]]
			playerInAction <- nwsFindTry(ws, 'player_in_action')		
		}
	}

	# End of game, small output
	if( nwsFindTry(ws,'winner') == user ){
		cat("!! CONGRATULATION,", nwsFindTry(ws,'winner'), " you won !!\n")
	} else
		cat("Sorry you lost, winner:", nwsFindTry(ws,'winner'), "\n")
	
	# close nws connection
	nwsClose(ws)
}

######################################################
# Computer Player for UNO
# very stupid - RANDOM
#####################################################
computerPlayerUNO <- function(hand, card_played)
{
	# try for every card in the hand
	for( h in 1:length(hand)){
		#split cards for color and numner
		hand_color <- strsplit(hand[h], "-")[[1]][1] 
 		hand_number <- strsplit(hand[h], "-")[[1]][2]
		played_color <- strsplit(card_played, "-")[[1]][1]
		played_number <- strsplit(card_played, "-")[[1]][2]
		# if color or number matches, return
		if(hand_color == played_color || hand_number == played_number)
			return(list(selectedCard=hand[h], playedCard=hand[h]))
		# if color wish card, randomly choose one color
		if(hand_color == "rybg")
			return(list(selectedCard=hand[h], playedCard=sample(c("red-rybg", "yellow-rybg", "blue-rybg", "green-rybg"),1)))
	}	
	# if there is no matching card in the hand, NO card can be played
	return(list(selectedCard="NO", playedCard="NO"))
}

##################################################
# Function to move played cards back to the stack
################################################
.check_card_stack <- function(ws, played, cards, number_cards=5)
{		
	tmp <- nwsListVars(ws, wsName=ws@wsName, showDataFrame=TRUE)
	ncards_stack <- tmp[tmp[,1]=="cards",]$"NumValues"
	if( ncards_stack < number_cards ){
		last_played_card <- nwsFetchTry(ws, played)
		rest_played <- vector()
		i=1
		while( !is.null(tmp <- nwsFetchTry(ws,played))){
			rest_played[i] <- tmp 
			i <- i + 1
		}
		rest_played <- sample(rest_played)
		for(i in 1:length(rest_played))
			nwsStore(ws, cards, rest_played[i])
		nwsStore(ws, played, last_played_card)
	}
}

##################################################
# Function to get played card
################################################
.playUnoCard <- function(cards_hand, played, 
		computerPlayer=FALSE, computerPlayerFunction=computerPlayerUNO)
{
  if(computerPlayer == TRUE){
		#for computer player
		tmp <- computerPlayerFunction(cards_hand, played)
		card_play <- tmp$selectedCard
		card_play_save <- tmp$playedCard
		cat("Play:", card_play_save, "\n")
		# computer player to fast for NWS
		Sys.sleep(0.2)
	} else{
		# for user
		card_play <- readline("Play: ")
		# TODO 
		# check ob gÃ¼ltige karte
		# ask for
		if(card_play=="rybg-0"){
			# ask for color by wish card
			col <- readline("Color: ")
			card_play_save <- paste(col, "rygb", sep="-")
		}else if(card_play=="rybg-4+"){
			# ask for color by wish card
			col <- readline("Color: ")
			card_play_save <- paste(col, "rygb4+", sep="-")
		}else
			card_play_save <- card_play
	}
	return(list(card_play,card_play_save))
}

##############################################################
# Function for Action depending on card
##############################################################
.playUnoAction <- function(ws, card_play, card_play_save, user, cards_hand, played, NO)
{
	require(nws)
	#split for color and number
	tmp <- strsplit(card_play_save, "-")
	card_play_color <- tmp[[1]][1]
	if( is.na(tmp[[1]][2]) )
		card_play_number <- ""
	else
		card_play_number <- tmp[[1]][2]
	played_color <- strsplit(unlist(played), "-")[[1]][1]
	played_number <- strsplit(unlist(played), "-")[[1]][2]
	
	playerInAction <- user
	
	if(card_play=="NO"){
		# if there is no matching card in the hand
		if(NO==0){
			# in first time get new card 
			cards_hand <- c(cards_hand, nwsFetchTry(ws,'cards'))
			nwsStore(ws, user, cards_hand) 
			NO <- 1
			card_play<-""
			#no player rotation
		} else if(NO==1){
			# in second time, do not get new card
			# rotate player
			playerInAction <- nwsFetchTry(ws, 'players')
			nwsStore(ws, 'players', playerInAction)
			nwsStore(ws, 'player_in_action', playerInAction)
			#card_play=NO
		}
		
	}else if(card_play=="rybg-0"){# && played_color == card_play_color){
		#remove card from hand
		cards_hand <- cards_hand[-which(cards_hand==card_play)]
		nwsStore(ws, user, cards_hand)
		# play card
		nwsStore(ws, 'played', card_play_save)
		#check for winner and goto next player
		if(length(cards_hand) != 0 ){
			# rotate player
			playerInAction <- nwsFetchTry(ws, 'players')
			nwsStore(ws, 'players', playerInAction)
			nwsStore(ws, 'player_in_action', playerInAction)
		} else
			nwsStore(ws, 'winner', playerInAction)
		
	}else if(card_play=="rybg-4+" ){#&& played_color == card_play_color){
		#remove card from hand
		cards_hand <- cards_hand[-which(cards_hand==card_play)]
		nwsStore(ws, user, cards_hand)
		# play card
		nwsStore(ws, 'played', card_play_save)
		#penalty has not been given
    		nwsStore(ws, 'penalty', FALSE)
		#check for winner and goto next player
		if(length(cards_hand) != 0 ){
			# rotate player
			playerInAction <- nwsFetchTry(ws, 'players')
			nwsStore(ws, 'players', playerInAction)
			nwsStore(ws, 'player_in_action', playerInAction)
		} else
			nwsStore(ws, 'winner', playerInAction)
		
	}else if(card_play_number == "BREAK" && (played_color == card_play_color || card_play_number == played_number)){
		#remove card from hand
		cards_hand <- cards_hand[-which(cards_hand==card_play)]
		nwsStore(ws, user, cards_hand)
		# play card
		nwsStore(ws, 'played', card_play_save)
		#check for winner and goto next player
		if(length(cards_hand) != 0 ){
			# rotate player 2 times
			playerInAction <- nwsFetchTry(ws, 'players')
			nwsStore(ws, 'players', playerInAction)
			playerInAction <- nwsFetchTry(ws, 'players')
			nwsStore(ws, 'players', playerInAction)
			nwsStore(ws, 'player_in_action', playerInAction)
		} else
			nwsStore(ws, 'winner', playerInAction)
		
	}else if(card_play_number=="BACK" && (played_color == card_play_color || card_play_number == played_number)){
		#remove card from hand
		cards_hand <- cards_hand[-which(cards_hand==card_play)]
		nwsStore(ws, user, cards_hand)
		# play card
		nwsStore(ws, 'played', card_play_save) 
		#check for winner
    		if(length(cards_hand) != 0 ){
	    	#if there are only two players, they just don´t have to rotate
     		 if(length(nwsFind(ws, 'players_logedin')) > 2){			
      		#  rotate all players
			players_tmp <- vector()
			i=1
			while( !is.null(tmp <- nwsFetchTry(ws,'players'))){
				players_tmp[i] <- tmp 
				i <- i + 1
			}
			for(i in (length(players_tmp)-1):1){
				nwsStore(ws, 'players', players_tmp[i])
			}
			nwsStore(ws, 'players', players_tmp[length(players_tmp)])
			playerInAction <- nwsFetchTry(ws, 'players')
			nwsStore(ws, 'players', playerInAction)
			nwsStore(ws, 'player_in_action', playerInAction)
			}
		} else
			nwsStore(ws, 'winner', playerInAction)
		
  	}else if(card_play_number=="2+" && card_play_number == played_number){
		#remove card from hand
		cards_hand <- cards_hand[-which(cards_hand==card_play)]
		nwsStore(ws, user, cards_hand)
		# play card
		nwsStore(ws, 'played', card_play_save)  
		#penalty has not been given
    		nwsStore(ws, 'penalty', FALSE)
		#check for winner and goto next player
		if(length(cards_hand) != 0 ){
			# rotate player
			playerInAction <- nwsFetchTry(ws, 'players')
			nwsStore(ws, 'players', playerInAction)
			nwsStore(ws, 'player_in_action', playerInAction)
		} else
			nwsStore(ws, 'winner', playerInAction)
			
	}else if(card_play_number=="2+" && played_color == card_play_color){
		#remove card from hand
		cards_hand <- cards_hand[-which(cards_hand==card_play)]
		nwsStore(ws, user, cards_hand)
		# play card
		nwsStore(ws, 'played', card_play_save)  
		#penalty has not been given
    		nwsStore(ws, 'penalty', FALSE)
		#check for winner and goto next player
		if(length(cards_hand) != 0 ){
			# rotate player
			playerInAction <- nwsFetchTry(ws, 'players')
			nwsStore(ws, 'players', playerInAction)
			nwsStore(ws, 'player_in_action', playerInAction)
		} else
			nwsStore(ws, 'winner', playerInAction)
		
	}else if(!(card_play %in% unlist(cards_hand))){
		cat("\tCard not in your cards!\n\t'NO' for new card.\n")
		card_play <- ""
		
	}else if( (card_play_color == played_color) || (card_play_number == played_number) ) {
		# play normal card with color and number
		#remove card from hand
		cards_hand <- cards_hand[-which(cards_hand==card_play)]
		nwsStore(ws, user, cards_hand)
		# play card
		nwsStore(ws, 'played', card_play_save)
		#check for winner and goto next player
		if(length(cards_hand) != 0 ){
			# rotate player
			playerInAction <- nwsFetchTry(ws, 'players')
			nwsStore(ws, 'players', playerInAction)
			nwsStore(ws, 'player_in_action', playerInAction)
		} else
			nwsStore(ws, 'winner', playerInAction)
		
	}else if(played_color != card_play_color && played_number != card_play_number){
		cat("\tCard does not match!\n")
		card_play <- ""

	} else
		warning("Error : unknown use case!")
	return(list(card_play, NO))
}
##################################################
# Function to summarize the points of the handcards
##################################################
.calculate <- function(ws)
{
	require(nws)
	#Set startvaluevector for variable points
	tmp<-vector()
	pointsvec<-vector()

	for(i in 1:length(nwsFindTry(ws, 'players_logedin'))){
		tmp<-c(tmp,100)
	}
	pointsvec<-nwsFetchTry(ws, 'points',tmp)
	if(is.null(pointsvec)){
		pointsvec<-tmp
	}
	names(pointsvec)<-nwsFindTry(ws,'players_logedin')
	for(p in nwsFindTry(ws,'players_logedin')){
	#Summarize Handcards
		cards_hand<-nwsFindTry(ws,p)
		handsum<-0;
		handsum<-handsum+(50*length(grep("rybg",cards_hand)))
		handsum<-handsum+(20*(length(grep("BREAK",cards_hand))+length(grep("BACK",cards_hand))+(length(grep("2",cards_hand))-length(grep("2$",cards_hand)))))
		handsum<-handsum+(length(grep("1",cards_hand)))
		handsum<-handsum+(2*length(grep("2$",cards_hand)))
		handsum<-handsum+(3*length(grep("3",cards_hand)))
		handsum<-handsum+(4*length(grep("4$",cards_hand))) 
		handsum<-handsum+(5*length(grep("5",cards_hand)))
		handsum<-handsum+(6*length(grep("6",cards_hand)))
      		handsum<-handsum+(7*length(grep("7",cards_hand)))
      		handsum<-handsum+(8*length(grep("8",cards_hand)))
      		handsum<-handsum+(9*length(grep("9",cards_hand)))
		pointsvec[p]<-handsum
	}
	nwsStore(ws,'points',pointsvec)
}