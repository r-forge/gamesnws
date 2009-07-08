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
	while ( is.null(players_logedin <- nwsFetchTry(ws, 'players_logedin')) ){}
	if( any(players_logedin == user)){
		nwsStore(ws, 'players_logedin', players_logedin)
		stop("User already exists, please change user name!")
	}else if( !any(players_logedin == 'master')){
		nwsStore(ws, 'players_logedin', players_logedin)
		stop("Game is already running, no more login possible!")
	}else
		nwsStore(ws, 'players_logedin', c(players_logedin, user))

	# Wait for other players and game start
	# if the player (=user) got cards, start the game
	# additional get cards for player (=user)
	# and declare card-varable for user
	nwsDeclare(ws, user, 'lifo')
	cat("Wait for other players and game start:\n")
	cards <- .txtProgressBarNWS(ws, user) 
 
	# GAME
	# play the game, as long as there is no winner
	while(is.null(nwsFindTry(ws, "winner")) ){
		# look for player in Action
		# if you are user in action, there is no winner and there is card at the table,
		# than play your cards
		pb <- txtProgressBar(min=0, max=10, style=1, width=20)
		i <- run <- 0
		while( nwsFindTry(ws, 'player_in_action')!=user && is.null(nwsFindTry(ws, "winner")) ){
			setTxtProgressBar(pb, i)
			if(run==0)
				i <- i+1
			else i <- i-1
			if(i==10) run<-1
			if(i==0) run<-0
			Sys.sleep(0.1)
		}
		close(pb)
		playerInAction <- nwsFindTry(ws, 'player_in_action')
		
		
		#Play Card if there is no winner
		card_play <- ""
		NO <- 0
		while(card_play=="" && is.null(nwsFindTry(ws, "winner")) && playerInAction==user ){
			
			# SOME OUTPUT
			# get all players
			players <- nwsFindTry(ws, "players_logedin")
			cat("Players: ")
			for(p in players)
				cat(p, "(",length(nwsFindTry(ws,p)),"): ", sep="")
			cat("\n")
			# get played card
			played <- nwsFindTry(ws, 'played')
			#split for color and number
			played_color <- strsplit(unlist(played), "-")[[1]][1]
			played_number <- strsplit(unlist(played), "-")[[1]][2]
			cat("Table:", played,"\n")
			
			# PENALTIE
			if( played_number =='2+'){
				cat("You got 2 penalty cards\n")
				cards <- c(cards, nwsFetchTry(ws,"cards"), nwsFetchTry(ws,"cards"))
				nwsStore(ws, user, cards) 
			}
			if( played_number =='rygb4+'){
				cat("You got 4 penalty cards\n")
				cards <- c(cards, nwsFetchTry(ws,"cards"), nwsFetchTry(ws,"cards"),nwsFetchTry(ws,"cards"), nwsFetchTry(ws,"cards"))
				nwsStore(ws, user, cards)
			}
			cat("Hand:", unlist(cards), "\n")
			
			# PLAY CARD
			if(computerPlayer == TRUE){
				#for computer player
				tmp <- computerPlayerFunction(cards, played)
				card_play <- tmp$sel
				card_play_save <- tmp$played
				cat("Play:", card_play_save, "\n")
				# computer palyer to fast for NWS
				Sys.sleep(0.5)
			} else{
				# for user
				card_play <- readline("Play: ")
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
			
			#ACTION DEPENDONG ON CARD TYPE
			#split for color and number
			card_play_color <- strsplit(card_play, "-")[[1]][1]
			card_play_number <- strsplit(card_play, "-")[[1]][2]
			
			if(card_play=="NO"){
				# if there is no matching card in the hand
				if(NO==0){
					# in first time get new card 
					cards <- c(cards, nwsFetchTry(ws,"cards"))
					nwsStore(ws, user, cards) 
					NO <- 1
					card_play<-""
					#no player rotation
				} else if(NO==1){
					# in second time, do not get new card
					# rotate player
					playerInAction <- nwsFetchTry(ws, 'players')
					nwsStore(ws, "players", playerInAction)
					nwsStore(ws, 'player_in_action', playerInAction)
					#card_play=NO
				}
				
			}else if(card_play=="rybg-0"){
				#remove card from hand
				cards <- cards[-which(cards==card_play)]
				nwsStore(ws, user, cards)
				# play card
				nwsStore(ws, "played", card_play_save)
				#check for winner and goto next player
				if(length(cards) != 0 ){
					# rotate player
					playerInAction <- nwsFetchTry(ws, 'players')
					nwsStore(ws, "players", playerInAction)
					nwsStore(ws, 'player_in_action', playerInAction)
				} else
					nwsStore(ws, "winner", playerInAction)
				
			}else if(card_play=="rybg-4+"){
				#remove card from hand
				cards <- cards[-which(cards==card_play)]
				nwsStore(ws, user, cards)
				# play card
				nwsStore(ws, "played", card_play_save)
				#check for winner and goto next player
				if(length(cards) != 0 ){
					# rotate player
					playerInAction <- nwsFetchTry(ws, 'players')
					nwsStore(ws, "players", playerInAction)
					nwsStore(ws, 'player_in_action', playerInAction)
				} else
					nwsStore(ws, "winner", playerInAction)
	
			}else if(card_play_number=="BREAK"){
				#remove card from hand
				cards <- cards[-which(cards==card_play)]
				nwsStore(ws, user, cards)
				# play card
				nwsStore(ws, "played", card_play_save)
				#check for winner and goto next player
				if(length(cards) != 0 ){
					# rotate player 2 times
					playerInAction <- nwsFetchTry(ws, 'players')
					nwsStore(ws, "players", playerInAction)
					playerInAction <- nwsFetchTry(ws, 'players')
					nwsStore(ws, "players", playerInAction)
					nwsStore(ws, 'player_in_action', playerInAction)
				} else
					nwsStore(ws, "winner", playerInAction)
	
			}else if(card_play_number=="BACK"){
				#remove card from hand
				cards <- cards[-which(cards==card_play)]
				nwsStore(ws, user, cards)
				# play card
				nwsStore(ws, "played", card_play_save)
				#check for winner and goto next player
				if(length(cards) != 0 ){
					#  rotate all players
					players_tmp <- vector()
					i=1
					while( !is.null(tmp <- nwsFetchTry(ws,'players'))){
						players_tmp[i] <- tmp 
						i <- i + 1
					}
					print(players_tmp)
					for(i in (length(players_tmp)-1):1){
						nwsStore(ws, 'players', players_tmp[i])
					}
					nwsStore(ws, 'players', players_tmp[length(players_tmp)])
					playerInAction <- nwsFetchTry(ws, 'players')
					nwsStore(ws, "players", playerInAction)
					nwsStore(ws, 'player_in_action', playerInAction)
				} else
					nwsStore(ws, "winner", playerInAction)
	
			}else if(card_play_number=="2+"){
				#remove card from hand
				cards <- cards[-which(cards==card_play)]
				nwsStore(ws, user, cards)
				# play card
				nwsStore(ws, "played", card_play_save)
				#check for winner and goto next player
				if(length(cards) != 0 ){
					# rotate player
					playerInAction <- nwsFetchTry(ws, 'players')
					nwsStore(ws, "players", playerInAction)
					nwsStore(ws, 'player_in_action', playerInAction)
				} else
					nwsStore(ws, "winner", playerInAction)
	
			}else if(!(card_play %in% unlist(cards))){
				cat("\tCard not in your cards!\n\t'NO' for new card.\n")
				card_play <- ""

			}else if(played_color != card_play_color && played_number != card_play_number){
				cat("\tCard does not match!\n")
				card_play <- ""

			}else {
				# play normal card with color and number
				#remove card from hand
				cards <- cards[-which(cards==card_play)]
				nwsStore(ws, user, cards)
				# play card
				nwsStore(ws, "played", card_play_save)
				#check for winner and goto next player
				if(length(cards) != 0 ){
					# rotate player
					playerInAction <- nwsFetchTry(ws, 'players')
					nwsStore(ws, "players", playerInAction)
					nwsStore(ws, 'player_in_action', playerInAction)
				} else
					nwsStore(ws, "winner", playerInAction)
			}
		}
	}

	# End of game, small output
	if(length(cards) == 0 ){
		cat("!! CONGRATULATION,", nwsFindTry(ws,"winner"), " you won !!\n")
	} else
		cat("Sorry you lost, winner:", nwsFindTry(ws,"winner"), "\n")
	
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
			return(list(sel=hand[h], played=hand[h]))
		# if color wish card, randomly choose one color
		if(hand_color == "rybg")
			return(list(sel=hand[h], played=sample(c("red-rybg", "yellow-rybg", "blue-rybg", "green-rybg"),1)))
	}	
	# if there is no matching card in the hand, NO card can be played
	return(list(sel="NO", played="NO"))
}


