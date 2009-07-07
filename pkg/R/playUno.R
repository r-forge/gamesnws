playUno <- function(name, user=Sys.info()["user"], computerPlayer=FALSE, computerPlayerFunction=cpf, ...)
{
	require(nws)
	# Login to nws
	ws <<- netWorkSpace(name, ...)
	
	# set user to nws
	nwsStore(ws, 'players', user)
	#TODO Check for users

	# Wait for other players and game start
	# if the player (=user) got cards, start the game
	# additional get cards for player (=user)
	cat("Wait for other players and game start:\n")
	cards <- .txtProgressBarNWS(ws, user) 
 
	# GAME
	# play the game, as long as you there is no winner
	while(is.null(nwsFindTry(ws, "winner")) ){
		# look for player in Action
		# if you are user in action, there is no winner and there is card at the table,
		# than play your cards
		playerInAction <- nwsFindTry(ws, 'players')
		pb <- txtProgressBar(min=0, max=10, style=1, width=20)
		i <- run <- 0
		while( playerInAction != user && is.null(nwsFindTry(ws, "winner")) &&!is.null(nwsFindTry(ws, "played"))  ){
			setTxtProgressBar(pb, i)
			if(run==0)
				i <- i+1
			else i <- i-1
			if(i==10) run<-1
			if(i==0) run<-0
			playerInAction <- nwsFindTry(ws, 'players')
		}
		close(pb)

		if( playerInAction == user ){ #entfernen?
			#Play Card
			give <- ""
			NO <- 0
			while(give==""){
				played <- nwsFindTry(ws, 'played')
				players <- nwsFindTry(ws, "startplayers")
				cat("Players: ")
				for(p in players)
					cat(p, "(",length(nwsFindTry(ws,p)),"): ", sep="")
				cat("\n")
				cat("Table:", played,"\n")
				cat("Hand:", unlist(cards), "\n")
				if(computerPlayer == TRUE){
					tmp <- cpf(cards, played)
					give <- tmp$sel
					give_save <- tmp$played
					cat("Play:", give_save, "\n")
				} else{
					give <- readline("Play: ")
					if(give=="rybg-0"){
						col <- readline("Color: ")
						give_save <- paste(col, "rygb", sep="-")
					} else
						give_save <- give
					
				}	

				played_color <- strsplit(unlist(played), "-")[[1]][1]
				played_number <- strsplit(unlist(played), "-")[[1]][2]
				give_color <- strsplit(give, "-")[[1]][1]
				give_number <- strsplit(give, "-")[[1]][2]

				if(give=="NO"){
					if(NO==0){
						newcard <- nwsFetchTry(ws,"cards")
						cards <- c(cards, newcard)
						nwsStore(ws, user, cards) 
						NO <- 1
						#no player rotation
					} else if(NO==1){
						# rotate player
						playerInAction <- nwsFetchTry(ws, 'players')
						nwsStore(ws, "players", playerInAction)
						NO <- 0
					}
				} else if(give=="rybg-0"){
					nwsStore(ws, "played", give_save)
					id <- which(cards==give)
					cards <- cards[-id]
					nwsStore(ws, user, cards)
 					if(length(cards) != 0 ){
						# rotate player
						playerInAction <- nwsFetchTry(ws, 'players')
						nwsStore(ws, "players", playerInAction)
					} else
						nwsStore(ws, "winner", playerInAction)
				} else if(!(give %in% unlist(cards))){
					cat("\tCard not in your cards!\n\t'NO' for new card.\n")
					give <- ""
				} else if(played_color != give_color && played_number != give_number){
					cat("\tCard does not match!\n")
					give <- ""
				} else {
					# play card
					nwsStore(ws, "played", give_save)
					id <- which(cards==give)
					cards <- cards[-id]
					nwsStore(ws, user, cards)
 					if(length(cards) != 0 ){
						# rotate player
						playerInAction <- nwsFetchTry(ws, 'players')
						nwsStore(ws, "players", playerInAction)
					} else
						nwsStore(ws, "winner", playerInAction)
				}
			}
		}
	}

	# End
	if(length(cards) == 0 ){
		cat("!! CONGRATULATION you won !!\n")
	} else
		cat("Sorry you lost, winner:", nwsFindTry(ws,"winner"), "\n")
	nwsClose(ws)
}


cpf <- function(hand, card_played)
{
	for( h in 1:length(hand)){
		hand_color <- strsplit(hand[h], "-")[[1]][1] 
 		hand_number <- strsplit(hand[h], "-")[[1]][2]
		played_color <- strsplit(card_played, "-")[[1]][1]
		played_number <- strsplit(card_played, "-")[[1]][2]
		if(hand_color == played_color || hand_number == played_number)
			return(list(sel=hand[h], played=hand[h]))
		if(hand_color == "rybg")
			return(list(sel=hand[h], played=sample(c("red-rybg", "yellow-rybg", "blue-rybg", "green-rybg"),1)))
	}	
	return(list(sel="NO", played="NO"))
}


