createPokerGame <- function(name, ...)
{
	require(nws)

	cat("\ngreat idea, will be implemented soon\n")
}

createPokerGameOLD <- function(name, bigBlind=50,...)
{
	require(nws)
	# create nws
	ws <<- netWorkSpace(name, ...)
	
	# put inital data to the nws
	cards <- list( 'Pik-A', 'Pik-K', 'Pik-Q', 'Pik-J', 
			'Pik-10', 'Pik-9', 'Pik-8', 'Pik-7', 'Pik-6', 'Pik-5', 'Pik-4', 'Pik-3', 'Pik-2',
			'Herz-A', 'Herz-K', 'Herz-Q', 'Herz-J', 
			'Herz-10', 'Herz-9', 'Herz-8', 'Herz-7', 'Herz-6', 'Herz-5', 'Herz-4', 'Herz-3', 'Herz-2',
			'Caro-A', 'Caro-K', 'Caro-Q', 'Caro-J', 
			'Caro-10', 'Caro-9', 'Caro-8', 'Caro-7', 'Caro-6', 'Caro-5', 'Caro-4', 'Caro-3', 'Caro-2',
			'Kreuz-A', 'Kreuz-K', 'Kreuz-Q', 'Kreuz-J', 
			'Kreuz-10', 'Kreuz-9', 'Kreuz-8', 'Kreuz-7', 'Kreuz-6', 'Kreuz-5', 'Kreuz-4', 'Kreuz-3', 'Kreuz-2')
	
	nwsStore(ws, 'cards', cards)
	nwsStore(ws, 'bigBlind', bigBlind)
	nwsDeclare(ws, 'pott', 'single')
	nwsStore(ws, 'pott', 0)
	nwsDeclare(ws, 'cardsPlayed', 'single')
	nwsStore(ws, 'players', NULL)
	nwsDeclare(ws, 'state', 'single')
	nwsStore(ws, 'state', "wait")
	
	cat("Send the name and the server address to the other players!\n")
	
	r <- ""
	while(r!="e"){
		r <- readline("Players online [o], Start Game [s], End Game [e]")
		if(r=="o"){
			players <- nwsFindTry(ws, 'players')
			cat("Players:", players, "\n")
		}
		if(r=="s"){	
			playMaster(ws, bigBlind)
		}
	}
	
	
	#######################################
	#End
	
	# Check for other players	
	players <- nwsFind(ws, 'players')
	
	#TODO löschen
	if(is.null(NULL)){ 
		players <- nwsFetchTry(ws, 'players')
		for( p in players)
			nwsDeleteWs(ws@server, paste(name, p, sep="_"))
		nwsDeleteWs(ws@server, name)
		nwsClose(ws)
	} else warning("There are other players online: TODO")
}

playMaster <- function(ws, bigBlind){
	
	nwsStore(ws, 'state', "cards")
	
	#Put cards to player
	cat("\tGive Cards\n")
	cards <- nwsFindTry(ws, "cards")
	players <- nwsFindTry(ws, "players")
	scards <- unlist(sample(cards, 2*length(players)))
	scards <- split(scards, 1:length(players))
	
	for( p in 1:length(players)){
		wsUser <- netWorkSpace(paste(ws@wsName,players[p],sep="_"), serverHost=ws@server@serverHost)
		# pay blinds
		if(p==1){
			nwsStore(wsUser, "money", nwsFindTry(wsUser, "money")-bigBlind/2 )
			nwsStore(ws, "pott", nwsFetchTry(ws, "pott")+bigBlind/2)
		}
		
		if(p==2){
			nwsStore(wsUser, "money", nwsFindTry(wsUser, "money")-bigBlind )
			nwsStore(ws, "pott", nwsFetchTry(ws, "pott")+bigBlind/2)
		}
		nwsStore(wsUser, "hand", unlist(scards[p]))
		nwsClose(wsUser)
		
	}
	
	#ask players for operation
	pb <- txtProgressBar(min=0, max=10, style=1, width=20)
	for( p in 1:length(players)){
		wsUser <- netWorkSpace(paste(ws@wsName,players[p],sep="_"), serverHost=ws@server@serverHost)
		nwsStore(wsUser, "operation", c("r", "c", "f"))
		operation <- nwsFindTry(wsUser, 'operation')
		i <- run <- 0
		while( length(operation)!=1 ){
			setTxtProgressBar(pb, i)
			if(run==0)
				i <- i+1
			else i <- i-1
			if(i==10) run<-1
			if(i==0) run<-0
			operation <- nwsFindTry(wsUser, 'operation')
		}
	}
	close(pb)
	
	# aufdecken von Karten
	id <- which(unlist(cards) %in% unlist(scards))
	cards <- unlist(cards)[-id]
	table_cards <- unlist(sample(cards, 3))
	nwsStore(ws, "cardsPlayed", table_cards)
}