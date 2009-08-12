
player <- function(name, user=Sys.info()["user"], ...)
{
	require(nws)
	
	cat("\ngreat idea, will be implemented soon\n")
}

playPokerOLD <- function(name, user=Sys.info()["user"], money=500, ...)
{
	require(nws)
	# Login to nws
	ws <<- netWorkSpace(name, ...)
	
	# set user
	players <- nwsFetchTry(ws, 'players')
	players <- c(players, user)
	nwsStore(ws, 'players', players)
	#TODO Check for users
	
	# set nws for user
	wsUser <<- netWorkSpace(paste(name, user, sep='_'), ...)
	nwsDeclare(wsUser, 'money', 'lifo')
	nwsStore(wsUser, 'money', money)
	nwsDeclare(wsUser, 'operation', "single")
	bigBlind <- nwsFindTry(ws,'bigBlind')
	cat("Big Blind:", bigBlind, "\n")
	
	# Wait for other players
	cat("Wait for other players:\n")
	state <- nwsFindTry(ws, 'state')
	pb <- txtProgressBar(min=0, max=10, style=1, width=20)
	i <- run <- 0
	while( state != 'cards' ){
		setTxtProgressBar(pb, i)
		if(run==0)
			i <- i+1
		else i <- i-1
		if(i==10) run<-1
		if(i==0) run<-0
		state <- nwsFindTry(ws, 'state')
	}
	close(pb);cat("\n")
	
	# Get cards
	players <- nwsFindTry(ws, 'players')
	for(p in 1:length(players)){
		wsUserGegner <- netWorkSpace(paste(ws@wsName,players[p],sep="_"), serverHost=ws@server@serverHost)
		if( players[p] == user ){
			hand <- nwsFetch(wsUser, 'hand')
			cat(players[p], ":", hand, sep=" ")
		}
		else 
			cat(players[p], ": XX XX", sep=" ")
		if(p==1) cat(" - pay:", bigBlind/2, "(", nwsFindTry(wsUserGegner, 'money'),")")
		if(p==2) cat(" - pay:", bigBlind, "(", nwsFindTry(wsUserGegner, 'money'),")")
		cat("\n")
		nwsClose(wsUserGegner)
	}
	
	# Wait for other players
	cat("Wait for other players:\n")
	operation <- nwsFindTry(wsUser, 'operation')
	pb <- txtProgressBar(min=0, max=10, style=1, width=20)
	i <- run <- 0
	while( is.null(operation) ){
		setTxtProgressBar(pb, i)
		if(run==0)
			i <- i+1
		else i <- i-1
		if(i==10) run<-1
		if(i==0) run<-0
		operation <- nwsFindTry(wsUser, 'operation')
	}
	close(pb)
	
	#Operation
	r <- ""
	while(r==""){
		r <- readline("raise [r], call [c], fold [f]")
		if(r=="r"){
			nwsStore(wsUser, 'operation', "r")
		}
		if(r=="c"){	
			nwsStore(wsUser, 'operation', "c")
		}
	}
	
	# Wait for other players and new cards
	cat("Wait for other players and new cards:\n")
	cardsPlayed <- nwsFindTry(ws, 'cardsPlayed')
	pb <- txtProgressBar(min=0, max=10, style=1, width=20)
	i <- run <- 0
	while( is.null(cardsPlayed) ){
		setTxtProgressBar(pb, i)
		if(run==0)
			i <- i+1
		else i <- i-1
		if(i==10) run<-1
		if(i==0) run<-0
		cardsPlayed <- nwsFindTry(ws, 'cardsPlayed')
	}
	close(pb)
	
	# Get cards and infos
	cat("Table:", cardsPlayed, "\n")
	for(p in 1:length(players)){	
		wsUserGegner <- netWorkSpace(paste(ws@wsName,players[p],sep="_"), serverHost=ws@server@serverHost)
		operation <- nwsFindTry(wsUserGegner, 'operation')
		if( players[p] == user ){
			cat(players[p], ":", hand, ":", operation, "\n", sep=" ")
		}
		else cat(players[p], ": XX XX :", operation, " \n")
		nwsClose(wsUserGegner)
	}
	
	
	return(NULL)
}

