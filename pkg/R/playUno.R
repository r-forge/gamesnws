###########################################################
# Main Function to play UNO for user
###########################################################
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
	# create playground
	#options(show.error.messages = FALSE
	players<-nwsFindTry(ws,'players_logedin')
	if(computerPlayer==TRUE){
		for(c in 1:length(players)){
			if(players[[c]]==user){
				cpu<-nwsFindTry(ws,'cpu')
				cpu[c]<-TRUE
				nwsStore(ws,'cpu',cpu)
			}
		}  
	}
	graphics <- nwsFindTry(ws, 'graphics')
	if(graphics){
	osinfo<-Sys.info()[1]
	if(grepl("Windows",osinfo)){
		windows(height=12,width=14,xpos=1,ypos=1,canvas="mediumseagreen",bg="transparent")
	}
	else{
	   X11(height=12,width=14,xpos=1,ypos=1,canvas="mediumseagreen",bg="transparent")
	}
	#options(show.error.messages = TRUE)
	#playercoord<-list(c(0.5,0.05),c(0.5,0.95),c(0.05,0.35),c(0.95,0.65),c(0.05,0.65),c(0.95,0.35),c(0.15,0.1),
	#c(0.85,0.9),c(0.85,0.1),c(0.15,0.9))
	plot.new()
	text(x=0.5,y=0.6,label="Welcome \nto UNO!",col="red",cex=3)
	text(x=0.5,y=0.4,label="Please wait until \nit's your turn...",col="black",cex=1.5)	
	}
	while(is.null(nwsFindTry(ws, 'winner')) ){
# 		players<-nwsFindTry(ws,'players_logedin')		
		string<-""
 		players<-nwsFindTry(ws,'players_logedin')
    		for(p in players){
      			string<-c(string,p)
    		}
    		playerInActionOld <- nwsFindTry(ws, 'player_in_action')
    		pb <- txtProgressBar(min=0, max=10, style=1, width=1,char=paste("Wait for ",playerInActionOld," to act"))
    		i <- 1
    		run <- 0
		while( nwsFindTry(ws, 'player_in_action')!=user && is.null(nwsFindTry(ws, 'winner')) ){
			playerInAction <- nwsFindTry(ws, 'player_in_action')
      			if(playerInActionOld!=playerInAction){# if player changed,new progressbar with player name
				close(pb)
				playerInActionOld<-playerInAction
				pb <- txtProgressBar(min=0, max=10, style=1, width=1,char=paste("Wait for ",playerInAction," to act"))
				i <- 1
				run <- 0
      			}
      			setTxtProgressBar(pb, i)
		  	if(run==0)          
				i <- i+1
		  	else 
        		i <- i-1
			if(i==10) run<-1
			if(i==1) run<-0
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
			cat(">>",p, " - (",length(nwsFindTry(ws,p))," card(s))<< ", sep="")
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
				cat("	Said Uno:",nwsFindTry(ws, 'uno'),"\n")
				cat("	Rules:",nwsFindTry(ws, 'rules'),"\n")
				cat("	Rulesbools:",nwsFindTry(ws, 'rulesbools'),"\n")
				for(p in players){
			  points <- nwsFindTry(ws,'points')
			  uno<-nwsFindTry(ws,'uno')
		    cat("(",p, " - (",length(nwsFindTry(ws,p))," card(s), ",points[p]," point(s), uno = ",uno[p],") ", sep="")
		    }
		    cat("\n")
		  }

			# get played card
			played <- nwsFindTry(ws, 'played')
			#split for color and number
			played_color <- strsplit(unlist(played), "-")[[1]][1]
			played_number <- strsplit(unlist(played), "-")[[1]][2]
			#draw tablecard
			cat("Table:", played,"\n")
			cards_hand <- nwsFindTry(ws, user)
			cat("Hand:", sort(unlist(cards_hand)), "\n") #sorted output
			# PENALTY
			pc<-FALSE
			if(graphics){.repaint(ws,cards_hand,user)}
				if(computerPlayer == FALSE){ # CPChange 
					if(played_number =='2+'&& nwsFind(ws, 'penalty') != 0){
						rulesbools<-nwsFindTry(ws, 'rulesbools')
						if(rulesbools[3]){
							readPC<-""
							while(readPC != "y" && readPC != "n"){           
								if(graphics==FALSE){readPC <- readline("Do you want to concatenate penalty?[y/n]")}
									else{readPC<-.askforpenalty()
									.repaint(ws,cards_hand,user)}
								if(readPC=="y"){
								pc<-TRUE
								}
								else if(readPC=="n"){
								pc<-FALSE
								}   
							}
						}
						if(!pc || !rulesbools[3]){
							pen<-nwsFetchTry(ws, 'penalty')
							.getpenalty(ws,pen,user,0)
							nwsStore(ws, 'penalty', 0)
							cards_hand <- nwsFindTry(ws, user)
						}       
					}
				
					if( played_number =='rybg4+' && nwsFindTry(ws, 'penalty') != 0){
						rulesbools<-nwsFindTry(ws, 'rulesbools')
						if(rulesbools[3]){
							readPC<-""
							while(readPC != "y" && readPC != "n"){
								if(graphics==FALSE){readPC <- readline("Do you want to concatenate penalty?[y/n]")}
									else{
										readPC<-.askforpenalty()
										.repaint(ws,cards_hand,user)
									}
									if(readPC=="y"){
									pc<-TRUE
									}
									else if(readPC=="n"){
									pc<-FALSE
									}   
								}
							} 
							if(!pc || !rulesbools[3]){
								pen<-nwsFetchTry(ws, 'penalty')
								.getpenalty(ws,pen,user,0)
								nwsStore(ws, 'penalty', 0)
								cards_hand <- nwsFindTry(ws, user)
							}       
						}
				}
				else{
					if(played_number =='2+'&& nwsFind(ws, 'penalty') != 0){
							pen<-nwsFetchTry(ws, 'penalty')
							.getpenalty(ws,pen,user,0)
							nwsStore(ws, 'penalty', 0)
							cards_hand <- nwsFindTry(ws, user)	
					}				
					if(played_number =='rybg4+'&& nwsFind(ws, 'penalty') != 0){
							pen<-nwsFetchTry(ws, 'penalty')
							.getpenalty(ws,pen,user,0)
							nwsStore(ws, 'penalty', 0)
							cards_hand <- nwsFindTry(ws, user)	
					}			


				}
			
			
			# PLAY CARD
# 			unovec<-nwsFindTry(ws, 'uno')
#     			unovec[playerInAction]<-FALSE
# 			nwsStore(ws, 'uno', unovec)
			tmp <- .playUnoCard(ws, cards_hand, played, 
					computerPlayer=computerPlayer, computerPlayerFunction=computerPlayerFunction,user)
			card_play <- tmp[[1]]
			card_play_save <- tmp[[2]]
			
      if(nwsFindTry(ws, 'penalty') != 0){
        tmp2 <- strsplit(card_play_save, "-")
  	     card_play_color <- tmp2[[1]][1]
  	     if( is.na(tmp2[[1]][2]) )
  		      card_play_number <- ""
  	     else
  		    card_play_number <- tmp2[[1]][2]
  			if(pc && !(card_play_number=="2+" || card_play_save=="rybg-4+")){
                pen<-nwsFetchTry(ws, 'penalty')
  							.getpenalty(ws,pen,user,0)
  							nwsStore(ws, 'penalty', 0)
  							cards_hand <- nwsFindTry(ws, user)	
        
        }
      }
			
			#ACTION DEPENDING ON CARD TYPE
			tmp <- .playUnoAction(ws, card_play, card_play_save, user, cards_hand, played, NO)
			card_play<- tmp[[1]]
			NO <- tmp[[2]]
			playerInAction <- nwsFindTry(ws, 'player_in_action')		
		}
	}

	# End of game, small output
	.calculate(ws)
	points<-nwsFindTry(ws,'points')
	sumpoints<-sum(points)
	if(graphics){polygon(c(0.3,0.3,0.7,0.7),c(0.3,0.7,0.7,0.3),col="blanchedalmond",border=NA)
		if( nwsFindTry(ws,'winner') == user ){
			text(x=0.5,y=0.6,label=paste("!! CONGRATULATION,", nwsFindTry(ws,'winner'), " you won with",sumpoints,"points!!\n"),cex=1)
		} else{
			text(x=0.5,y=0.6,label=paste("Sorry you lost, winner: ", nwsFindTry(ws,'winner'), ", with ",sumpoints," points!!\n",sep=""),cex=1)}
		#close windows/X11
		#Sys.sleep(2)
		if(computerPlayer==FALSE){
			clicked<-FALSE
			.drawsquarebutton(xval=0.5,yval=0.5,size=3,type=2,clicked=F,color="mediumseagreen")
			while(clicked==FALSE){
				klick<-locator(n=1)
				kx<-klick[[1]]
				ky<-klick[[2]]
				if(kx>(0.5-(3/100))&& kx<(0.5+(3/100)) && ky>(0.5-(3/100)) && ky<(0.5+(3/100))){
					.drawsquarebutton(xval=0.5,yval=0.5,size=3,type=2,clicked=T,color="mediumseagreen")
					Sys.sleep(0.5) 
					clicked<-TRUE  		
				}
			}
		} 	
		dev.off()
	}
	else{
		if( nwsFindTry(ws,'winner') == user ){
			cat("!! CONGRATULATION,", nwsFindTry(ws,'winner'), " you won with",sumpoints,"points!!\n")
		} else{
			cat("Sorry you lost, winner:", nwsFindTry(ws,'winner'), ",with",sumpoints,"points!!\n")}
	}
	# close nws connection
	nwsClose(ws)
}

######################################################
# Computer Player for UNO
# very stupid - RANDOM
#####################################################
computerPlayerUNO <- function(ws, hand, card_played)
{
  require(nws)
	unovec<-nwsFindTry(ws, 'uno')                                
	names(unovec)<- nwsFindTry(ws,'players_logedin')
  if(length(hand)==2 && !unovec[nwsFindTry(ws,'player_in_action')]){
    unovec[nwsFindTry(ws,'player_in_action')]<-TRUE
    nwsStore(ws,'uno',unovec)
    
  }
	 # try for every card in the hand                    #CPChange
	 for( h in 1:length(hand)){
		  #split cards for color and numner
		  hand_color <- strsplit(hand[h], "-")[[1]][1] 
 		  hand_number <- strsplit(hand[h], "-")[[1]][2]
		  played_color <- strsplit(card_played, "-")[[1]][1]
		  played_number <- strsplit(card_played, "-")[[1]][2]
		  # if color or number matches, return
		  if(hand_color == played_color || hand_number == played_number)
			 return(list(selectedCard=hand[h], playedCard=hand[h]))
		  # if color wish card, randomly choose one color #CPChange
      if(hand_color == "rybg" && played_number != "rybg")
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
.playUnoCard <- function(ws, cards_hand, played, 
		computerPlayer=FALSE, computerPlayerFunction=computerPlayerUNO,user)
{
  graphics <- nwsFindTry(ws, 'graphics')
  if(computerPlayer == TRUE){
		#for computer player
		tmp <- computerPlayerFunction(ws, cards_hand, played)
		card_play <- tmp$selectedCard
		card_play_save <- tmp$playedCard
		if(graphics==FALSE){cat("Play:", card_play_save, "\n")}
		# computer player to fast for NWS
		Sys.sleep(0.2)
	} else{
		# for user
		if(graphics==FALSE){card_play <- readline("Play: ")}
		#wait for user to click on a card or a button
		else{
			success<-F
			buttonlist<-.getbuttonlist()
			while(success==F){
				locatorBell=F
				klick<-locator(n=1)
				kx<-klick[[1]]
				ky<-klick[[2]]
				#check if card has been clicked	
				cardcoord<-.calccardcoord(cards_hand)
				for(c in 1:length(cards_hand)){				
					sizemod<-cardcoord[[c]][[3]]/100
					rx<- cardcoord[[c]][[1]]
					ry<- cardcoord[[c]][[2]]
					if(kx>rx-sizemod  && kx<rx+sizemod && ky>ry-sizemod*2 && ky<ry+sizemod*2){
						.drawunocard(x=rx,y=ry,label=strsplit(cards_hand[c], "-")[[1]][2],color=strsplit(cards_hand[c], "-")[[1]][1],size=cardcoord[[c]][[3]]+1)
						card_play<-cards_hand[c]
						success<-T                       
						Sys.sleep(1)
					}                                                
				}
				#check if button has been clicked and draw clicked-button	
				for(c in 1:length(buttonlist)){
					if(kx>(buttonlist[[c]][[1]]-(buttonlist[[c]][[4]]/100))&& kx<(buttonlist[[c]][[1]]+(buttonlist[[c]][[4]]/100)) && ky>(buttonlist[[c]][[2]]-(buttonlist[[c]][[4]]/100)) && ky<(buttonlist[[c]][[2]]+(buttonlist[[c]][[4]]/100))){
						.drawsquarebutton(xval=buttonlist[[c]][[1]],yval=buttonlist[[c]][[2]],type=buttonlist[[c]][[3]],size=buttonlist[[c]][[4]],color=buttonlist[[c]][[5]],clicked=T,label=buttonlist[[c]][[6]],labelcolor=buttonlist[[c]][[7]],labelfont=buttonlist[[c]][[8]],labelsize=buttonlist[[c]][[9]])
						success<-T 
						if(buttonlist[[c]][[6]]=="Help"){
							.showhelp()
							.repaint(ws,cards_hand,user)
						 print("test")
              card_play<-"get-info"		
						}
						else if(buttonlist[[c]][[6]]=="NO-Card"){
							card_play<-"NO"
						}
						else if(buttonlist[[c]][[6]]=="Uno"){
							card_play<-"say-uno"
						}
						Sys.sleep(0.5)
					}
				}
			}
  		}
		colbool<-FALSE
		if(card_play=="rybg-00"){
			while(!colbool){
			# ask for color by wish card
      			if(graphics==FALSE){col <- readline("Color: ")}
			else{
			col<-.askforcolor()
			.repaint(ws,cards_hand,user)}
			colbool<-(.colcheck(col))
			}
			card_play_save <- paste(col, "rybg", sep="-")
		}else if(card_play=="rybg-4+"){
			while(!colbool){
			# ask for color by wish card
      			if(graphics==FALSE){col <- readline("Color: ")}
			else{col<-.askforcolor()
			.repaint(ws,cards_hand,user)}
			colbool<-(.colcheck(col))
			}
			card_play_save <- paste(col, "rybg4+", sep="-")
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
	rulesbools<-nwsFindTry(ws, 'rulesbools')
	playerInAction <- user
	
	unovec<-nwsFindTry(ws, 'uno')                                
	names(unovec)<- nwsFindTry(ws,'players_logedin')
	if(unovec[playerInAction] && length(cards_hand) > 2){
		 unovec[playerInAction]<-FALSE
		 nwsStore(ws, 'uno',unovec)
	}
	if(card_play=="NO"){
	   # if there is no matching card in the hand
		  if(NO==0){
			   # in first time get new card 
			   .getpenalty(ws,1,user,3) 			 
			   NO <- 1
			   card_play<-""
			   #no player rotation
		  } else if(NO==1){
			   # in second time, do not get new card
			   .rotate(ws,1)
		  }
	  }
    else if(card_play=="say-uno"){ 
	  # announce that you play your second-last card
    card_play<-""
    unovec[playerInAction]<-TRUE
    nwsStore(ws, 'uno',unovec)
    }
    else if(card_play=="get-info"){ 
	  # get gameinformation
    card_play<-""
	graphics <- nwsFindTry(ws, 'graphics')
    if(graphics==FALSE){.getInfo(ws)}
    }
    else if(!(card_play %in% unlist(cards_hand))){
    # play a card, that is not in your hand or typing error
		cat("\tCard not in your cards!\n\t'NO' for new card.\n")
		card_play <- ""
	  }
    else if((card_play=="rybg-00") && (played_number!="rybg" || !rulesbools[2])){
     # play rybg-0 card
    .removecard(ws,card_play, cards_hand,user)
    .playcard(ws, card_play_save, playerInAction,unovec,card_play_number,card_play)
    }
    else if(card_play=="rybg-00" && played_number=="rybg" &&  rulesbools[2]){
     # play rybg-0 card on another rybg-card    
      .getpenalty(ws,1,user,5)
      card_play<-""		
    }
    else if(card_play=="rybg-4+" && (played_number!="rybg" || !rulesbools[2])){
     # play rybg-4+ card
    .removecard(ws,card_play, cards_hand,user)
		#penalty has not been given       
		pen<-nwsFetchTry(ws, 'penalty')
		pen<-pen+4
    nwsStore(ws, 'penalty', pen)
    .playcard(ws, card_play_save, playerInAction,unovec,card_play_number,card_play)
    }
    else if(card_play=="rybg-4+" && played_number=="rybg" && rulesbools[2]){
     # play rybg-4+ card on another rybg-card
      .getpenalty(ws,1,user,5)
      card_play<-""	
    }
    else if(card_play_number == "BREAK" && (played_color == card_play_color || card_play_number == played_number)){
		.removecard(ws,card_play, cards_hand,user)
    .playcard(ws, card_play_save, playerInAction,unovec,card_play_number,card_play)
	  }
    else if(card_play_number=="BACK" && (played_color == card_play_color || card_play_number == played_number)){
		.removecard(ws,card_play, cards_hand,user)
		.playcard(ws, card_play_save, playerInAction,unovec,card_play_number,card_play)		
 	  }
     else if(card_play_number=="2+" && card_play_number == played_number){
    # play a 2+ card on another one
    .removecard(ws,card_play, cards_hand,user)
    #penalty has not been given                
		pen<-nwsFetchTry(ws, 'penalty')
		pen<-pen+2
    nwsStore(ws, 'penalty', pen)  
    .playcard(ws, card_play_save, playerInAction,unovec,card_play_number,card_play)
	 }
   else if(card_play_number=="2+" && played_color == card_play_color){
		# play a 2+ card on a card with the same color
    .removecard(ws,card_play, cards_hand,user)
		#penalty has not been given        
		pen<-nwsFetchTry(ws, 'penalty')
		pen<-pen+2
    nwsStore(ws, 'penalty', pen)
		.playcard(ws, card_play_save, playerInAction,unovec,card_play_number,card_play)
	 }
   else if( (card_play_color == played_color) || (card_play_number == played_number) ) {
		# play normal card with color and number
		  .removecard(ws,card_play, cards_hand,user)
      .playcard(ws, card_play_save, playerInAction,unovec,card_play_number,card_play)	
	 }
   else if(played_color != card_play_color && played_number != card_play_number){
		# play a wrong card
    .getpenalty(ws,2,user,6)
		card_play <- ""
		.rotate(ws,1)
	 }
   else
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
#########################################################
#Function for getting penalties
#########################################################
.getpenalty <- function(ws,number,playerInAction,reasonnumber)
{
  require(nws)
  reason<-""
  if(reasonnumber==1){
     reason<-"because you forgot to say \"uno\""
  }else if(reasonnumber==2){
     reason<-"because you said \"uno\" without having a reason"
  }else if(reasonnumber==3){
     reason<-"because you played no card"
  }else if(reasonnumber==4){
     reason<-"because you played a rybg-card as your last card" 
  }else if(reasonnumber==5){
     reason<-"because you laid a rybg-card on another one" 
  }else if(reasonnumber==6){
     reason<-"because you played a wrong card" 
  }
  cards_hand<-nwsFindTry(ws, playerInAction)
  for(i in 1:number){
  cards_hand<-c(cards_hand, nwsFetchTry(ws,'cards'))
  }
  nwsStore(ws,playerInAction,cards_hand)
  graphics <- nwsFindTry(ws, 'graphics')
  cpu <- nwsFindTry(ws, 'cpu')
  players<-nwsFindTry(ws,'players_logedin')
  for(c in 1:length(players)){
     if(players[[c]]==playerInAction){
	       cpuval<-cpu[c]
	       unovec<-nwsFindTry(ws,'uno')
	       unovec[c]<-FALSE
         nwsStore(ws,'uno',unovec)
     }
  }
  if(graphics==FALSE){cat("You got ",number," penalty card(s)",reason,"!\n")}
  else if(cpuval==FALSE){
	xbut<-0.5
	ybut<-0.4
	polygon(c(0.3,0.3,0.7,0.7),c(0.3,0.7,0.7,0.3),col="blanchedalmond",border=NA)
	text(x=0.5,y=0.6,label=paste("You got ",number," penalty card(s) ,\n",reason,"!\n"),cex=1)
	clicked<-FALSE
	.drawsquarebutton(xval=xbut,yval=ybut,size=4,type=2,clicked=FALSE,color="mediumseagreen")
  	while(clicked==FALSE){
		klick<-locator(n=1)
		kx<-klick[[1]]
		ky<-klick[[2]]
		if(kx>(xbut-(3/100))&& kx<(xbut+(3/100)) && ky>(ybut-(3/100)) && ky<(ybut+(3/100))){
			.drawsquarebutton(xval=xbut,yval=ybut,size=4,type=2,clicked=TRUE,color="mediumseagreen")
 		   	Sys.sleep(0.5) 
      			clicked<-TRUE	
		}
  	} 	
  }
  	if(graphics==FALSE){cat("Hand:", sort(unlist(cards_hand)), "\n")} #sorted output
	else{.repaint(ws,cards_hand,playerInAction)}
}
########################################################
#Function to remove card from hand
########################################################
.removecard<-function(ws,card_play, cards_hand,user)
{
  require(nws)
  rulesbools<-nwsFindTry(ws, 'rulesbools')
  len<-length(cards_hand)
  cards_hand <- cards_hand[-which(cards_hand==card_play)]
  dif<-len-length(cards_hand)
  if(dif > 1 && !rulesbools[4]){
 	  cards_hand<-c(cards_hand,card_play)
 	}
 	nwsStore(ws,user,cards_hand)
}
#######################################################
#(Play card, check for winner, go to next)-Function
#######################################################
.playcard<-function(ws, card_play_save, playerInAction,unovec,card_play_number,card_play) 
{
  require(nws)
  # play card
		nwsStore(ws, 'played', card_play_save)
		cards_hand<-nwsFindTry(ws, playerInAction)
		rulesbools<-nwsFindTry(ws, 'rulesbools')
		
    if(length(cards_hand) == 1 && !unovec[playerInAction]){   
    .getpenalty(ws,2,playerInAction,1) 	  
    }	
    else if(length(cards_hand) > 1 && unovec[playerInAction]){    
    .getpenalty(ws,2,playerInAction,2)    
    }
    #unovec[playerInAction]<-FALSE
    nwsStore(ws,'uno',unovec)
		#check for winner and goto next player
		if(length(cards_hand) != 0 ){
			# rotate player
			if(card_play_number=="BREAK"){                        
        .rotate(ws,2)      
      }
			else if(card_play_number=="BACK"){
  	   #if there are only two players, they just don´t have to rotate 
   		   if(length(nwsFind(ws, 'players_logedin')) > 2){			         
	        #  reverse playorder                                        
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
       } 
			 else{
			 .rotate(ws,1)
			}    
		} else if(length(cards_hand) == 0 && (card_play=="rybg-00" || card_play=="rybg-4+")&& rulesbools[1]){
      .getpenalty(ws,1,playerInAction,4)
    }else
			nwsStore(ws, 'winner', playerInAction)
}        
############################################################
#Function to check if given color is a valid color
############################################################
.colcheck<-function(col)
{
  require(nws)
  if(col == "blue" || col == "green" || col == "red" || col == "yellow"){
    return (TRUE)
  }else return (FALSE)
}
###########################################################
#Function for player-rotation
###########################################################
.rotate<-function(ws,numberOfRots)
{
  require(nws)
  for(i in 1:numberOfRots){
	     playerInAction <- nwsFetchTry(ws, 'players')
			 nwsStore(ws, 'players', playerInAction)
	}		 
			 nwsStore(ws, 'player_in_action', playerInAction)
}
##########################################################
#Function for getting Information about the game
##########################################################
.getInfo<-function(ws)
{
 require(nws)
 maxi<- -Inf
 mini<- Inf
 sumi<- 0
 counter<- 0
 players<-nwsFindTry(ws,'players_logedin')
 for(p in players){
     len<-length(nwsFindTry(ws,p))
    if(len > maxi){
    maxi <- len
    }
    if(len < mini){
    mini <- len
    }
    sumi <- sumi + len 
    counter<- counter+1   
}
 cat("\nGame Information:")
 cat("\n Rules",nwsFindTry(ws,'rules'))
 cat("\n      ",nwsFindTry(ws,'rulesbools'))
 cat("\n Cards in Deck: 102")
 cat("\n Players in Game: ",counter)
 cat("\n Max#Cards:",maxi)
 cat("\n Min#Cards: ",mini)
 cat("\n Average#Cards: ",sumi/counter,"\n")
}        

###############################################################################
#Function to draw buttons (xval,yval:central buttonposition; size:buttonsize; label: buttonlabel; type: (0=label, 1= ok-hook, 2=cancel-cross), clicked: bordercolor,)
#########################################################################################
.drawsquarebutton<-function(xval=0.5,yval=0.5,size=10,label=NULL,type=0,clicked=F,labelsize=size/(nchar(label)+1+(nchar(label)/10)),color="azure",labelcolor="black",labelfont=1)
{       
	sizemod<-(size/100)
	xvec<-c(xval-sizemod,xval+sizemod)
	yvec<-c(yval-sizemod,yval+sizemod)
	if(color=="mediumseagreen"){
    		midcol<-"mediumseagreen"
   		lightcol<-"springgreen3"
    		darkcol<-"springgreen4"      
  	}else if(color=="green"){
    		midcol<-"green"
   		lightcol<-"lightgreen"
    		darkcol<-"green4"      		
	}else if(color=="red"){
    		midcol<-"red"
   		lightcol<-"pink"
    		darkcol<-"red4"      		
	}else if(color=="blue"){
    		midcol<-"blue"
   		lightcol<-"lightblue"
    		darkcol<-"blue4"      		
	}else if(color=="yellow"){
    		midcol<-"yellow"
   		lightcol<-"lightgoldenrodyellow"
    		darkcol<-"yellow4"      		
	}else{
    		midcol<-"azure2"
    		lightcol<-"azure1"
    		darkcol<-"azure4"
  	}
  
	polygon(c(xvec,xvec[2],xvec[1]),c(yvec[1],yvec,yvec[2]),col=midcol,border=NA)
	if(clicked==F){
		lines(c(xvec[1],xvec[2]),c(yvec[1],yvec[1]),col=darkcol,lwd=2)
		lines(c(xvec[2],xvec[2]),c(yvec[1],yvec[2]),col=darkcol,lwd=2)
		lines(c(xvec[1],xvec[1]),c(yvec[1],yvec[2]),col=lightcol,lwd=2)
		lines(c(xvec[2],xvec[1]),c(yvec[2],yvec[2]),col=lightcol,lwd=2)
	}else{
		lines(c(xvec[1],xvec[2]),c(yvec[1],yvec[1]),col=lightcol,lwd=2)
		lines(c(xvec[2],xvec[2]),c(yvec[1],yvec[2]),col=lightcol,lwd=2)
		lines(c(xvec[1],xvec[1]),c(yvec[1],yvec[2]),col=darkcol,lwd=2)
		lines(c(xvec[2],xvec[1]),c(yvec[2],yvec[2]),col=darkcol,lwd=2)
	}       
	if(type==0 && !is.null(label)){
		text(x=xval,y=yval,label=label,cex=labelsize,col=labelcolor,font=labelfont)
	}else if(type==1){
	 lines(c((xvec[1]+sizemod/4),(xvec[1]+(3*sizemod)/4)),c((yvec[1]+(2*sizemod)/3),(yvec[1]+sizemod/5)),col="green",lwd=size*2)	
	 lines(c((xvec[1]+(3*sizemod)/4),(xvec[2]-sizemod/4)),c((yvec[1]+sizemod/5),(yvec[2]-sizemod/5)),col="green",lwd=size*2)
	}else if(type==2){
	 lines(c((xvec[1]+sizemod/4),(xvec[2]-sizemod/4)),c((yvec[1]+sizemod/4),(yvec[2]-sizemod/4)),col="red",lwd=size*2)	
	 lines(c((xvec[1]+sizemod/4),(xvec[2]-sizemod/4)),c((yvec[2]-sizemod/4),(yvec[1]+sizemod/4)),col="red",lwd=size*2)	
	}  	
}
###############################################################################
#Function to draw uno cards
###############################################################################
.drawunocard<-function(xval=0.5,yval=0.5,size=5,label="NO",color)
{ 
	sizemod<-(size/100)
	xvec<-c(xval-sizemod,xval+sizemod)
	yvec<-c(yval-(2*sizemod),yval+(2*sizemod))
	if((color!="green"&& color!="red" && color!="blue" && color!="yellow")||label=="00"||label=="4+"||label=="rybg"||label=="rybg4+" ){
    		if(label=="rybg"||label=="rybg4+"){
			wishcolor<-color	
		}
		color<-"black"		
  	}
		polygon(c(xvec,xvec[2],xvec[1]),c(yvec[1],yvec,yvec[2]),col=color,border="white",lwd=size)
		symbols(x=xval,y=yval,circles=1,bg="white",add=T,inches=size/10)
  	if(label=="BACK"){
		polygon(c(xval-sizemod/1.5,xval-sizemod/4,xval-sizemod/4,xval+sizemod/4,xval+sizemod/4,xval-sizemod/4,xval-sizemod/4),c(yval+sizemod/5,yval+sizemod/1.5,yval+sizemod/3,yval+sizemod/3,yval,yval,yval-sizemod/4),col=color,border="black",lwd=size) 
		polygon(c(xval+sizemod/1.5,xval+sizemod/4,xval+sizemod/4,xval-sizemod/4,xval-sizemod/4,xval+sizemod/4,xval+sizemod/4),c(yval-sizemod/5,yval-sizemod/1.5,yval-sizemod/3,yval-sizemod/3,yval,yval,yval+sizemod/4),col=color,border="black",lwd=size) 
  	}else if(label=="BREAK"){ 
		symbols(x=xval,y=yval,circles=1,fg="black",add=T,inches=sizemod*5,lwd=size*2.5)
		lines(c(xval-sizemod/3,xval+sizemod/3),c(yval+sizemod/3,yval-sizemod/3),col="black",lwd=size*2.5)
		symbols(x=xval,y=yval,circles=1,fg=color,add=T,inches=sizemod*5,lwd=size*1.5)
		lines(c(xval-sizemod/3,xval+sizemod/3),c(yval+sizemod/3,yval-sizemod/3),col=color,lwd=size*1.5)
 	 }else if(label=="00"||label=="rybg"|label=="4+"||label=="rybg4+"){
		polygon(c(xval-sizemod/2,xval,xval,xval-sizemod/3),c(yval,yval,yval+sizemod/2,yval+sizemod/3),col="red",border="white")
		polygon(c(xval,xval,xval+sizemod/2,xval+sizemod/2),c(yval,yval+sizemod/2,yval+sizemod/2,yval),col="blue",border="white")
		polygon(c(xval,xval,xval+sizemod/3,xval+sizemod/2),c(yval,yval-sizemod/2,yval-sizemod/3,yval),col="yellow",border="white")
		polygon(c(xval,xval,xval-sizemod/2,xval-sizemod/2),c(yval,yval-sizemod/2,yval-sizemod/2,yval),col="green",border="white")
  	}else{
		text(x=xval+sizemod/17,y=yval,label=label,cex=size,col="black",font=2)
		text(x=xval-sizemod/10,y=yval,label=label,cex=size,col="black",font=2)
    		text(x=xval,y=yval,label=label,cex=size*0.9,col=color,font=2)
	}
	if(label=="4+"||label=="rybg4+"){
		text(x=xval-sizemod/2,y=yval+sizemod*1.5,label="4+",cex=size*0.3,col="white",font=2)
	}	
    	if(label=="rybg"||label=="rybg4+"){
		polygon(c(xval+sizemod/2,xval+sizemod/2,xval+sizemod,xval+sizemod),c(yval+sizemod/2,yval+sizemod*2,yval+sizemod*2,yval+sizemod/2),col=wishcolor,border="white")
	}
}
###############################################################################
#Function to show basic rules of the game and explain symbols
###############################################################################
.showhelp<-function()
{
	polygon(c(0.3,0.3,0.7,0.7),c(0.3,0.7,0.7,0.3),col="blanchedalmond",border=NA)
	.drawunocard(x=0.32,y=0.6,label="BACK",color="yellow",size=2)
	.drawunocard(x=0.32,y=0.5,label="BREAK",color="blue",size=2)
	.drawunocard(x=0.32,y=0.4,label="00",color="rybg",size=2)
	text(x=0.55,y=0.6,label="The order of play is reversed from clockwise\n to counter-clockwise, or from\n counter-clockwise to clockwise.",cex=1)
	text(x=0.55,y=0.5,label="The next player must skip their turn.",cex=1)
	text(x=0.55,y=0.4,label="The person playing it names a color,\n and the next legal play must be that\n color unless another wild is played.",cex=1)
	.drawsquarebutton(xval=0.5,yval=0.7,type=2,size=3)
	while(T){
		klick<-locator(n=1)
		kx<-klick[[1]]
		ky<-klick[[2]]
		if(kx>(0.5-(5/100))&& kx<(0.5+(5/100)) && ky>(0.74-(5/100)) && ky<(0.74+(5/100))){
			.drawsquarebutton(xval=0.5,yval=0.7,type=2,size=3,clicked=T)
        		Sys.sleep(0.5)
			break
		}
	}
}
###############################################################################
#Popup to ask Player for penaltyconcatenation
###############################################################################
.askforpenalty<-function()
{
	polygon(c(0.3,0.3,0.7,0.7),c(0.8,0.95,0.95,0.8),col="blanchedalmond",border=NA)
	text(x=0.5,y=0.9,label="Concatenate penalty?",cex=2,col="black")
	buttonlist<-list(list(0.45,0.85,1,3,"mediumseagreen","","",1,0),list(0.55,0.85,2,3,"mediumseagreen","","",1,0))
	.drawsquarebutton(xval=buttonlist[[1]][[1]],yval=buttonlist[[1]][[2]],type=buttonlist[[1]][[3]],size=buttonlist[[1]][[4]],color=buttonlist[[1]][[5]],clicked=F)
	.drawsquarebutton(xval=buttonlist[[2]][[1]],yval=buttonlist[[2]][[2]],type=buttonlist[[2]][[3]],size=buttonlist[[2]][[4]],color=buttonlist[[2]][[5]],clicked=F)
	while(T){
		klick<-locator(n=1)
		kx<-klick[[1]]
		ky<-klick[[2]]
		if(kx>(0.45-(3/100))&& kx<(0.45+(3/100)) && ky>(0.85-(3/100)) && ky<(0.85+(3/100))){
			.drawsquarebutton(xval=buttonlist[[1]][[1]],yval=buttonlist[[1]][[2]],type=buttonlist[[1]][[3]],size=buttonlist[[1]][[4]],color=buttonlist[[1]][[5]],clicked=T)
        		Sys.sleep(0.5)
			return("y")
		}
		else if(kx>(0.55-(3/100))&& kx<(0.55+(3/100)) && ky>(0.85-(3/100)) && ky<(0.85+(3/100))){
			.drawsquarebutton(xval=buttonlist[[2]][[1]],yval=buttonlist[[2]][[2]],type=buttonlist[[2]][[3]],size=buttonlist[[2]][[4]],color=buttonlist[[2]][[5]],clicked=T)
        		Sys.sleep(0.5)
			return("n")
		}
	}
}
###############################################################################
#Popup to ask Player for colorwish
###############################################################################
.askforcolor<-function()
{
	polygon(c(0.3,0.3,0.7,0.7),c(0.5,0.7,0.7,0.5),col="blanchedalmond",border=NA)
	text(x=0.5,y=0.65,label="What color should be played?",cex=2,col="black")
	buttonlist<-list(list(0.38,0.55,0,3,"red","","",1,0),list(0.46,0.55,0,3,"green","","",1,0),list(0.54,0.55,0,3,"yellow","","",1,0),list(0.62,0.55,0,3,"blue","","",1,0))
	for(c in 1:length(buttonlist)){
		.drawsquarebutton(buttonlist[[c]][[1]],buttonlist[[c]][[2]],size=buttonlist[[c]][[4]],color=buttonlist[[c]][[5]])
	}
	while(T){
		klick<-locator(n=1)
		kx<-klick[[1]]
		ky<-klick[[2]]
		for(c in 1:length(buttonlist)){
			if(kx>(buttonlist[[c]][[1]]-(buttonlist[[c]][[4]]/100))&& kx<(buttonlist[[c]][[1]]+(buttonlist[[c]][[4]]/100)) && ky>(buttonlist[[c]][[2]]-(buttonlist[[c]][[4]]/100)) && ky<(buttonlist[[c]][[2]]+(buttonlist[[c]][[4]]/100))){
      				.drawsquarebutton(xval=buttonlist[[c]][[1]],yval=buttonlist[[c]][[2]],size=buttonlist[[c]][[4]],color=buttonlist[[c]][[5]],clicked=T)
        			Sys.sleep(0.5)
				return(buttonlist[[c]][[5]])
			}
		}
	}
}
##################################################################################
#Function to repaint the display
##################################################################################
.repaint<-function(ws,cards_hand,user)
{
	require(nws)
	cardcoord<-list()
	plot.new()
	#create and draw buttons
	buttonlist<-.getbuttonlist()
	for(c in 1:length(buttonlist)){
		.drawsquarebutton(xval=buttonlist[[c]][[1]],yval=buttonlist[[c]][[2]],type=buttonlist[[c]][[3]],size=buttonlist[[c]][[4]],color=buttonlist[[c]][[5]],clicked=F,label=buttonlist[[c]][[6]],labelcolor=buttonlist[[c]][[7]],labelfont=buttonlist[[c]][[8]],labelsize=buttonlist[[c]][[9]])
	}
	players<-nwsFindTry(ws,'players_logedin')		
	# draw players
	seatcoord<-list(c(0.5,0.05),c(0.85,0.1),c(0.95,0.35),c(0.95,0.65),c(0.85,0.9),c(0.5,0.95),c(0.15,0.9),c(0.05,0.65),c(0.05,0.35),c(0.15,0.1))
	factor<-floor(10/length(players))
	playercoord<-vector()
	for(k in 1:length(players)){
		playercoord[k]<-seatcoord[k*factor]
	}
	unovec<-nwsFindTry(ws, 'uno')	
	for(k in 1:length(players)){
		x<-playercoord[[k]][1]
		y<-playercoord[[k]][2]
		text(x=x,y=y,label=paste(players[k],"\n(",length(nwsFindTry(ws,players[k])),")cards"))
		if(unovec[k]){
			text(x=x,y=y+0.02,label="UNO!",col="red",cex=1.5)
		}
	}
	# get played card
	played <- nwsFindTry(ws, 'played')
	#split for color and number
	played_color <- strsplit(unlist(played), "-")[[1]][1]
	played_number <- strsplit(unlist(played), "-")[[1]][2]
	#draw tablecard
	text(x=0.5,y=0.8,label="Tablecard:",cex=2)
	.drawunocard(xval=0.5,yval=0.6,label=played_number,color=played_color,size=8)
	#calculate cardcoordinates and draw cards
	cardcoord<-.calccardcoord(cards_hand)
	for(cc in 1:length(cards_hand)){
		.drawunocard(xval=cardcoord[[cc]][[1]],
			yval=cardcoord[[cc]][[2]],
			label=strsplit(unlist(cards_hand[[cc]]), "-")[[1]][2],
			color=strsplit(unlist(cards_hand[[cc]]), "-")[[1]][1],
			size=cardcoord[[cc]][[3]])
	}
  #show own player
  for(ccc in 1:length(players)){
     if(players[[ccc]]==user){
      koord<-playercoord[[ccc]]
     }
  }  
	parameter<-0.05
  polygon(c(koord[1]-parameter,koord[1]-parameter,koord[1]+parameter,koord[1]+parameter),c(koord[2]-parameter,koord[2]+parameter,koord[2]+parameter,koord[2]-parameter),col="yellow",lwd=3,density = 0)
}
#########################################################################################
#Function to calculate the positions of the cards
#########################################################################################
.calccardcoord<-function(cards_hand)
{
	cardcoord<-list()
	l_ch<-length(cards_hand)
	if(l_ch>10){
		distance<-0.04 
		size<-2}
	else if(l_ch>5){
		distance<-0.06
		size<-3}
	else{
		distance<-0.1
		size<-4}
	for(c in 1:l_ch){
		if(c<15){y<-0.15}
		else if(c<30){y<-0.25}
		else{y<-0.35}
		rownumber<-c%%15
		cardcoord[[c]]<-list(0.22+rownumber*distance,y,size)
	}
	return(cardcoord)
}

#########################################################################################
#Function to get the buttonlist
#########################################################################################
.getbuttonlist<-function()
{
return(buttonlist<-list(list(0.05,0.1,0,4,"mediumseagreen","Uno","black",2,2),list(0.05,0,0,4,"mediumseagreen","Help","black",2,2),list(0.05,0.85,0,4,"mediumseagreen","NO-Card","black",2,1)))	
}

















































