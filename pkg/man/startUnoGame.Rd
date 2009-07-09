\name{startUnoGame}
\Rdversion{1.1}
\alias{startUnoGame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
startUnoGame(ws, cardsStart = 7, minPlayers = 2, maxPlayers = 10, log = FALSE, logfile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ws}{
%%     ~~Describe \code{ws} here~~
}
  \item{cardsStart}{
%%     ~~Describe \code{cardsStart} here~~
}
  \item{minPlayers}{
%%     ~~Describe \code{minPlayers} here~~
}
  \item{maxPlayers}{
%%     ~~Describe \code{maxPlayers} here~~
}
  \item{log}{
%%     ~~Describe \code{log} here~~
}
  \item{logfile}{
%%     ~~Describe \code{logfile} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(ws, cardsStart=7, 
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
