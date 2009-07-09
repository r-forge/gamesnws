\name{createUnoGame}
\Rdversion{1.1}
\alias{createUnoGame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createUnoGame(wsName, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wsName}{
%%     ~~Describe \code{wsName} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function(wsName, ...)
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
