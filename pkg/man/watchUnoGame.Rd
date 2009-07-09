\name{watchUnoGame}
\Rdversion{1.1}
\alias{watchUnoGame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
watchUnoGame(ws, ..., logfile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ws}{
%%     ~~Describe \code{ws} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function(ws, ..., logfile=NULL)
{	
	# connect to nws
	require(nws)
	if( class(ws) != "netWorkSpace" ){
		# connect to nws
		ws <- netWorkSpace(ws, ...)
	}
	
	
	# read played cards
	card_played <- card_played_tmp <- ""
	user <- user_tmp <- ""
	while( is.null(winner <- nwsFindTry(ws, 'winner'))
		|| card_played != card_played_tmp 
		|| user != user_tmp
		){
		card_played <- nwsFindTry(ws, 'played') 

		#if cards change: output
		if( card_played != card_played_tmp ){
			cat(user, ": ", card_played, " (",length(nwsFindTry(ws,user)),")\n", sep="")
			user <- nwsFindTry(ws, 'player_in_action')
			user_tmp <- user # save player 
			card_played_tmp <- card_played # save card
		# if user change and no card: output
		}else if( user != user_tmp){
			cat(user_tmp, ": NO (",length(nwsFindTry(ws,user_tmp)),")\n", sep="")
			#user <- nwsFindTry(ws, 'players')
			user_tmp <- user
		}
		user <- nwsFindTry(ws, 'player_in_action')
		Sys.sleep(0.1)
	}
		
	#return winner
	return(winner)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
