\name{startUnoGame}
\Rdversion{1.1}
\alias{startUnoGame}
\title{Creates workspace and starts the UNO-Game - For Admin}
\description{The Admin creates a workspace, starts an UNO-Game and dicides what game-conditions apply.}
\usage{
startUnoGame( wsName, 
              cardsStart = 7, 
              minPlayers = 2, 
              maxPlayers = 10, 
              log = 0, 
              logfile = NULL, 
              debug = FALSE,
              config = NULL,
              graphics = TRUE,
              ...)
}
\arguments{
  \item{wsName}{workspacename}
  \item{cardsStart}{the number of cards, each player gets at the beginning}
  \item{minPlayers}{the number of players, that are necessary to start a game}
  \item{maxPlayers}{the number of players, that maximal can join a game}
  \item{log}{number of log mode : 0 = no logging, !0=logging }
  \item{logfile}{the file, where the log should be saved to(not yet implemented)}
  \item{debug}{a logical, if TRUE, there are several debuginformation shown during the game}
  \item{config}{vector of four logicals, if not set, the program asks for the rules later}
  \item{graphics}{a logical, if TRUE, a grapic device is shown to the players }
  \item{...}{Usual parameters to initialize a netWorkSpace(). See \url{http://nws-r.sourceforge.net/doc/nwsR-1.5.0.pdf} (Page 25) for further information}
}
\details{
After starting the game, with this function, the players could log in with playUno().
This function only creates and starts the game. It does not play.
}
\references{
\url{http://gamesnws.r-forge.r-project.org/}
}
\author{
Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>, 
Fabian Grandke
}
\examples{
\dontrun{
  startUnoGame("test",serverHost="localhost",config=c(T,T,T,T))
}
\dontrun{
  startUnoGame("test",serverHost="localhost", cardsStart=13, minPlayers=4)
}
}