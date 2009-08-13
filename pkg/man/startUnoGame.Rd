\name{startUnoGame}
\Rdversion{1.1}
\alias{startUnoGame}
\title{Starts the UNO-Game that is in the given workspace}
\description{After a UNO-Game is created in a workspace, it gets started with this function.}
\usage{
startUnoGame( ws, 
              cardsStart = 7, 
              minPlayers = 2, 
              maxPlayers = 10, 
              log = 0, 
              logfile = NULL, 
              debug = FALSE)
}
\arguments{
  \item{ws}{workspace with a created UNO-Game in it}
  \item{cardsStart}{the number of cards, each player gets at the beginning}
  \item{minPlayers}{the number of players, that are necessary to start a game}
  \item{maxPlayers}{the number of players, that maximal can join a game}
  \item{log}{number of log mode}
  \item{logfile}{the directory, where the log should be saved to(not yet implemented)}
  \item{debug}{logical variable if debug mode is enabled}
}
\details{
After starting the game, with this function, the players could log in with playUno().
When you started the game, you can decide the play mode.

}

\author{
Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>, 
Fabian Grandke
}
\examples{
\dontrun{
  ws<-createUnoGame("test","138.245.80.17")
  startUnoGame(ws,log=1)
}
\dontrun{
  ws<-createUnoGame("test","138.245.80.17")
  startUnoGame(ws, cardsStart=13, minPlayers=4)
}
}