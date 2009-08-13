\name{playUno}
\Rdversion{1.1}
\alias{playUno}
\title{The players play-function}
\description{The player calls the function and inputs all necessary values. 
The function rules the whole course of the UNO-game.}
\usage{
playUno(  name, 
          user = Sys.info()["user"], 
          computerPlayer = FALSE, 
          computerPlayerFunction = computerPlayerUNO, ...)
}
\arguments{
  \item{name}{Name of the workspace}
  \item{user}{playername}
  \item{computerPlayer}{a logical, if TRUE the player is controlled by computerplayer, else, human controlled}
  \item{computerPlayerFunction}{which computerplayer is used}
  \item{...}{Usual parameters to initialize a netWorkSpace(). See \url{http://nws-r.sourceforge.net/doc/nwsR-1.5.0.pdf} (Page 25) for further information}
}
\details{
This function rules the whole game course. It asks the player for decisions and shows the relevant information.
The funtion shows the last played card, what cards are on your hand, which players are involved and how many card each player has.
To control the game options, there are specific commands the player has to use:

say-uno: before playing the second-last card, player has to announce "UNO"
get-info: shows some game relevant information
color*-value**: the card that should be played(one of the players handcards)
NO: if the player isn't able or don't wants to play 
color*: after playing a rybg-card the player has to name a color, that has to be played next


*one of the "colors"(red,yellow,blue,green,rybg)
**one of the "card values"=(0:9,2+,4+,BREAK,BACK)

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
	playUno("test",user="tom",serverHost="localhost")
}
\dontrun{
  playUno("test2",user="paul",computerPlayer=TRUE)
}
}
