\name{playUno}
\Rdversion{1.1}
\alias{playUno}
\title{The function, that has to be called by each UNO-player}
\description{The player calls it and inputs all necessary values. 
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
  \item{computerPlayer}{is the player manual or automatical controlled}
  \item{computerPlayerFunction}{which computerplayer is used}
  \item{\dots}{}
}
\details{
This function rules everything, a player needs. It asks for decisions and shows information.
Commands:
say-uno: before playing the second-last card, you have to say uno
get-info: shows some game relevant information
color*-number**: plays card


*color=(red,yellow,blue,green,rybg)
**number=(0:9,2+,4+)

}
\author{
Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>, 
Fabian Grandke
}
\examples{
\dontrun{
	playUno("test",user="tom",serverHost="138.245.80.17")
}
\dontrun{
  playUno("test2",user="paul",computerPlayer=TRUE)
}
}
