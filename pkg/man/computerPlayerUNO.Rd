\name{computerPlayerUNO}
\Rdversion{1.1}
\alias{computerPlayerUNO}
\title{Computer Player for UNO}
\description{ Very stupid - random - computer player for UNO}
\usage{
	computerPlayerUNO(ws, 
                    hand, 
                    card_played)
}
\arguments{
  \item{ws}{workspace object, that is used in the whole program}
  \item{hand}{A vector of cards, which the player holds in his hand.}
  \item{card_played}{A string, which indicates the latest played card.}
}
\details{
	This function will be called in the playUno() function.
	Please feel free to write your own computer player and assign the new function to playUno() with the parameter computerPlayerFunction='yourComputerPlayer'!
  The computerplayer does not have to say "UNO". The value can just be changed.
}
\value{
A list of two named strings, 
\item{selected_card}{A string for the card, the player selected from his hand and has to be removed from his hand.}
\item{played_card}{A string for the card, the player plays. Especially for color-selection cards (rybg), a color-rybg string has to be returned. If no card is played "NO" is returned. Usual cards are in format "color-value".}
}
\references{
\url{http://gamesnws.r-forge.r-project.org/}
}
\author{
Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>, 
Fabian Grandke
}
