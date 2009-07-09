\name{computerPlayerUNO}
\Rdversion{1.1}
\alias{computerPlayerUNO}
\title{Computer Player for UNO}
\description{ Very stupid - random - computer player for UNO}
\usage{
	computerPlayerUNO(hand, card_played)
}
\arguments{
  \item{hand}{A vector of cards, which the player holds in his hand.}
  \item{card_played}{A string, which indicates the latest played card.}
}
\details{
	This function will be called in the playUno() Function.
	Please feel free to write your own computer player!
}
\value{
A list of two named strings, 
\item{selected_card}{A string for the card, the player selected from his hand and has to be removed from his hand.}
\item{played_card}{A string for the card, the player plays. Especially for color-selection cards (rybg), a color-rybg string has to be returned.}
}
\author{
Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>
}
