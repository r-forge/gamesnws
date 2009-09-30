\name{watchUnoGame}
\Rdversion{1.1}
\alias{.watchUnoGame}
\title{logs the game}
\description{writes a logfile of the game and returns the winner}
\usage{
.watchUnoGame( ws, 
              ..., 
              logfile = NULL)
}
\arguments{
  \item{ws}{Name of the workspace}
  \item{\dots}{no function yet, but in future some logging-specific attributes are available}
  \item{logfile}{the directory, where the log should be saved to(not yet implemented)}
}
\details{
This function will be called in a playUnoMaster()-function.
The logfile-argument is not yet implemented, so actually the logfile is written in your workingdirectory.
Different logging-modes will be possible.
}
\value{
\item{winner}{player that has won the UNO-Game}
}
\author{
Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>, 
Fabian Grandke
}
\examples{
\dontrun{
  if(log!=0){
		winner <- .watchUnoGame(ws, logfile=logfile)
  }
}
}