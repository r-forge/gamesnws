\name{createUnoGame}
\Rdversion{1.1}
\alias{.createUnoGame}
\title{Creates new UNO-Game}
\description{New UNO-Game into the given workspace}
\usage{.createUnoGame( wsName, 
                      ...)}
\arguments{
  \item{wsName}{Name of the workspace}
  \item{\dots}{Usual netWorkSpace()-attributes}
}
\details{
Checks if workspace allready exists.
Declares workspacevariables.
}
\value{
\item{ws}{the created workspace with all necessary variables for the UNO-Game}
}
\author{
Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>, 
Fabian Grandke
}
\examples{
\dontrun{
  ws<-.createUnoGame("test","138.245.80.17")
  startUnoGame(ws)
}
}