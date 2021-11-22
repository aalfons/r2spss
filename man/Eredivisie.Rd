\name{Eredivisie}
\alias{Eredivisie}
\docType{data}
\title{
Football players of the Dutch Eredivisie season 2013-14
}
\description{
Data on all football players in the Dutch Eredivisie, the highest football
league in the Netherlands, who played at least one match in the 2013-14
season.
}
\usage{data("Eredivisie")}
\format{
  A data frame with 417 observations on the following 20 variables.
  \describe{

    \item{\code{Player}}{the player's name.}

    \item{\code{Team}}{the team with which the player was under contract at the
    end of the 2013-14 season.}

    \item{\code{MarketValue}}{the player's market value after the 2013-14
    season.}

    \item{\code{Age}}{the player's age in years.}

    \item{\code{Height}}{the player's height in centimeters.}

    \item{\code{Foreign}}{a dummy variable with value 0 for Dutch players and
    value 1 for players without a Dutch nationality.}

    \item{\code{Position}}{the primary postion of the player
    (\code{"Goalkeeper"}, \code{"Defender"}, \code{"Midfielder"},
    or \code{"Forward"}).}

    \item{\code{BothFeet}}{a dummy variable with value 0 if the player has one
    stronger foot and value 1 if the player is equally strong with both feet.}

    \item{\code{AtClub}}{the number of years the player is with the current
    team.}

    \item{\code{Contract}}{the number of years remaining on the player's
    current contract.}

    \item{\code{Matches}}{the number of matches played in the 2013-14 season.}

    \item{\code{Goals}}{the number of goals scored in the 2013-14 season.}

    \item{\code{OwnGoals}}{the number of own goals scored in the 2013-14
    season.}

    \item{\code{Assists}}{the number of assists given in the 2013-14 season.}

    \item{\code{Yellow}}{the number of yellow cards received in the 2013-14
    season.}

    \item{\code{YellowRed}}{the number of yellow-red cards received in the
    2013-14 season.}

    \item{\code{Red}}{the number of red cards received in the 2013-14 season.}

    \item{\code{SubOn}}{the number of times the player was substituted on the
    field in the 2013-14 season.}

    \item{\code{SubOff}}{the number of times the player was substituted off the
    field in the 2013-14 season.}

    \item{\code{Minutes}}{the number of minutes played in the 2013-14 season.}

  }
}
\source{
\url{https://www.transfermarkt.de/}
}
\examples{
data("Eredivisie")
summary(Eredivisie)
}
\keyword{datasets}
