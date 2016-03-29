\name{vegbankStat}

\alias{vegbankStat}

\title{General statistics from TURBOVEG databases}

\description{
This function calculates general statistics of local TURBOVEG databases as
required by GIVD (Global Index of Vegetation-Plot Databases;
\url{http://www.givd.info/list_databases.html}).
}

\usage{
vegbankStat(db, tv_home)
}

\arguments{
	\item{db}{a character value with the name of the database.}
	\item{tv_home}{TURBOVEG installation path.}
}

\details{
\code{vegbankStat} is based on a script delivered by GIVD-staff for summarising
statistics required in the descriptions of databases.

As default \code{tv_home} is set as "C:/Turbowin" but can be changed via
\code{options(tv_home="new path")}.
}

\value{details are printed in the running console.}

\author{GIVD. Adapted by Miguel Alvarez.}

\seealso{
\code{\link{tv.site}}
}
