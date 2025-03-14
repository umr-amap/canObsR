% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_labelingFile.R
\name{create_labelingFile}
\alias{create_labelingFile}
\title{Create xlsx labeling file}
\usage{
create_labelingFile(crownsFile, site, dates, path_out = NULL)
}
\arguments{
\item{crownsFile}{sf or data.frame. Crowns file with at least the variables (id, family, genus, species)}

\item{site}{chr. site name (p.e "Bouamir").}

\item{dates}{chr. vector of dates (format should be 'YYYY_MM_DD', p.e c('2022_09_25','2022_10_10').}

\item{path_out}{The path where to save the xlsx file. If NULL, the file will not be exported and will be return as tibble.}
}
\value{
A tibble with the variable site, id, family, genus, species, n, obs, update, date, phenophase and comments.
where n, obs, update, phenophase and comments and comments will be NULL. This tibble can be used in the \code{shiny_labels()} applications to be filled.
}
\description{
A function to create the xlsx file where to encode the phenophase.
}
\examples{

library(canObsR)

data(crowns)

create_labelingFile(
crowns,
site = 'Site1',
dates = c('2025_05_10','2025_05_20'),
path_out = NULL
)

}
