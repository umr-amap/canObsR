% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_values.R
\name{merge_values}
\alias{merge_values}
\title{Merge labels and rgb values}
\usage{
merge_values(
  data_labeling,
  rgb_data,
  out_dir_path = NULL,
  file_type = ".RData"
)
}
\arguments{
\item{data_labeling}{\code{tibble} or \code{dataframe} which contains the labels directly import from the xlsx file.}

\item{rgb_data}{\code{tibble} or \code{dataframe} with the rgb metrics values.}

\item{out_dir_path}{chr. The path to the directory use to stored the file}

\item{file_type}{chr. By default it is '.RData' but can be '.csv' or '.xlsx'}
}
\value{
A tibble with the variable site, id, date, family, genus, species, phenophase, type, metric, band, value, obs, comments, update, Usable_crown.
}
\description{
A function to merge the labels from long format and the rgb metrics
values.
}
\examples{

library(canObsR)

data('"data_labeling"')
data('data_labeling')
merge_values(data_labeling,rgb_data)


}
