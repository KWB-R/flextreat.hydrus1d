% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conservative_tracer-traveltime.R
\name{interpolate_time}
\alias{interpolate_time}
\title{Helper function: interpolate time}
\usage{
interpolate_time(
  solute,
  percentiles = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)
)
}
\arguments{
\item{solute}{solute result tibble}

\item{percentiles}{(default: c(0.01, 0.05, 0.1, 0.25,0.5, 0.75, 0.9, 0.95, 0.99))}
}
\value{
tibble with interpolated times for percentiles
}
\description{
Helper function: interpolate time
}
