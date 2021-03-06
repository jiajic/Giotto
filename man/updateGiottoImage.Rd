% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/images.R
\name{updateGiottoImage}
\alias{updateGiottoImage}
\title{updateGiottoImage}
\usage{
updateGiottoImage(
  gobject,
  image_name,
  xmax_adj = 0,
  xmin_adj = 0,
  ymax_adj = 0,
  ymin_adj = 0,
  return_gobject = TRUE
)
}
\arguments{
\item{gobject}{giotto object}

\item{image_name}{spatial locations}

\item{xmax_adj}{adjustment of the maximum x-value to align the image}

\item{xmin_adj}{adjustment of the minimum x-value to align the image}

\item{ymax_adj}{adjustment of the maximum y-value to align the image}

\item{ymin_adj}{adjustment of the minimum y-value to align the image}

\item{return_gobject}{return a giotto object}
}
\value{
a giotto object or an updated giotto image if return_gobject = F
}
\description{
Updates the boundaries of a giotto image attached to a giotto object
}
