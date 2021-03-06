% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proj.r
\docType{data}
\name{us_laea_proj}
\alias{us_laea_proj}
\title{Albers equal-area conic convenience projection}
\format{An object of class \code{character} of length 1.}
\usage{
us_laea_proj
}
\description{
The following PROJ.4 string is used: \code{+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs}
}
\details{
In a ggplot2+ggalt context, you can just use variable name as the value for the \code{proj}
parameter to \code{ggalt::coord_proj()}. i.e. \code{coord_proj(us_laea_proj)}.
}
\examples{
us <- usa_composite()
us <- sp::spTransform(us, sp::CRS(us_laea_proj))
us <- usa_composite(proj="laea")
counties <- counties_composite("laea")
}
\seealso{
Other convenience projections: 
\code{\link{us_aeqd_proj}},
\code{\link{us_eqdc_proj}},
\code{\link{us_lcc_proj}}
}
\concept{convenience projections}
\keyword{datasets}
