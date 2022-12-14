% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-metadata.R
\name{get_round_ids}
\alias{get_round_ids}
\title{Get round ids}
\usage{
get_round_ids(con)
}
\arguments{
\item{con}{a \code{hub_connection} class object.}
}
\value{
a character vector of round ids.
}
\description{
Get round ids
}
\examples{
con <- connect_hub(system.file("hub_1", package = "hubUtils"))
get_round_ids(con)
con <- connect_hub(system.file("scnr_hub_1", package = "hubUtils"))
get_round_ids(con)
}
\seealso{
Other hub-metadata: 
\code{\link{get_task_id_vals}()},
\code{\link{get_task_ids}()}
}
\concept{hub-metadata}
