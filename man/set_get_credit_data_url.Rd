% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/url_utils.R
\name{set_get_credit_data_url}
\alias{set_get_credit_data_url}
\title{Set the API URL based on audience to retrieve credit data report.}
\usage{
set_get_credit_data_url(audience, order_id)
}
\arguments{
\item{audience}{\code{character} Must be one of: "dev" or "prod", corresponding to the Audience Parameter.}

\item{order_id}{\code{character} Order ID for credit report.}
}
\value{
\code{character} URL for credit data report endpoint.
}
\description{
Set the API URL based on audience to retrieve credit data report.
}
\examples{
\dontrun{
credit_data_url <- set_get_credit_data_url("dev", "abc123")
}

}
