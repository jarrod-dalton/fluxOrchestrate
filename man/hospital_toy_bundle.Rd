\name{hospital_toy_bundle}
\alias{hospital_toy_bundle}
\title{Create a toy hospital episode model bundle}
\usage{hospital_toy_bundle(hosp_params = NULL)}
\arguments{
\item{hosp_params}{Optional list of toy parameters. Defaults include
\code{admit_wait_mean} and \code{los_mean}.}
}
\value{A model bundle list with schema, propose/transition/stop functions.}
\description{
Returns a minimal demonstration bundle that alternates between outpatient and
inpatient care modes using admission/discharge events.
}
