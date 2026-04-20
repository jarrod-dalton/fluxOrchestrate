\name{orchestrated_bundle}
\alias{orchestrated_bundle}
\title{Compose multiple model bundles into a single orchestrated bundle}
\usage{orchestrated_bundle(models, policy = NULL, schema = NULL)}
\arguments{
\item{models}{Named list of model bundles to orchestrate.}
\item{policy}{Optional list overriding orchestration hooks such as
\code{eligible_models}, \code{event_priority}, \code{on_transition}, and \code{stop}.}
\item{schema}{Optional pre-merged schema. If \code{NULL}, schemas are merged from
\code{fluxCore::default_entity_schema()} and model bundle schemas.}
}
\value{A model bundle list compatible with \code{fluxCore::Engine}.}
\description{
Builds an orchestration-layer bundle that routes proposals and transitions to
underlying model bundles, applies priority-based process IDs, and supports
cross-model policy hooks.
}
