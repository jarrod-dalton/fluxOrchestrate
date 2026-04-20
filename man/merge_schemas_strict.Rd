\name{merge_schemas_strict}
\alias{merge_schemas_strict}
\title{Strictly merge compatible schemas}
\usage{merge_schemas_strict(...)}
\arguments{
\item{...}{One or more schemas, or a single list of schemas.}
}
\value{A merged schema list.}
\description{
Merges schema definitions and errors on conflicting definitions for the same
variable. Each input schema is validated via \code{fluxCore::schema_validate()}.
}
