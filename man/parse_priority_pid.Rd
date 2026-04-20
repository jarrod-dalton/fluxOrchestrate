\name{parse_priority_pid}
\alias{parse_priority_pid}
\title{Parse a precedence-encoded process ID}
\usage{parse_priority_pid(pid)}
\arguments{
\item{pid}{Character scalar produced by \code{priority_pid()}.}
}
\value{
A list with components \code{priority}, \code{model_id}, and \code{sub_pid}.
}
\description{
Decodes an orchestrated process ID back into priority and routing components.
}
