% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construct.R
\name{rls_construct_policy}
\alias{rls_construct_policy}
\title{Construct a row level security policy}
\usage{
rls_construct_policy(
  name,
  on,
  as = NULL,
  for_ = NULL,
  to = NULL,
  using = NULL,
  with = NULL
)
}
\arguments{
\item{name}{(character) the policy name. required}

\item{on}{(character) the table to apply the policy to. required}

\item{as}{(character) permissive (default) or restrictive.
permissive combines with "OR" while restrictive combines with "AND"}

\item{for_}{(character) permissive (default) or restrictive.}

\item{to}{(character) The role(s) to which the policy is to be applied.
The default is PUBLIC, which will apply the policy to all roles.}

\item{using}{(character) Specifies a filter that is applied to the WHERE
clause of a query}

\item{with}{(character) the check condition}
}
\value{
s3 object of class \code{rls_policy}
}
\description{
Construct a row level security policy
}
\examples{
x <- rls_construct_policy(
  name = "hide_confidential",
  on = "sometable",
  with = "confidential BOOLEAN",
  using = "confidential = false"
)
x
}
\references{
\url{https://www.postgresql.org/docs/current/sql-createpolicy.html}
}
