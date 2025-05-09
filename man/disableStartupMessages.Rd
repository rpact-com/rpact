% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_quality_assurance.R
\name{disableStartupMessages}
\alias{disableStartupMessages}
\title{Disable Startup Messages}
\usage{
disableStartupMessages()
}
\value{
This function does not return a value. It is called for its side effects.
}
\description{
This function disables the startup messages for the \code{rpact} package by setting
the \code{rpact.startup.message.enabled} option to \code{FALSE}.
}
\details{
Once this function is called, the startup messages will remain disabled until
explicitly re-enabled using the \code{enableStartupMessages()} function. The current
state is saved using the \code{saveOptions()} function.
}
\examples{
\dontrun{
disableStartupMessages()
}

}
\keyword{internal}
