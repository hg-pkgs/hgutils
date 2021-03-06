% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/description-utils.R
\name{description-functions}
\alias{description-functions}
\alias{read.description}
\alias{write.description}
\alias{update_description}
\title{Description functions}
\usage{
read.description()

write.description(description)

update_description(fieldname, value, after = NULL)
}
\arguments{
\item{description}{the DESCRIPTION file.}

\item{fieldname}{the name of the field.}

\item{value}{the new value.}

\item{after}{if the field name is new, the name of the field after which the element is placed.}
}
\description{
Read, write and update the DESCRIPTION file. \code{read.description} reads the
DESCRIPTION file in the current project directory and returns a named list.
\code{write.description} writes the named list back to disk, overwriting the
current DESCRIPTION file. Finally, \code{update_description} combines both functions
by reading the DESCRIPTION file, updating or creating a field and writing the result
back to disk.
}
\details{
The 'Depends', 'Imports' and 'Suggests' fields are sorted before writing the DESCRIPTION file.
}
\examples{
\dontrun{
description = read.description()
write.description(read.description())

#update date in description file
update_description("Date", format(Sys.Date(), "\%Y\%-\%m-\%d"))
}

}
