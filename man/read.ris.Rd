\name{read.ris}
\alias{read.ris}


\title{Convert RIS file of bibliographic records into \code{Reference} object}

\description{
Reads a file in Research Information Systems (RIS) format and returns \code{Reference} object.
}

\usage{
read.ris(file,id.type="author.year",encoding="UTF-8")
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{character containing path and filename for RIS file}
  \item{id.type}{Determines how unique id is made for keys, either \code{author.year}, \code{pubmed} or \code{numeric} for automatic generation, or user-supplied vector of character strings equal in length to number of records. See details.}
  \item{encoding}{assumed encoding for character strings in \code{file}, "latin1" or "UTF-8", handled as in \code{\link{scan}}}
}

\details{

Automatic id generation can be of type "author.year", where the last name of the first author and year of date of publication form the keys, e.g. "Foo2010". If there are non-unique instances of this key, a roman letter is appended. Other options are "pubmed", which takes the PubMed ID if available in RIS records of MEDLINE articles, or "numeric" which is simply the record index.

When a vector is supplied for \code{id.type}, this is taken to be the user-defined ids.

}

\value{

A \code{Reference} object.
}

\author{S. Kovalchik \email{s.a.kovalchik@gmail.com}}

\seealso{\code{\link{Reference-class}}, \code{\link{scan}}, \code{\link{pubmed.search}}}

\keyword{file}
\keyword{connection}
