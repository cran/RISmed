\name{EUtilsGet}
\alias{EUtilsGet}

\title{Results of an NCBI EUtils query}

\description{
Download results of a query for any database of the National Center for Biotechnology Information (NCBI).
}

\usage{
EUtilsGet(ids,type="efetch",db="pubmed")
}

\arguments{

\item{ids}{vector of NCBI records ids, i.e. returned ids from \code{EUtilsSummary}}
\item{type}{string indicating type of NCBI Eutility, \code{efetch}, \code{esummary}, etc.}
\item{db}{string indicating the NCBI database used in query}
}



\details{

Possible E-utilities to supply to \code{type} are \code{efetch}, \code{esummary}, and \code{elink}.

The database name supplied to \code{db} can be any valid Entrez database, such as \code{pubmed}, \code{protein}, \code{nlm}, etc.

For further details on available E-utilities and databases visit \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/}.

}

\value{
For queries to PubMed, a list by article, with named vectors for each field of the article result.

For all other database, a vector with each element name the field returned from \code{XML} output.
}

\seealso{\code{\link{EUtilsSummary}}}

\references{
Summary help:

\url{http://www.ncbi.nlm.nih.gov/books/NBK3827/}

EUtils ESearch

\url{http://eutils.ncbi.nlm.nih.gov/entrez/query/static/esearch_help.html}

}

\examples{

# PUBMED QUERY FOR MYELOMA ARTICLES FOR LAST YEAR
res <- EUtilsSummary("myeloma[ti]",reldate=365)

summary(res)

fetch <- EUtilsGet(id(res))

fetch[[1]]

}

\author{Stephanie Kovalchik \email{s.a.kovalchik@gmail.com}}
