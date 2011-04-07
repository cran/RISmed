\name{R2009}
\docType{data}
\alias{R2009}
\title{Instance of \code{Reference} Class}

\description{
	This dataset is an object of class \code{Reference}. It was obtained from a search of the PubMed Entrez database, using the function \code{pubmed.search}. The results of this query were all articles in 2009 mentioning an R package in the title or abstract.
}

\usage{R2009}

\format{A \code{Reference} object with 10 records.}

\source{PubMed database of the US National Library of Medicine}

\examples{

data(R2009)

#SHOW BRIEF META-DATA
R2009

summary(R2009)

#ABSTRACTS FOR FIRST 3 RECORDS
abstract(R2009)[1:3]

#KEYWORDS FOR FIRST 3 RECORDS
keyword(R2009)[1:3]

#TABULATE JOURNALS
table(as.data.frame(R2009)$journal)


}

\seealso{\code{\link{pubmed.search}},\code{\link{Reference-class}}}

\author{S. Kovalchik \email{s.a.kovalchik@gmail.com}}
