\name{MinutePubmed}
\alias{MinutePubmed}

\title{
Extracts \code{MinutePubmed} from  \code{Medline} object.
}

\description{
Extractor for the \code{MinutePubmed} of a \code{Medline} object. This date corresponds to the date the article was made available through Pubmed Central.
}
\usage{
MinutePubmed(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
MinutePubmed(myeloma)
}
