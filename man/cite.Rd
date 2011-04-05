\name{cite}
\docType{methods}
\alias{cite}
\alias{cite,Reference-method}


\title{Print citations for records of a \code{Reference} object}

\description{

Vector of citations for a \code{Reference} object using the style of the International Committee of Medical Journal Editors (ICMJE).

}


\section{Methods}{
\describe{

\item{\code{signature(object="Reference",author.count=3)}}{
\tabular{ll}{
\code{object} \tab \code{Reference} object. \cr
\code{author.count} \tab Number of authors to include. If exceeded, "et al." is used for remaining authors.
        }
    }
 }
}



\details{

For the uniform requirements of the ICMJE see \url{http://www.nlm.nih.gov/bsd/uniform_requirements.html}

}

\examples{

data(R2009)

cite(R2009)[1:5]

}

\author{S. Kovalchik \email{s.a.kovalchik@gmail.com}}

\keyword{methods}
