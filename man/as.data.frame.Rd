\name{as.data.frame}
\docType{methods}
\alias{as.data.frame}
\alias{as.data.frame,Reference-method}


\title{Convert \code{Reference} Object Into Data Frame}

\description{

Creating a data frame from a \code{Reference} object,

}


\section{Methods}{
\describe{

\item{\code{signature(x="Reference",au.depth=1,title.length=30)}}{
\tabular{ll}{
\code{x} \tab \code{Reference} object. \cr
\code{au.depth} \tab How many authors, in order of appearance, to include in data frame. \cr
\code{title.length} \tab How many characters of title to include. Default is 30 characters.
        }
    }
 }
}



\details{

The default \code{au.depth} is to list only the first author. When multiple authors occur and the \code{au.depth} is greater than 1, rows will be repeated for each additional author. To have all authors included set \code{au.depth} to \code{Inf}. Similarly, for full title, set \code{title.length} to \code{Inf}.

}

\author{S. Kovalchik \email{s.a.kovalchik@gmail.com}}

\keyword{methods}
