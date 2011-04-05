\name{Reference-class}
\docType{class}
\alias{Reference-class}

\title{Class "Reference"}

\description{
Class for bibliographic content that closely follows RIS format.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Reference", ...)}. Each slot represents a specific field, such as "journal" or "title", and each index item corresponds to an individual record.
}
\section{Slots}{
  \describe{
    \item{\code{id}:}{Object of class \code{"character"}. Record unique identifier. }
    \item{\code{au}:}{Object of class \code{"list"}. Authors (Last, First M) in order as in publication. }
    \item{\code{jo}:}{Object of class \code{"character"}. Journal.  }
    \item{\code{pb}:}{Object of class \code{"character"}. Publisher. }
    \item{\code{ti}:}{Object of class \code{"character"}. Title of record.}
    \item{\code{vl}:}{Object of class \code{"character"}. Volume. }
    \item{\code{is}:}{Object of class \code{"character"}. Issue.}
    \item{\code{year}:}{Object of class \code{"numeric"}. Year of publication.}
    \item{\code{month}:}{Object of class \code{"numeric"}. Month of publication.}
    \item{\code{day}:}{Object of class \code{"numeric"}. Day of publication.}
    \item{\code{sp}:}{Object of class \code{"character"}. Starting page. }
    \item{\code{ep}:}{Object of class \code{"character"}. Ending page.}
    \item{\code{ab}:}{Object of class \code{"character"}. Abstract.}
     \item{\code{kw}:}{Object of class \code{"list"}. Keywords.}
  }
}

\section{Methods}{

  \describe{

\item{show}{\code{signature(object = "Reference")}:
Abbreviated data.frame of \code{Reference} object.
  }

\item{summary}{\code{signature(object = "Reference",...)}:
Returns the number of records for \code{Reference} object and frequency counts of records by year.
  }

\item{as.data.frame}{\code{signature(x = "Reference",au.depth=1,title.length=30)}:
Create a data.frame from a \code{Reference} object.
  }

\item{abstract}{\code{signature(object = "Reference")}:
Obtain vector of abstracts from \code{Reference} object.
  }

\item{keyword}{\code{signature(object = "Reference")}:
Obtain list fo keywords from \code{Reference} object.
  }

 }
}


\keyword{classes}
