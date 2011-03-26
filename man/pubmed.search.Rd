\name{pubmed.search}
\alias{pubmed.search}

\title{Make Reference object from PubMed database search}

\description{

Query PubMed database of the National Library of Medicine. R implementation of ESearch CGI. Save to BibTex, RIS, or several other reference file formats. Automatically generates \code{Reference} object of search results.

}

\usage{
pubmed.search(query,limits,save=TRUE,destfile,format="bibtex",show=TRUE,encoding="UTF-8",...)
}

\arguments{

\item{query}{PubMed query string as used in ESearch.}
\item{limits}{List of possible query limits.}
\item{save}{Logical, whether a file of the query results is saved.}
\item{destfile}{Filename, without extension, to store search results.}
\item{format}{Type of export from HubMed. Possible choices \code{ris}, \code{bibtex}, \code{rdf}, \code{mods}.}
\item{show}{Logical, whether to show saved file.}
  \item{encoding}{Character indicating encoding for character strings for RIS file, "latin1" or "UTF-8", handled as in \code{\link{scan}}}
\item{...}{Other arguments to \code{download.file} for saved file.}

}



\details{

Queries are submitted to HubMed which requires an internet connection to process. The \code{query} is a string with a term, optionally followed by a tag description. Of the general form: \emph{term} [field]. Phrases are separated by "+" and terms are separated by a logical operator such as AND or OR. 

Some examples:

A query for articles with the phrase "randomized controlled trial" in the title would be:

query="randomized+controlled+trial[ti]"

Note: For PubMed tags, partial matching is used and capitalization is ignored, so the following two queries are equivalent to the above.

query="randomized+controlled+trial[TI]"

query="randomized+controlled+trial[titl]"

A query for articles on "rheumatoid arthritis" published in the \emph{Journal of the American Medical Association} would be:

query="rheumatoid+arthritis+AND+JAMA[ta]"

Possible fields for the PubMed database are:

\tabular{ll}{
Description\tab tag \cr
Affiliation \tab[AD]\cr
Article Identifier \tab[AID]\cr
All Fields \tab[ALL]\cr
Author \tab[AU]\cr
Book \tab[book]\cr
Comment Corrections
Corporate Author \tab[CN]\cr
Create Date \tab[CRDT]\cr
EC/RN Number \tab[RN]\cr
Editor \tab[ED]\cr
Entrez Date \tab[EDAT]\cr
Filter \tab[FILTER]\cr
First Author Name \tab[1AU]\cr
Full Author Name \tab[FAU]\cr
Full Investigator Name \tab[FIR]\cr
Grant Number \tab[GR]\cr
Investigator \tab[IR]\cr
ISBN \tab[ISBN]\cr
Issue \tab[IP]\cr
Journal \tab[TA]\cr
Language \tab[LA]\cr
Last Author \tab[LASTAU]\cr
Location ID \tab[LID]\cr
MeSH Date \tab[MHDA]\cr
MeSH Major Topic \tab[MAJR]\cr
MeSH Subheadings \tab[SH]\cr
MeSH Terms \tab[MH]\cr
NLM Unique ID \tab[JID]\cr
Other Term \tab[OT]\cr
Pagination \tab[PG]\cr
Personal Name as Subject \tab[PS]\cr
Pharmacological Action \tab[PA]\cr
Place of Publication \tab[PL]\cr
PMID \tab[PMID]\cr
Publisher \tab[PUBN]\cr
Date of print publication \tab [PPDT]\cr
Date of electronic publication\tab [EPDT]\cr
Publication Type \tab[PT]\cr
Secondary Source ID \tab[SI]\cr
Subset \tab[SB]\cr
Supplementary Concept\tab[NM]\cr
Text Words \tab[TW]\cr
Title \tab[TI]\cr
Title/Abstract \tab[TIAB]\cr
Transliterated Title \tab[TT]\cr
UID \tab[PMID]\cr
Volume \tab[VI]
}

The argument \code{limits} allows the user to restrict the date range and number of records returned for the query. \code{limits} is a list with named components that correspond the the limit fields of ESearch. Possible fields are:

\tabular{ll}{
\code{reldate} \tab Limits search results to be within the specified number of days from current date. \cr
\code{mindate} \tab Minimum of date range for search results (examples: 2002; 2002/01/01); must be supplied with \code{maxdate}.\cr
\code{maxdate} \tab Maximum of date range for search results; must be supplied with \code{mindate}.\cr
\code{datetype} \tab Which date field to use in setting date limits. Possible choices are \code{edat}, Entrez date, which is the date article was added to PubMed, or \code{ppdt}, the article publication date. Default is \code{edat}.\cr
\code{retstart} \tab Where in the sequence of returned results to begin retrieving, default is 0.\cr
\code{retmax} \tab Maximum number of records to retrieve, default is 1000.
}

}

\value{

Object of class \code{Reference-class}.

If \code{save} is specified, a file of formatted citations, saved as \code{destfile} and with extension according to selected \code{format}. Items are sorted by PubMed 8-digit ID in decreasing order.
}

\seealso{\code{\link{Reference-class}},\code{\link{download.file}},\code{\link{read.ris}},\code{\link{R2009}}}

\references{

For information on journal coverage for the PubMed database:

\url{http://www.ncbi.nlm.nih.gov/nlmcatalog/journals}

PubMed query help:

\url{http://www.ncbi.nlm.nih.gov/books/NBK3827/}

HubMed

\url{http://www.hubmed.org/}

EUtils ESearch

\url{http://eutils.ncbi.nlm.nih.gov/entrez/query/static/esearch_help.html}

}

\examples{

\dontrun{

# NOT RUN. IF RUN, WOULD SAVE TEMP.BIB IN WORKING DIRECTORY

# QUERY FOR CANCER ARTICLES IN CURRENT ISSUE OF NEJM

cancer <- pubmed.search("cancer[titl]+AND+The+New+England+journal+of+medicine[ta]",
list(reldate=7,datetype="ppdt"),dest="temp",format="bibtex")

head(as.data.frame(cancer))

# QUERY FOR R PACKAGES MENTIONED IN TITLE/ABSTRACT OF NLM PAPERS IN 2009

R2009 <- pubmed.search("R+package[tiab]",list(mindate="2009/01/01",maxdate="2009/12/31",datetype="ppdt"),dest="temp",format="ris")

table(as.data.frame(R2009)$journal)

abstract(R2009)[1]

}

}

\author{S. Kovalchik \email{s.a.kovalchik@gmail.com}}
