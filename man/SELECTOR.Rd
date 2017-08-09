\name{SELECTOR}
\alias{[.SELECTOR}
\alias{$.SELECTOR}
\alias{[[.SELECTOR}
\alias{[.BLOCK}
\alias{[[.BLOCK}
\title{Convenient subsetting operators on SELECTOR and BLOCK objects}
\usage{}
\arguments{
\item{x}{a SELECTOR or BLOCK object}
}
\description{
  The purpose of these operators is to make it convenient to extract
  values of scalar variables, vectors and data.frames within a SELECTOR
  or BLOCK object.

  \code{x$block} returns the block named \code{block}, e.g., \code{x$B}.

  \code{x[["var"]]} returns the value of the variable named \code{var},
  e.g., \code{x[["NMat"]]}.

  \code{x["var", number]} and \code{x["var", "varName"]}  get a
  two-dimensional data frame from the SELECTOR or BLOCK.
  One can specify the number of rows or alternatively specify the name
  of a variable within the selector file that specifies the number of
  rows.  For example, \code{o["thr",  "NMat"]}.
}

\example{
  o = genSelector()
  o$B
  o$F

    # Scalars
  o[["NMat"]]
  o[["P0"]]
  
   # vector result
  o[["TPrint"]]
  o[["POptm"]]

  o["thr", "NMat"]
  o["thr", 20]

  o["Ks",  "NMat"]
  
  # Avoid the KS in BLOCK B, and focus only on those in BLOCK F.
  # But NMat isn't within block F.
  o$F["Ks", getValue(o, "NMat", "integer")]
}
