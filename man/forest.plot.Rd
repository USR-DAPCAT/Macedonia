% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcionsMacedonia.R
\name{forest.plot}
\alias{forest.plot}
\title{Forest.plot}
\usage{
forest.plot(
  dadesmodel = "ramo",
  label = dadesmodel$Categoria,
  mean = dadesmodel$OR,
  lower = dadesmodel$Linf,
  upper = dadesmodel$Lsup,
  label_X = "OR (95\% CI)",
  intercept = 1
)
}
\arguments{
\item{dadesmodel}{dadesmodel}

\item{label}{label}

\item{mean}{mean}

\item{lower}{lower}

\item{upper}{upper}

\item{label_X}{label_X}

\item{intercept}{intercept}
}
\description{
Forest.plot
}
