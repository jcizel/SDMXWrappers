% Generated by roxygen2 (4.0.2): do not edit by hand
\name{sdmxGetAllData}
\alias{sdmxGetAllData}
\title{Pull all data from a given provider, and reshape them into the
analysis-ready format.}
\usage{
sdmxGetAllData(provider = "ECB", folder = "/Users/jankocizel/Downloads",
  flow = NULL)
}
\arguments{
\item{provider}{Data provider, e.g. "ECB"}

\item{folder}{Destination folder}

\item{flow}{Data flow to download. If NULL, download all flows for a given provider.}
}
\value{
NULL. The function saves all datasets as excel files in a
prespecified folder
}
\description{
.. content for \description{} (no empty lines) ..
}
\details{
.. content for \details{} ..
}
\author{
Janko Cizel
}

