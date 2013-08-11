\name{EasyHTMLReport-package}
\alias{EasyHTMLReport-package}
\alias{EasyHTMLReport}
\docType{package}
\title{
Easy HTML-Mail Report
}
\description{
It is a package that can be used to send HTML reports easily.
}
\details{
\tabular{ll}{
Package: \tab EasyHTMLReport\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2013-08-11\cr
License: \tab GPL-2 \cr
}

}
\author{
Yohei Sato <yohei0511@gmail.com>
}

\keyword{ package }

\examples{
\dontrun{
rmd.file <- "sample.Rmd"
mail.from <- "yokkuns@tkul.jp"
mail.to <- "yokkuns@tkul.jp"
mail.subject <- "Easy HTML Report"
easyHtmlReport(rmd.file,mail.from,mail.to,mail.subject)
}
}
