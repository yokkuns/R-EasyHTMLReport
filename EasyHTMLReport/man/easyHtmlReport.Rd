\name{easyHtmlReport}
\alias{easyHtmlReport}
\title{
Easy to send HTML reports.
}

\usage{
easyHtmlReport(rmd.file, from, to, subject, headers = list(), control = list(), markdown.options = c("hard_wrap", "use_xhtml", "smartypants"), stylesheet = "",echo.disable=TRUE)
}

\arguments{
  \item{rmd.file}{
    R Markdown file.
}
  \item{from}{
    mail from.
}
  \item{to}{
    mail to.
}
  \item{subject}{
    mail subject.
}
  \item{headers}{
    mail headers.
}
  \item{control}{
    mail control.
}
  \item{markdown.options}{
    markdownToHTML options.
}
  \item{stylesheet}{
    stylesheet file.
}
  \item{echo.disable}{
    if echo.disable=TRUE, to set the "echo=FALSE" and "message=FALSE"
}
}

\value{
None
}

\author{
  Yohei Sato
}

\seealso{
  sendmailR::sendmail,markdown::markdownToHTML
}
\examples{
\dontrun{
rmd.file <- "sample.Rmd"
mail.from <- "yokkuns@tkul.jp"
mail.to <- "yokkuns@tkul.jp"
mail.subject <- "Easy HTML Report"
easyHtmlReport(rmd.file,mail.from,mail.to,mail.subject)
}
}


