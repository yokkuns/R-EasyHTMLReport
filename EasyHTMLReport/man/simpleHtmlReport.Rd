\name{simpleHtmlReport}
\alias{simpleHtmlReport}
\title{
simple html report
}
\description{
send a simple email report from data.frame
}
\usage{
simpleHtmlReport(report.name, mail.from, mail.to, subject, report.data,
                 rmd.header=simpleRmdHeader,rmd.footer=simpleRmdFooter)
}

\arguments{
  \item{report.name}{ report name }
  \item{mail.from}{ mail from }
  \item{mail.to}{ mail to }
  \item{subject}{ mail subject }
  \item{report.data}{report data list. }
  \item{rmd.header}{ Generating function in the header of the Rmd }
  \item{rmd.footer}{ Generating function in the footer of the Rmd }
}
\details{
  report.data can specify the following:
  \describe{
  \item{data}{ data.frame }
  \item{id}{ Column name that is the ID. default is id }
  \item{variable}{ Column name that is the variable. default is variable }
  \item{value}{ Column name that is the value. default is value }
  \item{graph.text}{ Text to be inserted just before the graph. default is "Graph:" }
  \item{fig.width}{ figure width. default is 8 }
  \item{fig.height}{ figure height. default is 4}
  \item{graph.rmd.func}{ Generating function in the graph of the Rmd. default is simpleRmdGraphDateLine }
  \item{table.text}{ Text to be inserted just before the table. default is "Table:"}
  \item{table.rmd.func}{ Generating function in the graph of the Rmd. default is simpleRmdXtableTail }
  }
}
\value{
None
}

\examples{
\dontrun{
simpleHtmlReport("Simple HTML Report",
                 "yohei0511@gmail.com",
                 "yohei0511@gmail.com",
                 "simpleHtmlReport Test",
                 report.data=list(
                     "Report1" = list(data=data1),
                     "Report2" = list(data=data2),
                     "Report3" = list(data=data3)
                 ))
}
}
