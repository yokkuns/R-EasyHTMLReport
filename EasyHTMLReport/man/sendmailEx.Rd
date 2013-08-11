\name{sendmailEx}
\alias{sendmailEx}
\title{Extended version sendmailR::sendmail}
\usage{
  sendmailEx(from, to, subject, msg, headers = list(),
    control = list(),is.debug=F)
}
\arguments{
  \item{from}{From whom the mail message is (RFC2822 style
  address).}

  \item{to}{Recipient of the message (valid RFC2822 style
  address).}

  \item{subject}{Subject line of message.}

  \item{msg}{Body text of message or a list containing
  sendmailR::mime_part objects.}

  \item{headers}{Any other headers to include.}

  \item{control}{List of SMTP server settings. Valid values
  are the possible options for sendmailR::sendmail_options.}
  
  \item{is.debug}{ debug flag. If TRUE, not send mail.}
  
}
\description{
  It is a function that extends the sendmailR :: sendmail.
It is to be able to and can set the "content type" to "header" and send it to multiple destinations.
}
\examples{
\dontrun{
from <- sprintf("<sendmailR@\\\\\%s>", Sys.info()[4])
to <- "<olafm@datensplitter.net>"
subject <- "Hello from R"
body <- list("It works!", mime_part(iris))
sendmail(from, to, subject, body,
         control=list(smtpServer="ASPMX.L.GOOGLE.COM"))
}
}

