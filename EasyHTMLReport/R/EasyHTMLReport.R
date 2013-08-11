sendmailEx <- function(from,to,subject,msg,headers=list(),control=list(),is.debug=F){
    if( is.debug ){
        s <- "
from: %s
to: %s
subject: %s
body: %s"
        s <- sprintf(s,from,to,subject,body)
        cat(s)
        return(s)
    }
    sendmail(from,to,subject,body,headers=headers,control=control)
}

easyHtmlReport <-
function(rmd.file,from,to,subject,headers=list(),control=list(),
         markdown.options=c("hard_wrap","use_xhtml","smartypants"),
         stylesheet="", echo.disable=TRUE, is.debug=F){
  
  insert.echo.false <- function(s){
    s <- gsub("(```\\{r)([^\\}]*)(echo=T[^,]*)([^\\}]*)(\\})", "\\1\\2\\4\\5",s)
    s <- gsub("(```\\{r)([^\\}]*)(\\})", "\\1\\2,echo=FALSE\\3",s)
    s <- gsub("(```\\{r)([^\\}]*)(message=T[^,]*)([^\\}]*)(\\})", "\\1\\2\\4\\5",s)
    s <- gsub("(```\\{r)([^\\}]*)(\\})", "\\1\\2,message=FALSE\\3",s)
    s
  }
  
  if(echo.disable){
    f <- sprintf("%s.tmp.Rmd",rmd.file)
    content <- readLines(rmd.file)
    content.echo.false <- sapply(content, insert.echo.false)
    writeLines(content.echo.false, f)
  } else {
    f <- rmd.file
  }
  
  md.file <- paste(f,"md",sep=".")
  mail.html.file <- paste(f,".html",sep="") ## メール用

  knit(input=f,output=md.file)
  markdownToHTML(file=md.file,output=mail.html.file,
                 stylesheet=stylesheet,
                 options=markdown.options)

  html.str <- paste(readLines(mail.html.file),collapse="\n")
  html.str <- gsub("figure/","cid:",html.str)
  
  imgs <- sapply(list.files("figure"),
                 function(f){
                   mime_part(paste("figure",f,sep="/"),f)
                 })

  body <- unlist(list(list(html.str),imgs))
  headers <- list("Content-Type"="text/html; charset=\"utf-8\"")
  res <- sendmailEx(from,to,subject,body,headers=headers,control=control,is.debug=is.debug)
  
  file.remove(md.file,mail.html.file,paste("figure",list.files("figure"),sep="/")) 
  res
}


simpleRmdHeader <- function(report.name){
    s <- "# %s"

    sprintf(s, report.name)
}

simpleRmdLibrary <- function(){
  s <- "
  \r```{r warning=FALSE,error=FALSE,echo=FALSE,message=FALSE}
  \rlibrary(ggplot2)
  \rlibrary(scales)
  \rlibrary(xtable)
  \rlibrary(reshape2)
  \r```"
  
  s
}

simpleRmdReadData <- function(f){
s <- 
      "
    \r```{r warning=FALSE,error=FALSE,echo=FALSE,message=FALSE}
    \rdata <- read.csv('%s', header=F,sep='\\t')
    \rnames(data) <- c('x', 'variable', 'value')
    \r```"
  sprintf(s, f)
}

simpleRmdXtableTail <- function(table.text="Table:"){
    s <- 
"
  \r%s
  \r```{r warning=FALSE,error=FALSE,echo=FALSE,message=FALSE,results='asis'}
  \rdata.table <- dcast(data, x~variable, value.var='value')
  \rdata.table$x <- as.character(data.table$x)
  \rprint(xtable(tail(data.table)), 'html', include.rownames=F)
  \r```"
    sprintf(s, table.text)
}

simpleRmdXtableAll <- function(table.text="Table:"){
  s <- 
    "
  \r%s
  \r```{r warning=FALSE,error=FALSE,echo=FALSE,message=FALSE,results='asis'}
  \rdata.table <- dcast(data, x~variable, value.var='value')
  \rdata.table$x <- as.character(data.table$x)
  \rprint(xtable(data.table), 'html', include.rownames=F)
  \r```"
  sprintf(s, table.text)
}

simpleRmdGraphDateLine <- function(graph.txt="Graph:",fig.height=4,fig.width=8,xlab="",ylab="value",y.label="comma",...){
    s <- 
"
  \r%s
  \r```{r warning=FALSE,error=FALSE,echo=FALSE,message=FALSE,fig.height=%s,fig.width=%s}
  \rdata$x <- as.Date(data$x)
  \rylimits <- c(0,max(data$value))
  \rdata$variable <- as.factor(data$variable)
  \rgp <- ggplot(data, aes(x=x, y=value, col=variable)) +
  \rgeom_line() +
  \rxlab('%s') +
  \rylab('%s') +
  \rscale_y_continuous(label=%s, limits=ylimits)
  
  \rprint(gp)
  \r```"
    sprintf(s, graph.txt, fig.height, fig.width, xlab, ylab, y.label)
 }


simpleRmdGraphNoOutput <- function(...){
    s <- ""

    s
}

simpleRmdFooter <- function(footer.message="This report email was created in easyHtmlReport package."){
    s <- 
"--------------------------
%s
"
    sprintf(s,footer.message)
}

simpleHtmlReport <- function(report.name, mail.from, mail.to, subject, report.data,
                             rmd.header=simpleRmdHeader,rmd.footer=simpleRmdFooter){
  report.titles <- names(report.data)
  fl <- list()
  
  report.list <- 
    sapply(report.titles,
           function(key){
             d <- report.data[[key]]
             id <- ifelse(is.null(d$id),"id", d$id)
             variable <- ifelse(is.null(d$variable),"variable",d$variable)
             value <- ifelse(is.null(d$value), "value", d$value)
             graph.text <- ifelse(is.null(d$graph.text), "Graph:", d$graph.text)
             fig.width <- ifelse(is.null(d$fig.width), 8, d$fig.width)
             fig.height <- ifelse(is.null(d$fig.height), 4, d$fig.height)
             table.text <- ifelse(is.null(d$table.text), "Table:", d$table.text)
             fl[[key]] <<- sprintf("easy_html_report_tmp_%s.tsv",as.numeric(Sys.time()))
             Sys.sleep(1)
             graph.rmd.func <- ifelse(is.null(d$graph.rmd.func), 
                                      simpleRmdGraphDateLine, 
                                      d$graph.rmd.func)
             table.rmd.func <- ifelse(is.null(d$table.rmd.func),
                                      simpleRmdXtableTail,
                                      d$table.rmd.func)
             
             write.table(d$data[,c(id,variable,value)], file=fl[[key]], 
                         sep="\t", col.names=F, row.names=F)
             
             rmd <- c(sprintf("## %s", key),
                      simpleRmdReadData(fl[[key]]),
                      graph.rmd.func(graph.text, ylab=key, fig.width=fig.width,fig.height=fig.height),
                      table.rmd.func(table.text))
             gen.rmd
           })
  rmd.file <- sprintf("easy_html_report_tmp_%s.Rmd",as.numeric(Sys.time()))
  
  # write rmd file
  writeLines(c(rmd.header(report.name),
               simpleRmdLibrary(),
               report.list,
               rmd.footer()), 
             rmd.file)
  
  res <- easyHtmlReport(rmd.file, mail.from, mail.to, subject)
  
  fl[["rmd.file"]] <- rmd.file
  fl[["tmp.rmd.file"]] <- sprintf("%s.tmp.Rmd",rmd.file)
  for(tmp.file in fl){
    file.remove(tmp.file)
  }
  res
}
