########
# Part 1
########
require(gdata)
require(slam)
# if you don't have a package, make sure you run this command: install.packages("gdata")
## Needed Packages. Please install before running code:
#install.packages("gdata")
#install.packages("rvest")
#install.packages("plyr")
# install.packages("RCurl")
# install.packages("XML")
# install.packages("tm")
# install.packages("lubridate")
# install.packages("SnowballC")
# install.packages("reshape2")
# #install.packages("ggplot2")
# install.packages("WriteXLS")

# Load correct PERL installation you have locally
# perl <- "C:/strawberry/perl/bin/perl5.24.0.exe"
perl <- "C:/strawberry/perl/bin/perl5.30.0.exe"


# set our SEC EDGAR search variables to be the company's Central Index Key:
CIK <- c("0000092380")
company.name <- c("Southwest Airlines Co")
# enter filing type
file.type <- c("8-K")
entries.to.return <- c(100)
# to keep the results of this markdown document consistent, we will insert a date for the "prior to filter", though this would not be included in practice normally.
prior.date.filter <- c(20190918)

# build the SEC EDGAR search URL
edgar.search.url <- paste0("https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=",CIK,"&type=",file.type,"&dateb=",prior.date.filter,"&owner=exclude&count=",entries.to.return)

# return the URL
edgar.search.url



# load the rvest library ("harvest")
library(rvest)

## Loading required package: xml2S

# read the html from the SEC EDGAR search
sec.8k <- read_html(edgar.search.url)

# using selectgadget (vignette("selectorgadget")) we've identified that the t able containing the listing of the last 100 8-K filings has a CSS values of td:nth-child(i) from i in 1 ... 5 for "filiings", "format", "description", "fi ling date", and "File/Film Number" respectivively.

# store the names of the attributes from the table we want to scrape in a vec tor
sec.table.attributes <- c("filing","format","description","filing.date","File.Number")

# write a function that takes the names of the attributes for the table as in put x, and the html object as input y. the function will return the webscrap ed data frame
scrape.edgar <- function (x , y) {
  for (i in 1:length(x)) {
    css.path <- paste0("td:nth-child(",i,")")
    # html_nodes and html_text are from the rvest library
    z <- y %>%
      html_nodes(css.path) %>%
      html_text()
    
    # evaluate the first few observations of the retrieved data to see where the table really starts
    # uncomment the below to see the results for each invidiual vector printed to the console
    # print(head(z))
    
    # evaluating the first 6 observations of each of the vectors imme diately after scraping the page shows that the first 2 attributes start at ob servation 3, and the rest start at observation 2
    if (i %in% c(1,2)) {s <- c(3:102)} else {s <- c(2:101)}
    
    # store the retrived vectors in a data frame
    df <- data.frame(z[s], stringsAsFactors = FALSE)
    names(df) <- x[i]
    
    if (i == 1) {
      df.k <- df
    } else {
      df.k <- cbind(df.k,df)
    }
    
  }
  
  # return the data frame
  df.k
}

# run the function to store the returned data frame
sec.meta.data <- scrape.edgar(x = sec.table.attributes, y = sec.8k)

# show the first few observations
head(sec.meta.data)

# split the description vector on "Acc-no: " into two vectors
sec.meta.data <- within(sec.meta.data, accession.no <- data.frame(do.call('rbind', strsplit(description, split = "Acc-no: ",fixed = TRUE))))

# load the plyr package to use the mutate function to add a new vector to the data frame
library(plyr)

# create the new accession number vector by taking the first 20 characters of the second variable returned from the string split
sec.meta.data <- mutate(sec.meta.data, accession.num = substr(accession.no$X2,1,20))

# clean up by dropping the "accession.no" data frame from our sec.meta.data d ata frame
sec.meta.data <- sec.meta.data[,!names(sec.meta.data) %in% c("accession.no")]

# check the accession numbers returned
head(sec.meta.data[,c("description","accession.num")])

# build an attribute that is the URL for our corpus documents
sec.meta.data <- mutate(sec.meta.data, sec.filing.url = paste0("https://www.sec.gov/Archives/edgar/data/",CIK,"/",gsub(c("-"),"",accession.num, fixed = TRUE),"/",accession.num,".txt"))

# view the result with the accession.num
head(sec.meta.data[,c("accession.num","sec.filing.url")])


# load the RCurl and the XML libraries
library(RCurl)

## Loading required package: bitops

library(XML)

##
## Attaching package: 'XML'

## The following object is masked from 'package:rvest':
##
## 	xml

# write a function that does the following:
# 1. loops through each of the rows of our meta data
# 2. captures the URL for the complete submission text
# 3. parses the HTML returned from the URL
# 4. removes the line breaks and tags
# 5. packages all the returned text into a single character string
# 6. store the results in a vector
# let x be vector of urls to evaluate
retrieve.documents <- function (x) {
  
  for (i in 1:length(x)) {
    
    # download the html
    html.grab <- 	getURL(x[i], followlocation = TRUE)
    
    # parse the html
    
    html.parse <- htmlParse(html.grab, asText=TRUE)
    
    # retrieve the xmlValue from within the parsed tagsets
    html.text <- unlist(xpathSApply(html.parse, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue))

    # remove the line breaks "\n"
    html.text <- gsub('\\n', ' ', html.text)
    # remove the tabs "\t"
    html.text <- gsub('\\t', ' ', html.text)
    # remove "\z"
    html.text <- gsub('\\z', ' ', html.text)
    # remove "\Z"
    html.text <- gsub('\\Z', ' ', html.text)
    # remove "\u0092"
    html.text <- gsub('\u0092', ' ', html.text, fixed = TRUE)
    # remove "\u0093"
    html.text <- gsub('\u0093', ' ', html.text, fixed = TRUE)
    # remove "\u0094"
    html.text <- gsub('\u0094', ' ', html.text, fixed = TRUE)
    # remove "\u0095"
    html.text <- gsub('\u0095', ' ', html.text, fixed = TRUE)
    # remove "\u0096"
    html.text <- gsub('\u0096', ' ', html.text, fixed = TRUE)
    # remove "\u0097"
    html.text <- gsub('\u0097', ' ', html.text, fixed = TRUE)
    # remove any pipes so we may use pipe delimited files to store this data frame for later use
    html.text <- gsub("|", ' ', html.text, fixed = TRUE)
    
    # capture all the text into a single string
    html.text <- as.vector(paste(html.text, collapse = ' '))
    
    # row bind the result in a vector to store
    if (i == 1) {
      keep <- html.text
    } else {
      keep <- rbind(keep, html.text)
    }
    
  }
  
  # return the final vector of parsed text documents
  keep
}

# store the retrieved documents with the meta data
sec.meta.data$sec.text <- retrieve.documents(sec.meta.data$sec.filing.url)


# view one of the documents for an example of the output created
sec.meta.data[27,] 

# remove any line breaks "\n" from the description prior to writing out
sec.meta.data$description <- gsub('\\n', ' ', sec.meta.data$description)

# write data frame to pipe delimited text file
write.table(sec.meta.data, file = "./sec-8K-documents.txt", sep = "|", row.names = FALSE, quote = FALSE)


# require the tm library - Text Mining in R
require(tm)


# our corpus documents are stored in a vector of a data frame, therefore we'l l use the VectorSource function
sec.corpus <- Corpus(VectorSource(sec.meta.data$sec.text))

# display the corpus object
sec.corpus



# load the lubridate library to change the filing.date to a proper date forma t
library(lubridate)

sec.meta.data$filing.date <- ymd(sec.meta.data$filing.date)

# apply the meta data to the corpus
meta(sec.corpus, type= "corpus", tag = "author") <- rep(paste0("Company, CIK: ",CIK,", SEC"),100)
meta(sec.corpus, type = "corpus", tag = "datetimestamp") <- sec.meta.data$filing.date
meta(sec.corpus, type = "corpus", tag = "description") <- sec.meta.data$description
meta(sec.corpus, type = "corpus", tag = "heading") <- sec.meta.data$filing
meta(sec.corpus, type = "corpus", tag = "id") <- sec.meta.data$accession.num
meta(sec.corpus, type = "corpus", tag = "origin") <- sec.meta.data$sec.filing.url

# Print the meta data for the first three documents
for (i in 1:3) {print(paste("Document ",i)); print(meta(sec.corpus[[eval(i)]]))}

########
# Part 2
########

# import the previously saved text and document meta data
sec.meta.data <- read.table(file = "./sec-8K-documents.txt", sep = "|", header = TRUE, stringsAsFactors = FALSE, encoding="UTF-8", strip.white = TRUE, quote = "")
sec.meta.data <- data.frame(sec.meta.data, stringsAsFactors = FALSE)

# require the tm library - Text Mining in R
require(tm)

library(SnowballC)
# options("mc.cores" = 4) # change this value to the number of cores your computer has

# our corpus documents are stored in a vector of a data frame, therefore we'l l use the VectorSource function
sec.corpus <- Corpus(VectorSource(sec.meta.data$sec.text))

# load the lubridate library to change the filing.date to a proper date forma t
library(lubridate)

sec.meta.data$filing.date <- ymd(sec.meta.data$filing.date)

# apply the meta data to the corpus
meta(sec.corpus, type= "corpus", tag = "author") <- rep(paste0("Company, CIK: ",CIK,", SEC"),100)
meta(sec.corpus, type = "corpus", tag = "datetimestamp") <- sec.meta.data$filing.date
meta(sec.corpus, type = "corpus", tag = "description") <- sec.meta.data$description
meta(sec.corpus, type = "corpus", tag = "heading") <- sec.meta.data$filing
meta(sec.corpus, type = "corpus", tag = "id") <- sec.meta.data$accession.num
meta(sec.corpus, type = "corpus", tag = "origin") <- sec.meta.data$sec.filing.url

#### Link to dictionary list: http://www3.nd.edu/~mcdona l d/Word_Li s ts .html

# make all of the words lower case
sec.corpus <- tm_map(sec.corpus, content_transformer(tolower))

# remove standard english stop words
sec.corpus <- tm_map(sec.corpus, content_transformer(removeWords), stopwords('english'))

# remove punctuation
sec.corpus <- tm_map(sec.corpus, content_transformer(removePunctuation))

# remove numbers
sec.corpus <- tm_map(sec.corpus, content_transformer(removeNumbers))

# import the master list of stop words from Loughran and McDonald's financial stop words lists
#stop.words <- read.table("./stop-words-master.txt", header = TRUE, sep = "|", stringsAsFactors = FALSE)

# The remove words function can only handle 1,000 words at a time, so we'll n eed to loop
# through all of the words that are in the Loughran and McDonald stop words lists.

# Let x be a corpus
# Let y be a vector containing words to remove
removeManyWords <- function (x, y) {
  
  n <- ceiling(length(y)/1000)
  s <- 1
  
  e<- 1000
  
  for (i in 1:n) {
    
    x <- tm_map(x, content_transformer(removeWords), y[s:e])
    s <- s + 1000 
    e <- e + 1000
    
  }
  
  x
  
}

# remove all Loughran and McDonald stop words
#sec.corpus <- 	removeManyWords(sec.corpus,stop.words$stop.words)

# strip white space
sec.corpus <- tm_map(sec.corpus, content_transformer(stripWhitespace))

# inspect a document after preprocessing to show the changes
lapply(content(sec.corpus[27]), as.character)

# Create a term-document matrix with the documents as rows and terms as colum ns
sec.tdm <- DocumentTermMatrix(sec.corpus)

# return the dimensions of the matrix
dim(sec.tdm)

### Harvard Dictionary Link: http://www.wj h.ha rva rd.edu/~i nqui rer/homeca t.htm
# import the harvard iv-4 dictionary
harvard.url <- "http://www.wjh.harvard.edu/~inquirer/inquirerbasic.xls"
harvard.word.lists <- read.xls(harvard.url,perl=perl)

# create a vector of the positive sentiment words
positive.sent <- subset(harvard.word.lists, Positiv == c("Positiv"), select = c("Entry"))[,c("Entry")]

# show the first few positive sentiment words
head(positive.sent)


# create a vector of negative sentiment words
negative.sent <- subset(harvard.word.lists, Negativ == c("Negativ"), select = c("Entry"))[,c("Entry")]

# show the first few negative sentiment word
head(negative.sent)

# change the term document matrix into a data frame
sec.tdm.df <- as.data.frame(as.matrix(sec.tdm))

# calculate total terms per document
total.terms <- rowSums(sec.tdm.df)

# Convert words to upper case
colnames(sec.tdm.df)<-toupper(colnames(sec.tdm.df))

# calculate the total positive sentiment terms
fin.pos.terms   <- rowSums(sec.tdm.df[,names(sec.tdm.df) %in% positive.sent])
total.pos.terms <- rowSums(sec.tdm.df[,names(sec.tdm.df) %in% positive.sent])

# calculate the total negative
total.neg.terms <- rowSums(sec.tdm.df[,names(sec.tdm.df) %in% negative.sent])

# combine in a data frame with some of the original sec meta data
sec.sentiment <- cbind(sec.meta.data[,1:7], total.terms, total.pos.terms, total.neg.terms)

# calculate the percentages
library(plyr)

sec.sentiment <- mutate(sec.sentiment
                        ,pos.percent = total.pos.terms/total.terms * 100
                        ,neg.percent = total.neg.terms/total.terms * 100
                        ,net.sentiment = pos.percent - neg.percent
)

# show the first few observations of the output
head(sec.sentiment[,c("filing","filing.date","pos.percent","neg.percent","net.sentiment")])

# load the reshape2 library to melt the data frame to make it long instead of wide
library(reshape2)

sec.sentiment.melt <- melt(sec.sentiment,id.vars = c("accession.num","filing","filing.date"),measure.vars = c("pos.percent", "neg.percent","net.sentiment"))

# load the ggplot2 library
library(ggplot2)

# build the plot of sentiment percentages over time
g <- ggplot(data = subset(sec.sentiment.melt, variable %in% c("pos.percent","neg.percent")), mapping = aes(x = filing.date, y = value, group = variable))
                   
g <- g + geom_line(aes(color = factor(variable, labels = c("positive","negative")))) + theme(legend.position="top")

g <- g + labs(title = paste(company.name,"Traditional Sentiment Analysis of SEC 8-K Filings")
                                 ,x = "Filing Date"
                                 ,y = "Percent of Total Document Terms"
                                 ,color = "Sentiment"
                                 ,labels = c("postivie","negative"))

g <- g + geom_smooth(aes(color =factor(variable, labels = c("positive","negative"))), linetype = 1, method = "lm")
                   

# print the graph
g

# graph the net change in sentiment
g <- ggplot(data = subset(sec.sentiment.melt, variable %in% c("net.sentiment"
)), mapping = aes(x = filing.date, y = value, group = variable))
g <- g + geom_line() + theme(legend.position="top")
g <- g + labs(title = paste(company.name,"Net Sentiment of SEC 8-K Filings")
              ,x = "Filing Date"
              ,y = "Net Positive Percentage Point Sentiment of Document Terms"
              ,color = "Sentiment"
              ,labels = c("postivie","negative"))
g <- g + geom_smooth(linetype = 1, method = "lm")

# print the graph
g


url = "http://www3.nd.edu/~mcdonald/Word_Lists_files/LoughranMcDonald_MasterDictionary_2014.xlsx"
# import the financial sentiment dictionary
fin.dictionary <- read.xls(url)

# create a vector of positve terms
positive.sent.fin <- subset(fin.dictionary, Positive > 0, select = c("Word")) [,c("Word")]

# create a vector of negative terms
negative.sent.fin <- subset(fin.dictionary, Negative > 0, select = c("Word")) [,c("Word")]

# create a vector of uncertainty terms
uncertainty.sent.fin <- subset(fin.dictionary, Uncertainty > 0, select = c("Word"))[,c("Word")]

# create a vector of litigious terms
litigious.sent.fin <- subset(fin.dictionary, Litigious > 0, select = c("Word"))[,c("Word")]

# modal == 1 indicates strong
# modal == 2 indicates moderate
# model == 3 indicates weak
strong.sent.fin <- subset(fin.dictionary, Modal == 1, select = c("Word"))[,c("Word")]
moderate.sent.fin <- subset(fin.dictionary, Modal == 2, select = c("Word"))[,c("Word")]
weak.sent.fin <- subset(fin.dictionary, Modal == 3, select = c("Word"))[,c("Word")]

# now calculate the row sums:
# calculate the total positive sentiment terms
fin.pos.terms <- rowSums(sec.tdm.df[,names(sec.tdm.df) %in% positive.sent.fin])
fin.neg.terms <- rowSums(sec.tdm.df[,names(sec.tdm.df) %in% negative.sent.fin])
fin.unc.terms <- rowSums(sec.tdm.df[,names(sec.tdm.df) %in% uncertainty.sent.fin])
fin.lit.terms <- rowSums(sec.tdm.df[,names(sec.tdm.df) %in% litigious.sent.fin])
fin.strong.terms <- rowSums(sec.tdm.df[,names(sec.tdm.df) %in% strong.sent.fin])
fin.moderate.terms <- rowSums(sec.tdm.df[,names(sec.tdm.df) %in% moderate.sent.fin])
fin.weak.terms <- rowSums(sec.tdm.df[,names(sec.tdm.df) %in% weak.sent.fin])

# attach to our dataframe of sentiment analysis results
sec.sentiment <- cbind(sec.sentiment, fin.pos.terms, fin.neg.terms, fin.unc.terms, fin.lit.terms, fin.strong.terms, fin.moderate.terms, fin.weak.terms)

# calulate percent of total document terms
sec.sentiment <- mutate(sec.sentiment
                        ,fin.pos.percent = fin.pos.terms/total.terms * 100
                        ,fin.neg.percent = fin.neg.terms/total.terms * 100
                        ,fin.unc.percent = fin.unc.terms/total.terms * 100
                        ,fin.lit.percent = fin.lit.terms/total.terms * 100
                        ,fin.strong.percent = fin.strong.terms/total.terms * 100
                        ,fin.moderate.percent = fin.moderate.terms/total.terms * 100
                        ,fin.weak.percent = fin.weak.terms/total.terms * 100
)
head(sec.sentiment)

# melt the data frame again to make it long instead of wide
sec.sentiment.melt <- melt(sec.sentiment,id.vars = c("accession.num","filing","filing.date"), measure.vars = c("pos.percent", "neg.percent","net.sentiment","fin.pos.percent","fin.neg.percent","fin.unc.percent","fin.lit.percent","fin.strong.percent","fin.moderate.percent","fin.weak.percent"))

# graph the output
g <- ggplot(data = subset(sec.sentiment.melt, variable %in% c("fin.pos.percent","fin.neg.percent","fin.unc.percent","fin.lit.percent")),  mapping = aes(x =filing.date, y = value, group = variable))

g <- g + geom_line(aes(color = factor(variable, labels = c("positive","negative","uncertainty","litigious")))) + theme(legend.position="top")

g <- g + labs(title = paste(company.name,"Financial Sentiment Analysis of SEC 8-K Filings")
                                         ,x = "Filing Date"
                                         ,y = "Percent of Total Document Terms"
                                         ,color = "Sentiment")

g <- g + geom_smooth(aes(color =factor(variable, labels = c("positive","negative","uncertainty","litigious"))), linetype = 1, method = "lm", se = FALSE)
                           
# print the graph
g


# graph the model output
g <- ggplot(data = subset(sec.sentiment.melt, variable %in% c("fin.strong.percent","fin.moderate.percent","fin.weak.percent")),  mapping = aes(x = filing.date, y = value, group = variable))
g <- g + geom_line(aes(color = factor(variable, labels = c("strong","moderate","weak")))) + theme(legend.position="top")

g <- g + labs(title = paste(company.name,"Financial Modal Statement Analysis of SEC 8-K Filings")
              ,x = "Filing Date"
              ,y = "Percent of Total Document Terms"
              ,color = "Modal Statement Strength")
g <- g + geom_smooth(aes(color =factor(variable, labels = c("strong","moderate","weak"))), linetype = 1, method = "lm", se = FALSE)

# print the graph
g

# Save Data for further processing in Excel
#install.packages("WriteXLS")
library(WriteXLS)
WriteXLS(sec.sentiment, ExcelFileName="my_company_sentiment.xls")

                           

                           
                        