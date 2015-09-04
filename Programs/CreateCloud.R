library(twitteR); library(SnowballC); library(tm); library(RColorBrewer); library(wordcloud); 
setwd('/Users/elundquist/Repositories/tweecloud')

# set desired search term here (should be lowercase)
sterm <- 'tebow'

# 1. set up twitter authentication so that I can run queries through the API
#---------------------------------------------------------------------------

source('./Programs/authentication.r')
setup_twitter_oauth(ckey, csec, atok, asec)

# 2. search twitter for mentions of a specific podcast and convert text to a char vector
#---------------------------------------------------------------------------------------

# retrieve recent english-language tweets containing the search term and transform into character vector
tlist <- searchTwitter(sterm, n=500, lang='en', resultType='recent')
ttext <- sapply(tlist, function(x) x$getText())

# convert the text from UTF-8 to ASCII to remove emojis and other difficult characters
ttext <- iconv(ttext, from='utf8', to='ascii', sub="")
   
# remove the search term itself as well as the RE-TWEET indicator            
ttext <- gsub(sterm, '', tolower(ttext), fixed=TRUE) 
ttext <- gsub('RT',  '', tolower(ttext), fixed=TRUE)

# remove hyperlinks and URL escape sequences
ttext <- gsub('(http(s)?:.+$)|(http(s)?://.+ )', '', ttext)           
ttext <- gsub('&.+;', '', ttext)                    

# transform the text data into a virtual corpus to strip whitespace and remove stopwords
tcorp <- VCorpus(VectorSource(ttext))               
tcorp <- tm_map(tcorp, stripWhitespace)               
tcorp <- tm_map(tcorp, removeWords, stopwords('en')) 

# create a term-document matrix which shows counts of how often each term is mentioned in each document
tdm <- TermDocumentMatrix(tcorp, control = list(minWordLength = 3))
findFreqTerms(tdm, 5) 

# 3. create a WordCloud with the transformed twitter text and export as a JPEG
#-----------------------------------------------------------------------------

# create a two-column data frame that will hold [unique terms, frequency count]
tmat <- as.matrix(tdm)
freq <- sort(rowSums(tmat), decreasing=TRUE)
df <- data.frame(term=names(freq), freq=freq)
rownames(df) <- NULL

# create a wordcloud wrt the terms and associated frequencies and export as a jpeg
pal <- brewer.pal(8,"Dark2")
wordcloud(df$term, df$freq, min.freq=3, max.words=60, scale=c(2,0.75), colors = pal)

sterm <- gsub("[#@]", "", sterm)
dev.copy(jpeg, filename = sprintf("./Clouds/%s.jpg", sterm))
dev.off()


