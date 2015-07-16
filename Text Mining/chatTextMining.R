
cname = file.path('C:\\Users\\Anthony Val\\Documents\\R-programming\\Text Mining','Chat')
# dir(name) # Use this to check to see that your texts have loaded

# Loading the R package for text mining and then loading the texts into R

library(tm)
docs <- Corpus(DirSource(cname))

# inspecting the metadata of the documents
a=inspect(docs[1])
b=inspect(docs[2])


#=============================================================
# THE PREPROCESSING PART
#=============================================================

#removing punctuations
docs <- tm_map(docs,removePunctuation)

#removing numbers
docs <- tm_map(docs,removeNumbers)

#converting to lowercase
docs <- tm_map(docs,tolower)

#remove stopwords
docs <- tm_map(docs,removeWords,stopwords('english'))

#remove particular words
#docs <- tm_map(docs,removeWords,c(':/',':)',))

#stripping unnecessary whitespace from your documents
docs <- tm_map(docs,stripWhitespace)

# TO finish, be sure to use the following script once you have completed preprocessing
docs <- tm_map(docs,PlainTextDocument)

#====================================
# This is the end of preprocessing
#====================================


#=====================================================================
#   Stage the Data
#=====================================================================
dtm = DocumentTermMatrix(docs)
tdm = TermDocumentMatrix(docs) #transpose of the dtm matrix

#=====================================================================
#   Exploring data
#=====================================================================
freq <- colSums(as.matrix(dtm))   
length(freq)  

ord <- order(freq)  

#exporting dtm & tdm  as matrix 
m <- as.matrix(dtm)   
dim(m)   
#write.csv(m, file="dtm.csv")

n <- as.matrix(tdm)   
dim(n)   
#write.csv(n, file="tdm.csv")

# removing the sparse terms:
dtms = removeSparseTerms(dtm,0.1)

# view a table of the terms we selected when we removed the sparse terms
freq = colSums(as.matrix(dtms))
freq = sort(freq, decreasing=TRUE)

# a much better way to do the table
wf = data.frame(word=names(freq),freq=freq)

#==================================================================
# Plot Word Frequencies
#==================================================================

library(ggplot2)
p = ggplot(subset(wf,freq > 20), aes(word,freq))
p = p + geom_bar(stat='identity')
p = p + theme(axis.text.x=element_text(angle=45,hjust=1))


# ==========================================================
# Relationships between terms
# ==========================================================

findAssocs(dtm,'haha',corlimit=0.98)

# ==========================================================
# Word Clouds
# ==========================================================

library(wordcloud)

set.seed(42)
dark2 = brewer.pal(6,'Dark2')
wordcloud(names(freq),freq,min.freq = 5,rot.per=0.2,colors=dark2)

# ==========================================================
# Hierarchal Clustering
# ==========================================================
dtmss = removeSparseTerms(dtm,0.90)
library(cluster)
d = dist(t(dtmss),method='euclidian')
fit = hclust(d=d,method='ward')
plot(fit,hang=-5)

