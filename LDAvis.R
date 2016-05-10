devtools::install_github("cpsievert/LDAvisData")
library(LDAvisData)
data(reviews,package="LDAvisData")
library(tm)
stop_words=stopwords("SMART")
reviews=as.character(blocked_reasons$notes)
reviews=gsub(".*-", "", reviews)
reviews=gsub("", "", reviews) # remove apostrophes
reviews=gsub("[[:punct:]]", " ", reviews) # replace punctuation with space
reviews=gsub("[[:cntrl:]]", " ", reviews) #replace control characters with space
reviews=gsub("^[[:space:]]+", "", reviews) #remove whitespace at beginning of documents
reviews=gsub("[[:space:]]+$", "", reviews) # remove whitespace at end of documents 
reviews=tolower(reviews) # force to lower case

doc.list=strsplit(reviews,"[[:space:]]+") # tokenize on space and output as list
# compute the table of terms
term.table=table(unlist(doc.list)) 
term.table=sort(term.table, decreasing=TRUE)
reviews

del=names(term.table) %in% stop_words | term.table < 5
term.table=term.table[!del]
vocab=names(term.table)

get.terms=function(x){
  index=match(x,vocab)
  index=index[!is.na(index)]
  rbind(as.integer(index-1), as.integer(rep(1,length(index))))
}

documents=lapply(doc.list,get.terms)

D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

K <- 3
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, num.iterations = G, alpha = alpha, eta = eta, initial = NULL, burnin = 0,compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop

theta=t(apply(fit$document_sums+alpha,2,function(x) x/sum(x)))
phi=t(apply(t(fit$topics)+eta,2,function(x) x/sum(x)))

MovieReviews=list(phi=phi,theta=theta,doc.length=doc.length,vocab=vocab,term.frequency=term.frequency)
library(LDAvis)
json=createJSON(phi=MovieReviews$phi,theta=MovieReviews$theta,doc.length=MovieReviews$doc.length,vocab=MovieReviews$vocab,term.frequency=MovieReviews$term.frequency)
library(servr)
serVis(json,out.dir='vis',open.browser=T)
