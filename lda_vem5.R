library(XML)
library(topicmodels)
library(tm)
files = system("ls -1 /media/ashish/_data/cancerStudies/*.xml", intern = TRUE)

## preprocess files by storing detailed description of study in a seperate text file

ingestXml = function() {
  for (i in 1:filesize){
    textfile = paste(files[i],".txt",sep="")
    xmldoc = xmlParse(files[i])
    rootNode = xmlRoot(xmldoc)
    data = xmlSApply(rootNode, function(x) xmlSApply(x, xmlValue))
    data$detailed_description = paste(data$detailed_description,"\n",sep="\n")
    cat(data$detailed_description, file=textfile)
    if (i %% 50 == 0) {
      print(i)
    }
  }
}
    

text2Corpus = function() {
  ## load files into corpus
  setwd("/media/ashish/_data/cancerStudies")
  filenames = list.files(getwd(),pattern="*.txt")
  
  #read files into a character vector
  txtFiles = lapply(filenames,readLines)
  
  #create corpus from vector
  researchDocs = Corpus(VectorSource(txtFiles))
  
  ## transform to lower case
  researchDocs = tm_map(researchDocs,content_transformer(tolower))
  
  ## remove punctuation
  researchDocs = tm_map(researchDocs, removePunctuation)
  
  ## remove numbers
  researchDocs = tm_map(researchDocs, removeNumbers)
  
  ## remove whitespace
  researchDocs = tm_map(researchDocs, stripWhitespace)
  
  ## remove stopwords
  researchDocs = tm_map(researchDocs, removeWords, stopwords("english"))
  
  ## stem documents
  researchDocs = tm_map(researchDocs, stemDocument)
  
  ## create a document term matrix
  dtm = DocumentTermMatrix(researchDocs)
  
  rownames(dtm) = txtFiles
  
  ## add up column to find term freq
  freq = colSums(as.matrix(dtm))
  length(freq)
  ## sort them in decreasing order
  ord <- order(freq,decreasing=TRUE)
  freq[ord]
  ## save them in the CSV file but dont write any column headers
  write.table(freq[ord],"vem_word_freq.csv", col.names = FALSE)
  print("Word freq CSV file saved!")
  
  ## it is possible that this sparse matrix will contain row without entires.
  ## one idea is to compute the sum of words by row
  rowTotals = apply(dtm , 1, sum)
  dtm.new   = dtm[rowTotals> 0, ] 
  return(dtm.new)
}

runlda = function(datamatrix, topics, tc) {
  k = topics #number of topics 
  termcount = tc   #top terms in each topic
  controllist5 = list(alpha = 50/k, estimate.beta = TRUE,
                     nstart = 5, seed = list(44773, 29382, 1485, 99516, 112604),
                     best = TRUE, 
                     var = list(iter.max = 500, tol = 10^-6),
                     em = list(iter.max = 1000, tol = 10^-4),
                     initialize = "random")
  controllist1 = list(alpha = 50/k, estimate.beta = TRUE,
                      nstart = 1, seed = list(44773),
                      best = TRUE, 
                      var = list(iter.max = 500, tol = 10^-6),
                      em = list(iter.max = 1000, tol = 10^-4),
                      initialize = "random")
  
  print("Start LDA")
  ldaOut = LDA (datamatrix, k, method = "VEM", control = controllist5)
  print("write topics")
  ldaOut.topics = as.matrix(topics(ldaOut))
  write.csv(ldaOut.topics, file=paste("LDAVEM", k, "DocsToTopics.csv",sep=""))
  ldaOut.terms <- as.matrix(terms(ldaOut,termcount))
  write.csv(ldaOut.terms,file=paste("LDAVEM",k,"TopicsToTerms.csv",sep=""))
  topicProbabilities <- as.data.frame(ldaOut@gamma)
  write.csv(topicProbabilities,file=paste("LDAVEM",k,"TopicProbabilities.csv",sep=""))
  #Find relative importance of first and second most important topics  
  topic1ToTopic2 <- lapply(1:nrow(datamatrix),function(x) sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])
  
  #Find relative importance of second and third most important topics
  topic2ToTopic3 <- lapply(1:nrow(datamatrix),function(x) sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])
    
  write.csv(topic1ToTopic2,file=paste("LDAVEM",k,"Topic1ToTopic2.csv",sep=""))
  write.csv(topic2ToTopic3,file=paste("LDAVEM",k,"Topic2ToTopic3.csv",sep=""))
  print("LDA Analysis Complete")
  
}

filesize = length(files)
topicCount = 5
termCount = 8
print("files processes = ")
print(filesize)
ingestXml()
dtm = text2Corpus()
runlda(dtm, topicCount, termCount)
