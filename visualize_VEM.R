## visualize the topic model in a heat map

setwd("/media/ashish/_data/cancerStudies")
datafile = "LDAVEM10TopicProbabilities.csv"
topicDocMatrix = read.csv(file=datafile, header=TRUE, sep=",")

stackdata = melt(topicDocMatrix, id='X')
library(reshape2)
library(ggplot2)

# ggplot(stackdata, aes(x=X, y=value, fill=variable)) + 
#   geom_bar(stat="identity", width=.8, fill="tomato3") + 
#   labs(title="Bar Chart", 
#        subtitle="Topics Vs. Documents", 
#        caption="LDA: Topic Modelling") + 
#   theme(axis.text.x = element_text(angle=65, vjust=0.6))
theme_set(theme_gray())
ggplot(stackdata, aes(x = X, y = value, fill = variable)) + 
  labs(title="VEM Modelling Chart", 
       subtitle="Topics Vs. Documents", 
       caption="LDA: VEM Modelling") + 
  xlab("Document ID") +
  ylab("Topic Probability") +
  geom_bar(stat = "identity") + scale_fill_brewer(palette="Paired")
