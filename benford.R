library(XML)
library(ggplot2)

WORKING_DIR <- "/home/saperduper/projects/benford_law_test"
setwd(WORKING_DIR)

votes_file <- "votes.xml"
doc <- xmlTreeParse(votes_file, useInternalNodes=TRUE)
src <- xpathApply(xmlRoot(doc), "//row")

# create data frame with upvotes
for (i in 1:length(src)) {
  vote_id <- as.numeric(xpathSApply(src[[i]], "@Id"))
  post_id <- as.numeric(xpathSApply(src[[i]], "@PostId"))
  vote_type <- as.numeric(xpathSApply(src[[i]], "@VoteTypeId"))

  # only upvotes
  if (vote_type != 2) {
    next
  }
  
  row <- c(vote_id, post_id)
  if (i == 1) {
    data <- data.frame(t(row), stringsAsFactors=FALSE)
  } else {
    tmp <- data.frame(t(row), stringsAsFactors=FALSE)
    data <- rbind(data, tmp)
  }
}
colnames(data) <- c("vote_id", "post_id")

# count votes per post
votes_per_post <- count(data$post_id)
colnames(votes_per_post) <- c("post_id", "votes_no")

# Function for pulling out leading digit from some integer stored as string
leading.dig<-function(x) {
  as.numeric(strsplit(as.character(x),"")[[1]][1])
}   

# Count digits and store as data frame
dig_count<-cbind(table(sapply(as.vector(votes_per_post$votes_no),leading.dig)))
dig_count<-as.data.frame(dig_count)
colnames(dig_count)<-"DigitCount"

# Benford's distribution
dbenford<-function(d,base=10) {
  return(log(1+(1/d),base=base))
}

# Plot observed probability of leading digit and theoretical
png("benford.png",width=1000,height=800,res=100)
report.region<-ggplot(dig_count,aes(x=1:nrow(dig_count),y=DigitCount/sum(DigitCount)))+geom_path(aes(colour="Observed"))+
  geom_point(aes(colour="Observed"))+stat_function(fun=dbenford,args=list(base=10),aes(colour="Theoretical"))+
  scale_colour_manual(values=c("Observed"="darkblue","Theoretical"="darkred"),name="Leading Digits")+
  scale_x_continuous(breaks=1:nrow(dig_count))+ylab("Pr(Digit)")+xlab("Digits")+
  opts(title="Benford's law test for android.stackexchange.com upvotes (upvotes/post)")
print(report.region)
dev.off()

# Chi-square test for goodness of fit
chi<-chisq.test(x=dig_count$DigitCount/sum(dig_count$DigitCount),y=sapply(1:9,dbenford))

print(chi$p.value)