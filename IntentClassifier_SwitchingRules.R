## intent list - association rule 


setwd("C://Users/Administrator/Desktop")

intent_list<- read.csv("intent_list.csv", header= TRUE)

head(intent_list)

## compute support

intent01 <- aggregate(.~intent + next., intent_list, function(x) length(x))

head(intent01[order(intent01$X),])

names(intent01)

head(intent01)

names(intent01)[3]<- "cntrule"



## compute item popularity

intent02 <- aggregate(.~intent, intent_list, function(x) length(x))

names(intent02)[2] <- "cntitem"

print(intent02)



# compute measures

intent03<-merge(intent01, intent02, by.x="intent", by.y="intent", all.x=T)

head(intent03)

intent03$rconf <- intent03$cntrule / intent03$cntitem

n_intent <- length(unique(intent_list$intent))

intent03$rsupp <- intent03$cntrule / n_intent

intent04 <- merge(intent03, intent02, by.x="intent", by.y="intent", all.x=T)

names(intent04) <- c("intent","next","cntrule", "cntitem", "rconf", "rsupp", "cntitemy")

intent04$rlift <- intent04$rconf / (intent04$cntitemy / n_intent)

nrow(intent04)




# select rule set

intent05 <- intent04[intent04$rconf>=0.05 & intent04$rlift>1 & intent04$rsupp>=0.005, ]

nrow(intent05)


# explore relationship between measures

plot(jitter(intent05$rconf), jitter(intent05$rsupp), main="Association Rules - Confidence Vs. Support")

plot(jitter(intent05$rconf), jitter(intent05$rlift), main="Association Rules - Confidence Vs. Lift")

plot(jitter(intent05$rsupp), jitter(intent05$rlift), main="Association Rules - Support Vs. Lift")



head(intent05[order(-intent05$rlift),])

intent05[order(-intent05$rlift),]

result_rule<-intent05[order(-intent05$rsupp),]

head(result_rule)

result_rule<-result_rule[,1:6]

write.csv(result_rule, "Rule.csv")
