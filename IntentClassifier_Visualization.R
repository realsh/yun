# Intent classifier visualization - arulesViz

setwd("C://Users/")

intent<-read.csv("intent_list.csv", header=TRUE)

intent<-as.data.frame(intent)

intent.list<-split(intent$intent, intent$index)

intent.list

install.packages("arules")

install.packages("visNetwork")

install.packages("arulesViz")

library(arules)

library(visNetwork)

library(igraph)

library(arulesViz)

intent.trans <- as(intent.list, "transactions")

intent.trans

summary(intent.trans)

image(intent.trans)

intent.trans@itemsetInfo
  
intent.rules<-apriori(intent.trans)

#, parameter = list(support = 0.3, confidence = 0.3))

summary(intent.rules)

inspect(intent.rules)

intent.rules2 <- head(sort(intent.rules, by="support"), 50)

mode(intent.rules)

ig <- plot( intent.rules, method="graph", control=list(type="items") )

(intent.rules@quality)

plot(intent.rules, method="graph", control=list(type="items"))

plot(intent.rules, method="graph")

plot(intent.rules, method = "paracoord", control = list(reorder = TRUE))

ig<-plot(intent.rules2, method="graph", control=list(type="items"))

inspect(intent.rules2)

summary(intent.rules2)

igraph

ig_df$vertices

ig_df$edges

ig_df <- get.data.frame( ig, what = "both" )

net<-{visNetwork(

    nodes = data.frame(
  
      id = as.character(ig_df$vertices$name)
   
       ,value = ig_df$vertices$support # could change to lift or confidence
    
      ,title = ifelse(ig_df$vertices$label == "",ig_df$vertices$name, as.character(ig_df$vertices$label))
    
      ,ig_df$vertices
  
      )
  
    , edges = ig_df$edges
 
    )%>%
  
    visEdges( arrows="to" ) %>%
  
    visOptions( highlightNearest = T )
  
}


net %>% visSave(file = "network.html")

saveAsGraph(net,"networt.html")
