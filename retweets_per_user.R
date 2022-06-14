suppressPackageStartupMessages({
    library(readr)
    library(igraph)
    library(tidyr)
    library(dplyr)
    library(ggplot2)
    library(ggpubr)
    library(latex2exp)
    library(RColorBrewer)
    
})


# Aggiungi o togli bot
data <- data %>% filter(isBot == 0)

retweet <- data %>% filter(!is.na(FactType)) %>% # filter(isBot == 0) %>%
    filter(tweetType== "RT" )   #%>% filter(User %in% unique(fakes$User))
retweet_fakes <- retweet %>% # filter(isBot == 0) #%>% filter(User %in% unique(fakes$User))
    filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI"  | FactType == "CLICKBAIT" )

activity <- full_join(retweet %>% count(User), retweet %>% count("User"=toUser), by = "User") %>% 
    replace_na( replace = as.list(c(n.x = 0, n.y = 0))) %>%
    transmute("User" = User, "RTout" = n.x, "RTin" = n.y) %>%
    left_join( full_join(retweet_fakes %>% count(User), retweet_fakes %>% count("User"=toUser), by = "User") %>%
                   replace_na( replace = as.list(c(n.x = 0, n.y = 0))) %>%
                   transmute("User" = User, "RTout.fakes" = n.x, "RTin.fakes" = n.y) ,
               by = "User" ) %>%
    replace_na( replace = as.list(c("RTout.fakes" = 0, "RTin.fakes" = 0)) )

fakers <- activity %>%
    filter(RTin.fakes > 0 | RTout.fakes > 0) 

threshold = 0.2
core <- fakers %>% filter( RTin.fakes/RTin >= threshold & RTin > 0)
periphery <- fakers %>% filter( !(User %in% core$User) )

creators <- core$User
consumers <- periphery$User


# Però questa analisi è chiaramente dominata dai numeri piccoli. Scegliamo di rationare con numeri un po' più grandi aggiungendo un cutoff

cutoff <- 10

# Coloro che i consumatori hanno retweetato sono sempre gli stessi? + cutoff
retweeted_by_consumers <- lapply(consumers, function(x) retweet_fakes %>% filter(User == x ) %>% select(toUser) %>% as.matrix() %>% as.vector()  )
idx = sapply(retweeted_by_consumers, length) >= cutoff
retweeted_by_consumers <- retweeted_by_consumers[idx]
# lapply(1:length(retweeted_by_consumers), function(x) write.table(t(as.data.frame(retweeted_by_consumers[x])), 
#                                               'C:/Users/pierg/Documents/R/CNR_project/retweeted_by_consumers.csv', append= T, sep=',', 
#                                               quote = F, col.names = F))

ratio <- sapply(retweeted_by_consumers, length) /  sapply(retweeted_by_consumers, function(x) length(unique(x)) )

h1 <- ggplot(data.frame(ratio), aes(x = ratio)) + 
    geom_histogram(bins = 30, fill = "#22a0ff") + 
    theme_classic() + 
    scale_x_log10() + 
    ylab("Frequency") +
    # xlab("# Retweets / # Retweeted Accounts") + 
    xlab("Retweets per Retweeted Accounts") + 
    geom_vline(xintercept = median(ratio), col = "black", size = 2, lty = "dashed") +
    # geom_text(x = 1.5, y = 1250, label = "Are the consumers retweeting \n always the same accounts?", col = "black", size = 10) + 
    theme(
        axis.title = element_text(size = 35),
        axis.text = element_text(size = 25),
        panel.border = element_rect(color = "black", size = 1, fill = NA)
    )

h1


# Coloro che retwettano qualcuno sono sempre gli stessi? + cutoff
retweeting_creators <- lapply(creators, function(x) retweet_fakes %>% filter(toUser == x ) %>% select(User) %>% as.matrix() %>% as.vector()  )
idx = sapply(retweeting_creators, length) >= cutoff
retweeting_creators <- retweeting_creators[idx]

# lapply(1:length(retweeting_creators), function(x) write.table(t(as.data.frame(retweeting_creators[x])),
#                                               'C:/Users/pierg/Documents/R/CNR_project/retweeting_creators.csv', append= T, sep=',',
#                                               quote = F, col.names = F))

ratio2 <- sapply(retweeting_creators, length) / sapply(retweeting_creators, function(x) length(unique(x)) )

h2 <- ggplot(data.frame(ratio2), aes(x = ratio2)) + 
    geom_histogram(bins = 30, fill = "#ff4d00") + 
    theme_classic() + 
    scale_x_log10() + 
    ylab("Frequency") +
    # xlab("# Retweets / # Retweeting Accounts") + 
    xlab("Retweets per Retweeting Accounts") + 
    geom_vline(xintercept = median(ratio2), col = "black", size = 2, lty = "dashed") +
    #  geom_text(x = 1 , y =800, label = "Are the creators being retweeted \n always by the same accounts?", col = "black", size = 10) + 
    theme(
        axis.title = element_text(size = 35),
        axis.text = element_text(size = 25),
        panel.border = element_rect(color = "black", size = 1, fill = NA)
    )

h2

    
    
    
    







