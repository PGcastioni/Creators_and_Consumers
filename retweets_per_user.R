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

# Italia
# data <- read_tsv("C:/Users/pierg/Documents/R/CNR_project/network_ITA_it_20200122-20201202.tsv.gz")
data <- read_tsv("C:/Users/pierg/Documents/R/CNR_project/full_ITA_it_20200122-20201202.tsv.gz")
# Spagna
# data <- read_tsv("C:/Users/pierg/Documents/R/CNR_project/data_heuristica.tsv.gz")
# USA
# data <- read_tsv("C:/Users/pierg/Documents/R/CNR_project/network_USA_en_20200122-20200522.tsv.gz")
# GBR
# data <- read_tsv("C:/Users/pierg/Documents/R/CNR_project/network_GBR_en_20200122-20200522.tsv.gz")


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

png("C:/Users/pierg/Documents/R/CNR_project/RT_per_A.png", width = 1900, height = 950)
h12 <- ggarrange(h1, h2, ncol = 2, nrow = 1)
h12
dev.off()






### WORK NOT NEEDED ANYMORE: RECONSTRUCTING CASCADES ###

# 
# fakes <- data %>% filter(!is.na(FactType)) %>% 
#     filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI"  | FactType == "CLICKBAIT" )
# 
# Tfakes <- fakes %>% filter(tweetType == "T")
# RTfakes <- fakes %>% filter(tweetType == "RT")
# 
# 
# # Prima cosa: selezioniamo gli indirizzi per cui abbiamo un tweet originario (la data è importante per quest'ultimo punto)
# # Tutti gli url presenti sia in Tfakes che in RTfakes. COsì facendo ci restringiamo solo alle cascate che partono da un T, no da RT
# urls = unique(left_join(Tfakes, RTfakes, by="URL")$URL) 
# 
# 
# 
# # Ragionamento momentaneo usando un URL fissato
# # Eliminiamo i singoletti
# prova <- df %>% filter(toUser %in% Tfakes$User  )
# prova2 <-  Tfakes %>% filter(URL == url) %>% filter(User %in% prova$toUser)
# 
# 
# leaves <- lapply(urls, function(x) RTfakes %>% filter(URL == x & toUser %in% Tfakes$User[Tfakes$URL == x]  ) %>% select(Datetime, User, toUser, URL) )
# 
# # PROVIAMO A PLOTTARE
# g = graph.data.frame(prova3[,c('toUser','User')],directed = T)
# V(g)$color[names(V(g)) %in% creators] = "red"
# V(g)$color[names(V(g)) %in% consumers] = "blue"
# V(g)$label.color = ""
# V(g)$size = 5
# plot(g,edge.arrow.size=0.5,layout=layout_as_tree)
# 
# ## PROBLEMA COI COLORI## Ci sono alcuni nodi che non sono ne core ne peri
# 
# 
# f_urls = sort( table(fakes$URL), decreasing = TRUE)
# urls = names(f_urls)
# 
# # origin <- lapply(urls, function(x) Tfakes[Tfakes$URL == x, ] %>% select(User, Datetime) )
# # names(origin) <- urls
# leaves <- lapply(urls, function(x) RTfakes %>% filter(URL == x & toUser %in% Tfakes$User[Tfakes$URL == x]  ) %>% select(Datetime, User, toUser, URL) )
# leaves <- leaves[sapply(leaves, function(x) dim(x)[1]) >0 ]
# a = do.call(rbind.data.frame, leaves)
# a = lapply(1:dim(Tfakes)[1], function(x) a %>% filter(URL == Tfakes$URL[x] & toUser == Tfakes$User[x] & Datetime > Tfakes$Datetime[x]) )
# a <- a[sapply(a, function(x) dim(x)[1]) >0 ]
# a = do.call(rbind.data.frame, a)
# 
# names(origin) <- urls
# 
# leaves <- lapply()
#     
    
    
    
    
    
    
    







