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
# data <- read_tsv("C:/Users/pierg/Documents/R/CNR_project/full_ITA_it_20200122-20201202.tsv.gz")
# Spagna
# data <- read_tsv("C:/Users/pierg/Documents/R/CNR_project/data_heuristica.tsv.gz")
# USA
data <- read_tsv("C:/Users/pierg/Documents/R/CNR_project/network_USA_en_20200122-20200522.tsv.gz")



#----------------------------------------------------------------------------------------------------------------------
#------------------------------  BULK -------------------------------------------
#----------------------------------------------------------------------------------------------------------------------

tweet <- data %>% filter(!is.na(FactType)) %>%  # filter(isBot == 0) %>%  
    filter(tweetType== "T")  #%>% filter(User %in% unique(fakes$User))
retweet <- data %>% filter(!is.na(FactType)) %>% # filter(isBot == 0) %>% 
    filter(tweetType== "RT" )  #%>% filter(User %in% unique(fakes$User))
# filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI")

tweet_fakes <- data %>% filter(!is.na(FactType)) %>% # filter(isBot == 0) %>%  
    filter(tweetType== "T") %>% #%>% filter(User %in% unique(fakes$User))
    filter(FactType == "POLITICAL")
# filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI")
retweet_fakes <- data %>% filter(!is.na(FactType)) %>% # filter(isBot == 0) %>% 
    filter(tweetType== "RT") %>% #%>% filter(User %in% unique(fakes$User))
    filter(FactType == "POLITICAL")
# filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI")

activity <- full_join(retweet %>% count(User), retweet %>% count("User"=toUser), by = "User") %>% 
    replace_na( replace = as.list(c(n.x = 0, n.y = 0))) %>%
    transmute("User" = User, "RTout" = n.x, "RTin" = n.y) %>%
    left_join( full_join(retweet_fakes %>% count(User), retweet_fakes %>% count("User"=toUser), by = "User") %>%
                   replace_na( replace = as.list(c(n.x = 0, n.y = 0))) %>%
                   transmute("User" = User, "RTout.fakes" = n.x, "RTin.fakes" = n.y) ,
               by = "User" ) %>%
    replace_na( replace = as.list(c("RTout.fakes" = 0, "RTin.fakes" = 0)) )  %>%
    left_join(tweet %>% count(User), by="User") %>%
    replace_na( replace = as.list(c(n = 0)) ) %>% rename("T"=n ) %>%
    left_join(tweet_fakes %>% count(User), by="User") %>%
    replace_na( replace = as.list(c(n = 0)) ) %>% rename("T.fakes"=n )




RE <- data %>% filter(!is.na(FactType)) %>% filter(!is.na(toUser)) %>%
    # filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI") %>%
    filter(tweetType == "RE") # %>% filter(isBot == 0)
nodes_bulk <- unique( c(RE$User, RE$toUser ) )
edges <- retweet %>% select(User, toUser, FactType)
edges1 <- RE %>% select(User, toUser, FactType)

bulk1 <- inner_join(edges, edges1, by="User") %>% filter(toUser.x == toUser.y) %>% 
    transmute("User"=User, "toUser"= toUser.x) 

bulk2 <- inner_join(edges, edges1 %>% transmute( "toUser1" = User, "User" = toUser) %>% rename(toUser=toUser1), by="User") %>% 
    filter(toUser.x == toUser.y) %>% 
    transmute("User"=User, "toUser"= toUser.x) %>%
    filter(User != toUser) # elimina i self loop solo da questo per evitare doppioni
edges_bulk <- bind_rows(bulk1, bulk2)
nodes_bulk <- unique( c( edges_bulk$User, edges_bulk$toUser  ) )

net_bulk <- graph_from_data_frame(d = edges_bulk , vertices = nodes_bulk, directed = F)

activity <- activity %>% mutate("bulk" = User %in% nodes_bulk)

fakes <- data %>% filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI") %>% filter(tweetType == "RE"| tweetType == "RT" )
fakers <- activity %>%
    filter(RTin.fakes > 0 | RTout.fakes > 0) 


core <- fakers %>%
    filter( bulk == TRUE )
periphery <- fakers %>%
    filter( !(User %in% core$User) )

inner_fakers <- core
outer_fakers <- periphery

n1 <- length(inner_fakers$User) # Traffico dati per il core
n2 <- length(outer_fakers$User) # Traffico dati per la periphery
n3 <- length(fakers$User) - n1 - n2
c(n1, n2, n3)


#--- Qual è il traffico di info tra i vari livelli di fakers?
a1 <- length (( retweet_fakes %>% filter(User %in% inner_fakers$User, toUser %in% inner_fakers$User) )$User )
a2 <- length (( retweet_fakes %>% filter(User %in% inner_fakers$User, toUser %in% outer_fakers$User) )$User )
a3 <- length (( retweet_fakes %>% filter(User %in% inner_fakers$User, !( toUser %in% inner_fakers$User), !(toUser %in% outer_fakers$User) ) )$User )

b1 <- length (( retweet_fakes %>% filter(User %in% outer_fakers$User, toUser %in% inner_fakers$User) )$User )
b2 <- length (( retweet_fakes %>% filter(User %in% outer_fakers$User, toUser %in% outer_fakers$User) )$User )
b3 <- length (( retweet_fakes %>% filter(User %in% outer_fakers$User, !( toUser %in% inner_fakers$User), !(toUser %in% outer_fakers$User) ) )$User )

c(a1, a2, a3, b1, b2, b3)

barplot2 <- data.frame( by = c( "core to core", "core to per", "per to core", "per to per"  ),
                        traffic = c(a1/n1/n1, a2/n1/n2, b1/n2/n1, b2/n2/n2 )   )

ggplot(data = barplot2, aes(x=by, y=traffic*100, fill = by)) + 
    geom_bar(stat="identity") +
    theme_minimal() +
    scale_fill_manual( values = c("#ff4d00", "#22a0ff", "#ff4d00", "#22a0ff"), guide = FALSE) + 
    labs(title = "USA", y = "Density of retweets [%]", x="") + 
    theme(text=element_text(size=30),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 25))  
# scale_y_continuous( labels = c("0", "0.0001", "0.0002", "0.0003", "0.0004") , limits =  c(0, 1e-4, 2e-4, 3e-4, 4e-4) )

sum(retweet_fakes$User %in% core$User) # Retweets in core
sum(retweet_fakes$User %in% periphery$User) # Retweets in periphery



#----------------------------------------------------------------------------------------------------------------------
#------------------------------  CORE TEMPORAL ANALYSIS -------------------------------------------
#----------------------------------------------------------------------------------------------------------------------



date <- as.POSIXct(c("2020-01-01","2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01"))

core_size <- numeric(length(date)-1)
core_size1 <- numeric(length(date)-1)
for (i in seq(1, length(date)-1) ) {
    
    retweet <- data %>% filter(!is.na(FactType)) %>% filter(Datetime > date[i] & Datetime < date[i+1] ) %>% # filter(isBot == 0) %>% 
        filter(tweetType== "RT" )  #%>% filter(User %in% unique(fakes$User))
    # filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI")
    
    retweet_fakes <- data %>% filter(!is.na(FactType)) %>% filter(Datetime > date[i] & Datetime < date[i+1] ) %>% # filter(isBot == 0) %>% 
        filter(tweetType== "RT") %>% #%>% filter(User %in% unique(fakes$User))
        filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI")
    
    activity_temporal <- full_join(retweet %>% count(User), retweet %>% count("User"=toUser), by = "User") %>% 
        replace_na( replace = as.list(c(n.x = 0, n.y = 0))) %>%
        transmute("User" = User, "RTout" = n.x, "RTin" = n.y) %>%
        left_join( full_join(retweet_fakes %>% count(User), retweet_fakes %>% count("User"=toUser), by = "User") %>%
                       replace_na( replace = as.list(c(n.x = 0, n.y = 0))) %>%
                       transmute("User" = User, "RTout.fakes" = n.x, "RTin.fakes" = n.y) ,
                   by = "User" ) %>%
        replace_na( replace = as.list(c("RTout.fakes" = 0, "RTin.fakes" = 0)) ) 
    
    RE <- data %>% filter(!is.na(FactType)) %>% filter(!is.na(toUser)) %>% filter(Datetime > date[i] & Datetime < date[i+1] ) %>%
        # filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI") %>%
        filter(tweetType == "RE") # %>% filter(isBot == 0)
    nodes_bulk <- unique( c(RE$User, RE$toUser ) )
    edges <- retweet %>% select(User, toUser, FactType)
    edges1 <- RE %>% select(User, toUser, FactType)
    
    bulk1 <- inner_join(edges, edges1, by="User") %>% filter(toUser.x == toUser.y) %>% 
        transmute("User"=User, "toUser"= toUser.x) 
    
    bulk2 <- inner_join(edges, edges1 %>% transmute( "toUser1" = User, "User" = toUser) %>% rename(toUser=toUser1), by="User") %>% 
        filter(toUser.x == toUser.y) %>% 
        transmute("User"=User, "toUser"= toUser.x) %>%
        filter(User != toUser) # elimina i self loop solo da questo per evitare doppioni
    edges_bulk <- bind_rows(bulk1, bulk2)
    nodes_bulk <- unique( c( edges_bulk$User, edges_bulk$toUser  ) )
    
    activity_temporal <- activity_temporal %>% mutate("bulk" = User %in% nodes_bulk)
    fakers <- activity_temporal %>% filter(RTin.fakes > 0 | RTout.fakes > 0) 
    
    core <- fakers %>% filter( bulk == TRUE )
    periphery <- fakers %>% filter( !(User %in% core$User) )
    
    n1 <- length(core$User) # Traffico dati per il core
    # n2 <- length(fakers$User)
    n2 <- length(unique(c(retweet$User, retweet$toUser)))
    print(n2)
    core_size[i] <- n1/n2
    core_size1[i] <- n1
    
}
plot(1:11, core_size*100, 'l')
a <- as.data.frame( cbind(x=1:11, y=core_size*100) )
ggplot(a, aes(x=x, y=y, label=core_size1))+geom_line(cex=2) + theme_minimal()+theme(text=element_text(size=25))+geom_label()+
    labs(x="# of the month", y ="% of users in the core", title = "Core size variations (bulk definition)")












