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


# USA
data <- # Here you should define a dataframe with the Tweets IDs used in the paper, each classified according to subject as done in the following paper https://doi.org/10.1038/s41562-020-00994-6


# Aggiungi o togli bot
data <- data %>% filter(isBot == 0)

### NOTE: throughout the code the terms "core" and "periphery" are used instead of "creators" and "consumers" #####


cp_structure <- function(retweet, retweet_fakes, threshold, Tweet = T, GCC = F, communities.plot = F, community_detection = F) {
    
    retweet <- data %>% filter(!is.na(FactType)) %>% # filter(isBot == 0) %>%
        filter(tweetType== "RT" )   #%>% filter(User %in% unique(fakes$User))
    retweet_fakes <- retweet %>% # filter(isBot == 0) #%>% filter(User %in% unique(fakes$User))
        filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI"  | FactType == "CLICKBAIT" )
    
    if (Tweet == T){
        ## ERRORE ## Qui al posto di data dovrebbe andarci retweet
        
        tweet <- data %>% filter(!is.na(FactType)) %>%  # filter(isBot == 0) %>%  
            filter(tweetType== "T")  #%>% filter(User %in% unique(fakes$User))
        
        tweet_fakes <- data %>% filter(!is.na(FactType)) %>% # filter(isBot == 0) %>%  
            filter(tweetType== "T") %>% #%>% filter(User %in% unique(fakes$User))
            filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI"  | FactType == "CLICKBAIT" )
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
        
    } else {
        activity <- full_join(retweet %>% count(User), retweet %>% count("User"=toUser), by = "User") %>% 
            replace_na( replace = as.list(c(n.x = 0, n.y = 0))) %>%
            transmute("User" = User, "RTout" = n.x, "RTin" = n.y) %>%
            left_join( full_join(retweet_fakes %>% count(User), retweet_fakes %>% count("User"=toUser), by = "User") %>%
                           replace_na( replace = as.list(c(n.x = 0, n.y = 0))) %>%
                           transmute("User" = User, "RTout.fakes" = n.x, "RTin.fakes" = n.y) ,
                       by = "User" ) %>%
            replace_na( replace = as.list(c("RTout.fakes" = 0, "RTin.fakes" = 0)) )
        
    }
    
    
    # ----------------------------- DIVIDI IL NETWORK IN COMUNITA' -----------------------------------
    
    fakes <- retweet %>% filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI" | FactType == "CLICKBAIT" )
    
    if ( GCC == T | community_detection == T ) {
        nodes <- unique( c(retweet$User, retweet$toUser ) )
        edges <- retweet %>% select(User, toUser, FactType)
        net <- graph_from_data_frame(d = edges , vertices = nodes, directed = F)
    }
    
    
    #-- Per prima cosa decidiamo se caratterizzare solo la GCC o no
    if (GCC == T) {
        comp <- components(net)
        
        max(comp$csize) # Dimensione della GCC
        sum(comp$csize[-which(max(comp$csize) == comp$csize)]) # Numero di persone al di fuori della GCC
        sum(comp$csize[-which(max(comp$csize) == comp$csize)])/max(comp$csize) # Rapporto tra le due quantità
        
        nodes <- names(comp$membership[comp$membership == 1])
        edges <- edges %>% filter(User %in% nodes, toUser %in% nodes) # %>% filter(toUser %in% nodes)
        net <- graph_from_data_frame(d = edges , vertices = nodes, directed = F)
        
    }
    
    if (community_detection == T) {
        #--- Dividiamo il network in communità
        groups <- cluster_louvain(net)
        # groups <- cluster_infomap(net)
        sizes <- sizes(groups)
        
        fakers <- activity %>%
            filter(RTin.fakes > 0 | RTout.fakes > 0) %>%
            mutate(group = sapply(User, function(x) groups$membership[which(groups$names == x)] ) )
        
        #--- Qual è la distribuzione di scettici nel network?
        skeptic_distribution <- fakers %>% count(group) 
        idx <- as.numeric( skeptic_distribution$group[skeptic_distribution$n > 10 ] )
        # idx <- as.numeric( skeptic_distribution$group[skeptic_distribution$n == max(skeptic_distribution$n) ] )
        max_fakes_community <- edges %>% filter(User %in% groups$names[groups$membership %in% idx] | toUser %in% groups$names[groups$membership %in% idx])
        
        core <- fakers %>% filter( RTin.fakes/RTin >= threshold & RTin > 0)
        periphery <- fakers %>% filter( !(User %in% core$User) )
        
        inner_fakers <- core %>% filter(group %in% idx)
        outer_fakers <- periphery %>% filter(group %in% idx)
        
        
    } else {
        fakers <- activity %>%
            filter(RTin.fakes > 0 | RTout.fakes > 0) 
        
        core <- fakers %>% filter( RTin.fakes/RTin >= threshold & RTin > 0)
        periphery <- fakers %>% filter( !(User %in% core$User) )
        
        inner_fakers <- core
        outer_fakers <- periphery
    }
    
    
    #--- Plot the size of the largest communities with the associated number of skeptics
    if (communities.plot){
        barplot <- data.frame( sizes[sizes>=200], "skeptics" = 0 )
        colnames(barplot) <- c("group", "total", "skeptics")
        barplot$skeptics[barplot$group %in% skeptic_distribution$group] <- skeptic_distribution$n[skeptic_distribution$group %in% barplot$group]
        barplot <- barplot %>% gather(key = type, value = "User", -group)
        ggplot(data=barplot, aes(x=group, y=User, fill = type)) + 
            geom_bar(stat="identity", position=position_dodge()) + 
            theme_classic() + xlab("Largest communities") + ylab("Number of users") + labs(fill="Type") +
            # scale_fill_brewer(type = "div", palette = 4)
            scale_fill_discrete(type = c("red", "blue")) +
            theme(axis.title.x = element_text(size=20),
                  axis.title.y = element_text(size=20))
        
    }
    
    
    #--- Calcoliamo il numero di retweet per ogni singola coppia origine-destinazione all'interno della comunità idx
    n1 <- length(inner_fakers$User) # Traffico dati per il core
    n2 <- length(outer_fakers$User) # Traffico dati per la periphery
    n3 <- length(fakers$User) - n1 - n2
    # c(n1, n2, n3)
    
    
    #--- Qual è il traffico di info tra i vari livelli di fakers?
    a1 <- length (( retweet_fakes %>% filter(User %in% inner_fakers$User, toUser %in% inner_fakers$User) )$User )
    a2 <- length (( retweet_fakes %>% filter(User %in% inner_fakers$User, toUser %in% outer_fakers$User) )$User )
    a3 <- length (( retweet_fakes %>% filter(User %in% inner_fakers$User, !( toUser %in% inner_fakers$User), !(toUser %in% outer_fakers$User) ) )$User )
    
    b1 <- length (( retweet_fakes %>% filter(User %in% outer_fakers$User, toUser %in% inner_fakers$User) )$User )
    b2 <- length (( retweet_fakes %>% filter(User %in% outer_fakers$User, toUser %in% outer_fakers$User) )$User )
    b3 <- length (( retweet_fakes %>% filter(User %in% outer_fakers$User, !( toUser %in% inner_fakers$User), !(toUser %in% outer_fakers$User) ) )$User )
    
    # c(a1, a2, a3, b1, b2, b3)
    
    # barplot2 <- data.frame( "by"= c(rep("core",3), rep("periphery",3)),
    #                         "from"=rep(c("core", "periphery", "the rest"),2),
    #                         traffic = c(a1/n1/n1, a2/n1/n2, a3/n1/n3, b1/n2/n1, b2/n2/n2, b3/n2/n3 )   )
    barplot2 <- data.frame( by = c( "creator RT creator", "creator RT consumer", "consumer RT creator", "consumer RT consumer"  ),
                            traffic = c(a1/n1/n1, a2/n1/n2, b1/n2/n1, b2/n2/n2 ) )
    
    ggplot(data = barplot2, aes(x=factor(by, levels = by), y=traffic*100, fill = by)) + 
        geom_bar(stat="identity") +
        theme_classic() +
        scale_fill_manual( values = c("#22a0ff", "#ff4d00", "#22a0ff", "#ff4d00"), guide = FALSE) + 
        labs(title = paste("Threshold:", 100*threshold,"%"), y = "Density of retweets [%]", x="") + 
        theme(text=element_text(size=15),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 15) )  
    # scale_y_continuous( labels = c("0", "0.0001", "0.0002", "0.0003", "0.0004") , limits =  c(0, 1e-4, 2e-4, 3e-4, 4e-4) )
    
    return(g1)
    



}

a1 <- cp_structure(retweet, retweet_fakes, threshold = 0.2, Tweet = F)   
a1

#----------------------------------------------------------------------------------------------------------------------
#------------------------------  CORE E PERIPHERY -------------------------------------------
#----------------------------------------------------------------------------------------------------------------------

retweet <- data %>% filter(tweetType== "RT" ) %>% filter(!is.na(FactType))
retweet_fakes <- retweet %>% filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI"  | FactType == "CLICKBAIT" )

images <- list()
thr <- seq(0.2, 0.2, 0.2)
for (i in seq_along(thr)) {
    # g1 <- cp_structure(data = data, threshold = thr[i], Tweet = F)   
    a1 <- cp_structure(retweet, retweet_fakes, threshold = thr[i], Tweet = F)   
    images[[i]] <- a1
    print(i)
}

ggarrange(plotlist = images, nrow = 2, ncol = 2)


a1 <- cp_structure(retweet, retweet_fakes, threshold = 0.2, Tweet = F)   







#----------------------------------------------------------------------------------------------------------------------
#------------------------------  ANALYSIS OF CONTENT DISTRIBUTION -------------------------------------------
#----------------------------------------------------------------------------------------------------------------------
                                  
# To obtaine figure 1

library(DescTools)
content_distribution <- TRUE
if (content_distribution){

    # -- CENTRALIZATION OF RETWEETS ---------------------------------    
    df <- data %>% filter(!is.na(FactType), FactType !=  "SATIRE", FactType !=  "MISSING", FactType != "OTHER", FactType != "SHADOW") # %>% filter(isBot == 0)
    df$FactType[df$FactType == "CONSPIRACY/JUNKSCI" | df$FactType == "FAKE/HOAX" | df$FactType == "CLICKBAIT" ] <- "FAKE"
    users <- lapply( unique(df$FactType) , function(x) df$User[df$FactType == x]  )
    interactions <- lapply(users, table)
    # gini <- sapply(interactions, Gini)
    # plot(count(df, FactType)$n, gini)
    # hist(gini, breaks = 10)
    
    # -- Relative number of retweet per subject -----
    pal <- c("#ff0d00", colorRampPalette(c("#BDD7E7", "#003366"))(length(unique(df$FactType)) - 1) )
    # pal <- viridis(4, direction = -1)
    miniplot <- data.frame(type = unique(df$FactType), number = sapply(interactions, sum), col = pal )
    miniplot <- miniplot[order(miniplot$type),]
    miniplot$col = pal
    
    d2 <- ggplot(miniplot, aes(x = type, y = number / sum(number) * 100 )) +
        geom_bar(stat="identity", position=position_dodge(), fill = pal) +
        xlab("Type of content") +
        ylab("% of content") +
        theme_classic() + 
        theme( axis.ticks = element_blank(),
               axis.text.x = element_blank(),
               axis.title = element_text(size = 30),
               text = element_text(size = 25),
               plot.background = element_rect(colour = "black", fill=NA, size=1))
    
    d2
    
    
    
    cumulative <- lapply(interactions, function(x) cumsum(sort(x, decreasing = T)) )
    probs <- seq(0, 1, by = 0.01)
    # probs <- c(0.01, 0.05,  0.1, 0.25, 0.5)
    # sapply(cumulative, function(x) quantile(x, probs=probs) )
    
    a <- sapply(cumulative, function(x) quantile(x, probs=probs)/max(x)*100 ) 
    a <- as.data.frame(cbind(probs, a))
    sapply( unique(df$FactType) , function(x) length(df$User[df$FactType == x])  )
    colnames(a) = unique(c("prob", df$FactType) )
    # a$w <- rep( sapply( unique(df$FactType) , function(x) length(df$User[df$FactType == x])  ), each=5 )
    a <- a %>% gather(key = "type", value="activity", -prob )
    # pal <- c("#ff0d00", blu.colors(4, start = 0.2, end = 0.8))
    
    d1 <- ggplot(data = a, aes(x = prob*100, y = activity, color = type) ) + 
        geom_line( lwd = 4 ) +
        geom_segment( aes(x = 0, y = 0, xend = 100, yend = 100), color = "black", lty = "dashed", lwd = 2.5 ) + 
        scale_color_manual( values = pal) +
        theme_classic() + 
        # scale_x_log10()+
        scale_x_discrete(name = "% of users", breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100) ) +
        scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
        labs(color = "Type of content") +
        theme(legend.title = element_text(size = 40),
              text = element_text(size=40),
              axis.title = element_text(size = 50) ) + 
        ylab("% of content") 
    
    d1
    
    d1 + annotation_custom(ggplotGrob(d2), xmin = 55, xmax = 100, 
                           ymin = 0, ymax = 45)
    
    ## Change the plot from boxplot to Lorentz curves
    
    
    
}

# ------ ANALISI TEMPORALE ------------------------------------------

temporal_analysis <- FALSE
if (temporal_analysis){
    date <- as.POSIXct(c("2020-01-01","2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01"))
    temporal_activity <- data.frame("FactType" = sort(unique(df$FactType)))
    for (i in seq(1, length(date)-1) ) {
        temporal_activity[,i+1] <- df %>% filter(Datetime >= date[i] & Datetime < date[i+1]) %>% group_by(FactType) %>% count()  %>% ungroup() %>% select(n)
    }
    colnames(temporal_activity)[-1] <- seq(1, length(date)-1)
    temporal_activity <- temporal_activity %>% gather("period", "activity", -FactType)
    ggplot(data = temporal_activity, aes(x = as.numeric(period), y=activity, color = as.factor(FactType))) + 
        geom_line(size=2) +
        # ylim(c(0,4000))+
        scale_y_log10() + xlab("# months since 1 Jan 2020") + ylab("# of tweet and retweet") + labs(color="Content type") +
        theme_classic() +
        theme(text = element_text(size=20))+
        scale_color_brewer(type = "div", palette = "RdBu")
}


# ------- VISUALIZE THE GRAPH --------------------------------

graph_visualization <- FALSE
if (graph_visualization){
    library(GGally)
    library(intergraph)
    library(sna)
    n <- 199
    size1 <- 50
    size2 <- n - size1
    net <- sample_sbm(n, block.sizes = c(size1, size2), pref.matrix = cbind(c(0.8, 0.08), c(0.08, 0)), directed = TRUE)
    ggnet2(net, color = c(rep("#ff4d00", size1), rep("#22a0ff", size2)), alpha = 0.75, edge.alpha = 0.5)
        
}




####### >Histogram with the number of follower


followers_c <- sapply(core[core$RTout.fakes!=0,]$User, function(x){
    as.numeric( retweet_fakes %>% filter(User == x) %>% select(followers_count) %>% summarise(mean=mean(followers_count)) )
})
followers_c <- followers_c[!is.nan(followers_c)]

followers_p <- sapply(periphery[periphery$RTout.fakes != 0,]$User, function(x){
    as.numeric( retweet_fakes %>% filter(User == x) %>% select(followers_count) %>% summarise(mean=mean(followers_count)) )
})
followers_p <- followers_p[!is.nan(followers_p)]


limit_p <- quantile(followers_p, probs=0.95)
limit_c <- quantile(followers_c, probs=0.95)
limit <- max( limit_c, limit_p)
step <- 5000
h_p <- hist(followers_p[followers_p < limit], 
     breaks = seq(min(followers_p[followers_p < limit]), max(followers_p[followers_p < limit]) + step , by = step ),
     plot = F)


h_c <- hist(followers_c[followers_c < limit], 
     breaks = seq(min(followers_c[followers_c < limit]), max(followers_c[followers_c < limit]) + step, by = step),
     plot = F)


barplot <- data.frame(x = h_c$breaks[seq_along(h_c$counts)], 
                      follower_c = h_c$counts/sum(h_c$counts),
                      follower_p = h_p$counts/sum(h_p$counts) )
barplot <- gather(barplot, key = "where", value = "prob", -x )




ggplot(data = barplot, aes(x = x, y = prob*100, fill = where ) ) + 
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_classic() +
    scale_fill_manual(values = c("#ff4d00","#22a0ff"), label = c("core","periphery") ) +
    labs(fill = "Group",
         x = "Number of follower",
         y = "Frequency [%]",
         title = "") +
    theme(text = element_text(size = 20),
          legend.position = c(0.8, 0.8),)

sum(followers_c>limit)
sum(followers_p>limit)


    

