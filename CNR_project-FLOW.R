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


#----------------------------------------------------------------------------------------------------------------------
#------------------------------ FLOW ANALYSIS  -------------------------------------------
#----------------------------------------------------------------------------------------------------------------------

date <- as.POSIXct( as.Date("2020-01-22") + seq(0, length( numeric( max(data$Datetime)-min(data$Datetime) ) ), 1) )
retweet <- data %>% filter(tweetType== "RT" ) %>% filter(!is.na(FactType))
retweet_fakes <- retweet %>% filter(tweetType== "RT" ) %>% 
    # filter(tweetType== "RT") %>% #%>% filter(User %in% unique(fakes$User))
    # filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI")
    filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI" | FactType == "CLICKBAIT" )

activity <- data.frame("User" = sort(unique(c( retweet_fakes$User, retweet_fakes$toUser))) )

time_average <- 1
for (i in seq(1, length(date)-time_average) ) {
    
    retweet_temporal <- retweet %>% 
        filter(Datetime > date[i] & Datetime < date[i+time_average] ) # %>% filter(isBot == 0) %>% 
    # filter(User %in% unique(fakes$User))
    # filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI")
    
    retweet_fakes <- retweet_temporal %>% # filter(Datetime > date[i] & Datetime < date[i+1] ) %>% # filter(isBot == 0) %>% 
        # filter(tweetType== "RT") %>% #%>% filter(User %in% unique(fakes$User))
        # filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI")
        filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI" | FactType == "CLICKBAIT" )
    
    # activity_temporal <-  retweet_fakes %>% count("User"=toUser)  %>%
    #     transmute("User" = User, "RTin.fakes" = n) %>%
    #     left_join(  retweet_temporal %>% count("User"=toUser)  %>%
    #                     transmute("User" = User, "RTin" = n) ,
    #                 by = "User" ) %>%
    #     replace_na( replace = as.list(c( "RTin.fakes" = 0)) ) %>%
    #     transmute("User" = User, "ratio" = RTin.fakes/RTin )
    
    activity_temporal <- full_join(retweet_temporal %>% count(User), retweet_temporal %>% count("User"=toUser), by = "User") %>% 
        replace_na( replace = as.list(c(n.x = 0, n.y = 0))) %>%
        transmute("User" = User, "RTout" = n.x, "RTin" = n.y) %>%
        left_join( full_join(retweet_fakes %>% count(User), retweet_fakes %>% count("User"=toUser), by = "User") %>%
                       replace_na( replace = as.list(c(n.x = 0, n.y = 0))) %>%
                       transmute("User" = User, "RTout.fakes" = n.x, "RTin.fakes" = n.y) ,
                   by = "User" ) %>%
        replace_na( replace = as.list(c("RTout.fakes" = 0, "RTin.fakes" = 0)) )
    
    activity_temporal[ activity_temporal$RTout.fakes == 0 & activity_temporal$RTin.fakes == 0, 6] <- "O"
    activity_temporal[ activity_temporal$RTin > 0 & activity_temporal$RTin.fakes/activity_temporal$RTin > 0.2 , 6] <- "C"
    activity_temporal[ is.na(activity_temporal$...6) , 6] <- "P"
    names(activity_temporal)[6] <- "where"
    
    activity <- left_join(activity, activity_temporal[,c(1,6)], by = "User") %>%
        replace_na( replace = as.list(c( "where" = "O")) )
    
    names(activity)[i+1] <- as.character(date[i])
    
    
    print(i)
    
}



# COARSE GRAINING , "O" "P" AND "C"
frequency <- data.frame("C"= apply(activity == "C", FUN = sum, MARGIN = 1), 
                        "P"= apply(activity == "P", FUN = sum, MARGIN = 1) )
frequency <- frequency %>% mutate("sum" = C+P)
frequency <- frequency[ frequency$sum != 0, ]

a <- activity[frequency$sum != 0 , -1 ] # restringiamoci subito al sottoinsieme in cui gli user rientrano piů di una volta
frequency <- frequency[frequency$sum != 0 , ] 
IdxC <- a == "C"
IdxP <- a == "P"
IdxO <- a == "O"

# These functions contain all the lenghts of the time scales necessary to go from one group (C or P) to the immediate successor (C or P)
jumpsC2C <- jumpsC2P <- jumpsP2C <- jumpsP2P <- numeric(0)

for (i in 1:nrow(a)) {
    
    idxC <- as.numeric( which(IdxC[i,]) )
    idxP <- as.numeric( which(IdxP[i,]) )
    
    CC_distance <- c(diff(idxC), -1) # l'ultimo termine č ausiliario e serve per far tornare le cose
    PP_distance <- c(diff(idxP), -1)
    
    nc <- length(idxC)
    np <- length(idxP)
    
    # Definiamo una matrice dalle dimensioni np x nc
    if (nc != 0 & np != 0) {
        CP_distance <- matrix( rep(idxP, nc), np, nc) - matrix( rep(idxC, np), np, nc, byrow = TRUE)
    } else if (nc == 0) {
        CP_distance <- matrix( rep(0, np) , np, 1)
    } else if (np == 0) {
        CP_distance <- matrix( rep(0, nc) , 1, nc)
    }
    
    for (j in seq_along(CC_distance) ) {
        
        x <- c( CC_distance[j], CP_distance[ , j] )
        x <- x[ x > 0 ]
        
        if (length(x) == 0) {
            
        } else if (min(x) == CC_distance[j] ) {
            jumpsC2C <- c(jumpsC2C, min(x) - 1 )
        } else {
            jumpsC2P <- c(jumpsC2P, min(x) - 1 )
        }
    }
    
    for (j in seq_along(PP_distance)){
        
        y <- c( PP_distance[j], - CP_distance[j , ] )
        y <- y[ y > 0 ]
        
        if (length(y) == 0) {
            
        } else if (min(y) == PP_distance[j] ) {
            jumpsP2P <- c(jumpsP2P, min(y) - 1 )
        } else {
            jumpsP2C <- c(jumpsP2C, min(y) - 1 )
        }
        
    }
    
    if ( (i %% 1000) == 0 ) print(i %/% 1000)
}

# GRAFICO 1: starting from core

breaks <- c(0, 3, 7, 18, 46, 121)
tags <- c("0-2", "3-6", "7-17", "18-45", "46+")
tags <- factor( tags, level = tags)

# N_c <- sum(frequency$C)
N_c <- length(jumpsC2C) + length(jumpsC2P)

# breaks <- c(0, 1, 3, 7, 41)


group_tags_C2C <- cut(jumpsC2C, breaks = breaks, include.lowest = TRUE, right = FALSE)
group_tags_C2P <- cut(jumpsC2P, breaks = breaks, include.lowest = TRUE, right = FALSE)
barplot <- bind_cols( data.frame( names(summary(group_tags_C2C)) ),
                      data.frame( as.numeric( summary(group_tags_C2C)) / N_c ),
                      data.frame( as.numeric( summary(group_tags_C2P)) / N_c )
                     )
names(barplot) <- c("return_time", "C2C", "C2P")
barplot <- gather(barplot, key = "type", value = "prob", -return_time)
barplot$return_time <- factor(barplot$return_time, levels = barplot$return_time[seq_along(breaks[-1])])
# barplot <- barplot %>% bind_rows( data.frame(return_time = "no return",
#                                             type = "none" ,
                                             # prob = sum( frequency$C == 1 & frequency$P == 0 ) / N_c ) )
#                                             prob = sum( frequency$C == 1 )  ) )

r1 <- ggplot( data = barplot , aes(x = as.factor(return_time) , y = prob, fill = type) ) + 
    geom_bar(stat="identity", position=position_dodge()) +
    theme_classic() + xlab("Return time (days)") + ylab("Probability") + labs(fill="Return to") + # labs(title = "If you start from the core, where are you going to go next?") +
    scale_x_discrete( labels =  tags ) +
    scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), limits = c(0, 0.55)) +
    scale_fill_discrete(type = c("#ff4d00", "#22a0ff", "darkgreen"), labels = c("Creator", "Consumer", "None")) +
    theme(axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          axis.text.x = element_text(size=15),
          axis.text.y = element_text(size=15),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 15) )



( sum(summary(group_tags_C2C)) + sum(summary(group_tags_C2P))  ) / N_c


# GRAFICO 2: starting from PERIPHERY

# N_p <- sum(frequency$P)
N_p <- length(jumpsP2P) + length(jumpsP2C)

# breaks <- c(0, 1, 3, 7, 39)
group_tags_P2P <- cut(jumpsP2P, breaks = breaks, include.lowest = TRUE, right = FALSE)
group_tags_P2C <- cut(jumpsP2C, breaks = breaks, include.lowest = TRUE, right = FALSE)
barplot <- bind_cols( data.frame( names(summary(group_tags_P2P)) ),
                      data.frame( as.numeric( summary(group_tags_P2C)) / N_p ),
                      data.frame( as.numeric( summary(group_tags_P2P)) / N_p )
)
names(barplot) <- c("return_time", "P2C", "P2P")
barplot <- gather(barplot, key = "type", value = "prob", -return_time)
barplot$return_time <- factor(barplot$return_time, levels = barplot$return_time[seq_along(breaks[-1])])
# barplot <- barplot %>% bind_rows( data.frame(return_time = "no return",
#                                             type = "none" ,
#                                             prob = sum( frequency$P == 1 & frequency$C == 0 ) ) )

r2 <- ggplot( data = barplot , aes(x = return_time , y = prob, fill = type) ) + 
    geom_bar(stat="identity", position=position_dodge()) +
    theme_classic() + xlab("Return time (days)") + ylab("Probability") + labs(fill="Return to") + # labs(title = "If you start from the periphery, where are you going to go next?", fill="Return") +
    scale_x_discrete(labels = tags) +
    scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), limits = c(0, 0.55)) +
    scale_fill_discrete(type = c("#ff4d00", "#22a0ff", "darkgreen"), labels = c("Creator", "Consumer", "None")) +
    theme(axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          axis.text.x = element_text(size=15),
          axis.text.y = element_text(size=15),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 15))

ggarrange(r1, r2, nrow = 1, ncol = 2, legend = "bottom", common.legend = T)

# ATTENZIONE: Perchč ci sono alcune righe di activity in cui non compare nč P nč C?

sum(frequency$P == 0) / nrow(frequency) * 100 # only core
sum(frequency$C == 0) / nrow(frequency) * 100 # only periphery
sum(frequency$P != 0 & frequency$C != 0) / nrow(frequency) * 100




jumpsC2C
jumpsC2P
jumpsP2C
jumpsP2P







### Table of frequency

User_tot = sum(frequency$sum != 0)

# People only once in a fake news group
length(frequency[frequency$sum == 1 & frequency$C == 1, ]$C) / User_tot * 100# core only once
length(frequency[frequency$sum == 1 & frequency$P == 1, ]$C) / User_tot * 100# peri only once


# Only twice
length(frequency[frequency$sum == 2 & frequency$C == 2, ]$C) / User_tot * 100 # core twice
length(frequency[frequency$sum == 2 & frequency$P == 2, ]$C) / User_tot * 100 # peri twice
length(frequency[frequency$sum == 2 & frequency$P == 1 & frequency$C == 1, ]$C) / User_tot * 100 # Mixed

# less then 10
length(frequency[frequency$sum <= 10 & frequency$P == 0, ]$C) / User_tot * 100 # core twice
length(frequency[frequency$sum <= 10 & frequency$C == 0, ]$C) / User_tot * 100 # peri twice
length(frequency[frequency$sum <= 10 & frequency$P != 0 & frequency$C != 0, ]$C) / User_tot * 100 # Mixed





# test matrix
# [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
# [1,] "C"  "O"  "O"  "P"  "C"  "O"  "C"  "P" 
# [2,] "P"  "P"  "O"  "O"  "P"  "O"  "O"  "O" 
# [3,] "C"  "P"  "O"  "O"  "O"  "P"  "O"  "O" 
