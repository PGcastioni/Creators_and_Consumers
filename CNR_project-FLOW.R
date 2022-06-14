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


duration_markov <- function(a) {
    
    duration <- data.frame("C"=0, "P"=0)
    # Numero di volte in cui qualcuno entra in C o P, anche se da parte di una persona già precedentemente uscitavi
    b <- c <- a
    for (i in 2:ncol(a) ) {
        b[ a[,i] == "C" & a[,i-1] == "P" , i ] <- "1"
        b[ a[,i] == "C" & a[,i-1] == "O" , i ] <- "1"
        c[ a[,i] == "P" & a[,i-1] == "C" , i ] <- "2"
        c[ a[,i] == "P" & a[,i-1] == "O" , i ] <- "2"
        
    }
    
    N_c <- sum(b == "1")
    N_p <- sum(c == "2")
    
    b <- c <- a
    for (i in 3:ncol(a) ) {
        b[ a[,i] == "O" & a[,i-1] == "C" & a[,i-2] == "O" , i ] <- "1"
        b[ a[,i] == "O" & a[,i-1] == "C" & a[,i-2] == "P" , i ] <- "1"
        b[ a[,i] == "P" & a[,i-1] == "C" & a[,i-2] == "O" , i ] <- "1"
        b[ a[,i] == "P" & a[,i-1] == "C" & a[,i-2] == "P" , i ] <- "1"
        
        c[ a[,i] == "O" & a[,i-1] == "P" & a[,i-2] == "O" , i ] <- "2"
        c[ a[,i] == "O" & a[,i-1] == "P" & a[,i-2] == "C" , i ] <- "2"
        c[ a[,i] == "C" & a[,i-1] == "P" & a[,i-2] == "O" , i ] <- "2"
        c[ a[,i] == "C" & a[,i-1] == "P" & a[,i-2] == "C" , i ] <- "2"
        
    }
    
    b <- b[, 4:ncol(a) ]
    c <- c[, 4:ncol(a) ]
    
    duration[1,1] <- gather(b, key = "time", value ="dir") %>% count(dir) %>% filter(dir==1) %>% select(n) / N_c
    duration[1,2] <- gather(c, key = "time", value ="dir") %>% count(dir)  %>% filter(dir==2) %>% select(n) / N_p
    
    b <- c <- a
    for (i in 4:ncol(a) ) {
        b[ a[,i] == "O" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "O" , i ] <- "1"
        b[ a[,i] == "O" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "P" , i ] <- "1"
        b[ a[,i] == "P" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "O" , i ] <- "1"
        b[ a[,i] == "P" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "P" , i ] <- "1"
        
        c[ a[,i] == "O" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "O" , i ] <- "2"
        c[ a[,i] == "O" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "C" , i ] <- "2"
        c[ a[,i] == "C" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "O" , i ] <- "2"
        c[ a[,i] == "C" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "C" , i ] <- "2"
        
    }
    
    b <- b[, 5:ncol(a) ]
    c <- c[, 5:ncol(a) ]
    
    duration[2,1] <- gather(b, key = "time", value ="dir") %>% count(dir) %>% filter(dir==1) %>% select(n) / N_c
    duration[2,2] <- gather(c, key = "time", value ="dir") %>% count(dir)  %>% filter(dir==2) %>% select(n) / N_p
    
    b <- c <- a
    for (i in 5:ncol(a) ) {
        b[ a[,i] == "O" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "C" & a[,i-4] == "O" , i ] <- "1"
        b[ a[,i] == "O" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "C" & a[,i-4] == "P" , i ] <- "1"
        b[ a[,i] == "P" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "C" & a[,i-4] == "O" , i ] <- "1"
        b[ a[,i] == "P" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "C" & a[,i-4] == "P" , i ] <- "1"
        
        c[ a[,i] == "O" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "P" & a[,i-4] == "O" , i ] <- "2"
        c[ a[,i] == "O" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "P" & a[,i-4] == "C" , i ] <- "2"
        c[ a[,i] == "C" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "P" & a[,i-4] == "O" , i ] <- "2"
        c[ a[,i] == "C" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "P" & a[,i-4] == "C" , i ] <- "2"
        
    }
    
    b <- b[, 6:ncol(a) ]
    c <- c[, 6:ncol(a) ]
    
    duration[3,1] <- gather(b, key = "time", value ="dir") %>% count(dir) %>% filter(dir==1) %>% select(n) / N_c
    duration[3,2] <- gather(c, key = "time", value ="dir") %>% count(dir)  %>% filter(dir==2) %>% select(n) / N_p
    
    b <- c <- a
    for (i in 6:ncol(a) ) {
        b[ a[,i] == "O" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "C" & a[,i-4] == "C" & a[,i-5] == "O" , i ] <- "1"
        b[ a[,i] == "O" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "C" & a[,i-4] == "C" & a[,i-5] == "P" , i ] <- "1"
        b[ a[,i] == "P" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "C" & a[,i-4] == "C" & a[,i-5] == "O" , i ] <- "1"
        b[ a[,i] == "P" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "C" & a[,i-4] == "C" & a[,i-5] == "P" , i ] <- "1"
        
        c[ a[,i] == "O" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "P" & a[,i-4] == "P" & a[,i-5] == "O" , i ] <- "2"
        c[ a[,i] == "O" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "P" & a[,i-4] == "P" & a[,i-5] == "C" , i ] <- "2"
        c[ a[,i] == "C" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "P" & a[,i-4] == "P" & a[,i-5] == "O" , i ] <- "2"
        c[ a[,i] == "C" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "P" & a[,i-4] == "P" & a[,i-5] == "C" , i ] <- "2"
        
    }
    
    b <- b[, 7:ncol(a) ]
    c <- c[, 7:ncol(a) ]
    
    duration[4,1] <- gather(b, key = "time", value ="dir") %>% count(dir) %>% filter(dir==1) %>% select(n) / N_c
    duration[4,2] <- gather(c, key = "time", value ="dir") %>% count(dir)  %>% filter(dir==2) %>% select(n) / N_p
    
    b <- c <- a
    for (i in 7:ncol(a) ) {
        b[ a[,i] == "O" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "C" & a[,i-4] == "C" & a[,i-5] == "C" & a[,i-6] == "O" , i ] <- "1"
        b[ a[,i] == "O" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "C" & a[,i-4] == "C" & a[,i-5] == "C" & a[,i-6] == "P" , i ] <- "1"
        b[ a[,i] == "P" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "C" & a[,i-4] == "C" & a[,i-5] == "C" & a[,i-6] == "O" , i ] <- "1"
        b[ a[,i] == "P" & a[,i-1] == "C" & a[,i-2] == "C" & a[,i-3] == "C" & a[,i-4] == "C" & a[,i-5] == "C" & a[,i-6] == "P" , i ] <- "1"
        
        c[ a[,i] == "O" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "P" & a[,i-4] == "P" & a[,i-5] == "P" & a[,i-6] == "O" , i ] <- "2"
        c[ a[,i] == "O" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "P" & a[,i-4] == "P" & a[,i-5] == "P" & a[,i-6] == "C" , i ] <- "2"
        c[ a[,i] == "C" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "P" & a[,i-4] == "P" & a[,i-5] == "P" & a[,i-6] == "O" , i ] <- "2"
        c[ a[,i] == "C" & a[,i-1] == "P" & a[,i-2] == "P" & a[,i-3] == "P" & a[,i-4] == "P" & a[,i-5] == "P" & a[,i-6] == "C" , i ] <- "2"
        
    }
    
    b <- b[, 8:ncol(a) ]
    c <- c[, 8:ncol(a) ]
    
    duration[5,1] <- gather(b, key = "time", value ="dir") %>% count(dir) %>% filter(dir==1) %>% select(n) / N_c
    duration[5,2] <- gather(c, key = "time", value ="dir") %>% count(dir)  %>% filter(dir==2) %>% select(n) / N_p
    
    duration <- duration %>% replace_na(as.list(c("P"=0)))
    duration$dur <- 1:5
    names(duration)[1:2] <- c("core", "periphery")
    
    return(duration)
    
}


flows_analysis <- function(a) {
    # Numero di persone che sono entrate nella P almeno una volta. # Possono anche aver fatto parte del C ad un certo punto
    N_p2 <- sum(a == "P") 
    # Numero di persone che sono entrate nella C almeno una volta. # Possono anche aver fatto parte del P ad un certo punto
    N_c2 <- sum(a == "C") 
    
    b <- a
    for (i in 2:nrow(a) ) {
        b[ a[,i] == "O" & a[,i-1] == "O" , i ] <- "O to O"
        b[ a[,i] == "O" & a[,i-1] == "P" , i ] <- "P to O"
        b[ a[,i] == "O" & a[,i-1] == "C" , i ] <- "C to O"
        b[ a[,i] == "P" & a[,i-1] == "O" , i ] <- "O to P"
        b[ a[,i] == "P" & a[,i-1] == "P" , i ] <- "P to P"
        b[ a[,i] == "P" & a[,i-1] == "C" , i ] <- "C to P"
        b[ a[,i] == "C" & a[,i-1] == "O" , i ] <- "O to C"
        b[ a[,i] == "C" & a[,i-1] == "P" , i ] <- "P to C"
        b[ a[,i] == "C" & a[,i-1] == "C" , i ] <- "C to C"
        
    }
    
    b <- b[, 3:ncol(a) ]
    
    
    aa <- gather(b, key = "time", value ="dir") %>% count(dir)
    # aa <- aa[c(1,9,3,7,2,4,8,6),]
    # aa <- aa[c(1,9,3,7,4,6),]
    aa <- aa[c(1,7,4,9,3,6),]
    aa$n <- aa$n / c(N_c2, N_c2, N_c2, N_p2, N_p2, N_p2)
    
    return(aa)
}

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

# PRIMO GRAFICO -  QUANTI TIME STEP CONSECUTIVI SI RIMANE IN UN GRUPPO?
duration <- duration_markov(a = activity)
# Time step di 1 giorno
ggplot(data = gather(duration, key = "where", value = "n", -dur ), aes(x=dur, y=n, fill=where)) + 
    geom_bar(stat="identity", position=position_dodge()) + 
    scale_fill_discrete(type = c("#ff4d00", "#22a0ff")) + 
    theme_classic() + 
    labs(x = "Stay duration (unit of time = 3 days)", 
         y = "Probability", 
         title = "What is the typical stay duration in core/periphery",
         fill = "Group") +
    annotate("text", x=5, y=0.7, label = "b)", size = 15) +
    theme(text=element_text(size=20))
    



# SECONDO GRAFICO -  QUALI SONO I MOVIMENTI DELLA GENTE
aa <- flows_analysis(activity)

# barplot(aa$n[-9], legend.text = c("P", "c"))

ggplot(data = aa, aes(x = factor(dir, levels = dir) , y = n)) + 
    geom_bar(stat="identity" , fill = c("#ff4d00", 'orange', 'red', "#22a0ff", '#000099', 'darkgreen')) +
    theme_classic() +
    xlab( "User flow direction (C = Core, P = Periphery, O = Outside)" ) + 
    ylab( "Transition probability" ) +
    ylim(0, 0.8) +
    labs(title= "What are the most common movements among those who share fake news?" ) +
    annotate("text", x=1, y=0.7, label = "a)", size = 15) +
    theme( text = element_text(size = 20),
           title = element_text(size = 15))





# COARSE GRAINING , ESISTONO "O" "P" AND "C"
frequency <- data.frame("C"= apply(activity == "C", FUN = sum, MARGIN = 1), 
                        "P"= apply(activity == "P", FUN = sum, MARGIN = 1) )
frequency <- frequency %>% mutate("sum" = C+P)
frequency <- frequency[ frequency$sum != 0, ]

a <- activity[frequency$sum != 0 , -1 ] # restringiamoci subito al sottoinsieme in cui gli user rientrano più di una volta
frequency <- frequency[frequency$sum != 0 , ] 
IdxC <- a == "C"
IdxP <- a == "P"
IdxO <- a == "O"

# These functions contain all the lenghts of the time scales necessary to go from one group (C or P) to the immediate successor (C or P)
jumpsC2C <- jumpsC2P <- jumpsP2C <- jumpsP2P <- numeric(0)

for (i in 1:nrow(a)) {
    
    idxC <- as.numeric( which(IdxC[i,]) )
    idxP <- as.numeric( which(IdxP[i,]) )
    
    CC_distance <- c(diff(idxC), -1) # l'ultimo termine è ausiliario e serve per far tornare le cose
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

# GRAFICO 1: PARTENZA DAL CORE

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


# GRAFICO 2: PARTENZA DALLA PERIPHERY

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

# ATTENZIONE: Perchè ci sono alcune righe di activity in cui non compare nè P nè C?

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
