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
# GBR
# data <- read_tsv("C:/Users/pierg/Documents/R/CNR_project/network_GBR_en_20200122-20200522.tsv.gz")


#----------------------------------------------------------------------------------------------------------------------
#------------------------------  CORE TEMPORAL ANALYSIS -------------------------------------------
#----------------------------------------------------------------------------------------------------------------------


# date <- as.POSIXct(c("2020-01-01","2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01"))
date <- as.POSIXct( as.Date("2020-01-22") + seq(0, length( numeric( max(data$Datetime)-min(data$Datetime) ) ), 1) )

df <- data.frame(0, 0, 0, 0, 0, 0)
names(df) <- c("content_fake", "content_tot", "user_core", "user_peri", "user_fake", "user_tot" )

retweet <- data %>% filter(tweetType== "RT" ) %>% filter(!is.na(FactType)) # filter(isBot == 0) %>% 
#%>% filter(User %in% unique(fakes$User))
# filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI")



for (i in seq(1, length(date)-1) ) {
    
    rm(retweet_temporal, retweet_fakes)
    retweet_temporal <- retweet %>% 
        filter(Datetime > date[i] & Datetime < date[i+1] ) # %>% filter(isBot == 0) %>% 
    # filter(User %in% unique(fakes$User))
    # filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI")
    
    retweet_fakes <- retweet_temporal %>% # filter(Datetime > date[i] & Datetime < date[i+1] ) %>% # filter(isBot == 0) %>% 
        # filter(tweetType== "RT") %>% #%>% filter(User %in% unique(fakes$User))
        filter(FactType == "FAKE/HOAX" | FactType == "CONSPIRACY/JUNKSCI" | FactType == "CLICKBAIT" )
    
    activity_temporal <- full_join(retweet_temporal %>% count(User), retweet_temporal %>% count("User"=toUser), by = "User") %>% 
        replace_na( replace = as.list(c(n.x = 0, n.y = 0))) %>%
        transmute("User" = User, "RTout" = n.x, "RTin" = n.y) %>%
        left_join( full_join(retweet_fakes %>% count(User), retweet_fakes %>% count("User"=toUser), by = "User") %>%
                       replace_na( replace = as.list(c(n.x = 0, n.y = 0))) %>%
                       transmute("User" = User, "RTout.fakes" = n.x, "RTin.fakes" = n.y) ,
                   by = "User" ) %>%
        replace_na( replace = as.list(c("RTout.fakes" = 0, "RTin.fakes" = 0)) ) 
    
    fakers <- activity_temporal %>% filter(RTin.fakes > 0 | RTout.fakes > 0) 
    core <- fakers %>% filter( RTin.fakes/RTin > 0.2 & RTin > 0)
    periphery <- fakers %>% filter( !(User %in% core$User) )
    
    #--- Compute the number of people in the core in each week plus
    content_tot <- length( retweet_temporal$User ) # Somma di tutti i retweet_temporal
    user_tot <- length(unique(c(retweet_temporal$User, retweet_temporal$toUser))) # Somma di tutti gli user
    user_fake <- length(unique(c(retweet_fakes$User, retweet_fakes$toUser)))
    content_fake <-length( retweet_fakes$User ) # Somma di tutti i retweet fake (dentro e fuori dal core)
    user_core <- length(core$User) # Somma di tutti gli utenti scettici
    user_peri <- length(periphery$User)
    
    print(i)
    df[i,] <- c(content_fake, content_tot, user_core, user_peri, user_fake, user_tot )
    
}

# ---- Ultimiamo la definizione del dataframe aggiungendo valori relativi e i periodi in cui non ci sono fake
df$time <- date[-1]
df <- df %>% filter(user_tot != 0, content_tot != 0)
df$user_core.rel <- df$user_core/df$user_tot
df$user_peri.rel <- df$user_peri/df$user_tot
df$content_fake.rel <- df$content_fake/df$content_tot
# df$time <- time.name[ 1:11 ]

# --- Save the non-averaged version
df_save <- df
lines <- df

# --- Smooth out all the data
n = 10
lines$content_fake.rel <- stats::filter(lines$content_fake.rel, rep(1/n, n), sides = 2)
lines$user_peri.rel <- stats::filter(lines$user_peri.rel, rep(1/n, n), sides = 2)
lines$user_core.rel <- stats::filter(lines$user_core.rel, rep(1/n, n), sides = 2)
lines <- lines[!is.na(lines$content_fake.rel), ]

# ---- Andamento di dimensione core, periphery e fake news volume
scale1 <- ( max(lines$content_fake.rel) - min(lines$content_fake.rel) )  
scale2 <- ( max(lines$user_peri.rel) - min(lines$user_peri.rel) ) 
scale3 <- ( max(lines$user_core.rel) - min(lines$user_core.rel) ) 

lines$user_peri.rel <- lines$user_peri.rel * scale1 / scale2
lines$user_core.rel <- lines$user_core.rel * scale1 / scale3
lines <- gather(lines[, c("time", "user_core.rel", "user_peri.rel", "content_fake.rel")] , 
                key = "type", value = "fraction", -time )

g1 <- ggplot(lines, aes( x = time , y = fraction , color = factor(type)) ) + 
    geom_line( cex= 3  ) +  
    ylim(0.03, 0.098) +
    theme_classic() +
    scale_color_manual(values = c("black", "#ff4d00", "#22a0ff" ), labels = c("Fake retweets", "Creators (rescaled)", "Consumers (rescaled)") ) +
    theme( text=element_text(size=25),
           axis.text.x = element_text(angle = 0, size = 30, hjust = 0),
           axis.text.y = element_text(size = 30),
           legend.position = c(0.7, 0.9),
           legend.text = element_text(size = 40),
           axis.title.x = element_text(size = 40),
           axis.title.y = element_text(size = 40)) + 
    # geom_label( ) +
    labs(x = "day", y = "Rescaled fraction of retweets/users", color = "" )



g1





#----------------------------------------------------------------------------------------------------------------------
#------------------------------  CAUSAL INFERENCE -------------------------------------------
#----------------------------------------------------------------------------------------------------------------------

source("C:/Users/pierg/Documents/R/CNR_project/Basic_Timeseries_Causal_Inference.R")
library(pracma)
library(rEDM)
getCC(list(df_save$user_core.rel, df_save$user_peri.rel), method = "pearson")
getCR(list(df_save$user_peri.rel, df_save$content_fake.rel), lag = 0)
getCR(list(df_save$user_core.rel, df_save$content_fake.rel), lag = 0)
# getGC(list(df$user_core.rel, df$content_fake.rel), lag = 1, pval=F)
# getMI(list(df$user_core.rel, df$content_fake.rel), bins = 5)
# getTE(list(df$user_core.rel, df$content_fake.rel), lx = 1, ly = 1, type = 'quantiles', quantiles = 95)



# params <- expand.grid(cause = 7:8, effect = 7:8, tp = seq(-10,+10))
# params <- params %>% filter(cause != effect)

# --- Plot di Cross correlation per diversi time-delay ------------
out <- do.call(rbind, 
               lapply( seq(0, 10) , function(i) {
                   ccm(df_save[,c(9,10)], E = 10, lib_sizes = NROW(df_save), 
                       random_libs = FALSE, lib_column = names(df_save)[10], target_column = names(df_save)[9], 
                       tp = i, silent = TRUE )
               }
               )
)


ggplot(out, aes(x=tp, y=out[,2] ) ) + geom_line(cex = 2) + 
    geom_line(data=out, aes(x=tp,y=out[,3]), color = 'red', cex=2 ) +
    labs(x = "Time-delay" , y="Cross Map Causality", color = "Causality direction") +
    theme_classic() +
    theme( text =  element_text( size = 15) ) 


# --- Quali sono i surrogati ----------------------

null_hyp <- b <- a <- as.data.frame(0:10)
N_iter <- 500
for(j in 1:N_iter) {
    
    # dummy <- do.call(rbind,
    #                  lapply( seq(0, 10) , function(i) {
    #                      ccm(  df_save[ randperm(1:nrow(df_save)), 7:8 ] , E = 9, lib_sizes = NROW(df_save),
    #                          random_libs = FALSE, lib_column = names(df_save)[8], target_column = names(df_save)[7],
    #                          tp = i, silent = TRUE)
    #                  }
    #                  )
    # )[,2:3]
    
    dummy <- do.call(rbind,
                     lapply( seq(0, 10) , function(i) {
                         ccm(apply( df_save[ , c(9, 10) ], 2, randperm ), E = 10, lib_sizes = NROW(df_save),
                             random_libs = FALSE, lib_column = names(df_save)[10], target_column = names(df_save)[9],
                             tp = i, silent = TRUE)
                     }
                     )
    )[,2:3]
    
    a[,j] <- dummy[,1]
    b[,j] <- dummy[,2]
    print(j)
    
}
names(null_hyp)[1] <- "tp"
null_hyp$core_cause <- apply(a, 1, mean)
null_hyp$fake_cause <- apply(b, 1, mean)
null_hyp$core_cause_sd <- apply(a, 1, sd)
null_hyp$fake_cause_sd <- apply(b, 1, sd)
out1 <- out
names(out1)[2:3] <- c("consumers cause fakes", "fakes cause consumers")
out1 <- gather(out1[,c(2,3,6)], value = "rho", key = "direction", -tp)

d1 <- ggplot(null_hyp, aes(x=tp, y=core_cause ) ) + geom_line(cex = 1.5, lty = "dashed" ) + 
    geom_line(data=null_hyp, aes(x=tp,y=fake_cause), color = 'red', cex = 1.5, lty = "dashed" ) + 
    geom_ribbon( aes( ymin = core_cause-2*core_cause_sd, ymax = core_cause+2*core_cause_sd, x=tp, fill = "95% CL around \nnull model expectation "), alpha = 0.5 ) +
    scale_fill_manual("", values="grey80") +
    labs(x = "Time-delay" , y="Cross Map Causality", color = "Causality direction", title = "") +
    theme_classic() +
    scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(-0.35, 1.05)) +
    scale_x_continuous( breaks = c(0, 2, 4, 6, 8, 10) ) + 
    theme( text =  element_text( size = 25),
           axis.text.x = element_text(size = 20),
           legend.position = c(0.6, 0.8),
           legend.text = element_text(size = 18) ) +
    geom_line( data= out1, aes(x=tp,y=rho, color=direction), cex=2 ) +
    labs(x = "Time-delay (days)" , y="Cross Map Causality", color = "Causality direction") +
    scale_color_manual( values = c("black", "red") )

d1

ggarrange(g1, d1, nrow = 1, ncol = 2)