################################# METHODS TO COMPUTE THE CONNECTIVITY ################################# 
# Correlation coefficients -----
getCC <- function(ts, method){
  N <- length(ts)
  CCmat <- matrix(0, nrow = N, ncol = N)
  
  for(i in 1:(N - 1)) {
    vec <- lapply((i + 1):N, function(j) {
      cor(ts[[i]], ts[[j]], method=method )
    })
    
    CCmat[i, (i + 1):N] <-  unlist(vec)
    CCmat[(i + 1):N, i] <-  unlist(vec)
  }
  return(CCmat)
}

# Cross-correlation coefficient (with lag) ----
getCR <- function(ts, lag){
  N <- length(ts)
  CRmat <- matrix(1, nrow = N, ncol = N)
  
  for(i in 1:(N-1)){
    
    vec <- lapply((i + 1):N, function(j) {
      ccf(ts[[i]], ts[[j]], plot=F, lag.max=lag)$acf[1]
    })
    
    CRmat[i, (i + 1):N] <-  unlist(vec)
    CRmat[(i + 1):N, i] <-  unlist(vec)
  }
  return(CRmat)
}

# Granger Causality -----
getGC <- function(ts, lag, pval=F){
  
  output <- ifelse(pval,4,3)
  N <- length(ts)
  GCmat <- matrix(0, nrow = N, ncol = N)
  
  for(i in 1:N){
    
    GCvec <- lapply(1:N, function(j) {
      if(i!=j){
        lmtest::grangertest(ts[[i]], ts[[j]], lag)[2,output]  # [2,3] for the statistic, [2,4] for the p-value
      }else{0}
    })
    
    GCmat[i, ] <-  unlist(GCvec)
    
  }
  return(GCmat)
}

# Mutual Information -----

getMI <- function(ts, bins){
  
  N <- length(ts)
  MImat <- matrix(0, nrow = N, ncol = N)
  
  for(i in 1:(N-1)){
    
    MIvec <- lapply((i+1):N, function(j) {
      disc <- entropy::discretize2d(ts[[i]], ts[[j]], numBins1 = bins, numBins2 = bins)
      mi.mean <- entropy::mi.empirical(disc, unit = "log2")
      return(mi.mean)
    })
    
    MImat[i, (i+1):N] <-  unlist(MIvec)
    MImat[(i+1):N, i] <-  unlist(MIvec)
    
  }
  return(MImat)
}

# Transfer Entropy ----

getTE <- function(ts, lx, ly, type, quantiles=NULL, bins=NULL){
  
  N <- length(ts)
  TEmat <- matrix(0, nrow = N, ncol = N)
  
  for(i in 1:N){
    
    TEvec <- lapply(1:N, function(j) {
      RTransferEntropy::calc_te(ts[[i]], ts[[j]], lx = lx, ly = ly, q = 1, entropy = c('Shannon'), type = type, quantiles=quantiles, bins = bins)
    })
    
    TEmat[i, ] <-  unlist(TEvec)
    
  }
  return(TEmat)
}

# USEFUL IF YOU HAVE MORE THAN 2 TIMESERIES:

#plot.ConAdj ----
  # Compare Connectivity matrix (colours) with adjacency matrix (labels)
  # Input: connectivity = Matrix of connectivity measure NxN (the output of the tools above)
  #        A = the adjacency matrix A of the target network (ground-truth, if known)
  # Output: plot
  
  plot.ConAdj <- function(connectivity, A=NULL, x.loop=F, legend.name='Connectivity', void_theme=T, title=NULL) { 
    library(ggplot2)
    
    dcon <- reshape2::melt(connectivity)
    if (!is.null(A)) {
      dA <- reshape2::melt(A)
    } else{
      dA <- reshape2::melt(matrix(NA, dim(connectivity)[1], dim(connectivity)[2]))
    }
    if (x.loop) {
      xloop <- matrix(NA, dim(connectivity)[1], dim(connectivity)[2])
      diag(xloop) <- 'X'
      xloop <- reshape2::melt(xloop)
      loop.size=4
    } else{
      xloop <- matrix(0, dim(connectivity)[1], dim(connectivity)[2])
      xloop <- reshape2::melt(xloop)
      loop.size=0
    }
    
    return(
      ggplot(data = dcon, aes(x=Var2, y=rev(Var1), fill=value)) + 
        geom_tile(color = "white") +
        geom_text( aes(dA$Var2, rev(dA$Var1), label = dA$value), color = "black", size = 2) +
        geom_text( aes(xloop$Var2, rev(xloop$Var1), label = xloop$value), color = "black", size = loop.size) +
        scale_fill_gradient2(na.value = 'whitesmoke', low = "blue", high = "red3", mid = "white", midpoint = 0, space = "Lab", name=legend.name) +
        theme_minimal() + coord_fixed() + labs(title=title, x='', y='') + 
        if(void_theme==T){
          theme_void()
        }else{
          theme(text=element_text(family='Times'),
                axis.text = element_text( vjust = 0, size = 12, hjust = 0),
                axis.text.x = element_text(angle = 90),
                legend.text = element_text(size = 13),
                legend.title = element_text(size = 15),
                legend.key.size = unit(25,'point'),
                legend.key.height = unit(30,'point')
          )
        }
    )
  }
