library(ggplot2)
library(reshape2)

### Contains utils used for many different utils used during regression analysis

train_test_split = function(dataframe, percent, seed = 4360) { 
  train_size = floor(percent*nrow(dataframe))
  obs = nrow(dataframe) # observations in dataset
  
  set.seed(seed) # for reproducibility 
  train_index = sample(seq_len(obs), train_size)
  
  return(train_index)
  
  # code adapted from https://stackoverflow.com/a/17200430
  
} #train_test_split


plot_cormat = function(dataf) { 
  ### Code adapted from: 
  #www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
  
  cormat = cor(dataf)
  melted_cormat = melt(cormat)
  melted_cormat$value = round(melted_cormat$value, 2)
  
  ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) + 
    geom_tile(color="white") + 
    #create gradient
    scale_fill_gradient2(low = "purple", high = "blue", 
                         mid = "white", midpoint = 0, limit = c(-1,1), 
                         space = "Lab", name = "Correlation") + 
    # Tild y text
    theme(axis.text.x = element_text(angle = 90)) + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) + 
    theme(
      axis.title.x = element_blank(),  #blanks multiple fields 
      axis.title.y = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.border = element_blank(), 
      panel.background = element_blank(), 
      axis.ticks = element_blank(), )
  
} #plot_cormat


scatter = function (xcol, ycol, dataf, xlabel, ylabel, save = NULL) {
  if (!is.null(save)) { 
    
    }
  scatter_plot = ggplot(dataf, aes(xcol, ycol)) + 
    geom_point(size = 2, color = "blue") + 
    xlab(xlabel) + 
    ylab(ylabel) 
  
  print(scatter_plot)
} #scatter

