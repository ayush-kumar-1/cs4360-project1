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


scatter = function(frame, response, row, col) {
  par(mfrow = (c(row,col)), 
      mar = c(4, 4, 3, 3))
  num_col = ncol(frame)
  for (i in 1:num_col) { 
    plot(frame[,i], frame[,response], 
         xlab = names(frame)[i], 
         ylab = response)
  } # for i 
} #scatter

rsq_cv = function(formula, dataset, k, response, seed = 4360) { 
  set.seed(seed) 
  n = nrow(dataset)
  partition_size = as.integer(n/k)
  partition = sample(seq_len(n))
  
  running_sse = 0
  for (i in 1:k){
    lower = 1+((i-1)*partition_size)
    upper = i*partition_size
    testing_selection = partition[lower:upper]
    #divide for cross validation
    training_set = dataset[-testing_selection,]
    testing_set = dataset[testing_selection, ]
    #run the model and predict testing set data
    model = lm(formula, data = training_set)
    yhat = predict(model, testing_set)
    
    #calculate sse and add it running total 
    sse = sum((yhat-testing_set[, response])^2)
    running_sse = running_sse + sse
  } # for i
  
  sst = var(dataset[, response])*n 
  rsq = 1 - (running_sse/sst)
  
  return(rsq)
  
} #rsq_cv

