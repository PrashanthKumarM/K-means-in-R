library(fields)
library(clues)

c_distance <- function(a,b) {
  d <- dist(rbind(a,b))
  return(sum(d))
}

c_average <- function(a, cen) {
  tot <- c()
  for(i in 1:length(a)){
    if(is.null(a[[i]]))
      return(cen)
    else{
      d <- dist(rbind(a[[i]], cen))
      tot <- append(tot, d) 
    }
  }
  tot <- sort(tot)
  return(a[[match(tot[length(tot)/2],tot)]])
}

clean_data <- function(data) {
  for(i in 1:ncol(data)){
    for(j in 1:nrow(data)){
      if(data[[i]][[j]] == '?' || is.na(data[[i]][[j]])){
        data[[i]][[j]] = 0
        data[[i]][[j]] = median(c(data[[i]]), na.rm = TRUE)
        print(paste('SCN is :',data[[1]][[j]], "Attribute is : ", colnames(data)[[i]], "New Data is : ", data[[i]][[j]], sep = " ",collapse = NULL))
      }
    }
  }
  return(data)
}

calculate_variance <- function(data){
  max <- 0
  var <- 0
  for(i in 1:ncol(data)){
    total_variance <- 0
    for(j in 1:nrow(data)){
      total_variance <- total_variance + (as.numeric(data[[i]][[j]]) - mean(c(data[[i]])))**2
    }
    if(var < (total_variance/(nrow(data)-1))){
      var <- total_variance/(nrow(data)-1)
      col <- i
    }
  }
  return(col)
}

# Code for calculating Positive Predictive Value

calculate_ppv <- function(B1, B2, data){
  count <- 0
  matching_b1 = which(match(data[c(as.numeric(rownames(B1))), "V11"], c(B1$class_predicted)) == 1)
  matching_b2 = which(match(data[c(as.numeric(rownames(B2))), "V11"], c(B2$class_predicted)) == 1)
  print("The matches for predicted value in B1: ")
  print(matching_b1)
  print("count of True Positives in B1: ")
  print(length(matching_b1))
  print("False Positives for B1: ")
  print((length(B1$class_predicted) - length(matching_b1)))
  print("The matches for predicted value in B2: ")
  print(matching_b2)
  print("count of True positives in B2: ")
  print(length(matching_b2))
  print("False Positives for B2: ")
  print((length(B2$class_predicted) - length(matching_b2)))
  ppv1 = length(matching_b1)/(length(matching_b1)+(length(B1$class_predicted) - length(matching_b1)))
  print("PPV of B1 :")
  print(ppv1);
  ppv2 = length(matching_b2)/(length(matching_b2)+(length(B2$class_predicted) - length(matching_b2)))
  print("PPV of B2 :")
  print(ppv2);
}

calculate_lowest_entropy <- function(data) {
  entr <- 10000
  low_entr <- 0
  for(i in 1:ncol(data)){
    print(entropy(c(data[[i]])))
    if(entropy(c(data[[i]])) < entr){
      entr <- entropy(c(data[[i]]))
      low_entr <- i
    }
  }
  print("the lowest entropy value is: ")
  print(entr)
  print("The attribute with lowest entropy is: ")
  print(colnames(data)[[low_entr]])
}

calculate_kl <- function(data){
  row <- data.frame(A1=numeric(0), A2=numeric(0), A3=numeric(0), A4=numeric(0), A5=numeric(0), A6=numeric(0), A7=numeric(0), A8=numeric(0), A9=numeric(0))
  for(i in 1:ncol(data)){
    klval <- c()
    name2 <- colnames(data)[[i]]
    for(j in 1:ncol(data)){
      name1 = colnames(data)[[j]]
      klval <- append(klval, KL.empirical(c(data[[i]]), c(data[[j]])))
    }
    row[nrow(row)+1, ] <- klval
    rownames(row) <- colnames(data[,i])
  }
  print(xtable(row))
}

Orig_Cancer_Data <- read.csv(file="breast-cancer-wisconsin.csv", header=F, sep=",")
Orig_Cancer_Data <- clean_data(Orig_Cancer_Data)
Cancer_Data <- Orig_Cancer_Data
Cancer_Data$V1 <- NULL
Cancer_Data$V11 <- NULL
k <- 5
# Proper usage of the custom k means

ca_Data <- CustomKMeans(Cancer_Data, "c_distance", "c_average", k)

#################################

old_cent_data <- ca_Data
for(i in 2:k){ca_Data[[1]][i,] <- ca_Data[[i]][1,]}
cent_data <- ca_Data[[1]]
print("Centroids after k-means is: ")
print(ca_Data)

# Plotting the k means data

plot(Cancer_Data, cent_data, col="orange")

#############################

# Proper way of finding block data


Blocks <- ComputeBlocks(Cancer_Data, old_cent_data, c_distance, k, 2)


###################################


for(y in 1:k){
for(i in 2:length(Blocks[[y]])){Blocks[[y]][[1]][i,] <- Blocks[[y]][[i]][1,]}
}
B1 = Blocks[[1]][[1]]
B2 = Blocks[[2]][[1]]
B3 = Blocks[[3]][[1]]
B4 = Blocks[[4]][[1]]
B5 = Blocks[[5]][[1]]
F = c(1:698) #Total number of rows
F[1:698] <- 0
F[as.integer(rownames(B1))] <- 1
F[as.integer(rownames(B2))] <- 2
F[as.integer(rownames(B3))] <- 3
F[as.integer(rownames(B4))] <- 4
F[as.integer(rownames(B5))] <- 5

# Plotting the cluster data


plotClusters(sapply(Cancer_Data,as.integer), F, xlim=range(1:10), ylim=range(1:10))


###################

B1$class_predicted <- 4
B2$class_predicted <- 2
calculate_ppv(B1, B2, Cancer_Data)