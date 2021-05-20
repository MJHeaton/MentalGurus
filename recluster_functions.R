#################
### Functions ###
#################


recluster <- function(input.df,                        # Dataframe containing observations to perform clustering on.
                      num.clusters.1 = 4,              # How many clusters in initial layer?
                      num.clusters.2 = 3,              # How many subclusters in second layers?
                      all2 = FALSE,                    # If TRUE, iterates through all combinations of subclusters up to the number specified
                      nrep.center.check = 100,         # Number of times to iterate k-means algorithm
                      set = TRUE,                      # TRUE if want to set random seed
                      seed = 2232021,                  # Specify random seed
                      first.only = FALSE) {            # TRUE if want only an initial layer of clusters
  print("-------------------------------------------------------------------")
  print(paste0("Clustering - First Layer: ", num.clusters.1, " Clusters. Second Layer: ", num.clusters.2, " Clusters."))
  print("-------------------------------------------------------------------")
  dat <- input.df
  
  ### First Layer
  layer1 <- list()
  centers.first <- c()
  if(set) set.seed(seed)
  pb <- progress::progress_bar$new(
    format = "  First Layer [:bar] :percent eta: :eta",
    total = nrep.center.check, clear = FALSE, width= 100)
  for(i in 1:nrep.center.check) {
    pb$tick()
    Sys.sleep(1 / nrep.center.check)
    
    k <- kmeans(dat, centers = num.clusters.1, iter.max = 1000, algorithm = "Lloyd")
    centers.first <- rbind(centers.first, k$centers)
  }
  layer1[["center_iterations"]] <- centers.first
  hc.first <- hclust(dist(centers.first))
  layer1[["hc.first"]] <- hc.first
  hc.first.1 <- cutree(hc.first, k = num.clusters.1)
  split <- c()
  for(m in 1:num.clusters.1) {
    split <- cbind(split, structure((centers.first[hc.first.1 == m,]), dimnames = list(NULL, paste0(c("EXT", "EST", "AGR", "CSN", "OPN"),m))))
  }
  split <- as.data.frame(split)
  layer1[["split"]] <- split
  top5 <- aggregate(list(numdup=rep(1,nrow(split))), split, length) %>% 
    arrange(desc(numdup)) %>% head(5) 
  layer1[["top5"]] <- top5
  centers.first.final <- top5 %>% head(1) %>% select(-numdup) %>% matrix(byrow = TRUE, nrow = num.clusters.1, ncol = 5)
  layer1[["centers"]] <- centers.first.final
  k <- kmeans(dat, centers = centers.first.final, iter.max = 1000, algorithm = "Lloyd")
  layer1[["k"]] <- k
  cluster1.vec <- k$cluster
  results <- layer1
  
  if (first.only == FALSE & all2 == FALSE) {
    ### Second Layer
    layer2 <- list()
    cluster2.vec <- numeric(length(cluster1.vec))
    for(j in 1:num.clusters.1) {
      temp.df <- dat[cluster1.vec == j,]
      temp.list <- list()
      centers2 <- c()
      pb <- progress::progress_bar$new(
        format = paste("  Cluster ", j, " [:bar] :percent eta: :eta"),
        total = nrep.center.check, clear = FALSE, width= 100)
      for(i in 1:nrep.center.check) {
        pb$tick()
        Sys.sleep(1 / nrep.center.check)
        
        k2 <- kmeans(temp.df[,1:5], centers = num.clusters.2, iter.max = 1000, algorithm = "Lloyd")
        centers2 <- rbind(centers2, k2$centers)
      }
      temp.list[["center_iterations"]] <- centers2
      hc.second <- hclust(dist(centers2))
      temp.list[["hc.second"]] <- hc.second
      hc.second.1 <- cutree(hc.second, k = num.clusters.2)
      split2 <- c()
      for(m in 1:num.clusters.2) {
        split2 <- cbind(split2, structure(centers2[hc.second.1 == m,], dimnames = list(NULL, paste0(c("EXT", "EST", "AGR", "CSN", "OPN"),m))))
      }
      split2 <- as.data.frame(split2)
      temp.list[["split"]] <- split2
      top5 <- aggregate(list(numdup=rep(1,nrow(split2))), split2, length) %>% 
        arrange(desc(numdup)) %>% head(5) 
      temp.list[["top5"]] <- top5
      centers.second.final <- top5 %>% head(1) %>% select(-numdup) %>% matrix(byrow = TRUE, nrow = num.clusters.2, ncol = 5)
      temp.list[["centers"]] <- centers.second.final
      k2 <- kmeans(temp.df, centers = centers.second.final, iter.max = 1000, algorithm = "Lloyd")
      temp.list[["k"]] <- k2
      cluster2.vec[cluster1.vec == j] <- k2$cluster
      layer2[[paste0("Clust",j)]] <- temp.list
    }
    
    ### Overall
    layer3 <- list()
    recluster <- data.frame(dat, "cluster" = cluster1.vec, "cluster2" = cluster2.vec) %>% mutate(cluster_id = paste(cluster, cluster2, sep = "_")) 
    reclusterSum <- recluster %>% group_by(cluster, cluster2) %>% summarize_at(c("EXT", "EST", "AGR", "CSN", "OPN"), mean, na.rm = TRUE) %>% ungroup 
    reclusterGathered <- reclusterSum %>% gather(Construct, Value, EXT:OPN) %>% mutate(label = paste(cluster, cluster2, sep = "_"))
    reclusterGathered2 <- recluster %>% group_by(cluster) %>% summarize_at(c("EXT", "EST", "AGR", "CSN", "OPN"), mean, na.rm = TRUE) %>% ungroup %>% 
      gather(Construct, Value, EXT:OPN)
    layer3[["cluster1.vec"]] <- cluster1.vec
    layer3[["cluster2.vec"]] <- cluster2.vec
    layer3[["plotdf"]] <- reclusterGathered
    layer3[["plotdf2"]] <- reclusterGathered2
    
    ### Saving Results
    results <- list("first" = layer1,"second" = layer2,"results" = layer3)
  }
  
  ### Checking best value of subclusters
    if (first.only == FALSE & all2 == TRUE) {
      ### Second Layer
      layer2 <- list()
      cluster2.vec <- numeric(length(cluster1.vec))
      for(j in 1:num.clusters.1) {
        temp.df <- dat[cluster1.vec == j,]
        layer2[[paste0("Clust",j)]] <- list()
        for(n in 2:num.clusters.2) {
          temp.list <- list()
          centers2 <- c()
          pb <- progress::progress_bar$new(
            format = paste("  Cluster ", j, " Subcluster ", n, " [:bar] :percent eta: :eta"),
            total = nrep.center.check, clear = FALSE, width= 100)
          for(i in 1:nrep.center.check) {
            pb$tick()
            Sys.sleep(1 / nrep.center.check)
            
            k2 <- kmeans(temp.df[,1:5], centers = n, iter.max = 1000, algorithm = "Lloyd")
            centers2 <- rbind(centers2, k2$centers)
          }
          temp.list[["center_iterations"]] <- centers2
          hc.second <- hclust(dist(centers2))
          temp.list[["hc.second"]] <- hc.second
          hc.second.1 <- cutree(hc.second, k = n)
          if (length(unique(table(hc.second.1))) == 1) {
            split2 <- c()
            for(m in 1:n) {
              split2 <- cbind(split2, structure(centers2[hc.second.1 == m,], dimnames = list(NULL, paste0(c("EXT", "EST", "AGR", "CSN", "OPN"),m))))
            }
            split2 <- as.data.frame(split2)
            temp.list[["split"]] <- split2
            top5 <- aggregate(list(numdup=rep(1,nrow(split2))), split2, length) %>% 
              arrange(desc(numdup)) %>% head(5) 
            temp.list[["top5"]] <- top5
            centers.second.final <- top5 %>% head(1) %>% select(-numdup) %>% matrix(byrow = TRUE, nrow = n, ncol = 5)
          } else {
            split2 <- c()
            for(m in 1:n) {
              temp.split <- as.data.frame(centers2[hc.second.1 == m,])
              if(ncol(temp.split) == 1) temp.split <- as.data.frame(t(temp.split))
              temp.top <- aggregate(list(numdup=rep(1,nrow(temp.split))), temp.split, length) %>% 
                arrange(desc(numdup)) %>% head(1) 
              split2 <- rbind(split2, temp.top)
            }
            temp.list[["not_even"]] <- "yep"
            centers.second.final <- split2[,-6]
          }
          temp.list[["centers"]] <- centers.second.final
          k2 <- kmeans(temp.df, centers = centers.second.final, iter.max = 1000, algorithm = "Lloyd")
          temp.list[["k"]] <- k2
          temp.list[["min.dist"]] <- min(dist(k2$centers))
          layer2[[paste0("Clust",j)]][[paste0("Subclust",n)]] <- temp.list
        }
      }
      ### Overall
      layer3 <- list()
      recluster <- data.frame(dat, "cluster" = cluster1.vec, "cluster2" = cluster2.vec) %>% mutate(cluster_id = paste(cluster, cluster2, sep = "_")) 
      reclusterGathered2 <- recluster %>% group_by(cluster) %>% summarize_at(c("EXT", "EST", "AGR", "CSN", "OPN"), mean, na.rm = TRUE) %>% ungroup %>% 
        gather(Construct, Value, EXT:OPN)
      layer3[["plotdf2"]] <- reclusterGathered2
    
    ### Saving Results
    results <- list("first" = layer1,"second" = layer2,"results" = layer3)
  }
  
  return(results)
}


graph.recluster <- function(input.df, label.vec = NULL, selection1 = NULL, selection2 = NULL, first.only = FALSE) {
  reclusterGathered <- input.df
  
  if (length(selection1) > 0) {
    ### BY GROUP
    if(first.only) {
      ggplot() +   geom_point(reclusterGathered %>% filter(cluster %in% selection1), 
                              mapping = aes(x = as.factor(Construct), y = Value), color = "black",size = 5) + 
        geom_line(reclusterGathered %>% filter(cluster %in% selection1), 
                  mapping = aes(x = as.factor(Construct), y = Value, 
                                group = as.factor(cluster)), size = 2) + 
        geom_point(reclusterGathered, 
                   mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(cluster)), size = 3) + 
        labs(title = "Cluster Centers", x = "Construct", color = "Cluster Grouping") + theme(plot.title = element_text(hjust = 0.5))
    } else {
      if (is.null(selection2)) selection2 <- 1:length(unique(reclusterGathered$cluster2))
      ggplot() +   geom_point(reclusterGathered %>% filter(cluster %in% selection1, cluster2 %in% selection2), 
                              mapping = aes(x = as.factor(Construct), y = Value), color = "black",size = 5) + 
        geom_line(reclusterGathered %>% filter(cluster %in% selection1, cluster2 %in% selection2), 
                  mapping = aes(x = as.factor(Construct), y = Value, 
                                group = as.factor(label)), size = 2) + 
        geom_point(reclusterGathered, 
                   mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(cluster)), size = 3) + 
        labs(title = "Cluster Centers", x = "Construct", color = "Cluster Grouping") + theme(plot.title = element_text(hjust = 0.5))
    }
  } else
    if (length(label.vec) > 0) {
      ### BY LABEL
      if(first.only) {
        return("Provide Valid Input")
      } else {
        ggplot() +   geom_point(reclusterGathered %>% filter(label %in% label.vec), 
                                mapping = aes(x = as.factor(Construct), y = Value), color = "black",size = 5) + 
          geom_line(reclusterGathered %>% filter(label %in% label.vec), 
                    mapping = aes(x = as.factor(Construct), y = Value, 
                                  group = as.factor(label)), size = 2) + 
          geom_point(reclusterGathered, 
                     mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(cluster)), size = 3) + 
          labs(title = "Cluster Centers", x = "Construct", color = "Cluster Grouping") + theme(plot.title = element_text(hjust = 0.5))
      }
    }
  else {
    ggplot() + 
      geom_point(reclusterGathered, mapping = aes(x = as.factor(Construct), y = Value, color = as.factor(cluster)), size = 3) + 
      labs(title = "Cluster Centers", x = "Construct", color = "Cluster Grouping") + theme(plot.title = element_text(hjust = 0.5))
  }
}

