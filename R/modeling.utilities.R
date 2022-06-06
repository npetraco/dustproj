#' "Harmonize" one or more questioned dust vector to a set of known dust vectors
#'
#' XXXX
#'
#' The function will put the two sets of dust vectors into the same category space
#'
#' @param XX The XX
#' @details The function will put the two sets of dust vectors into the same category space.
#' It keeps categories which appear in at leat one of the vectors and drops categories wich appear
#' in neither set.
#' @return The function will XX
#'
#'
#' @export
harmonize.QtoKs<-function(Q.mat, K.mat) {

  if(!is.matrix(Q.mat)){
    stop("Q input must be a matrix!")
  }
  if(!is.matrix(K.mat)){
    stop("K input must be a matrix!")
  }

  harmonized.vects <- rbind(Q.mat, K.mat)
  non.zeroQ        <- colSums(harmonized.vects) != 0
  harmonized.vects <- harmonized.vects[,non.zeroQ]

  Q.harmonized <- harmonized.vects[1:nrow(Q.mat),]
  K.harmonized <- harmonized.vects[(nrow(Q.mat)+1):nrow(harmonized.vects),]

  kept.category.idxs <- which(non.zeroQ==T) # Union of Q and K category indices occurring at least once
  kept.IDs <- names(kept.category.idxs)
  # Should be the Category ID numbers labeled at the top of the master data-matrix:
  kept.IDs <- as.numeric(sapply(1:length(kept.IDs), function(xx){strsplit(kept.IDs[xx],"X")[[1]][2]}))

  if( sum(kept.IDs==kept.category.idxs) != length(kept.category.idxs) ){
    print(data.frame(names(kept.category.idxs), kept.IDs, kept.category.idxs))
    stop("Something wrong with column IDs. Have X in front?")
  }

  Q.only.idxs        <- which(colSums(K.harmonized) == 0)
  K.only.idxs        <- (1:ncol(harmonized.vects))[-Q.only.idxs]
  names(K.only.idxs) <- colnames(K.harmonized)[K.only.idxs]

  #print(Q.only.idxs)
  Q.only.category.IDs <- kept.IDs[Q.only.idxs]
  K.only.category.IDs <- kept.IDs[-Q.only.idxs] # could be kept.IDs[K.only.idxs] too

  harmonized.info <- list(Q.harmonized, K.harmonized,
                          kept.IDs,
                          Q.only.category.IDs, Q.only.idxs,
                          K.only.category.IDs, K.only.idxs)
  names(harmonized.info) <- c("Q.harmonized", "K.harmonized",
                              "QK.Category.IDs",
                              "Q.only.category.IDs", "Q.only.harmonized.idxs",
                              "K.only.category.IDs", "K.only.harmonized.idxs")

  return(harmonized.info)

}


#' Harmonize Q (row) vector with K(s) and build adjacency matrix and edge matrix for use with building local Q-K graph and potentials
#'
#' Function was make.model.rep in testing scripts
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @details XXXX
#'
#' @return The function will XX
#'
#'
#' @export
local.model.prep <- function(a.Q.vec, a.K.mat, population.adj.mat=NULL, model.type = "model1", printQ=F) {

  harmonized.info <- harmonize.QtoKs(a.Q.vec, a.K.mat)

  harmonized.K.only.idxs <- harmonized.info$K.only.harmonized.idxs
  harmonized.Q.only.idxs <- harmonized.info$Q.only.harmonized.idxs

  all.KQ.idxs <- sort(c(harmonized.K.only.idxs, harmonized.Q.only.idxs))
  #print(all.KQ.idxs)

  model.adj.mat  <- array(0, c(length(all.KQ.idxs), length(all.KQ.idxs)))
  model.edge.mat <- NULL

  # Models 1, 2, 3: Connect all Konly nodes. For model1: Treat all Qonly nodes as independent, so go no further.
  for(i in 1:length(harmonized.K.only.idxs)) {
    for(j in 1:length(harmonized.K.only.idxs)) {

      if(harmonized.K.only.idxs[i] != harmonized.K.only.idxs[j]) {

        # Symmetric model adjacency matrix
        model.adj.mat[harmonized.K.only.idxs[i], harmonized.K.only.idxs[j]] <- 1

        # Model edge matrix
        if(harmonized.K.only.idxs[i] < harmonized.K.only.idxs[j]) {
          #print(paste0(harmonized.K.only.idxs[i], "-", harmonized.K.only.idxs[j]))
          model.edge.mat <- rbind(model.edge.mat, c(harmonized.K.only.idxs[i], harmonized.K.only.idxs[j]))
        }
      }
    }
  }


  # Model 2: Connect all Qonly nodes that are dependent in the population, but don't connect them to K nodes
  if(model.type == "model2") {

    Q.only.category.IDs <- harmonized.info$Q.only.category.IDs # The category ID in the population

    for(i in 1:length(harmonized.Q.only.idxs)) {
      for(j in 1:length(harmonized.Q.only.idxs)) {

        if(harmonized.Q.only.idxs[i] != harmonized.Q.only.idxs[j]) {

          # First check to see if the Qonlys are connected in the population mat
          #print("HERE")
          Qn1.ID <- Q.only.category.IDs[i]
          Qn2.ID <- Q.only.category.IDs[j]
          #print(paste0("X",Qn1.ID,"-X",Qn2.ID,"?"))
          if(population.adj.mat[Qn1.ID,Qn2.ID] == 1){ # If Q nodes are connected in the population, connect them here
            if(printQ == T) {
              print(paste0("Model 2: ============= YES ============== X",Qn1.ID,"-X",Qn2.ID,"! "))
            }

            # Symmetric model adjacency matrix
            model.adj.mat[harmonized.Q.only.idxs[i], harmonized.Q.only.idxs[j]] <- 1

            # Model edge matrix
            if(harmonized.Q.only.idxs[i] < harmonized.Q.only.idxs[j]) {
              #print(paste0(harmonized.Q.only.idxs[i], "-", harmonized.Q.only.idxs[j]))
              model.edge.mat <- rbind(model.edge.mat, c(harmonized.Q.only.idxs[i], harmonized.Q.only.idxs[j]))
            }

          }

        }
      }
    }

  }

  # Model 3: Connect all Qonly nodes that are dependent in the population, AND connect them to K nodes if they are dependent in the population
  if(model.type == "model3") {

    Q.only.category.IDs <- harmonized.info$Q.only.category.IDs # The category ID in the population
    QK.category.IDs     <- harmonized.info$QK.Category.IDs     # The category ID in the population

    count <- 1
    for(i in 1:length(harmonized.Q.only.idxs)) {
      for(j in 1:length(QK.category.IDs)) {

        if(Q.only.category.IDs[i] != QK.category.IDs[j]) {

          # First check to see if the Qonlys are connected in the population mat
          #print("HERE")
          Q.ID   <- Q.only.category.IDs[i]
          QoK.ID <- QK.category.IDs[j]
          #print(paste0("X",Q.ID,"-X",QoK.ID,"? ", count))
          count <- count + 1

          if(population.adj.mat[Q.ID,QoK.ID] == 1){ # If nodes are connected in the population, connect them here
            if(printQ == T) {
              print(paste0("Model 3: ========== YES ========= X",Q.ID,"-X",QoK.ID," ", count))
            }

            # Symmetric model adjacency matrix
            model.adj.mat[harmonized.Q.only.idxs[i], j] <- 1 # j runs over all QK indices, so don't need the translation
            model.adj.mat[j, harmonized.Q.only.idxs[i]] <- 1 # j runs over all QK indices, so don't need the translation

            # Model edge matrix
            if(harmonized.Q.only.idxs[i] < j) {
              model.edge.mat <- rbind(model.edge.mat, c(harmonized.Q.only.idxs[i], j))
            }

          }

        }

      }
    }

  }

  model.info        <- list(harmonized.info, model.adj.mat, model.edge.mat)
  names(model.info) <- c("harmonized.info", "model.adj.mat", "model.edge.mat")

  return(model.info)

}


#' Use population and local dust samples to make node and edge "affinities" a-la Koller, pp. 103-104
#'
#' XXXXX
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @details XXXX
#'
#' @return The function will XX
#'
#'
#' @export
make.QK.harmonized.affinities <- function(a.harmonized.info.list, an.edge.mat, population.datamat, num.local.sims=NULL, est.type="MLE", prob.adj=0.005, normalizeQ=F, printQ=F) {

  K.harmonized           <- a.harmonized.info.list$K.harmonized
  K.only.harmonized.idxs <- a.harmonized.info.list$K.only.harmonized.idxs
  #K.only.category.IDs    <- a.harmonized.info.list$K.only.category.IDs
  Q.only.harmonized.idxs <- a.harmonized.info.list$Q.only.harmonized.idxs
  #Q.only.category.IDs    <- a.harmonized.info.list$Q.only.category.IDs
  QK.Category.IDs        <- a.harmonized.info.list$QK.Category.IDs

  # Build out local (harmonized) K sample to be num.local.sims + number of real K samples
  if(is.null(num.local.sims)) {
    num.local.sims.loc <-  nrow(population.datamat) - nrow(K.harmonized) # Default to balance the local sample counts with the population size
  } else {
    num.local.sims.loc <- num.local.sims
  }
  K.harmonized.prob.mat <- groupwise.category.freq.mat2(K.harmonized, rep(1,nrow(K.harmonized)), est.type = est.type, prob.adj = prob.adj)
  K.harmonized.local    <- simulate.dust.sample.simple(num.local.sims.loc, prob.vec = K.harmonized.prob.mat)
  K.harmonized.local    <- rbind(K.harmonized, K.harmonized.local) # Tack real and simulated samples together
  #print(dim(K.harmonized.local))


  # Node affinities:
  num.nodes <- length(QK.Category.IDs)

  # First determine if node is K or Qonly
  for(node.idx in 1:num.nodes) {

      if(node.idx %in% K.only.harmonized.idxs) {
        node.idx.st <- "K"                               # i.e. index is in K and Q index set
        node.idx.ID <- node.idx                          # For K nodes, take sample from the K.harmonized.local sample
        node.sample <- K.harmonized.local[, node.idx.ID]
      } else if(node.idx %in% Q.only.harmonized.idxs) {
        node.idx.st <- "Qonly"                           # i.e. the index set K didn't have this one
        node.idx.ID <- QK.Category.IDs[node.idx]         # For Qonly nodes, take sample from the population
        node.sample <- population.datamat[, node.idx.ID]
      } else {
        stop("Node", node.idx, "not found in K or Q index sets!")
      }

      if(printQ == T) {
        print(paste0("Node ", node.idx, " is: ", node.idx.st))
      }

  }



  # Edge affinities:
  # for(i in 1:nrow(an.edge.mat)) {
  #
  #   # First determine if nodes are K or Qonly
  #   idx1 <- an.edge.mat[i,1]
  #   idx2 <- an.edge.mat[i,2]
  #
  #   if(idx1 %in% K.only.harmonized.idxs) {
  #     idx1.st      <- "K"  # i.e. index is in K and Q index set
  #     idx1.ID      <- idx1 # For K nodes, take sample from the K.harmonized.local sample
  #     node1.sample <- K.harmonized.local[, idx1.ID]
  #   } else if(idx1 %in% Q.only.harmonized.idxs) {
  #     idx1.st      <- "Qonly"                       # i.e. the index set K didn't have this one
  #     idx1.ID      <- QK.Category.IDs[idx1]         # For Qonly nodes, take sample from the population
  #     node1.sample <- population.datamat[, idx1.ID]
  #   } else {
  #     stop("Node", i ," = ", idx1, "not found in K or Q index sets!")
  #   }
  #
  #   if(idx2 %in% K.only.harmonized.idxs) {
  #     idx2.st      <- "K"                           # i.e. index is in K and Q index set
  #     idx2.ID      <- idx2                          # For K nodes, take sample from the K.harmonized.local sample
  #     node2.sample <- K.harmonized.local[, idx2.ID]
  #   } else if(idx2 %in% Q.only.harmonized.idxs) {
  #     idx2.st      <- "Qonly"                       # i.e. the index set K didn't have this one
  #     idx2.ID      <- QK.Category.IDs[idx2]         # For Qonly nodes, take sample from the population
  #     node2.sample <- population.datamat[, idx2.ID]
  #   } else {
  #     stop("Node", i ," = ", idx2, "not found in K or Q index sets!")
  #   }
  #
  #   # Check for NAs in node samples which indicate something went wrong:
  #   if(NA %in% node1.sample) {
  #     print(paste0("Node ", idx1, " of edge ", i, " is: ", idx1.st, " with idx1.ID = ", idx1.ID))
  #     stop("NAs in node1.sample for edge: ", i, " Something is wrong!")
  #   }
  #   if(NA %in% node2.sample) {
  #     print(paste0("Node ", idx2, " of edge ", i, " is: ", idx2.st, " with idx1.ID = ", idx2.ID))
  #     stop("NAs in node2.sample for edge: ", i, " Something is wrong!")
  #   }
  #
  #
  #   # Edge "affinity" matrix:
  #   eam <- table(
  #     factor(node1.sample, levels = c(1,0)),
  #     factor(node2.sample, levels = c(1,0))
  #   )
  #
  #   # Fix any 0s to 1s in eam because we cant take log(0) to get parameter (energy)
  #   zero.idxs <- which(eam == 0, arr.ind = T)
  #   if(nrow(zero.idxs) == 4){
  #     print(paste0("Node ", idx1, " of edge ", i, " is: ", idx1.st))
  #     print(paste0("Node ", idx2, " of edge ", i, " is: ", idx2.st))
  #     print(eam)
  #     print(paste(nrow(zero.idxs), "0-counts replaced for edge:", i))
  #     stop("Something is wrong. Shouldn't be 4 0s to replace!")
  #   }
  #
  #   if(nrow(zero.idxs) != 0 ) {
  #     eam[zero.idxs] <- 1
  #     if(printQ==T) {
  #       print(paste(nrow(zero.idxs), "0-counts replaced for edge:", i))
  #       #print(node1.sample)
  #       #print(node2.sample)
  #       #print(paste("idx1", idx1))
  #       #print(paste("QK.Category.IDs[idx1]", QK.Category.IDs[idx1]))
  #       #print(paste("idx1.ID", idx1.ID))
  #       #print("------")
  #       #print(paste("idx2", idx1))
  #       #print(paste("QK.Category.IDs[idx2]", QK.Category.IDs[idx2]))
  #       #print(paste("idx2.ID", idx2.ID))
  #     }
  #   }
  #
  #   # Label dimensions by local node ID index
  #   names(attributes(eam)$dimnames) <- c(idx1, idx2)
  #
  #   # normalize to %-scale
  #   if(normalizeQ == T){
  #     #ceiling(eam/sum(eam) * 100)
  #     eam <- eam/sum(eam) * 100
  #   }
  #
  #
  #   if(printQ == T) {
  #     print(paste0("Node ", idx1, " of edge ", i, " is: ", idx1.st))
  #     print(paste0("Node ", idx2, " of edge ", i, " is: ", idx2.st))
  #     #print(eam)
  #   }
  #
  # }

}
