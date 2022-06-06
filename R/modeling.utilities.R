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

  # This should just be 1:length(QK.Category.IDs)
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


#' Use population and local dust samples to make node and edge "affinities" a-la Koller, pp. 103-104, "harmonized" to Q and Ks
#'
#' The function was called make.QK.harmonized.affinities in harmonize_dustvectors_test4.R
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
make.QK.local.harmonized.affinities <- function(a.harmonized.info.list, an.edge.mat, population.datamat, num.local.sims=NULL, est.type="MLE", prob.adj=0.005, normalizeQ=F, printQ=F) {

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
  num.nodes       <- length(QK.Category.IDs)
  node.affinities <- rep(list(NULL), num.nodes)

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

    # Node "affinity" matrix:
    nam        <- as.table(c(sum(node.sample==1), sum(node.sample==0)))
    names(nam) <- c(1,0)

    # Label dimension by local node ID index
    names(attributes(nam)$dimnames) <- node.idx

    # Check for at least 1 1. Non-occurring categories should have been tossed already
    if(nam[1] == 0) {
      print(paste0("Node ", node.idx, " is: ", node.idx.st))
      stop("Problem with node", node.idx, " There are 0 occurances!")
    }

    # normalize to %-scale
    if(normalizeQ == T){
      nam <- nam/sum(nam) * 100
    }

    node.affinities[[node.idx]] <- nam

    if(printQ == T) {
      print(paste0("Node ", node.idx, " is: ", node.idx.st))
      #print(nam)
    }

  }


  # Edge affinities:
  num.edges       <- nrow(an.edge.mat)
  edge.affinities <- rep(list(NULL), num.edges)

  for(i in 1:num.edges) {

    # First determine if nodes are K or Qonly
    idx1 <- an.edge.mat[i,1]
    idx2 <- an.edge.mat[i,2]

    if(idx1 %in% K.only.harmonized.idxs) {
      idx1.st      <- "K"  # i.e. index is in K and Q index set
      idx1.ID      <- idx1 # For K nodes, take sample from the K.harmonized.local sample
      node1.sample <- K.harmonized.local[, idx1.ID]
    } else if(idx1 %in% Q.only.harmonized.idxs) {
      idx1.st      <- "Qonly"                       # i.e. the index set K didn't have this one
      idx1.ID      <- QK.Category.IDs[idx1]         # For Qonly nodes, take sample from the population
      node1.sample <- population.datamat[, idx1.ID]
    } else {
      stop("Node", i ," = ", idx1, "not found in K or Q index sets!")
    }

    if(idx2 %in% K.only.harmonized.idxs) {
      idx2.st      <- "K"                           # i.e. index is in K and Q index set
      idx2.ID      <- idx2                          # For K nodes, take sample from the K.harmonized.local sample
      node2.sample <- K.harmonized.local[, idx2.ID]
    } else if(idx2 %in% Q.only.harmonized.idxs) {
      idx2.st      <- "Qonly"                       # i.e. the index set K didn't have this one
      idx2.ID      <- QK.Category.IDs[idx2]         # For Qonly nodes, take sample from the population
      node2.sample <- population.datamat[, idx2.ID]
    } else {
      stop("Node", i ," = ", idx2, "not found in K or Q index sets!")
    }

    # Check for NAs in node samples which indicate something went wrong:
    if(NA %in% node1.sample) {
      print(paste0("Node ", idx1, " of edge ", i, " is: ", idx1.st, " with idx1.ID = ", idx1.ID))
      stop("NAs in node1.sample for edge: ", i, " Something is wrong!")
    }
    if(NA %in% node2.sample) {
      print(paste0("Node ", idx2, " of edge ", i, " is: ", idx2.st, " with idx1.ID = ", idx2.ID))
      stop("NAs in node2.sample for edge: ", i, " Something is wrong!")
    }


    # Edge "affinity" matrix:
    eam <- table(
      factor(node1.sample, levels = c(1,0)),
      factor(node2.sample, levels = c(1,0))
    )

    # Fix any 0s to 1s in eam because we cant take log(0) to get parameter (energy)
    zero.idxs <- which(eam == 0, arr.ind = T)
    if(nrow(zero.idxs) == 4){
      print(paste0("Node ", idx1, " of edge ", i, " is: ", idx1.st))
      print(paste0("Node ", idx2, " of edge ", i, " is: ", idx2.st))
      print(eam)
      print(paste(nrow(zero.idxs), "0-counts replaced for edge:", i))
      stop("Something is wrong. Shouldn't be 4 0s to replace!")
    }

    if(nrow(zero.idxs) != 0 ) {
      eam[zero.idxs] <- 1
      if(printQ==T) {
        print(paste(nrow(zero.idxs), "0-counts replaced for edge:", i))
        #print(node1.sample)
        #print(node2.sample)
        #print(paste("idx1", idx1))
        #print(paste("QK.Category.IDs[idx1]", QK.Category.IDs[idx1]))
        #print(paste("idx1.ID", idx1.ID))
        #print("------")
        #print(paste("idx2", idx1))
        #print(paste("QK.Category.IDs[idx2]", QK.Category.IDs[idx2]))
        #print(paste("idx2.ID", idx2.ID))
      }
    }

    # Label dimensions by local node ID index
    names(attributes(eam)$dimnames) <- c(idx1, idx2)

    # normalize to %-scale
    if(normalizeQ == T){
      #ceiling(eam/sum(eam) * 100)
      eam <- eam/sum(eam) * 100
    }

    edge.affinities[[i]] <- eam

    if(printQ == T) {
      print(paste0("Node ", idx1, " of edge ", i, " is: ", idx1.st))
      print(paste0("Node ", idx2, " of edge ", i, " is: ", idx2.st))
      #print(eam)
    }

  }

  names(node.affinities) <- as.character(1:num.nodes)
  names(edge.affinities) <- sapply(1:num.edges, function(xx){paste0(an.edge.mat[xx,], collapse = "-")})

  affinity.info <- list(node.affinities, edge.affinities, K.harmonized.local)
  names(affinity.info) <- c("node.affinities", "edge.affinities", "K.harmonized.local")

  return(affinity.info)

}


#' Harmonize Q (row) vector with K(s) and build adjacency matrix and edge matrix for use with building population Q-K graph and potentials
#'
#' FXXXXXX
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
population.model.prep <- function(a.Q.vec, a.K.mat, population.adj.mat, printQ=F) {

  harmonized.info <- harmonize.QtoKs(a.Q.vec, a.K.mat)

  QK.Category.IDs <- harmonized.info$QK.Category.IDs

  # This should just be 1:length(QK.Category.IDs)
  #all.KQ.idxs <- sort(c(harmonized.K.only.idxs, harmonized.Q.only.idxs))
  #print(all.KQ.idxs)
  all.KQ.idxs <- 1:length(QK.Category.IDs)
  num.nodes   <- length(all.KQ.idxs)

  model.adj.mat  <- population.adj.mat[QK.Category.IDs, QK.Category.IDs]
  model.adj.mat  <- as.matrix(model.adj.mat) # Was in data.frame format which some things don't like
  rownames(model.adj.mat) <- all.KQ.idxs # **NOTE: Takes IDs from NC-IDs -> "Harmonized"-IDs wrt this adjacency matrix
  colnames(model.adj.mat) <- all.KQ.idxs # **NOTE: Takes IDs from NC-IDs -> "Harmonized"-IDs wrt this adjacency matrix

  model.edge.mat <- NULL
  for(i in 1:num.nodes) {
    for(j in 1:num.nodes) {
      if(i < j) {
        if(model.adj.mat[i,j] == 1) {
          model.edge.mat <- rbind(model.edge.mat, c(i,j))
        }
      }
    }
  }

  model.info        <- list(harmonized.info, model.adj.mat, model.edge.mat)
  names(model.info) <- c("harmonized.info", "model.adj.mat", "model.edge.mat")

  return(model.info)

}


#' Use population dust samples to make node and edge "affinities" a-la Koller, pp. 103-104, "harmonized" to Q and Ks
#'
#' The function was called
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
make.QK.population.harmonized.affinities <- function(a.harmonized.info.list, an.edge.mat, population.datamat, num.local.sims=NULL, est.type="MLE", prob.adj=0.005, normalizeQ=F, printQ=F) {

  #K.harmonized           <- a.harmonized.info.list$K.harmonized
  K.only.harmonized.idxs <- a.harmonized.info.list$K.only.harmonized.idxs
  #K.only.category.IDs    <- a.harmonized.info.list$K.only.category.IDs
  Q.only.harmonized.idxs <- a.harmonized.info.list$Q.only.harmonized.idxs
  #Q.only.category.IDs    <- a.harmonized.info.list$Q.only.category.IDs
  QK.Category.IDs        <- a.harmonized.info.list$QK.Category.IDs


  # Node affinities:
  num.nodes       <- length(QK.Category.IDs)
  node.affinities <- rep(list(NULL), num.nodes)

  # Recycled code from make.QK.local, so some of this is supirfluous
  for(node.idx in 1:num.nodes) {
    if(node.idx %in% K.only.harmonized.idxs) {
      node.idx.st <- "K"                               # Don't need, but: i.e. index is in K and Q index set
      node.idx.ID <- QK.Category.IDs[node.idx]         # Get all data from the population
      node.sample <- population.datamat[, node.idx.ID] # Get all data from the population
    } else if(node.idx %in% Q.only.harmonized.idxs) {
      node.idx.st <- "Qonly"                           # Don't need, but: i.e. index is in K and Q index set
      node.idx.ID <- QK.Category.IDs[node.idx]         # Get all data from the population
      node.sample <- population.datamat[, node.idx.ID] # Get all data from the population
    } else {
      stop("Node", node.idx, "not found in K or Q index sets!")
    }

    # Node "affinity" matrix:
    nam        <- as.table(c(sum(node.sample==1), sum(node.sample==0)))
    names(nam) <- c(1,0)

    # Label dimension by local node ID index
    names(attributes(nam)$dimnames) <- node.idx

    # Check for at least 1 1. Non-occurring categories should have been tossed already
    if(nam[1] == 0) {
      print(paste0("Node ", node.idx, " is: ", node.idx.st))
      stop("Problem with node", node.idx, " There are 0 occurances!")
    }

    # normalize to %-scale
    if(normalizeQ == T){
      nam <- nam/sum(nam) * 100
    }

    node.affinities[[node.idx]] <- nam

    if(printQ == T) {
      print(paste0("Node ", node.idx, " is: ", node.idx.st))
      print(nam)
    }

  }


  # Edge affinities:
  num.edges       <- nrow(an.edge.mat)
  edge.affinities <- rep(list(NULL), num.edges)

  for(i in 1:num.edges) {

    # Recycling code from make.QK.local. Some of it supurflus
    idx1 <- an.edge.mat[i,1]
    idx2 <- an.edge.mat[i,2]

    if(idx1 %in% K.only.harmonized.idxs) {
      idx1.st      <- "K"                           # Don't need here but: i.e. index is in K and Q index set
      idx1.ID      <- QK.Category.IDs[idx1]         # Here, get all data from population
      node1.sample <- population.datamat[, idx1.ID] # Here, get all data from population
    } else if(idx1 %in% Q.only.harmonized.idxs) {
      idx1.st      <- "Qonly"                       # Don't need here but: i.e. the index set K didn't have this one
      idx1.ID      <- QK.Category.IDs[idx1]         # Here, get all data from population
      node1.sample <- population.datamat[, idx1.ID] # Here, get all data from population
    } else {
      stop("Node", i ," = ", idx1, "not found in K or Q index sets!")
    }

    if(idx2 %in% K.only.harmonized.idxs) {
      idx2.st      <- "K"                           # Don't need here but: i.e. index is in K and Q index set
      idx2.ID      <- QK.Category.IDs[idx2]         # Here, get all data from population
      node2.sample <- population.datamat[, idx2.ID] # Here, get all data from population
    } else if(idx2 %in% Q.only.harmonized.idxs) {
      idx2.st      <- "Qonly"                       # Don't need here but: i.e. index is in K and Q index set
      idx2.ID      <- QK.Category.IDs[idx2]         # Here, get all data from population
      node2.sample <- population.datamat[, idx2.ID] # Here, get all data from population
    } else {
      stop("Node", i ," = ", idx2, "not found in K or Q index sets!")
    }

    # Check for NAs in node samples which indicate something went wrong:
    if(NA %in% node1.sample) {
      print(paste0("Node ", idx1, " of edge ", i, " is: ", idx1.st, " with idx1.ID = ", idx1.ID))
      stop("NAs in node1.sample for edge: ", i, " Something is wrong!")
    }
    if(NA %in% node2.sample) {
      print(paste0("Node ", idx2, " of edge ", i, " is: ", idx2.st, " with idx1.ID = ", idx2.ID))
      stop("NAs in node2.sample for edge: ", i, " Something is wrong!")
    }


    # Edge "affinity" matrix:
    eam <- table(
      factor(node1.sample, levels = c(1,0)),
      factor(node2.sample, levels = c(1,0))
    )

    # Fix any 0s to 1s in eam because we cant take log(0) to get parameter (energy)
    zero.idxs <- which(eam == 0, arr.ind = T)

    # Something is wrong if 4 0s come up:
    if(nrow(zero.idxs) == 4){
      print(paste0("Node ", idx1, " of edge ", i, " is: ", idx1.st))
      print(paste0("Node ", idx2, " of edge ", i, " is: ", idx2.st))
      print(eam)
      print(paste(nrow(zero.idxs), "0-counts replaced for edge:", i))
      stop("Something is wrong. Shouldn't be 4 0s to replace!")
    }

    if(nrow(zero.idxs) != 0 ) {
      eam[zero.idxs] <- 1
      if(printQ==T) {
        print(paste(nrow(zero.idxs), "0-counts replaced for edge:", i))
      }
    }

    # Label dimensions by local node ID index
    names(attributes(eam)$dimnames) <- c(idx1, idx2)

    # normalize to %-scale
    if(normalizeQ == T){
      #ceiling(eam/sum(eam) * 100)
      eam <- eam/sum(eam) * 100
    }

    edge.affinities[[i]] <- eam

    if(printQ == T) {
      print(paste0("Node ", idx1, " of edge ", i, " is: ", idx1.st))
      print(paste0("Node ", idx2, " of edge ", i, " is: ", idx2.st))
      #print(eam)
    }

  }


  names(node.affinities) <- as.character(1:num.nodes)
  names(edge.affinities) <- sapply(1:num.edges, function(xx){paste0(an.edge.mat[xx,], collapse = "-")})

  affinity.info <- list(node.affinities, edge.affinities)
  names(affinity.info) <- c("node.affinities", "edge.affinities")

  return(affinity.info)


}



#' Extract the info needed to feed to CRF/CRFutil
#'
#' The function was called
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
get.component.graph.info <- function(component.graph.nodes, a.model.adj.mat, affinities.info, a.harmonized.info) {

  harmonized.idxs <- as.numeric(component.graph.nodes)
  if(NA %in% harmonized.idxs) { # component.graph.nodes must contain only numbers. No non-number characters!
    stop("NAs from component.graph.nodes names. Check to see that they are numbers.")
  }

  # Form adjacency matrix of this connected component:
  component.graph.idxs <- 1:length(harmonized.idxs)
  component.adj.mat    <- a.model.adj.mat[harmonized.idxs,harmonized.idxs]
  colnames(component.adj.mat) <- component.graph.idxs # **NOTE: "Harmonized"-IDs to this specific connected component IDs
  rownames(component.adj.mat) <- component.graph.idxs # **NOTE: "Harmonized"-IDs to this specific connected component IDs
  #print(component.adj.mat)

  # Node info:
  num.nodes          <- length(harmonized.idxs)
  node.names         <- names(affinities.info$node.affinities)
  node.affinity.idxs <- sapply(1:num.nodes, function(xx){which(node.names == harmonized.idxs[xx])})
  node.affinities    <- affinities.info$node.affinities[node.affinity.idxs]
  #print(node.affinities)



  # Edge info:
  # Form edge mats of this connected component:
  hm.edge.mat <- NULL # Edge matrix in terms of "Harmonized" node indices, which are probably non-contiguous
  cc.edge.mat <- NULL # Edge matrix in terms of component graph idxs, which are contiguous
  for(i in 1:length(component.graph.idxs)) {
    for(j in 1:length(component.graph.idxs)) {
      if(harmonized.idxs[i] < harmonized.idxs[j]) {
        if(i<j){ # Just to double check that this is true too
          if(component.adj.mat[i,j] == 1) {
            hm.edge.mat <- rbind(hm.edge.mat, c(harmonized.idxs[i], harmonized.idxs[j]))
            cc.edge.mat <- rbind(cc.edge.mat, c(i,j))
          }
        } else {
          stop("Check conected component edge node order. Should be L < R!") # i<j if check
        }
      }
    }
  }
  #print(hm.edge.mat)
  #print(cc.edge.mat)

  # Pull out edge affinities required for this connected component graph:
  num.edges     <- nrow(hm.edge.mat)
  # Edge names of this connected component, in-terms of the harmonized indices
  hm.edge.names <- sapply(1:num.edges, function(xx){paste0(hm.edge.mat[xx,], collapse = "-")})

  # All edge names (also in-terms of the harmonized indices)
  edge.affinity.names <- names(affinities.info$edge.affinities)

  # See what edge numbers are of the edges for this component graph
  edge.affinity.idxs  <- sapply(1:length(hm.edge.names), function(xx){which(edge.affinity.names == hm.edge.names[xx])})

  # Pull out the required edge affinities:
  edge.affinities     <- affinities.info$edge.affinities[edge.affinity.idxs]
  #print(edge.affinities)


  # Node index/ID translation info after all the reductions/re-indexing we've done:
  idx.translation.mat           <- cbind(a.harmonized.info$QK.Category.IDs[harmonized.idxs], harmonized.idxs, component.graph.idxs)
  colnames(idx.translation.mat) <- c("QK.Category.IDs", "harmonized.idxs", "component.graph.idxs") # **NOTE: QK.Category.IDs are the NC indices from the population data, i.e. the OG indices AFTER non-occurring categories were dropped and re-indexing was done.
  #print(idx.translation.mat)

  graph.component.info <- list(
    component.adj.mat,
    hm.edge.mat,
    cc.edge.mat,
    idx.translation.mat,
    node.affinities,
    edge.affinities
  )
  names(graph.component.info) <- c(
    "component.adj.mat",
    "harmonized.edge.mat",
    "component.edge.mat",
    "idx.translation.mat",
    "node.affinities",
    "edge.affinities"
  )

  return(graph.component.info)

}
