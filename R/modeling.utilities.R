#' "Harmonize" one or more questioned dust vector to a set of known dust vectors
#'
#' XXXX
#'
#' The function will put the two sets of dust vectors into the same category space
#'
#' @param XX The XX
#' @details The function will put the two sets of dust vectors into the same category space.
#' It keeps categories which appear in at least one of the vectors and drops categories which appear
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


#' Version 1: Harmonize Q (row) vector with K(s) and build adjacency matrix and edge matrix for use with building local Q-K graph and potentials
#'
#' Function was make.model.rep in testing scripts
#'
#' The function FULLY CONNECTS the categories in the K sample.
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

#' Version 2: Harmonize Q (row) vector with K(s) and build adjacency matrix and edge matrix for use with building local Q-K graph and potentials
#'
#' Function was make.model.rep in testing scripts
#'
#' This version runs a real/simulated K sample through grapHD to get a local graph. ALSO
#' this version currently treats Qonly categories as INDEPENDENT in the local model. We
#' justify this because the Qonly categories were not observed in anny of the K samples
#'
#' At some point IMPLEMENT OTHER DEPENDENCY STRUCTURES FOR THE Qonlys
#'
#' @param XX The XX
#' @details XXXX
#'
#' @return The function will XX
#'
#'
#' @export
local.model.prep2 <- function(a.Q.vec, a.K.mat, population.datamat, num.local.sims=NULL, est.typ="MLE", prob.adj =0.005, printQ=F, plotQ=F, Category.IDs.plotQ=T) {

  QK.harmonized.info     <- harmonize.QtoKs(a.Q.vec, a.K.mat)
  K.harmonized           <- QK.harmonized.info$K.harmonized
  K.only.harmonized.idxs <- QK.harmonized.info$K.only.harmonized.idxs
  Q.only.harmonized.idxs <- QK.harmonized.info$Q.only.harmonized.idxs
  QK.Category.IDs        <- QK.harmonized.info$QK.Category.IDs

  # Simulate QK (based on K.harmonized) for use with making local potentials.
  # **NOTE Qonlys are simulated too, but we'll only be using the Konly columns for now (6-8-22)
  pvec <- groupwise.category.freq.mat2(K.harmonized, rep(1, nrow(K.harmonized)), est.type = est.typ, prob.adj = prob.adj)

  # Build out local (harmonized) K sample to be num.local.sims + number of real K samples
  if(is.null(num.local.sims)) {
    num.local.sims.loc <-  nrow(population.datamat) - nrow(K.harmonized) # Default to balance the local sample counts with the population size
  } else {
    num.local.sims.loc <- num.local.sims
  }

  # **NOTE (again), we WON'T be using the sims in the Qonly columns.
  # We will be substituting the Qonly sims with the actual population data.
  # We'll do this for storage because We'll use the population Qonly data to estimate
  # Pr(Categ_Qonly) in the ver-2 make affinities function because they will be treated as independent
  QK.harmonized.local <- simulate.dust.sample.simple(num.local.sims.loc, prob.vec = pvec)
  QK.harmonized.local <- rbind(
    K.harmonized,       # This is the real K sample (harmonized)
    QK.harmonized.local # This is the sims
  )
  #print(dim(QK.harmonized.local))

  # NEW approach in version 2: Use real/simulated Ks with grapHD to get a "local" graph.
  # In version 1, we just fully connected all the Ks
  Konly.sims <- QK.harmonized.local[,K.only.harmonized.idxs] # data for K nodes only (real and simulated)

  # Input into grapHD must be in terms of factors for discrete data:
  Konly.sims <- sapply(1:ncol(Konly.sims), function(xx){as.factor(Konly.sims[,xx])})
  Konly.sims <- data.frame(Konly.sims, stringsAsFactors = T)
  rownames(Konly.sims) <- NULL
  colnames(Konly.sims) <- NULL

  # Build graph for Konly, assuming Qonlys are independent because they appeared in NONE of the K samples
  # print("Must have grapHD installed, which is not on CRAN. Also turn it on manually")
  KForest               <- minForest(Konly.sims, stat = "AIC")

  # Edges come out re-indexed because Qonly columns were dropped.
  # Re-index them back to harmonized indices
  edges.harmonized <- cbind(
    K.only.harmonized.idxs[ KForest$edges[,1] ],
    K.only.harmonized.idxs[ KForest$edges[,2] ]
  )
  rownames(edges.harmonized) <- NULL
  colnames(edges.harmonized) <- c("K.harmonized.idx1","K.harmonized.idx2")
  #print(edges.harmonized)

  minForest.edge.scores           <- data.frame(KForest$edges, edges.harmonized, KForest$statSeq) # Return these
  colnames(minForest.edge.scores) <- c("idx1","idx2","K.harmonized.idx1","K.harmonized.idx2", "minForest.score")
  check.edges                     <- KForest$error # Return these. ** NOTE: If these are not null, CHECK these with fisher test??
  #print(KForest)
  #print(minForest.edge.scores)

  num.nodes           <- ncol(K.harmonized)
  local.adjacency.mat <- edges2adj(edges.harmonized, n.nodes = num.nodes)
  #print(dim(local.adjacency.mat))

  if(plotQ==T) {
    local.grapHD <- as.gRapHD(edges.harmonized, p=num.nodes)

    v <- 1:num.nodes
    node.cols <- rep("", num.nodes)
    node.cols[K.only.harmonized.idxs] <- "green"
    node.cols[Q.only.harmonized.idxs] <- "red"
    #print(node.cols)

    # These should be the Konly categories that weren't there 100% of the time. Color them yellow.
    K.harmonized.counts <- colSums(K.harmonized)
    K.semis <- which( (K.harmonized.counts < nrow(K.harmonized)) & (K.harmonized.counts !=0))
    node.cols[K.semis] <- "yellow"

    if(Category.IDs.plotQ==T) {
      node.names <- paste0("X",QK.Category.IDs)
    } else {
      node.names <- v
    }

    if(!is.null(dev.list())){
      dev.off()
    }
    plot(local.grapHD, numIter=1000, vert.label=T, vert.radii=rep(.028,length(v)),
         vert.labels=node.names, vert.hl=v, col.hl=node.cols)
  }

  # Substitute in Qonly pop data into QK.harmonized.local for calculating local Pr(Qonly) later
  # (IE we treat the local Qonly as independent, so we don't care which other categories
  # they co-occur with. We just need them for independent node computations.
  # NOTE: the Qonly columns and rows in the local adjacency matrix should all be 0
  QK.harmonized.local[,Q.only.harmonized.idxs] <- population.datamat[ , QK.Category.IDs[Q.only.harmonized.idxs] ]

  # Check that the substitution was made correctly
  #scheckv <- nrow(population.datamat)
  #print(scheckv == sum(QK.harmonized.local[,Q.only.harmonized.idxs[1]] == population.datamat[,QK.Category.IDs[Q.only.harmonized.idxs[1]] ]))
  #print(scheckv == sum(QK.harmonized.local[,Q.only.harmonized.idxs[2]] == population.datamat[,QK.Category.IDs[Q.only.harmonized.idxs[2]] ]))
  #print(scheckv == sum(QK.harmonized.local[,Q.only.harmonized.idxs[3]] == population.datamat[,QK.Category.IDs[Q.only.harmonized.idxs[3]] ]))
  #check.all.Qonlysubs <- sapply(1:length(Q.only.harmonized.idxs), function(xx){scheckv == sum(QK.harmonized.local[,Q.only.harmonized.idxs[xx]] == population.datamat[,QK.Category.IDs[Q.only.harmonized.idxs[xx]] ])})
  #print(check.all.Qonlysubs)

  minForest.edges <- data.frame(edges.harmonized, KForest$edges)
  #print(data.frame(minForest.edge.scores, minForest.edges))

  local.info <- list(
    QK.harmonized.info,    # Because we harmonized before we did anythong in here, so keep this for the record
    QK.harmonized.local,   # Real and Simulated K data. Population Qonly data
    local.adjacency.mat,   # Adjacency matrix for this local model
    edges.harmonized,      # Edge matrix for this local model
    minForest.edges,       # Translation between Konly.harmonized.idxs, and their re-indexed version used by minForest
    minForest.edge.scores, # Edge scores and edge indices from minForest
    KForest                # The grapHD minForest object for the Konlys
  )

  names(local.info) <- c(
    "QK.harmonized.info",    # Because we harmonized before we did anything in here, so keep this for the record
    "QK.harmonized.local",   # Real and Simulated K data. Population Qonly data
    "model.adjacency.mat",   # Adjacency matrix for this local model
    "edge.mat",              # Edge matrix for this local model
    "minForest.edges",       # Translation between Konly.harmonized.idxs, and their re-indexed version used by minForest
    "minForest.edge.scores", # Edge scores and edge indices from minForest
    "KForest"                # The grapHD minForest object for the Konlys
  )

  return(local.info)

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
  # Tack real and simulated samples together
  K.harmonized.local    <- rbind(K.harmonized, K.harmonized.local)
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

    # Fix any 0s to 1s in nam because we cant take log(0) to get parameter (energy)
    zero.idxs <- which(nam == 0)

    # Something is wrong if 2 0s come up:
    if(length(zero.idxs) == 2){
      print(paste0("Node ", node.idx, " is: ", idx1.st))
      print(nam)
      print(paste(length(zero.idxs), "0-counts replaced for node:", i))
      stop("Something is wrong. Shouldn't be 2 0s to replace!")
    }

    if(length(zero.idxs) != 0 ) {
      nam[zero.idxs] <- 1

      if(printQ==T) {
        print(paste(length(zero.idxs), "0-counts replaced for edge:", i))
      }
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

  affinity.info <- list(node.affinities, edge.affinities, K.harmonized.local, K.harmonized.prob.mat)
  names(affinity.info) <- c("node.affinities", "edge.affinities", "K.harmonized.local", "Category.Probs.for.KSims")

  return(affinity.info)

}


#' Version 2: Use local K dust samples and sim samples to make node and edge "affinities" a-la Koller, pp. 103-104, "harmonized" to Q and Ks
#'
#' The function works more like the simple population construction of affinities, rather than the more complicated make.QK.local.harmonized.affinities version 1
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
make.QK.local.harmonized.affinities2 <- function(a.QK.local.prep.info.list, normalizeQ=F, scale.factor=1, printQ=F) {

  a.harmonized.info.list <- a.QK.local.prep.info.list$QK.harmonized.info # Because we harmonized before we did anything in here, so keep this for the record
  an.edge.mat            <- a.QK.local.prep.info.list$edge.mat   # Edge matrix for this local model
  population.datamat     <- a.QK.local.prep.info.list$QK.harmonized.local      # Real and Simulated K data AND Population Qonly data. This is not a population datamatrix. Just recycling the name.
  #QK.harmonized.info,    # Because we harmonized before we did anything in here, so keep this for the record
  #QK.harmonized.local,   # Real and Simulated K data. Population Qonly data
  #local.adjacency.mat,   # Adjacency matrix for this local model
  #edges.harmonized,      # Edge matrix for this local model
  #print(population.datamat)


  K.only.harmonized.idxs <- a.harmonized.info.list$K.only.harmonized.idxs
  Q.only.harmonized.idxs <- a.harmonized.info.list$Q.only.harmonized.idxs
  QK.Category.IDs        <- a.harmonized.info.list$QK.Category.IDs
  #print(K.only.harmonized.idxs)
  #print(Q.only.harmonized.idxs)
  #print(QK.Category.IDs)

  # Node affinities:
  num.nodes       <- length(QK.Category.IDs)
  node.affinities <- rep(list(NULL), num.nodes)

  # Recycled code from version 1make.QK.local, so some of this is superfluous
  for(node.idx in 1:num.nodes) {
    if(node.idx %in% K.only.harmonized.idxs) {
      node.idx.st <- "K"                               # Don't need, but: i.e. index is in K and Q index set
      #node.idx.ID <- QK.Category.IDs[node.idx]         # Get all data from the "population": Here a K real/sim column
      node.idx.ID <- node.idx                          # Get all data from the "population": Here a K real/sim column
      node.sample <- population.datamat[, node.idx.ID] # Get all data from the "population": Here a K real/sim column
    } else if(node.idx %in% Q.only.harmonized.idxs) {
      node.idx.st <- "Qonly"                           # Don't need, but: i.e. index is in K and Q index set
      #node.idx.ID <- QK.Category.IDs[node.idx]         # Get all data from the population: Here a Qonly really should be from the population
      node.idx.ID <- node.idx                          # Get all data from the population: Here a Qonly really should be from the population
      node.sample <- population.datamat[, node.idx.ID] # Get all data from the population: Here a Qonly really should be from the population
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

    # Fix any 0s to 1s in nam because we cant take log(0) to get parameter (energy)
    zero.idxs <- which(nam == 0)

    # Something is wrong if 2 0s come up:
    if(length(zero.idxs) == 2){
      print(paste0("Node ", node.idx, " is: ", idx1.st))
      print(nam)
      print(paste(length(zero.idxs), "0-counts replaced for node:", i))
      stop("Something is wrong. Shouldn't be 2 0s to replace!")
    }

    if(length(zero.idxs) != 0 ) {
      nam[zero.idxs] <- 1

      if(printQ==T) {
        print(paste(length(zero.idxs), "0-counts replaced for edge:", i))
      }
    }

    # Scale affinities: (was normalize to %-scale)
    if(normalizeQ == T){
      nam <- nam/sum(nam) * scale.factor
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

    # Recycling code from make.QK.local. Some of it is superfluous
    idx1 <- an.edge.mat[i,1]
    idx2 <- an.edge.mat[i,2]

    if(idx1 %in% K.only.harmonized.idxs) {
      idx1.st      <- "K"                           # Don't need here but: i.e. index is in K and Q index set
      #idx1.ID      <- QK.Category.IDs[idx1]         # Here, get all data from population
      idx1.ID      <- idx1                          # Get all data from the "population": Here a K real/sim column
      node1.sample <- population.datamat[, idx1.ID] # Get all data from the "population": Here a K real/sim column
    } else if(idx1 %in% Q.only.harmonized.idxs) {
      idx1.st      <- "Qonly"                       # Don't need here but: i.e. the index set K didn't have this one
      #idx1.ID      <- QK.Category.IDs[idx1]         # Get all data from the population: Here a Qonly really should be from the population
      idx1.ID      <- idx1                          # Get all data from the population: Here a Qonly really should be from the population
      node1.sample <- population.datamat[, idx1.ID] # Get all data from the population: Here a Qonly really should be from the population
    } else {
      stop("Node", i ," = ", idx1, "not found in K or Q index sets!")
    }

    if(idx2 %in% K.only.harmonized.idxs) {
      idx2.st      <- "K"                           # Don't need here but: i.e. index is in K and Q index set
      #idx2.ID      <- QK.Category.IDs[idx2]         # Here, get all data from population
      idx2.ID      <- idx2                          # Get all data from the "population": Here a K real/sim column
      node2.sample <- population.datamat[, idx2.ID] # Get all data from the "population": Here a K real/sim column
    } else if(idx2 %in% Q.only.harmonized.idxs) {
      idx2.st      <- "Qonly"                       # Don't need here but: i.e. index is in K and Q index set
      #idx2.ID      <- QK.Category.IDs[idx2]         # Here, get all data from population
      idx2.ID      <- idx2                          # Get all data from the population: Here a Qonly really should be from the population
      node2.sample <- population.datamat[, idx2.ID] # Get all data from the population: Here a Qonly really should be from the population
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
      eam <- eam/sum(eam) * scale.factor
    }

    edge.affinities[[i]] <- eam

    if(printQ == T) {
      print(paste0("Node ", idx1, " of edge ", i, " is: ", idx1.st))
      print(paste0("Node ", idx2, " of edge ", i, " is: ", idx2.st))
      print(eam)
    }

  }


  names(node.affinities) <- as.character(1:num.nodes)
  names(edge.affinities) <- sapply(1:num.edges, function(xx){paste0(an.edge.mat[xx,], collapse = "-")})

  affinity.info <- list(node.affinities, edge.affinities)
  names(affinity.info) <- c("node.affinities", "edge.affinities")

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
make.QK.population.harmonized.affinities <- function(a.harmonized.info.list, an.edge.mat, population.datamat, normalizeQ=F, printQ=F) {

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

    # Fix any 0s to 1s in nam because we cant take log(0) to get parameter (energy)
    zero.idxs <- which(nam == 0)

    # Something is wrong if 2 0s come up:
    if(length(zero.idxs) == 2){
      print(paste0("Node ", node.idx, " is: ", idx1.st))
      print(nam)
      print(paste(length(zero.idxs), "0-counts replaced for node:", i))
      stop("Something is wrong. Shouldn't be 2 0s to replace!")
    }

    if(length(zero.idxs) != 0 ) {
      nam[zero.idxs] <- 1

      if(printQ==T) {
        print(paste(length(zero.idxs), "0-counts replaced for edge:", i))
      }
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

    # Recycling code from make.QK.local. Some of it is superfluous
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

  if(length(harmonized.idxs) == 1) {

    # None of these are applicable for just a node
    component.adj.mat    <- NULL
    hm.edge.mat          <- NULL
    cc.edge.mat          <- NULL
    edge.affinities      <- NULL
    component.graph.idxs <- 1

    # For one unconnected node, just pull the node affinity BUT hold off on  normalizing. Well
    # do that in make.component.mrf

    # Node info (recycled from below):
    num.nodes          <- length(harmonized.idxs) # Should be 1 here.
    node.names         <- names(affinities.info$node.affinities)
    node.affinity.idxs <- sapply(1:num.nodes, function(xx){which(node.names == harmonized.idxs[xx])})
    node.affinities    <- affinities.info$node.affinities[node.affinity.idxs]

    # if(sum(node.affinities[[1]]) != 100) { # **NOTE: Assumes we normalized on the %-scale, which we did above.
    #   node.affinities[[1]] <- node.affinities[[1]]/sum(node.affinities[[1]]) * 100
    # }

  } else { # More than two nodes are a connected component graph

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

  } # end else

  # Log the affinities (ie make them energies) for use with config.energy() function, etc in CRFutil
  log.node.affinities <- log_list(node.affinities)
  if(length(harmonized.idxs) > 1) {
    #print("Here graph")
    log.edge.affinities <- log_list(edge.affinities)
  } else {
    #print("Here node only")
    log.edge.affinities <- NULL # No (log) edge affinities for single nodes
  }

  #print(log.node.affinities)
  #print(log.edge.affinities)


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
    edge.affinities,
    log.node.affinities,
    log.edge.affinities
  )
  names(graph.component.info) <- c(
    "component.adj.mat",
    "harmonized.edge.mat",
    "component.edge.mat",
    "idx.translation.mat",
    "node.affinities",
    "edge.affinities",
    "log.node.affinities",
    "log.edge.affinities"
  )

  return(graph.component.info)

}


#' Make CRF object (an MRF), insert affinities and get logZ
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
make.component.mrf_SAFE <- function(con.nodes.vec, prep.info, affinities.info) {

  num.nodes <- length(con.nodes.vec)

  # Gather required info for the MRF:           # FIX: ONLY PERTAINS TO LOCAL MODELS!!!!!!
  graph.comp.info <- get.component.graph.info(
    component.graph.nodes = con.nodes.vec,
    a.model.adj.mat       = prep.info$model.adjacency.mat, # Will split out adjacency matrix of this graph component from whole model adjacency matrix
    affinities.info       = affinities.info,
    a.harmonized.info     = prep.info$QK.harmonized.info)
  #print(graph.comp.info)

  # Reformat node affinities into a num.nodes x 2 matrix for use with CRF
  node.affinities       <- t(sapply(1:num.nodes, function(xx){graph.comp.info$node.affinities[[xx]]}))
  # For checks:
  harmonized.node.names <- names(graph.comp.info$node.affinities) # The (harmonized) node names in the graph component. Should be the same as con.nodes.vec
  harmonized.node.idxs  <- as.numeric(harmonized.node.names)      # Again, should be the same as con.nodes.vec. Just convert to numeric for use as indices later
  #print(harmonized.node.names)
  #print(node.affinities)
  #print(num.nodes)

  # Edge affinities
  edge.affinities       <- graph.comp.info$edge.affinities
  # For checks:
  harmonized.edge.names <- names(edge.affinities)
  #print(edge.affinities)
  #print(harmonized.edge.names)

  # Make a basic CRF object from the graph component adjacency matrix and input the affinities
  component.mrf          <- make.crf(graph.comp.info$component.adj.mat, 2)
  component.mrf$node.pot <- node.affinities
  component.mrf$edge.pot <- edge.affinities

  # Use Junction or Loopy BP depending on graph size:
  if(num.nodes <= 20) {
    bp.info <- infer.junction(component.mrf)
  } else {
    bp.info <- infer.lbp(component.mrf, max.iter = 10000)
  }
  #print(bp.info$logZ)

  # This is redundant, but doing this for convenience of formatting, error checking and safety.
  # This ensures a copy of the input affinities follow the logZ they are associated with.
  # They a also contained in component.mrf. The node.affinities are just formatted as a matrix in there.
  # affinities.info.in.this.MRF <- list(
  #                                      graph.comp.info$node.affinities,
  #                                      graph.comp.info$edge.affinities
  #                                    )
  # names(affinities.info.in.this.MRF) <- c("component.graph.node.affinities", "component.graph.edge.affinities")

  # ****Decided to just return graph.comp.info instead, which contains graph.comp.info$node.affinities
  # and graph.comp.info$edge.affinities among other information need for graph energy calculation



  component.mrf.info <- list(
    harmonized.node.idxs,
    harmonized.node.names,
    harmonized.edge.names,
    #affinities.info.in.this.MRF, # This is in graph.component.info
    graph.comp.info,
    component.mrf,
    bp.info
  )

  names(component.mrf.info) <- c(
    "harmonized.node.idxs.for.mrf",
    "harmonized.node.names.for.mrf",
    "harmonized.edge.names.for.mrf",
    #"affinities.info.for.mrf",
    "component.graph.info",
    "component.mrf",
    "bp.info"
  )

  return(component.mrf.info)

}


#' Make CRF object (an MRF), insert affinities and get logZ, generalized to handle single nodes too
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
make.component.mrf <- function(con.nodes.vec, prep.info, affinities.info) {

  num.nodes <- length(con.nodes.vec)

  # Gather required info for the MRF:           # FIX: ONLY PERTAINS TO LOCAL MODELS!!!!!!
  graph.comp.info <- get.component.graph.info(
    component.graph.nodes = con.nodes.vec,
    a.model.adj.mat       = prep.info$model.adjacency.mat, # Will split out adjacency matrix of this graph component from whole model adjacency matrix
    affinities.info       = affinities.info,
    a.harmonized.info     = prep.info$QK.harmonized.info)
  #print(graph.comp.info)

  # For checks:
  harmonized.node.names <- names(graph.comp.info$node.affinities) # The (harmonized) node names in the graph component. Should be the same as con.nodes.vec
  harmonized.node.idxs  <- as.numeric(harmonized.node.names)      # Again, should be the same as con.nodes.vec. Just convert to numeric for use as indices later
  #print(harmonized.node.names)
  #print(node.affinities)
  #print(num.nodes)

  if(num.nodes == 1){ # For one node, no need for an MRF. Just do this:

    node.affinities <- graph.comp.info$node.affinities
    #print("Here")
    #print(harmonized.node.names)
    #print(node.affinities)
    #print(num.nodes)

    # **NOTE: This MUST change if we switch from using counts as the node/edge potentials
    # Normalize node affinities:
    if(sum(node.affinities[[1]]) != 1) {
      node.affinities[[1]] <- node.affinities[[1]]/sum(node.affinities[[1]])
    }
    component.mrf                       <- node.affinities
    graph.comp.info$node.affinities     <- node.affinities # Substitute the normalized affinities
    graph.comp.info$log.node.affinities <- log_list(node.affinities)  # Souldn't be needed but, substitute the normalized affinities
    #print(graph.comp.info$node.affinities)

    # Non of these are relevant if there is only one node
    harmonized.edge.names <- NULL
    bp.info               <- NULL

    component.mrf.info <- list(
      harmonized.node.idxs,
      harmonized.node.names,
      harmonized.edge.names,
      graph.comp.info,
      component.mrf,
      bp.info
    )


  } else if(num.nodes > 1){ # Go on if there is more then one node

    # Reformat node affinities into a num.nodes x 2 matrix for use with CRF
    node.affinities       <- t(sapply(1:num.nodes, function(xx){graph.comp.info$node.affinities[[xx]]}))

    # Edge affinities
    edge.affinities       <- graph.comp.info$edge.affinities
    # For checks:
    harmonized.edge.names <- names(edge.affinities)
    #print(edge.affinities)
    #print(harmonized.edge.names)

    # Make a basic CRF object from the graph component adjacency matrix and input the affinities
    component.mrf          <- make.crf(graph.comp.info$component.adj.mat, 2)
    component.mrf$node.pot <- node.affinities
    component.mrf$edge.pot <- edge.affinities

    # Use Junction or Loopy BP depending on graph size:
    if(num.nodes <= 20) {
      bp.info <- infer.junction(component.mrf)
    } else {
      bp.info <- infer.lbp(component.mrf, max.iter = 10000)
    }
    #print(bp.info$logZ)

    # Returning graph.comp.info which is redundant, because much of the important affinity info
    # is contained on component.mrf. BUT, doing this for convenience of formatting (all affinities
    # are in list form), error checking and safety.
    # This also ensures a copy of the input affinities follow the logZ they are associated with.
    component.mrf.info <- list(
      harmonized.node.idxs,
      harmonized.node.names,
      harmonized.edge.names,
      graph.comp.info,
      component.mrf,
      bp.info
    )

  } else {
    stop("No nodes in this component: num.nodes = ", num.nodes)
  }


  names(component.mrf.info) <- c(
    "harmonized.node.idxs.for.mrf",
    "harmonized.node.names.for.mrf",
    "harmonized.edge.names.for.mrf",
    "component.graph.info",
    "component.mrf",
    "bp.info"
  )

  return(component.mrf.info)

}


#' Compute the probability of a multinode configuration along with associted info
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
compute.component.graph.dust.config.prob.info_SAFE <- function(config.vec, an.mrf.info, ff, printQ=F) {


  log.Z    <- an.mrf.info$bp.info$logZ
  edge.mat <- an.mrf.info$component.graph.info$component.edge.mat

  log.node.affinities <- an.mrf.info$component.graph.info$log.node.affinities
  log.edge.affinities <- an.mrf.info$component.graph.info$log.edge.affinities
  #print(log.node.affinities)
  #print(log.edge.affinities)

  cfg.enrgy <- config.energy(
                              config    = config.vec,
                              edges.mat = edge.mat,
                              one.lgp   = log.node.affinities,
                              two.lgp   = log.edge.affinities,
                              ff        = f)
  cfg.log.prob <- cfg.enrgy - log.Z
  cfg.prob     <- exp(cfg.log.prob)

  if(printQ==T) {
    print(paste0("log(Z):     ", log.Z))
    print(paste0("Energy(X):  ", cfg.enrgy))
    print(paste0("log[Pr(X)]: ", cfg.log.prob))
    print(paste0("Pr(X):      ", cfg.prob))
  }

  prob.info <- list(
    log.Z,
    cfg.enrgy,
    cfg.log.prob,
    cfg.prob
  )

  names(prob.info) <- list(
    "logZ",
    "energy",
    "log.prob",
    "prob"
  )

  return(prob.info)

}


#' Compute the probability of a graph component configuration along with associated info
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
compute.component.graph.dust.config.prob.info <- function(config.vec, an.mrf.info, ff, printQ=F) {

  if(length(config.vec) == 1) { # One node

    node.probs <- an.mrf.info$component.graph.info$node.affinities[[1]]
    # These should already be normalized:
    if(sum(node.probs) != 1) {
      print(node.probs)
      stop("Node 1/0 probabilities don't sum to 1!")
    }

    if(config.vec == 1) {
      cfg.prob     <- node.probs[1]
      cfg.log.prob <- log(cfg.prob)
    } else if(config.vec == 0) {
      cfg.prob     <- node.probs[2]
      cfg.log.prob <- log(cfg.prob)
    } else {
      print(config.vec)
      stop("config.vec elements must be 1 or 0!")
    }

    # Not applicable for one node
    log.Z        <- NULL
    cfg.enrgy    <- NULL

  } else { # Multi-node

    log.Z    <- an.mrf.info$bp.info$logZ
    edge.mat <- an.mrf.info$component.graph.info$component.edge.mat

    log.node.affinities <- an.mrf.info$component.graph.info$log.node.affinities
    log.edge.affinities <- an.mrf.info$component.graph.info$log.edge.affinities
    #print(log.node.affinities)
    #print(log.edge.affinities)

    cfg.enrgy <- config.energy(
      config    = config.vec,
      edges.mat = edge.mat,
      one.lgp   = log.node.affinities,
      two.lgp   = log.edge.affinities,
      ff        = f)
    cfg.log.prob <- cfg.enrgy - log.Z
    cfg.prob     <- exp(cfg.log.prob)

  }

  if(printQ==T) {
    print(paste0("log(Z):     ", log.Z))
    print(paste0("Energy(X):  ", cfg.enrgy))
    print(paste0("log[Pr(X)]: ", cfg.log.prob))
    print(paste0("Pr(X):      ", cfg.prob))
  }

  prob.info <- list(
    log.Z,
    cfg.enrgy,
    cfg.log.prob,
    cfg.prob
  )

  names(prob.info) <- list(
    "logZ",
    "energy",
    "log.prob",
    "prob"
  )

  return(prob.info)

}


#' Compute the probability of a an entire dust configuration acros all it's associated graphs, along with associated info
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
compute.dust.config.prob.info <- function(config.vec, connected.nodes.list, a.prep.info, an.affinities.info, printQ=F) {

  # Needed for the energy function:
  ss1 <- 1 # present state
  ss2 <- 0 # absent state
  ffn <- function(y){ as.numeric(c((y==ss1),(y==ss2))) }

  component.prob.vec     <- array(NA, length(connected.nodes.list))
  component.log.prob.vec <- array(NA, length(connected.nodes.list))
  for(i in 1:length(connected.nodes.list)) {

    if(printQ == T) {
      print(paste0("Nodes involved in component ", i, ":"))
      print(connected.nodes.list[[i]])
    }

    # Put together the MRF for the component graph
    component.graph.mrf.info  <- make.component.mrf(connected.nodes.list[[i]], a.prep.info, an.affinities.info)

    # ******* CRITICAL!!!!!!!!!!! Nodes indices that make up the component graph
    component.graph.node.idxs <- component.graph.mrf.info$harmonized.node.idxs.for.mrf

    config.chunck <- config.vec[component.graph.node.idxs]
    if(printQ == T) {
      print("Component configuration:")
      print(config.chunck)
    }

    pr.info <- compute.component.graph.dust.config.prob.info(config.chunck, component.graph.mrf.info, ffn, printQ = printQ)
    if(printQ == T) {
      print("-----------------------------------")
    }

    component.prob.vec[i]     <- pr.info$prob
    component.log.prob.vec[i] <- pr.info$log.prob

  }

  config.prob     <- prod(component.prob.vec)
  config.log.prob <- sum(component.log.prob.vec)

  config.prob.info <- list(
    component.prob.vec,
    component.log.prob.vec,
    config.prob,
    config.log.prob
  )

  names(config.prob.info) <- c(
    "component.probs",
    "component.log.probs",
    "config.prob",
    "config.log.prob"
  )

  return(config.prob.info)

}


#' Plot a graph using the info from a model prep using grapHD
#'
#' The function XXXX
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
plot.graph <- function(a.prep.info, Category.IDs.plotQ=T){

  K.harmonized           <- a.prep.info$QK.harmonized.info$K.harmonized
  K.only.harmonized.idxs <- a.prep.info$QK.harmonized.info$K.only.harmonized.idxs
  Q.only.harmonized.idxs <- a.prep.info$QK.harmonized.info$Q.only.harmonized.idxs
  QK.Category.IDs        <- a.prep.info$QK.harmonized.info$QK.Category.IDs
  edges.harmonized       <- a.prep.info$edge.mat
  num.nodes              <- ncol(K.harmonized)
  #print(edges.harmonized)

  local.grapHD <- as.gRapHD(edges.harmonized, p=num.nodes)

  v <- 1:num.nodes
  node.cols <- rep("", num.nodes)
  node.cols[K.only.harmonized.idxs] <- "green"
  node.cols[Q.only.harmonized.idxs] <- "red"

  # These should be the Konly categories that weren't there 100% of the time. Color them yellow.
  K.harmonized.counts <- colSums(K.harmonized)
  K.semis <- which( (K.harmonized.counts < nrow(K.harmonized)) & (K.harmonized.counts !=0))
  node.cols[K.semis] <- "yellow"
  #print(node.cols)

  if(Category.IDs.plotQ==T) {
    node.names <- paste0("X",QK.Category.IDs)
  } else {
    node.names <- v
  }

  if(!is.null(dev.list())){
    dev.off()
  }
  plot(local.grapHD, numIter=1000, vert.label=T, vert.radii=rep(.028,length(v)),
       vert.labels=node.names, vert.hl=v, col.hl=node.cols)

}
