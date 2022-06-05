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



#' Build adjacency matrix and edge matrix for use with building Q-K graph
#'
#' XXXX
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
make.model.rep <- function(a.Q.vec, a.K.mat, population.adj.mat=NULL, model.type = "model1") {

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
            print(paste0("============= YES ============== X",Qn1.ID,"-X",Qn2.ID,"!"))

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
            print(paste0("========== YES ========= X",Q.ID,"-X",QoK.ID," ", count))

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

  model.info        <- list(model.adj.mat, model.edge.mat)
  names(model.info) <- c("model.adj.mat", "model.edge.mat")

  return(model.info)

}
