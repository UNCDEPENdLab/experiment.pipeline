# Controls clusters for paralellism
#
# Contains useful functions to help control NeuroMap parallelism globally.
# Utilizes global clustering to enhance performance and reduce overhead
# created by thread spawning.

#' Initialize clusters
#'
#' Initialize global clusters for parallel processing.
#' Utilizes configuration defined by config.yaml and system configurations
#' to define number of cores to utilize.
#' All values are given to the System global config in the NeuroMap environment.
#'
#' @export
init.clusters <- function() {
  system <- NeuroMap$SYSTEM

  if((NeuroMap$CONFIG$enable_parallel)) {
    num.cores <- min(parallel::detectCores(), NeuroMap$CONFIG$cores_threshold)
  } else {
    num.cores <- 1
  }
  system <- append(system, list(cores = num.cores))

  cl <- parallel::makeCluster(num.cores, outfile = "")
  parallel::clusterEvalQ(cl, library(DataPipeline))
  system <- append(system, list(cluster = cl))

  assign("SYSTEM", system, envir = NeuroMap)
}

#' Terminates clusters
#'
#' Closes sockit to http port to stop all parallel processing using
#' the NeuroMap cluster.
#' Note that this function exists to explicitly terminate the cluster.
#'
#' @export
terminate.clusters <- function() {
  parallel::stopCluster(NeuroMap$SYSTEM$cluster)
}
