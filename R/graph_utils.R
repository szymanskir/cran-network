summarise_graph <- function(graph, rowname) {
  data.frame(
    "Name" = rowname,
    "Vertex Count" = igraph::vcount(graph),
    "Edge Count" = igraph::ecount(graph),
    "Density" = igraph::edge_density(graph),
    "Transitivity" = igraph::transitivity(graph),
    "Mean vertex degree" = mean(igraph::degree(graph)),
    "Mean Distance" = igraph::mean_distance(graph),
    "Is DAG" = igraph::is_dag(graph),
    "Is connected" = igraph::is_connected(graph),
    "Alpha power law" = igraph::fit_power_law(igraph::degree(graph))$alpha
  )
}

degree_hist <- function(graph, mode, title = NULL) {
  vertex_degrees <- igraph::degree(graph, mode = mode)
  ggplot2::qplot(vertex_degrees) +
    scale_x_log10() +
    scale_y_log10() +
    labs(x = "Vertex Degree", title = title)
}


top_n_pkgs <- function(graph, n = 10) {
  vertex_degrees <- igraph::degree(graph, mode = "in")
  top_pkgs <- sort(vertex_degrees, decreasing = TRUE) %>% 
    utils::head(10)
  
  top_pkgs_names <- names(top_pkgs)
  names(top_pkgs) <- NULL
  
  data.frame(
    "Package Name" = top_pkgs_names,
    "References" = top_pkgs
  )
}

run_archiving_attack <- function(pkg_graph, pkg_name) {
  all_deps <- list()
  depth_level <- 1
  new_deps <- igraph::neighbors(pkg_graph, pkg_name, mode = "in")
  all_deps[[depth_level]] <- new_deps
  
  depth_level <- 2
  while (length(new_deps) != 0) {
    new_deps_list<- lapply(
        new_deps, 
        function(pkg_name, pkg_graph) igraph::neighbors(pkg_graph, pkg_name, mode = "in"),
        pkg_graph = pkg_graph
      )
    
    names(new_deps_list) <- NULL
    new_deps<- do.call(igraph::union, new_deps_list)
    all_deps[[depth_level]] <- new_deps
    depth_level <- depth_level + 1
  }
  
  all_deps_set <- do.call(igraph::union, all_deps)
  igraph::vcount(pkg_graph) - (length(all_deps_set) + 1)
}


archiving_attacks_simulation <- function(pkg_graph, pkg_names, parallelize = FALSE, reg_dir = NA) {
  run_single_attack <- function(pkg_graph, pkg_name) {
    logging::loginfo(sprintf("Running simulation for %s", pkg_name))
    pkg_count <- NA
    tryCatch({
      pkg_count <- run_archiving_attack(pkg_graph, pkg_name)
    }, error = function(c) {
      logging::logerror(sprintf("Simulation run failed for %s", pkg_name))
    })
    
    data.table::data.table(
      pkg_name = pkg_name,
      pkg_count = pkg_count
    )
  }
  
  if (parallelize) {
    batchtools::makeClusterFunctionsMulticore()
    result_list <- batchtools::btlapply(
      pkg_names, 
      run_single_attack, 
      pkg_graph = pkg_graph, 
      reg = batchtools::makeRegistry(file.dir = reg_dir)
    )
  } else {
    result_list <- lapply(pkg_names, run_single_attack, pkg_graph = pkg_graph)
  }
  
  data.table::rbindlist(
    result_list
  )  
}
