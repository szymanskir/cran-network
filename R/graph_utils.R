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
    head(10)
  
  data.frame(
    "Package Name" = names(top_pkgs),
    "References" = top_pkgs
  )
}