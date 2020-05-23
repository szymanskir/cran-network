summarise_graph <- function(graph) {
  data.frame(
    "Vertex Count" = igraph::vcount(graph),
    "Edge Count" = igraph::ecount(graph),
    "Density" = igraph::edge_density(graph),
    "Transitivity" = igraph::transitivity(graph),
    "Mean vertex degree" = mean(igraph::degree(graph)),
    "Mean Distance" = igraph::mean_distance(graph)
  )
}

degree_hist <- function(graph) {
  vertex_degrees <- igraph::degree(graph)
  qplot(vertex_degrees)
}
