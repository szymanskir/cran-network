library(igraph)
devtools::load_all(".")
drake::expose_imports("cranNetwork")


plan <- drake::drake_plan(
  suggests_adj_list = prepare_pkg_adj_list("Suggests"),
  suggests_graph_df = create_pkg_graph_df(suggests_adj_list),
  suggests_graph = graph_from_data_frame(
    d = suggests_graph_df$edges, 
    vertices = suggests_graph_df$vertices
  ),
  
  direct_deps_adj_list = prepare_pkg_adj_list(c("Depends", "Imports")),
  direct_graph_df = create_pkg_graph_df(direct_deps_adj_list),
  depends_graph = graph_from_data_frame(
    d = direct_graph_df$edges, 
    vertices = direct_graph_df$vertices
  )
)

drake::make(plan)
