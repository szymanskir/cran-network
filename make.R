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
  ),
  
  degree_hist_plot = target(
    degree_hist(graph, mode = "in", title = title),
    transform = map(
      graph = list(suggests_graph, depends_graph),
      title = c("Suggests graph", "Depends/Imports graph"),
      .id = graph
    ),
  ),
  summary = target(
    summarise_graph(graph, rowname),
    transform = map(
      graph = list(suggests_graph, depends_graph),
      rowname = c("Suggests", "Deps")
    )
  ),
  graphs_summary = target(
    data.table::rbindlist(list(summary)),
    transform = combine(summary)
  ),
  
  top = target(
    top_n_pkgs(graph),
    transform = map(graph = list(suggests_graph, depends_graph))
  ),
  initial_analysis_report = rmarkdown::render(knitr_in("notebooks/01-initial_analysis.Rmd"))
)

drake::make(plan)
