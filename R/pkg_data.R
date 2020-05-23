get_available_packages <- function() {
 available_pkgs <- available.packages()
 pkg_names <- available_pkgs[, "Package"]
 names(pkg_names) <- NULL
 pkg_names
}

prepare_pkg_adj_list <- function(relation_types) {
  stopifnot(all(relation_types %in% c("Depends", "Imports", "Suggests")))
  pkg_names <- get_available_packages()
  tools::package_dependencies(packages = pkg_names, which = relation_types)
}

create_pkg_graph_df <- function(pkg_adj_list) {
  create_single_pkg_graph_df <- function(pkg_name) {
    pkg_deps <- pkg_adj_list[[pkg_name]]
    
    if (length(pkg_deps) == 0) {
      data.table::data.table(
        from = NULL,
        to = NULL
      )
    } else {
      data.table::data.table(
        from = pkg_name,
        to = pkg_adj_list[[pkg_name]]
      )
    }
  }
  
  relation_df <- data.table::rbindlist(lapply(names(pkg_adj_list), create_single_pkg_graph_df))
  vertex_df <- data.table::data.table(name = names(pkg_adj_list))
  
  relation_df <- relation_df[from %in% vertex_df$name & to %in% vertex_df$name]
  
  list(
    edges = relation_df,
    vertices = vertex_df
  )
}
