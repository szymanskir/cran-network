test_that("Archiving simulation is working correctly", {
  pkg_list <- list(
    pkgA1 = c("pkgB1"),
    pkgA2 = c("pkgB1"),
    pkgB1 = c("pkgC"),
    pkgB2 = c("pkgC"),
    pkgC = character(0),
    pkgD = character(0)
  )
  
  deps_graph_df <- create_pkg_graph_df(pkg_list)
  deps_graph <- igraph::graph_from_data_frame(
    d = deps_graph_df$edges, 
    directed = TRUE, 
    vertices = deps_graph_df$vertices
  )
  
  expect_equal(5, run_archiving_attack(deps_graph, "pkgA1"))
  expect_equal(3, run_archiving_attack(deps_graph, "pkgB1"))
  expect_equal(1, run_archiving_attack(deps_graph, "pkgC"))
})
