test_that("Graph df is created properly", {
  pkg_list <- list(
    pkgA = c("pkgB"),
    pkgB = c("pkgC"),
    pkgC = character(0),
    pkgD = character(0)
  )
  
  expected_result <- list(
    edges = data.table::data.table(
      from = c("pkgA", "pkgB"),
      to = c("pkgB", "pkgC")
    ),
    vertices = data.table::data.table(
      name = c("pkgA", "pkgB", "pkgC", "pkgD")
    )
  )
  
  result <- create_pkg_graph_df(pkg_list)
  expect_true(all.equal(expected_result, result))
})


test_that("Graph df does not include packages outside of CRAN", {
  pkg_list <- list(
    pkgA = c("pkgB"),
    pkgB = c("pkgC"),
    pkgC = c("outOfCranPkg"),
    pkgD = character(0)
  )
  
  expected_result <- list(
    edges = data.table::data.table(
      from = c("pkgA", "pkgB"),
      to = c("pkgB", "pkgC")
    ),
    vertices = data.table::data.table(
      name = c("pkgA", "pkgB", "pkgC", "pkgD")
    )
  )
  
  result <- create_pkg_graph_df(pkg_list)
  expect_true(all.equal(expected_result, result))
})