test_that("datasets strings change as selection updates", {
  selected <- reactiveValues(geography = vector())
  testServer(selectGeographyServer, args = list(selected = selected), {
    selected$geography <- "ltla_shp_england"
    session$flushReact()
    expect_equal(selected$geography, "ltla_shp_england")
  })
})