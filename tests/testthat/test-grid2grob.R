test_that("grid2grob captures a grid drawing", {
    g <- grid2grob(grid::grid.rect(gp = grid::gpar(fill = "grey90")))

    expect_s3_class(g, "grob")
    expect_true("rect" %in% grob_types(g))
    cleanup_graphics()
})

test_that("grid2grob renders a non-blank grob", {
    g <- grid2grob(grid::grid.circle(gp = grid::gpar(fill = "black")))

    renders_non_blank(function() grid::grid.draw(g))
    cleanup_graphics()
})

test_that("grid2grob of an empty page returns nothing to convert", {
    ## this NULL is what as.grob.recordedplot reports as a recording that holds
    ## no graphics, as opposed to a blank page that yields an empty gTree
    expect_null(grid2grob(grid::grid.newpage()))
    cleanup_graphics()
})
