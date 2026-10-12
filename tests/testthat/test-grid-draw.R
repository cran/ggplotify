test_that("grid.draw plots base input of every supported class", {
    for (plot_object in list(~barplot(1:10),
                             expression(barplot(1:10)),
                             function() barplot(1:10))) {
        renders_non_blank(function() invisible(grid.draw(plot_object)))
    }
    cleanup_graphics()
})

test_that("grid.draw returns the value of the base plot call", {
    open_device()

    heights <- grid.draw(~barplot(1:10))

    expect_length(as.vector(heights), 10)
    cleanup_graphics()
})

test_that("grid.draw restores the caller graphics parameters", {
    open_device()
    before <- blank_par()

    invisible(grid.draw(~plot(1:10)))

    after <- graphics::par(no.readonly = TRUE)
    ## grid.draw restores every entry, including the par(new) flag that
    ## base2grob leaves flipped
    expect_identical(after, before)
    cleanup_graphics()
})

test_that("grid.draw methods are registered", {
    ## grid owns the generic, so the methods are registered in grid's table
    registered <- registered_s3_methods("grid")
    for (cls in c("expression", "formula", "function")) {
        expect_true(paste0("grid.draw.", cls) %in% registered)
    }
})
