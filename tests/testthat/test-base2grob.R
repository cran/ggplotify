test_that("base2grob converts formula, expression and function input", {
    for (plot_object in list(~barplot(1:10),
                             expression(barplot(1:10)),
                             function() barplot(1:10))) {
        g <- suppressWarnings(base2grob(plot_object))

        expect_s3_class(g, "grob")
        expect_gte(length(grob_types(g)), 2)
    }
    cleanup_graphics()
})

test_that("base2grob renders a non-blank grob", {
    g <- suppressWarnings(base2grob(~plot(1:10, type = "b")))

    renders_non_blank(function() grid::grid.draw(g))
    cleanup_graphics()
})

test_that("base2grob looks up variables in the supplied environment", {
    env <- new.env()
    env$values <- 1:10

    from_env <- suppressWarnings(base2grob(~barplot(values), envir = env))
    expect_s3_class(from_env, "grob")
    expect_true(any(grob_types(from_env) %in% c("rect", "polygon")))
    cleanup_graphics()
})

test_that("base2grob rejects input it cannot replay", {
    expect_error(base2grob(NULL), 'Argument needs to be of class "expression"')
    expect_error(base2grob(1:10), "Argument needs to be of class")
    cleanup_graphics()
})

test_that("base2grob restores the caller graphics parameters", {
    open_device()
    before <- blank_par()

    invisible(suppressWarnings(base2grob(~plot(1:10))))

    after <- graphics::par(no.readonly = TRUE)
    ## base2grob returns every parameter but par(new), which it leaves flipped
    mutable <- setdiff(names(after), "new")
    expect_identical(after[mutable], before[mutable])
    cleanup_graphics()
})
