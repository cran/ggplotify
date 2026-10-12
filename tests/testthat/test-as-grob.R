test_that("as.grob dispatches base input to the base conversion path", {
    for (plot_object in list(~barplot(1:10),
                             expression(barplot(1:10)),
                             function() barplot(1:10))) {
        g <- suppressWarnings(as.grob(plot_object))

        expect_s3_class(g, "grob")
        expect_gte(length(grob_types(g)), 2)
    }
    cleanup_graphics()
})

test_that("as.grob returns a grob unchanged", {
    g <- suppressWarnings(as.grob(~barplot(1:10)))

    expect_identical(as.grob(g), g)
    cleanup_graphics()
})

test_that("as.grob converts a ggplot object to its grob table", {
    p <- ggplot2::ggplot(ggplot2::aes(x, y), data = data.frame(x = 1:3, y = 1:3)) +
        ggplot2::geom_point()

    expect_s3_class(as.grob(p), "gtable")
    cleanup_graphics()
})

test_that("as.grob methods for the core input classes are registered", {
    registered <- registered_s3_methods("ggplotify")
    for (cls in c("expression", "formula", "function", "grob", "ggplot")) {
        expect_true(paste0("as.grob.", cls) %in% registered)
    }
})

test_that("as.grob passes envir on to base2grob", {
    env <- new.env()
    env$values <- 1:10

    g <- suppressWarnings(as.grob(~barplot(values), envir = env))

    expect_s3_class(g, "grob")
    expect_true(any(grob_types(g) %in% c("rect", "polygon")))
    cleanup_graphics()
})

test_that("as.grob has no method for an unconverted object", {
    expect_error(as.grob(1:10), "no applicable method")
})
