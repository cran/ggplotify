test_that("as.ggplot wraps a base plot in a ggplot object", {
    p <- suppressWarnings(as.ggplot(~barplot(1:10)))

    expect_true(inherits(p, "ggplot"))
    expect_s3_class(suppressMessages(ggplot2::ggplotGrob(p)), "gtable")

    renders_non_blank(function() print(p))
    cleanup_graphics()
})

test_that("as.ggplot accepts function and expression input", {
    for (plot_object in list(function() barplot(1:10), expression(barplot(1:10)))) {
        p <- suppressWarnings(as.ggplot(plot_object))
        expect_true(inherits(p, "ggplot"))
    }
    cleanup_graphics()
})

test_that("as.ggplot wraps an existing grob", {
    g <- suppressWarnings(as.grob(~barplot(1:10)))
    p <- suppressWarnings(as.ggplot(g))

    expect_true(inherits(p, "ggplot"))
    expect_s3_class(suppressMessages(ggplot2::ggplotGrob(p)), "gtable")
    cleanup_graphics()
})

test_that("as.ggplot applies scale and adjustment", {
    p <- suppressWarnings(as.ggplot(~barplot(1:10), scale = 0.5, hjust = 0.1, vjust = 0.1))

    expect_true(inherits(p, "ggplot"))
    renders_non_blank(function() print(p))
    cleanup_graphics()
})

test_that("as.ggplot rotates through the grid capture path", {
    open_device()
    p <- suppressWarnings(as.ggplot(~barplot(1:10), angle = 90))

    expect_true(inherits(p, "ggplot"))
    expect_s3_class(suppressMessages(ggplot2::ggplotGrob(p)), "gtable")
    cleanup_graphics()
})
