test_that("as.grob converts a recordedplot to a non-empty grob", {
    recording <- record_base_plot()
    g <- as.grob(recording)

    expect_s3_class(g, "grob")

    types <- grob_types(g)
    expect_gte(length(types), 5)
    expect_true(any(types %in% c("points", "lines", "segments")))
    expect_true(any(types == "text"))

    renders_non_blank(function() grid::grid.draw(g))
    cleanup_graphics()
})

test_that("a conversion leaves the recording replayable", {
    recording <- record_base_plot()
    first <- as.grob(recording)
    second <- as.grob(recording)

    expect_identical(sort(grob_types(first)), sort(grob_types(second)))
    renders_non_blank(function() grDevices::replayPlot(recording))
    cleanup_graphics()
})

test_that("as.grob on a recording equals the documented function wrapper", {
    recording <- record_base_plot()
    direct <- as.grob(recording)
    wrapper <- as.grob(function() grDevices::replayPlot(recording))

    expect_identical(sort(grob_types(direct)), sort(grob_types(wrapper)))

    devices_left_by <- function(convert) {
        open_device()
        invisible(suppressWarnings(convert()))
        length(grDevices::dev.list())
    }
    expect_identical(devices_left_by(function() as.grob(recording)),
                     devices_left_by(function() as.grob(function() grDevices::replayPlot(recording))))
    cleanup_graphics()
})

test_that("an empty recording fails with an informative error", {
    ## recordPlot() on a device whose display list is inhibited holds no
    ## graphics; replayPlot() only warns about it, so as.grob() has to say so.
    recording <- record_base_plot(displaylist = FALSE)
    open_device()

    expect_error(suppressWarnings(as.grob(recording)), "No graphics were captured")
    cleanup_graphics()
})

test_that("a recording of a blank device converts to a grob without content", {
    close_devices()
    grDevices::pdf(tempfile(fileext = ".pdf"))
    grDevices::dev.control(displaylist = "enable")
    plot.new()
    recording <- grDevices::recordPlot()
    invisible(grDevices::dev.off())

    g <- suppressWarnings(as.grob(recording))
    expect_s3_class(g, "grob")
    expect_length(g$children, 0)
    cleanup_graphics()
})

test_that("conversion restores the caller graphics parameters", {
    recording <- record_base_plot()
    open_device()
    before <- blank_par()

    invisible(suppressWarnings(as.grob(recording)))

    after <- graphics::par(no.readonly = TRUE)
    ## the conversion returns every parameter but par(new), which it leaves flipped
    mutable <- setdiff(names(after), "new")
    expect_identical(after[mutable], before[mutable])
    cleanup_graphics()
})

test_that("the recordedplot method is registered", {
    expect_true("as.grob.recordedplot" %in% registered_s3_methods("ggplotify"))
})

test_that("a recording can be used as a ggimage subview", {
    skip_if_not_installed("ggimage")

    recording <- record_base_plot()
    ## data.frame(subview = list(grob)) tries to coerce the grob to a data frame
    subview_data <- data.frame(x = 1, y = 1)
    subview_data$subview <- list(as.grob(recording))

    p <- ggplot2::ggplot(ggplot2::aes(x, y), data = data.frame(x = c(0, 2), y = c(0, 2))) +
        ggplot2::geom_blank() +
        ggimage::geom_subview(ggplot2::aes(x = x, y = y, subview = subview), data = subview_data) +
        ggplot2::xlim(0, 2) +
        ggplot2::ylim(0, 2)

    expect_s3_class(suppressMessages(ggplot2::ggplotGrob(p)), "gtable")
    cleanup_graphics()
})
