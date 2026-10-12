close_devices <- function() {
    for (i in seq_len(50)) {
        if (is.null(grDevices::dev.list())) {
            break
        }
        try(invisible(grDevices::dev.off()), silent = TRUE)
    }
}

## converting without an active device makes R open its default one, which
## writes its plot file next to the tests
cleanup_graphics <- function() {
    close_devices()
    unlink("Rplots.pdf")
}

open_device <- function() {
    close_devices()
    grDevices::pdf(tempfile(fileext = ".pdf"))
}

record_base_plot <- function(displaylist = TRUE) {
    close_devices()
    grDevices::pdf(tempfile(fileext = ".pdf"))
    grDevices::dev.control(
        displaylist = if (displaylist) "enable" else "inhibit")
    plot(1:10, type = "b", main = "recorded")
    recording <- grDevices::recordPlot()
    invisible(grDevices::dev.off())
    recording
}

grob_types <- function(x) {
    children <- x$children
    if (inherits(x, "gTree") && length(children) > 0) {
        return(unlist(lapply(children, grob_types)))
    }

    class(x)[1]
}

## a blank cairo page compresses to a few hundred bytes; every drawing the
## converters produce is several times larger, the smallest being a plot passed
## to as.ggplot with scale = 0.5
render_size <- function(draw) {
    close_devices()
    file <- tempfile(fileext = ".png")
    grDevices::png(file, width = 300, height = 300, type = "cairo")
    draw()
    invisible(grDevices::dev.off())

    file.size(file)
}

renders_non_blank <- function(draw) {
    skip_if_not(capabilities("cairo"), "cairo device unavailable")
    expect_gt(render_size(draw), 2 * render_size(function() grid::grid.newpage()))
}

blank_par <- function() {
    graphics::par(no.readonly = TRUE)
}

## an S3 method is registered in the table of the namespace that owns it, so
## this is the only way to tell a registered method from one that is simply
## defined; methods for a foreign generic land in that generic's table
registered_s3_methods <- function(package) {
    base::ls(get(".__S3MethodsTable__.", envir = asNamespace(package)))
}
