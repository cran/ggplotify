## -----------------------------------------------------------------------------
#| echo: false
#| results: hide
knitr::opts_chunk$set(fig.width = 8, fig.height = 4.5, message = FALSE)


## -----------------------------------------------------------------------------
#| warning: false
library(ggplot2)
library(ggplotify)

p1 <- as.grob(~barplot(1:10, main = "formula"))
p2 <- as.grob(expression(barplot(11:20, main = "expression")))
p3 <- as.grob(function() barplot(21:30, main = "function"))


## -----------------------------------------------------------------------------
#| eval: false
# as.grob(~barplot(n), envir = environment())


## -----------------------------------------------------------------------------
#| warning: false
library(lattice)

p4 <- as.grob(densityplot(~mpg | cyl, data = mtcars))
p5 <- as.grob(ggplot(mtcars, aes(wt, mpg)) + geom_point())


## -----------------------------------------------------------------------------
#| fig.height: 6
#| warning: false
pdf(tempfile(fileext = ".pdf"))
dev.control(displaylist = "enable")
plot(1:10, type = "b", main = "recorded")
recording <- recordPlot()
invisible(dev.off())

as.ggplot(recording)


## -----------------------------------------------------------------------------
#| warning: false
pdf(tempfile(fileext = ".pdf"))
plot(1:10)
empty <- recordPlot()
invisible(dev.off())

try(as.grob(empty))


## -----------------------------------------------------------------------------
#| fig.width: 7
#| fig.height: 7
#| warning: false
library(grid)

grid.newpage()
grid.draw(p1)
vp <- viewport(x = .4, y = .72, width = .4, height = .35)
pushViewport(vp)
grid.draw(p2)
upViewport()


## -----------------------------------------------------------------------------
#| warning: false
grid.draw(~barplot(1:10))


## -----------------------------------------------------------------------------
#| warning: false
as.ggplot(~barplot(1:10)) +
    annotate("text", x = .6, y = .5, label = "Hello base plot",
             size = 6, color = "firebrick", angle = 45) +
    annotate("point", x = .2, y = .8, color = "steelblue", size = 4)


## -----------------------------------------------------------------------------
#| warning: false
as.ggplot(~barplot(1:10, main = "base plot"), scale = .95, hjust = .05, vjust = .05)


## -----------------------------------------------------------------------------
#| warning: false
as.ggplot(~plot(1:10, type = "b", main = "base plot"), angle = 90,
          scale = .6, hjust = -.15)


## -----------------------------------------------------------------------------
#| warning: false
as.ggplot(~plot(1:10, type = "b", main = "base plot"), angle = 45, scale = .45)


## -----------------------------------------------------------------------------
library(vcd)
data(Titanic)

as.ggplot(grid2grob(mosaic(Survived ~ Sex, data = as.data.frame(Titanic)))) +
    annotate("label", x = .5, y = .97, label = "converted from vcd",
             size = 4.5, fill = "white")


## -----------------------------------------------------------------------------
#| fig.width: 12
#| fig.height: 7
#| warning: false
library(aplot)

plot_list(~barplot(1:10), ggplot(mtcars, aes(wt, mpg)) + geom_point(),
          ~plot(sin), densityplot(~mpg | cyl, data = mtcars),
          ncol = 2, labels = LETTERS[1:4])


## -----------------------------------------------------------------------------
#| fig.width: 12
#| fig.height: 4
#| warning: false
plot_list(gglist = list(p1, p2, p3, p5), ncol = 4, widths = c(1, 1, 1, 1.2))


## -----------------------------------------------------------------------------
#| fig.width: 10
#| fig.height: 4.5
#| warning: false
library(colorspace)
library(patchwork)

col <- rainbow_hcl(3)
color <- col[iris$Species]

as.ggplot(~plot(iris$Sepal.Length, iris$Sepal.Width, col = color, pch = 15,
                xlab = "Sepal.Length", ylab = "Sepal.Width",
                main = "base graphics"),
          scale = .9) +
    ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
    geom_point(shape = 15) +
    scale_color_manual(values = col, name = "") +
    ggtitle("ggplot2")


## -----------------------------------------------------------------------------
#| fig.width: 8
#| warning: false
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
    geom_point() +
    annotation_custom(as.grob(recording), xmin = 6, xmax = 7.8,
                      ymin = 3.3, ymax = 4.4)

