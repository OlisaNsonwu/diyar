library("ggplot2")
library("cowplot")
library("diyar")

exact <- c(number_line(1, 2), number_line(1, 2))
reverse <- c(number_line(4, 3), number_line(3, 4))
inbetween <- c(number_line(5, 8), number_line(6, 7))
across <- c(number_line(9, 11), number_line(10, 12))
chain <- c(number_line(13, 15), number_line(15, 17))
aligns_start <- c(number_line(18, 20), number_line(18, 19))
aligns_end <- c(number_line(18, 20), number_line(19, 20))

nl2 <- c(exact, reverse, inbetween, across, chain, aligns_start, aligns_end)
strata <- sort(rep(seq_len(length(nl2)/2), 2))
nl <- episodes(nl2, strata = strata, case_length = index_window(nl2))
a <- episodes(seq(Sys.Date(), Sys.Date() + 99, by = "1 day"), 25)
b <- episodes(seq(Sys.Date(), Sys.Date() + 99, by = "1 day"), 25,
              episode_type = "rolling")
c <- partitions(seq(Sys.Date(), Sys.Date() + 99, by = "1 day"),
                custom_sort = sample(1:10000,100), length.out = 4,
                separate = T)
cr1 <- sample(c(1:5, rep(NA, 25)), 50, replace = T)
cr2 <- sample(c(1:5, rep(NA, 15)), 50, replace = T)
cr3 <- sample(c(1:5, rep(NA, 10)), 50, replace = T)
cr4 <- sample(c(1:5, rep(NA, 10)), 50, replace = T)
d <- links(criteria = list(cr1, cr2, cr3, cr4))
r <- episodes(seq(Sys.Date(), Sys.Date() + 99, by = "1 day"), 25,episode_type = "recursive")

theme <- "dark"
bd_line <- "white"
plt1 <- schema(nl, seed = 2, theme = theme, show_labels = c("case_overlap_methods"))
plt2 <- schema(a, seed = 2, theme = theme)
plt3 <- schema(b, seed = 2, theme = theme)
plt4 <- schema(r, seed = 2, theme = theme)
plt5 <- schema(c, seed = 2, theme = theme, show_labels = FALSE)
plt6 <- schema(d, seed = 2, theme = theme, show_labels = FALSE)

f <- plot_grid(plt1 + theme(plot.background = element_rect(color = bd_line)),
               plt2 + theme(plot.background = element_rect(color = bd_line)),
               plt3 + theme(plot.background = element_rect(color = bd_line)),
               plt4 + theme(plot.background = element_rect(color = bd_line)),
               plt5 + theme(plot.background = element_rect(color = bd_line)),
               plt6 + theme(plot.background = element_rect(color = bd_line)),
               labels = c("Number lines", "Fixed episodes",
                          "Rolling episodes", "Recursive episodes",
                          "Panes", "Record-groups"),
               label_colour = bd_line,
               label_size = 12)

ggsave(plot = f, filename = "fig_r1_dark.png", width = 15, height = 9, units = "in")
ggsave(plot = f, filename = "docs/fig_r1_dark.png", width = 15, height = 9, units = "in")
