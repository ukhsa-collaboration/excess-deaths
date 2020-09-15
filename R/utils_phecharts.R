theme_phe <- function(theme = "phe", base_size = 11, base_family = "", base_line_size = base_size/22, 
                       base_rect_size = base_size/22) {
  phe_key <- list(phe = list(colour_title = "#00B092", colour_strip = "#002776", 
                             colour_strip_background = "white", base_colour = "black", 
                             line_colour = "#D2D1B6", axis_line_colour = "black"), 
                  fingertips = list(colour_title = "black", colour_strip = "white", 
                                    colour_strip_background = "#02AE94", base_colour = "#11175E", 
                                    line_colour = "#666666", axis_line_colour = "#666666"))
  half_line <- base_size/2
  ggplot2::theme_grey(base_size = base_size, base_family = base_family, 
                      base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(line = element_line(colour = phe_key[[theme]]$line_colour, 
                              size = 0.5, linetype = 1, lineend = "butt"), 
          rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
          text = element_text(family = base_family, 
                              face = "plain", 
                              colour = phe_key[[theme]]$base_colour, 
                              size = base_size, lineheight = 0.9, hjust = 0.5, 
                              vjust = 0.5, angle = 0, margin = margin(), debug = FALSE), 
          axis.line = element_line(colour = phe_key[[theme]]$axis_line_colour), 
          axis.line.x = element_line(), axis.line.y = element_line(), 
          axis.text = element_text(size = rel(0.8)), 
          axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1),
          axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2), hjust = 1), 
          axis.ticks.length = unit(half_line/2, "pt"), 
          axis.ticks = element_line(colour = phe_key[[theme]]$axis_line_colour), 
          axis.title.x = element_text(margin = margin(t = 0.8 * 
                                                        half_line, b = 0.8 * half_line/2)), 
          axis.title.y = element_text(angle = 90, 
                                      margin = margin(r = 0.8 * half_line, l = 0.8 * half_line/2)), 
          legend.background = element_blank(), 
          legend.margin = margin(), legend.key = element_blank(), 
          legend.key.size = unit(1.2, "lines"), legend.key.height = NULL, 
          legend.key.width = NULL, legend.text = element_text(size = rel(0.8)), 
          legend.text.align = NULL, legend.title = element_text(hjust = 0), 
          legend.title.align = NULL, legend.position = "right", 
          legend.direction = NULL, legend.justification = "center", 
          legend.box = NULL, panel.background = element_blank(), 
          panel.border = element_blank(), panel.grid.major = element_line(), 
          panel.grid.major.y = element_line(colour = "#ECECDE"), 
          panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), 
          panel.spacing = unit(half_line, "pt"), panel.spacing.x = NULL, 
          panel.spacing.y = NULL, panel.ontop = FALSE, 
          strip.background = element_rect(fill = phe_key[[theme]]$colour_strip_background, 
                                          colour = NA), 
          strip.text = element_text(colour = phe_key[[theme]]$colour_strip,
                                    size = rel(1.1), face = "bold"), 
          strip.text.x = element_text(margin = margin(t = half_line, 
                                                      b = half_line), hjust = 0.1), 
          strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
          strip.switch.pad.grid = unit(0.1, "cm"), 
          strip.switch.pad.wrap = unit(0.1, "cm"), 
          plot.background = element_blank(), 
          plot.title = element_text(size = rel(1.9), 
                                    margin = margin(b = half_line * 1.2), 
                                    hjust = 0, 
                                    colour = phe_key[[theme]]$colour_title, 
                                    lineheight = 0.8, face = "bold"), 
          plot.subtitle = element_text(size = rel(1.2), hjust = 0), 
          plot.caption = element_text(hjust = 1),
          plot.margin = margin(half_line, half_line, half_line, 
                               half_line), complete = TRUE)
}

brewer_phe <- function (theme = "phe", n = Inf, names = FALSE) {
  colour_key <- list(phe = c("#822433", "#00B092", "#002776", 
                             "#DAD7CB", "#A4AEB5", "#E9994A", "#EAAB00", "#00A551", 
                             "#8CB8C6", "#00549F", "#532D6D", "#C51A4A"))
  brewer_names <- list(phe = c("PHEred", "teal", "navy", "mushroom", 
                               "coolgrey", "peach", "yellow", "grass", "sky", "moonlight", 
                               "plum", "rose"))
  if (!(theme %in% names(colour_key))) {
    stop("name not in available pre-loaded palettes")
  }
  brewer_phe <- colour_key[[theme]]
  if (names == TRUE) {
    brewer_names <- brewer_names[[theme]]
    names(brewer_phe) <- brewer_names
  }
  if (length(n) > 1) 
    stop("n must have length of 1")
  if (n == Inf) {
    brewer_phe <- brewer_phe
  }
  else if (n > length(brewer_phe)) {
    warning(paste("warning,", n, "colours requested but only", 
                  length(brewer_phe), "available.", length(brewer_phe), 
                  "colours returned."))
  }
  else if (n < 1) {
    stop("n must be positive")
  }
  else {
    brewer_phe <- brewer_phe[1:n]
  }
  return(brewer_phe)
}
