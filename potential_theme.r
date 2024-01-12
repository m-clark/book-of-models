function (font = Sys.getenv("LARES_FONT"), size = 12, main_colour = "darkorange3", 
    hard_colour = "black", soft_colour = "grey30", plot_colour = "transparent", 
    panel_colour = "transparent", background = "transparent", 
    no_facets = FALSE, legend = NULL, grid = TRUE, axis = TRUE, 
    clean = FALSE, mg = 9, pal = 0, palette = NULL, which = "fc", 
    ...) 
{
    ret <- theme_minimal(base_size = size)
    if (clean) 
        axis <- grid <- FALSE
    if (background != "transparent") 
        panel_colour <- plot_colour <- background
    if (isFALSE(legend)) 
        legend <- "none"
    font <- .font_global(font, quiet = FALSE)
    ret <- ret + theme(text = element_text(family = font))
    update_geom_defaults("text", list(colour = hard_colour, family = font))
    update_geom_defaults("label", list(colour = hard_colour, 
        family = font))
    update_geom_defaults("point", list(colour = hard_colour, 
        alpha = 0.95))
    update_geom_defaults("line", list(colour = hard_colour, alpha = 0.95))
    update_geom_defaults("area", list(fill = main_colour, alpha = 0.95))
    update_geom_defaults("rect", list(fill = main_colour, alpha = 0.95))
    update_geom_defaults("density", list(fill = main_colour, 
        alpha = 0.95))
    update_geom_defaults("bar", list(fill = main_colour, alpha = 0.95))
    update_geom_defaults("col", list(fill = main_colour, alpha = 0.95))
    update_geom_defaults("boxplot", list(fill = main_colour, 
        alpha = 0.9))
    aux <- ifelse("top" %in% legend, "right", "left")
    xj <- switch(tolower(substr(aux, 1, 1)), b = 0, l = 0, m = 0.5, 
        c = 0.5, r = 1, t = 1)
    yj <- switch(tolower(substr(aux, 2, 2)), b = 0, l = 0, m = 0.5, 
        c = 0.5, r = 1, t = 1)
    if (!is.null(legend)) {
        ret <- ret + theme(legend.title = element_text(color = soft_colour, 
            size = size * 0.9, face = "bold"), legend.position = legend, 
            legend.justification = c(ifelse(legend %in% c("top", 
                "bottom"), 0, 0.5), ifelse(legend == "top", 0, 
                ifelse(legend %in% "left", 1, 0.5))), legend.margin = margin(-3, 
                0, -4, 0))
    }
    if (inherits(grid, "character") || grid) {
        grid_col <- "#CCCCCC"
        ret <- ret + theme(panel.grid = element_line(color = grid_col, 
            size = 0.2))
        ret <- ret + theme(panel.grid.major = element_line(color = grid_col, 
            size = 0.1))
        ret <- ret + theme(panel.grid.minor = element_line(color = grid_col, 
            size = 0.05))
        if (inherits(grid, "character")) {
            if (regexpr("X", grid)[1] < 0) 
                ret <- ret + theme(panel.grid.major.x = element_blank())
            if (regexpr("Y", grid)[1] < 0) 
                ret <- ret + theme(panel.grid.major.y = element_blank())
            if (regexpr("x", grid)[1] < 0) 
                ret <- ret + theme(panel.grid.minor.x = element_blank())
            if (regexpr("y", grid)[1] < 0) 
                ret <- ret + theme(panel.grid.minor.y = element_blank())
        }
    }
    else {
        ret <- ret + theme(panel.grid = element_blank())
    }
    ret <- ret + theme(axis.line = element_blank())
    ret <- ret + theme(axis.ticks = element_blank(), axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank())
    ret <- ret + theme(axis.text.x = element_text(size = size * 
        0.8, margin = margin(t = 0), colour = soft_colour))
    ret <- ret + theme(axis.text.y = element_text(size = size * 
        0.8, margin = margin(r = 0), colour = soft_colour))
    ret <- ret + theme(axis.title = element_text(size = size * 
        0.85, family = font, colour = soft_colour))
    ret <- ret + theme(axis.title.x = element_text(hjust = xj, 
        size = size * 0.85, family = font, face = "bold", colour = soft_colour))
    ret <- ret + theme(axis.title.y = element_text(hjust = yj, 
        size = size * 0.85, colour = soft_colour, family = font, 
        face = "bold"))
    ret <- ret + theme(axis.title.y.right = element_text(hjust = yj, 
        size = size * 0.85, angle = -90, colour = soft_colour, 
        family = font, face = "bold"))
    if (axis == FALSE) {
        ret <- ret + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
            axis.title.x = element_blank(), axis.title.y = element_blank())
    }
    if (is.character(axis)) {
        if (tolower(axis) == "x") {
            ret <- ret + theme(axis.text.y = element_blank(), 
                axis.title.y = element_blank())
        }
        if (tolower(axis) == "y") {
            ret <- ret + theme(axis.text.x = element_blank(), 
                axis.title.x = element_blank())
        }
    }
    ret <- ret + theme(strip.text = element_text(hjust = 0, size = size * 
        0.9, colour = soft_colour, face = "bold", family = font))
    ret <- ret + theme(panel.spacing = grid::unit(0.8, "lines"))
    if (no_facets) {
        ret <- ret + theme(strip.background = element_blank(), 
            strip.text = element_blank())
    }
    ret <- ret + theme(plot.title = element_text(size = size * 
        1.25, margin = margin(b = size * 0.3), family = font, 
        face = "bold", color = "black"))
    ggc <- stringr::str_split(as.character(packageVersion("ggplot2")), 
        "\\.")[[1]]
    if (ggc[1] >= 3 && ggc[2] >= 3) {
        ret <- ret + theme(plot.title.position = "plot")
    }
    ret <- ret + theme(plot.subtitle = element_text(hjust = 0, 
        size = size * 1.1, colour = soft_colour, margin = margin(b = size * 
            0.5), family = font, face = "italic"))
    ret <- ret + theme(plot.caption = element_text(hjust = 1, 
        size = size * 0.85, margin = margin(t = size * 0.9), 
        family = font, color = soft_colour))
    ret <- ret + theme(panel.background = element_rect(fill = panel_colour, 
        colour = NA), plot.background = element_rect(fill = plot_colour, 
        colour = NA))
    ret <- ret + theme(plot.margin = margin(mg, mg, mg, mg))
    if (!is.null(palette)) {
        if (is.null(names(palette))) {
            names(palette) <- as.vector(palette)
        }
        colours_pal <- palette
    }
    else {
        colours_pal <- lares_pal()$palette
    }
    if (pal == 1) {
        ret <- list(ret, scale_fill_manual(values = names(colours_pal)))
        ret <- list(ret, scale_colour_manual(values = as.vector(colours_pal)))
    }
    if (pal == 2) {
        ret <- list(ret, scale_colour_manual(values = names(colours_pal)))
    }
    if (pal == 3) {
        ret <- list(ret, scale_fill_manual(values = names(colours_pal)))
    }
    if (pal == 4) {
        which <- tolower(which)
        if ((grepl("c", which) && grepl("t", which))) {
            stop("In your 'which' parameter, pass only 'c' OR 't', not both")
        }
        suppressMessages({
            ret <- list(ret)
            if (grepl("f", which)) 
                ret <- append(ret, gg_fill_customs())
            if (grepl("c", which)) 
                ret <- append(ret, gg_colour_customs())
            if (grepl("t", which)) 
                ret <- append(ret, gg_text_customs())
        })
    }
    return(ret)
}