library(tidyverse)
library(patchwork)
library(gt)


# create a theme

# sysfonts::font_add_google(name = "Roboto Condensed", family = "roboto-condensed")

# nonsense required for ggplot + latex to figure things out, still doesn't work;
# NOTE: it seems the font will render correctly via a browser viewer, but not in acrobat reader or acrobat, which is 
# ridiculous but at least we can save the browser pdf for correct rendering.
# probably the only way to get plots to behave for latex/pdf will likely be to save them out and use conditional 
# includegraphics; this is also partly ggplot as it will give warnings for pdf but not html
extrafont::loadfonts(quiet = TRUE)
# extrafont::font_import(pattern = "RobotoCondensed", prompt = FALSE)
# extrafont::font_import(pattern = "Arial Narrow", prompt = FALSE)
# showtext::showtext_auto()


# check for narrow fonts, if not available, use default
# plot_fonts = c('roboto condensed', 'arial narrow')

# check_fonts = which(plot_fonts %in% tolower(unique(systemfonts::system_fonts()$family)))

# plot_ff = stringr::str_to_title(ifelse(length(check_fonts) == 0, '', plot_fonts[which.min(check_fonts)]))

plot_ff = 'Roboto Condensed'
# plot_ff = 'Arial Narrow'
# plot_ff = NULL


theme_clean = function(
    font_size = 12,
    font_family = plot_ff,
    center_axis_labels = FALSE
) {
  if (center_axis_labels) {
    haxis_just_x = 0.5
    vaxis_just_y = 0.5
    v_rotation_x = 0
    v_rotation_y = 0
  } else {
    haxis_just_x = 0
    vaxis_just_y = 1
    v_rotation_x = 0
    v_rotation_y = 0
  }

  ggplot2::theme(
    text = ggplot2::element_text(
      family = font_family,
      face = 'plain',
      color = 'gray30',
      size = font_size,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      lineheight = 0.9,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    axis.title.x = ggplot2::element_text(
      hjust = haxis_just_x,
      angle = v_rotation_x,
      size  = 1.2 * font_size,
      face = 'bold'
    ),
    axis.title.y = ggplot2::element_text(
      vjust = vaxis_just_y,
      hjust = 0,
      angle = v_rotation_y,
      size  = 1.2 * font_size,
      family = font_family,
      face = 'bold',
    ),
    axis.ticks = ggplot2::element_line(color = 'gray30'),
    title = ggplot2::element_text(
      color = 'gray30', 
      size = font_size * 1.25,
      margin = margin(b = font_size * 0.3),
      family = font_family,
      face = 'bold',
    ),
    plot.subtitle = ggplot2::element_text(
      color = 'gray30', 
      size = font_size * 1, 
      hjust = 0,
      family = font_family,
      face = 'bold',      
    ),
    plot.caption = ggplot2::element_text(
      color = 'gray30', 
      size = font_size * .75, 
      hjust = 0,
      family = font_family,
      face = 'bold',      
    ),
    legend.position = 'bottom',
    legend.key = ggplot2::element_rect(fill = 'transparent', color = NA),
    legend.background = ggplot2::element_rect(fill = 'transparent', color = NA),
    legend.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = 'gray95'),
    strip.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = 'transparent', color = NA),
  )
}


# theme_other  =  theme_clean = function (
# ) {


# }

# set the theme as default
theme_set(theme_clean())

# set other point/line default colors; in most cases, we can use the color from
# default discrete scale for more consistency across plots.
# paletteer::palettes_d$colorblindr$OkabeIto
update_geom_defaults('vline', list(color = 'gray25', alpha = .25)) # vlines and hlines are typically not attention grabbers so set alpha
update_geom_defaults('hline', list(color = 'gray25', alpha = .25)) # usually a zero marker
update_geom_defaults('point', list(color = '#E69F00', alpha = .5)) # alpha as usually there are many points
update_geom_defaults('smooth', list(color = '#56B4E9', alpha = .15))
update_geom_defaults('line', list(color = '#56B4E9', alpha = .75))
update_geom_defaults('path', list(color = '#56B4E9', alpha = .75))
update_geom_defaults('abline', list(color = '#56B4E9', alpha = 1))
update_geom_defaults('bar', list(color = '#E69F00', fill = '#E69F00'))
update_geom_defaults('col', list(color = '#E69F00', fill = '#E69F00'))
update_geom_defaults('dotplot', list(color = '#E69F00', fill = '#E69F00'))

# use colorblind safe colors for categories; if you supply a continuous value to
# color you'll get an error, but you just have to use `myplot +
# scale_color_continous()` or whatever to override this; likewise you can always
# override this scale for categorical schemes if desired also. Note that this
# will apply for both color and fill, which is usually what we want.

okabe_ito = c(
  '#E69F00',
  '#56B4E9',
  '#009E73',
  '#F0E442',
  '#0072B2',
  '#D55E00',
  '#CC79A7',
  '#999999'
)

# Use the following to overwrite basic ggplot to use color scheme
# ggplot = function(...) ggplot2::ggplot(...) +
#   # okabe ito colorblind safe scheme
#   scale_color_manual(
#     values = okabe_ito,
#     drop = FALSE,
#     aesthetics = c('color', 'fill')
#   )

# Tables ------------------------------------------------------------------


gt = function(..., decimals = 2, title = NULL, subtitle = NULL) {
  gt::gt(...) %>%
    gt::fmt_number(
      columns = where(is.numeric),
      decimals = decimals
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = 'gray25'),
      locations = gt::cells_body(
        columns = gt::vars(
          where(is.numeric)
        )
      )
    ) %>%
    # gt::tab_header(title = title, subtitle = subtitle) %>%
    # gt::tab_options(
    #   table.font.size = 12
    # ) |> 
    gtExtras::gt_theme_nytimes() |>
    tab_options(quarto.disable_processing = TRUE) # May have unintended consequences see https: //github.com/quarto-dev/quarto-cli/issues/6945
}




gt_theme =
  list(
    # report median (IQR) and n (percent) as default stats in `tbl_summary()`
    'tbl_summary-str:continuous_stat' = '{mean} ({sd})',
    'tbl_summary-str:categorical_stat' = '{n} ({p})'
  )


gtsummary::set_gtsummary_theme(gt_theme)

tbl_summary = function(..., title = '', butcher = TRUE) {
  tbl_out = gtsummary::tbl_summary(
    ...,
    digits = list(
      all_continuous() ~ c(1, 1),
      all_categorical() ~ c(0, 1)
    )
  ) %>%
    gtsummary::modify_caption(caption = title)
  #
  # # trim dataset etc from table; may lose other functionality
  if (butcher) {
    tbl_out = tbl_out %>%
      gtsummary::tbl_butcher()
  }
  #
  tbl_out
}






options(digits = 4) # number of digits of precision for floating point output