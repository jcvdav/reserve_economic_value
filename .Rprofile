source("renv/activate.R")

palette <- c("steelblue", "darkorange1", "cadetblue", "#E41A1C")

# Set a global theme -----------------------------------------------------------
ggplot2::theme_set(
  ggplot2::theme_bw()
)
ggplot2::theme_update(
  line = ggplot2::element_line(color = "black",
                               linewidth = 0.1),
  panel.background = ggplot2::element_rect(fill = NA,
                                           colour = "black",
                                           linewidth = 0.1),
  panel.grid.major = ggplot2::element_line(color = "black",
                                           linewidth = 0.1,
                                           linetype = "dashed"),
  panel.grid.minor = ggplot2::element_blank(),
  legend.background = ggplot2::element_blank(),
  legend.key = ggplot2::element_blank(),
  strip.background = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(hjust = 0 ),
  text = ggplot2::element_text(size = 8),
  axis.text.y = ggplot2::element_text(size = 5)
)

com_order <- c("El Rosario",
               "Isla Natividad",
               "La Bocana",
               "Puerto Libertad",
               "Isla San Pedro Mártir",
               "Isla San Pedro Nolasco",
               "Maria Elena",
               "Punta Herrero",
               "Banco Chinchorro")
