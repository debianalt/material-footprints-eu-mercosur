# =============================================================================
# _paper_theme.R  — tema y exportación unificados para TODAS las figuras
# Sourced por 05_figures.R y 06_comtrade.R.
#
# Principio: ancho de exportación FIJO = ancho de columna de la revista
# (Ecological Economics, doble columna = 190 mm). Si todas las figuras se
# guardan al mismo ancho al que se muestran, ninguna se reescala y el mismo
# tamaño de fuente en pt se ve idéntico en todas.
# =============================================================================

suppressMessages(library(ggplot2))

# Ancho único de exportación (Elsevier doble columna)
WIDTH_IN <- 190 / 25.4            # 7.48 in

# Tamaño único para labels de datos (geom_text / geom_text_repel / annotate).
# ggplot2 mide `size` en mm; /.pt lo convierte a ~pt. ~11 pt al tamaño final.
LBL <- 11 / .pt                   # ≈ 3.87

# Tema de publicación: sans-serif, base 11 pt, generoso y legible al tamaño
# final (190 mm sin reescalado).
theme_paper <- function(base = 11) {
  theme_bw(base_size = base, base_family = "sans") %+replace%
    theme(
      axis.title        = element_text(size = base),
      axis.text         = element_text(size = base - 1.5, colour = "grey20"),
      legend.title      = element_text(size = base - 1),
      legend.text       = element_text(size = base - 1.5),
      legend.position   = "bottom",
      legend.key.size   = unit(0.9, "lines"),
      strip.background  = element_blank(),
      strip.text        = element_text(size = base - 1, face = "bold"),
      plot.title        = element_text(size = base + 1, face = "bold", hjust = 0,
                                       margin = margin(b = 4)),
      plot.subtitle     = element_text(size = base - 2, hjust = 0,
                                       margin = margin(b = 6)),
      plot.caption      = element_text(size = base - 2, hjust = 0,
                                       colour = "grey30",
                                       margin = margin(t = 6)),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(linewidth = 0.3, colour = "grey90"),
      plot.margin       = margin(8, 10, 8, 8),
      complete          = TRUE
    )
}

# Exportación uniforme: ancho fijo, alto por contenido. TIFF 300 dpi (envío)
# + PNG 300 dpi (preview Obsidian nítido), mismas dimensiones.
save_fig <- function(p, name, h = 5) {
  ggsave(file.path(FIGS, paste0(name, ".tiff")),
         plot = p, device = "tiff", dpi = 300,
         width = WIDTH_IN, height = h, units = "in", compression = "lzw")
  ggsave(file.path(FIGS, paste0(name, ".png")),
         plot = p, dpi = 300,
         width = WIDTH_IN, height = h, units = "in")
  cat("Guardada:", name, sprintf("(%.2f x %.2f in)\n", WIDTH_IN, h))
}
