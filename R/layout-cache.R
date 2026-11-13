# Layout cache: reuse the frame-invariant parts of a rendered frame.
#
# ggplot_gtable() spends most of its time on axes, strips, legends and theme
# lookups that are identical in every frame when the view is static. The first
# frame a process draws is rendered in full and kept as a template. For later
# frames only the layers are redrawn: the panels are reassembled with
# ggplot2's own Facet$draw_panel_content() and spliced into the template.
# The plot title, subtitle and caption are re-rendered when their (glue) text
# changes; a change to any other label means a full render.
#
# Opt in with options(gganimate.cache_layout = TRUE).

cacheable_facets <- c('FacetNull', 'FacetWrap', 'FacetGrid')
cacheable_coords <- c('CoordCartesian', 'CoordFixed', 'CoordFlip')
cacheable_labels <- c('title', 'subtitle', 'caption')

# An empty cache, or NULL if caching is off or the plot doesn't qualify. Each
# parallel worker gets its own copy and fills it from the first frame it
# draws, on the output device, whose font metrics size text and legends.
layout_cache <- function(plot) {
  if (!isTRUE(getOption('gganimate.cache_layout', FALSE))) return(NULL)
  if (!inherits(plot$scene$view, 'ViewStatic')) return(NULL)
  if (!class(plot$layout$facet)[1] %in% cacheable_facets) return(NULL)
  if (!class(plot$layout$coord)[1] %in% cacheable_coords) return(NULL)
  cache <- new.env(parent = emptyenv())
  cache$table <- NULL
  cache$failed <- FALSE
  cache
}

# Keep `table`, the full render of `frame`, as the template if there isn't one
cache_template <- function(cache, frame, table) {
  if (!is.null(cache$table) || cache$failed) return(invisible(cache))
  slot <- locate_panels(table, nrow(frame$layout$layout))
  if (is.null(slot)) {
    cache$failed <- TRUE
    cli::cli_warn(c(
      'Layout caching disabled: the panels could not be located in the rendered plot.',
      i = 'Every frame will be fully rendered instead. This may mean the installed {.pkg ggplot2} ({utils::packageVersion("ggplot2")}) assembles plots differently from what {.pkg gganimate} expects.'
    ))
    return(invisible(cache))
  }
  cache$table <- table
  cache$slot <- slot
  cache$labels <- frame$plot$labels
  invisible(cache)
}

# Index of each panel's grob in `table$grobs`, or NULL if any is missing.
# draw_panel_content() names panel i "panel-i" (ggname keeps a ".gTree.<n>"
# suffix). Cell names in the gtable differ between facets, grob names do not.
locate_panels <- function(table, n_panels) {
  grob_names <- vapply(table$grobs, function(g) g$name %||% '', character(1))
  grob_names <- sub('\\..*$', '', grob_names)
  slot <- match(paste0('panel-', seq_len(n_panels)), grob_names)
  if (anyNA(slot) || anyDuplicated(slot)) return(NULL)
  slot
}

# Returns a gtable, or NULL if this frame cannot use the cache
render_frame_cached <- function(frame, cache) {
  if (is.null(cache$table)) return(NULL)
  labels <- frame$plot$labels
  label_names <- union(names(labels), names(cache$labels))
  changed <- label_names[!vapply(
    label_names,
    function(n) identical(labels[[n]], cache$labels[[n]]),
    logical(1)
  )]
  if (!all(changed %in% cacheable_labels)) return(NULL)

  layout <- frame$layout
  theme <- frame$plot$theme
  data <- frame$data

  # Mirrors the first steps of ggplot_gtable() and Layout$render()
  geom_grobs <- Map(function(l, d) l$draw_geom(d, layout), frame$plot$layers, data)
  panels <- layout$facet$draw_panel_content(
    geom_grobs, layout$layout, layout$panel_scales_x, layout$panel_scales_y,
    layout$panel_params, layout$coord, data, theme, layout$facet_params
  )
  if (length(panels) != length(cache$slot)) return(NULL)

  table <- cache$table
  table$grobs[cache$slot] <- panels

  for (n in changed) {
    k <- which(table$layout$name == n)
    if (length(k) != 1) return(NULL)
    grob <- ggplot2::element_grob(
      ggplot2::calc_element(paste0('plot.', n), theme), labels[[n]],
      margin_y = TRUE, margin_x = TRUE
    )
    table$grobs[[k]] <- grob
    table$heights[table$layout$t[k]] <- grid::grobHeight(grob)
  }
  table
}
