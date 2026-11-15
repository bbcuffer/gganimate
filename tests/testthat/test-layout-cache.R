# A plot that exercises facets, a legend, axis titles and a per-frame title
cache_test_plot <- function() {
  ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
    geom_point(size = 3) +
    facet_wrap(~am) +
    transition_states(gear) +
    labs(title = 'Gear: {closest_state}', caption = 'fixed caption')
}

# Render every frame and return the files' md5 sums, plus how many frames
# went through the cache
render_frames <- function(plot, device, cache, nframes = 5) {
  withr::local_options(gganimate.cache_layout = cache)
  used <- 0L
  render_frame_cached_orig <- render_frame_cached
  local_mocked_bindings(render_frame_cached = function(frame, cache) {
    out <- render_frame_cached_orig(frame, cache)
    if (!is.null(out)) used <<- used + 1L
    out
  })
  built <- prerender(plot, nframes)
  frame_vars <- draw_frames(built, seq_len(get_nframes(built)), device = device,
                            ref_frame = 1, width = 200, height = 200)
  withr::defer(unlink(frame_vars$frame_source))
  list(md5 = unname(tools::md5sum(frame_vars$frame_source)), used = used)
}

expect_cache_matches_full_render <- function(device, plot = cache_test_plot()) {
  full <- render_frames(plot, device, cache = FALSE)
  cached <- render_frames(plot, device, cache = TRUE)
  expect_equal(full$used, 0L)
  # The first frame is rendered in full, as the template
  expect_equal(cached$used, length(cached$md5) - 1L)
  expect_identical(cached$md5, full$md5)
}

test_that('cached rendering is identical to full rendering with the png device', {
  expect_cache_matches_full_render('png')
})

test_that('cached rendering is identical to full rendering with the ragg_png device', {
  skip_if_not_installed('ragg')
  expect_cache_matches_full_render('ragg_png')
})

test_that('cached rendering is identical to full rendering with the svglite device', {
  skip_if_not_installed('svglite')
  expect_cache_matches_full_render('svglite')
})

test_that('cached rendering handles facet_null, facet_grid and coord_flip', {
  base <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + transition_states(gear)
  expect_cache_matches_full_render('png', base)
  expect_cache_matches_full_render('png', base + facet_grid(vs ~ am) + coord_flip())
})

test_that('the cache is off by default', {
  withr::local_options(gganimate.cache_layout = NULL)
  expect_null(layout_cache(prerender(cache_test_plot(), 3)))
})

test_that('unsupported views and coords are not cached', {
  withr::local_options(gganimate.cache_layout = TRUE)
  for (plot in list(cache_test_plot() + view_follow(), cache_test_plot() + coord_polar())) {
    expect_null(layout_cache(prerender(plot, 3)))
  }
})

test_that('a mismatched panel layout warns and falls back to full rendering', {
  withr::local_options(gganimate.cache_layout = TRUE)
  # Simulate a ggplot2 that names its panel grobs differently
  render_frame_orig <- render_frame
  local_mocked_bindings(render_frame = function(frame) {
    table <- render_frame_orig(frame)
    table$grobs <- lapply(table$grobs, function(g) {
      if (!is.null(g$name)) g$name <- sub('^panel-', 'pnl-', g$name)
      g
    })
    table
  })
  built <- prerender(cache_test_plot(), 3)
  capture.output(warnings <- capture_warnings(
    frame_vars <- draw_frames(built, seq_len(get_nframes(built)), device = 'png',
                              ref_frame = 1, width = 100, height = 100)
  ))
  expect_length(warnings, 1)
  expect_match(warnings, 'Layout caching disabled')
  expect_true(all(file.exists(frame_vars$frame_source)))
})

test_that('a font unknown to the pdf device gives no warnings with the cache on', {
  # An earlier version checked the cache on pdf(NULL), which warns for every
  # system font missing from its PostScript font database
  skip_on_os(c('windows', 'linux', 'solaris')) # Arial is a system font on macOS
  withr::local_options(gganimate.cache_layout = TRUE)
  built <- prerender(cache_test_plot() + theme(text = element_text(family = 'Arial')), 3)
  capture.output(expect_no_warning(
    draw_frames(built, seq_len(get_nframes(built)), device = 'png', ref_frame = 1,
                width = 100, height = 100)
  ))
})

test_that('cached parallel rendering is identical to full sequential rendering', {
  skip_mirai()
  plot <- cache_test_plot()
  full <- render_frames(plot, 'png', cache = FALSE)
  local_daemons(2)
  withr::local_options(gganimate.cache_layout = TRUE)
  built <- prerender(plot, 5)
  frame_vars <- draw_frames(built, seq_len(get_nframes(built)), device = 'png',
                            ref_frame = 1, width = 200, height = 200)
  expect_identical(unname(tools::md5sum(frame_vars$frame_source)), full$md5)
})

test_that('cached parallel rendering redraws glue titles from the main session', {
  skip_mirai()
  frame_label <- main_only(function() 'from the main session')
  # Built here so that glue looks up frame_label() in this environment
  plot <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + transition_states(gear) +
    labs(title = '{closest_state} {frame_label()}')
  full <- render_frames(plot, 'png', cache = FALSE)
  local_daemons(2)
  withr::local_options(gganimate.cache_layout = TRUE)
  built <- prerender(plot, 5)
  frame_vars <- expect_no_warning(
    draw_frames(built, seq_len(get_nframes(built)), device = 'png',
                ref_frame = 1, width = 200, height = 200)
  )
  expect_identical(unname(tools::md5sum(frame_vars$frame_source)), full$md5)
})
