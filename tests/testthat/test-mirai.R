test_that('parallel rendering produces the same frames as sequential rendering', {
  skip_mirai()
  p <- mirai_test_plot()
  render <- function() {
    animate(p, nframes = 6, device = 'png', width = 100, height = 100,
            renderer = file_renderer(withr::local_tempdir(.local_envir = parent.frame(2)),
                                     overwrite = TRUE))
  }
  capture.output(seq_files <- render())
  seq_vars <- attr(seq_files, 'frame_vars')

  local_daemons(2)
  par_files <- render()
  par_vars <- attr(par_files, 'frame_vars')

  expect_length(par_files, length(seq_files))
  expect_true(all(file.exists(par_files)))
  expect_equal(unname(tools::md5sum(par_files)), unname(tools::md5sum(seq_files)))
  cols <- setdiff(names(seq_vars), 'frame_source')
  expect_equal(par_vars[cols], seq_vars[cols])
  expect_false(job_left_on_workers())
})

test_that('parallel png output follows the main session\'s bitmapType', {
  skip_mirai()
  skip_if_not(capabilities('cairo'))
  # Workers start with default options; e.g. RStudio sets bitmapType = 'cairo'
  withr::local_options(bitmapType = 'cairo')
  sequential <- render_sequential_md5(mirai_test_plot())
  local_daemons(2)
  expect_identical(render_md5(mirai_test_plot()), sequential)
})

# Frame failures and warnings ----------------------------------------------------

# Make drawing fail for frames after the second. The title is '{frame}'.
fail_late_frames <- function(plot) {
  plot$scene$draw_frame <- function(self, frame, ...) {
    if (as.integer(frame$plot$labels$title) > 2) stop('boom {not cli markup}')
    grid::grid.newpage()
  }
  plot
}

test_that('frames that fail are reported together', {
  plot <- fail_late_frames(prerender(mirai_test_plot(), 4))
  capture.output(expect_warning(
    draw_frames(plot, seq_len(4), device = 'png', ref_frame = 1),
    '2 frames failed to render.*boom \\{not cli markup\\}'
  ))
})

test_that('frames that fail on workers are reported and workers are cleaned up', {
  skip_mirai()
  local_daemons(2)
  plot <- fail_late_frames(prerender(mirai_test_plot(), 4))
  expect_warning(
    draw_frames(plot, seq_len(4), device = 'png', ref_frame = 1),
    '2 frames failed to render.*boom \\{not cli markup\\}'
  )
  expect_false(job_left_on_workers())
})

test_that('an error is thrown when every frame fails', {
  plot <- prerender(mirai_test_plot(), 4)
  plot$scene$draw_frame <- function(...) stop('boom')
  capture.output(expect_error(
    draw_frames(plot, seq_len(4), device = 'png', ref_frame = 1),
    'All 4 frames failed to render'
  ))
  skip_mirai()
  local_daemons(2)
  expect_error(
    draw_frames(plot, seq_len(4), device = 'png', ref_frame = 1),
    'All 4 frames failed to render.*mirai::everywhere'
  )
  expect_false(job_left_on_workers())
})

test_that('a warning raised in every frame is given once', {
  plot <- prerender(mirai_test_plot(), 4)
  plot$scene$draw_frame <- function(...) {
    warning('careful')
    grid::grid.newpage()
  }
  capture.output(warnings <- capture_warnings(
    draw_frames(plot, seq_len(4), device = 'png', ref_frame = 1)
  ))
  expect_identical(warnings, 'careful')
  skip_mirai()
  local_daemons(2)
  expect_identical(
    capture_warnings(draw_frames(plot, seq_len(4), device = 'png', ref_frame = 1)),
    'careful'
  )
})

# Choosing sequential rendering -------------------------------------------------

test_that('options(gganimate.parallel = FALSE) renders sequentially', {
  skip_mirai()
  local_daemons(2)
  withr::local_options(gganimate.parallel = FALSE)
  local_mocked_bindings(
    mirai_map = function(...) stop('mirai_map should not be used'),
    .package = 'mirai'
  )
  expect_length(render_sequential_md5(mirai_test_plot()), 4)
})

test_that('a failed worker setup warns and falls back to sequential rendering', {
  skip_mirai()
  local_daemons(2)
  worker_error <- structure(
    'there is no package called gganimate',
    class = c('miraiError', 'errorValue', 'try-error')
  )
  local_mocked_bindings(
    everywhere = function(...) list(worker_error),
    mirai_map = function(...) stop('mirai_map should not be used'),
    .package = 'mirai'
  )
  capture.output(expect_warning(
    md5 <- render_md5(mirai_test_plot()),
    'Could not set up the mirai workers'
  ))
  expect_identical(md5, render_sequential_md5(mirai_test_plot()))
})

test_that('a package missing on the workers falls back to sequential rendering', {
  skip_mirai()
  local_daemons(2)
  local_mocked_bindings(
    session_packages = function() list(loaded = 'notAPackage123', attached = character())
  )
  capture.output(expect_warning(
    md5 <- render_md5(mirai_test_plot()),
    'notAPackage123'
  ))
  expect_identical(md5, render_sequential_md5(mirai_test_plot()))
})

test_that('the current device is drawn sequentially even with daemons running', {
  skip_mirai()
  local_daemons(2)
  local_mocked_bindings(
    mirai_map = function(...) stop('mirai_map should not be used for the current device'),
    .package = 'mirai'
  )
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  frame_vars <- draw_frames(prerender(mirai_test_plot(), 3), seq_len(3),
                            device = 'current', ref_frame = 1)
  expect_equal(nrow(frame_vars), 3)
  expect_false('frame_source' %in% names(frame_vars))
})

# Per-frame code runs in the main session ----------------------------------------

test_that('glue labels are interpolated in the main session', {
  skip_mirai()
  frame_label <- main_only(function() 'from the main session')
  # Built here so that glue looks up frame_label() in this environment
  p <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + transition_states(gear) +
    labs(title = '{closest_state} {frame_label()}')
  sequential <- expect_no_warning(render_sequential_md5(p))
  local_daemons(2)
  expect_identical(expect_no_warning(render_md5(p)), sequential)
})

test_that('a label that glue cannot interpolate warns once and is kept as written', {
  p <- mirai_test_plot() + labs(subtitle = '{no_such_variable}')
  capture.output(warnings <- capture_warnings(sequential <- render_md5(p)))
  expect_length(warnings, 1)
  expect_match(warnings, 'Could not interpolate the label')
  skip_mirai()
  local_daemons(2)
  warnings <- capture_warnings(parallel <- render_md5(p))
  expect_length(warnings, 1)
  expect_identical(parallel, sequential)
})

test_that('moving views are set in the main session', {
  skip_mirai()
  y_labels <- main_only(function(x) paste0(x, 't'))
  x_breaks <- main_only(function(lims) seq(floor(lims[1]), ceiling(lims[2]), by = 5))
  plots <- list(
    follow = mirai_test_plot() + scale_y_continuous(labels = y_labels) + view_follow(),
    step = mirai_test_plot() + scale_x_continuous(breaks = x_breaks) +
      view_step_manual(xmin = c(10, 15), xmax = c(30, 35), ymin = 1, ymax = 6, wrap = FALSE),
    flip = mirai_test_plot() + scale_y_continuous(labels = y_labels) + coord_flip() + view_follow(),
    facets = mirai_test_plot() + facet_wrap(~am) + scale_y_continuous(labels = y_labels) +
      view_follow()
  )
  sequential <- lapply(plots, render_sequential_md5, nframes = 8)
  local_daemons(2)
  expect_identical(lapply(plots, render_md5, nframes = 8), sequential)
})

test_that('moving views send each frame\'s guide params, not its Guides object', {
  skip_mirai()
  local_daemons(2)
  sent <- NULL
  local_mocked_bindings(setup_workers = function(job) {
    sent <<- job
    FALSE
  })
  p <- mirai_test_plot() + view_follow()
  capture.output(render_md5(p, nframes = 10))
  expect_length(sent$parts, 10)
  expect_false(inherits(sent$parts[[1]]$panel_params[[1]]$guides, 'Guides'))
  # What each frame adds: the scales its panel ranges refer to are shared, and
  # serialised once. A frame's Guides object alone would add about 2 MB.
  size <- function(x) length(serialize(x, NULL))
  per_frame <- (size(sent$parts) - size(sent$parts[1])) / (length(sent$parts) - 1)
  expect_lt(per_frame, 100e3)
})

# The workers' environment -------------------------------------------------------

test_that('the session theme and ggplot2 options apply to parallel frames', {
  skip_mirai()
  old_theme <- theme_set(theme_dark())
  withr::defer(theme_set(old_theme))
  withr::local_options(ggplot2.discrete.colour = c('red', 'blue', 'green'))
  p <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point() +
    transition_states(gear)
  sequential <- render_sequential_md5(p)
  local_daemons(2)
  expect_identical(render_md5(p), sequential)
})

test_that('workers load and attach the packages of the main session', {
  skip_mirai()
  skip_if_not_installed('svglite')
  withr::local_package('svglite')
  local_daemons(2)
  render_md5(mirai_test_plot())
  on_workers <- mirai::everywhere(
    c(isNamespaceLoaded('svglite'), 'package:svglite' %in% search())
  )[]
  expect_true(all(unlist(on_workers)))
})

test_that('draw-time code using session objects fails with a hint until set up', {
  skip_mirai()
  # A labeller defined in the global environment, as in a user's script
  assign('.gganimate_suffix', '!', envir = globalenv())
  withr::defer(rm('.gganimate_suffix', envir = globalenv()))
  add_suffix <- function(x) paste0(x, .gganimate_suffix)
  environment(add_suffix) <- globalenv()
  p <- mirai_test_plot() + facet_wrap(~am, labeller = as_labeller(add_suffix))

  sequential <- render_sequential_md5(p)
  local_daemons(2)
  expect_error(render_md5(p), "object '.gganimate_suffix' not found", fixed = TRUE)
  mirai::everywhere(assign('.gganimate_suffix', '!', envir = globalenv()))[]
  expect_identical(render_md5(p), sequential)
})
