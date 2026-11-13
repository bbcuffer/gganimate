#' Render a gganim object
#'
#' This function takes a gganim object and renders it into an animation. The
#' nature of the animation is dependent on the renderer, but defaults to using
#' `gifski` to render it to a gif. The length and framerate is decided on render
#' time and can be any two combination of `nframes`, `fps`, and `duration`.
#' Rendering is happening in discrete time units. This means that any event in
#' the animation is rounded of to the nearest frame (e.g. entering will always
#' take a whole number of frames). This means that rounding artifacts are
#' possible when only rendering few frames. To avoid this you can increase the
#' `detail` argument. `detail` will get multiplied to `nframes` and the
#' resulting number of frames will get calculated, but only `nframes` evenly
#' spaced frames are rendered.
#'
#' @param plot,x A `gganim` object
#' @param nframes The number of frames to render (default `100`)
#' @param fps The framerate of the animation in frames/sec (default `10`)
#' @param duration The length of the animation in seconds (unset by default)
#' @param detail The number of additional frames to calculate, per frame (default `1`)
#' @param renderer The function used to render the generated frames into an
#' animation. Gets a vector of paths to images along with the framerate. (by
#' default it will use [gifski_renderer()] if gifski is installed. If not it
#' will use [magick_renderer()] if magick is installed and then [av_renderer()]
#' if av is installed. If all fails it will use the [file_renderer()])
#' @param device The device to use for rendering the single frames. Possible
#' values are `'png'`, `'ragg_png'` (requires the ragg package), `'jpeg'`,
#' `'tiff'`, `'bmp'`, `'svg'`, and `'svglite'` (requires the svglite package).
#' (default `'png'`)
#' @param ref_frame The frame to use for fixing dimensions of the plot, e.g. the
#' space available for axis text. Defaults to the first frame. Negative values
#' counts backwards (-1 is the last frame) (default `1`)
#' @param start_pause,end_pause Number of times to repeat the first and last
#' frame in the animation (default is `0` for both)
#' @param rewind Should the animation roll back in the end (default `FALSE`)
#' @param ... Arguments passed on to the device.
#' For available device arguments, see [grDevices::png()] or [grDevices::svg()]
#'
#' @return The return value of the [renderer][renderers] function
#'
#' @details  `print.gganim`()  is an alias for `animate()` in the same way as
#' `print.ggplot()` is an alias for `plot.ggplot()`. This ensures that gganimate
#' behaves ggplot2-like and produces the animation when the object is printed.
#' The `plot()` method is different and produces a single frame for inspection
#' (by default frame 50 out of 100).
#'
#' Animations can be saved to disk using [anim_save()] in much the same way
#' [ggsave()][ggplot2::ggsave] works for static plots.
#'
#' @section Defaults:
#' It is possible to overwrite the defaults used by gganimate for the animation
#' by setting them with [options()] (prefixed with `gganimate.`. As an example,
#' if you would like to change the default nframes to 50 you would call
#' `options(gganimate.nframes = 50)`. In order to set default device arguments
#' (those you would normally pass through with `...`) you should use the
#' `gganimate.dev_args` options and provide a list of arguments e.g.
#' `options(gganimate.dev_args = list(width = 800, height = 600))` Defaults set
#' this way can still be overridden by giving arguments directly to `animate()`.
#'
#' **knitr Support:** \cr
#' It is possible to specify the arguments to `animate()` in the chunk options
#' when using `gganimate` with `knitr`. Arguments specified in this way will
#' have precedence over defaults, but not over arguments specified directly in
#' `animate()`. The arguments should be provided as a list to the `gganimate`
#' chunk option, e.g. `{r, gganimate = list(nframes = 50, fps = 20)}`. A few
#' build-in knitr options have relevance for animation and will be used unless
#' given specifically in the `gganimate` list option. The native knitr options
#' supported are:
#'
#' - `dev`: will set `device`
#' - `dev.args`: will set additional arguments to the device (`...`)
#' - `fig.width`, `fig.height`, `fig.asp`, `fig.dim`: will set `width` and
#'   `height` of the device.
#'
#' @section Parallel rendering:
#' If [mirai][mirai::daemons] daemons are running, the frames are drawn in
#' parallel on those workers whenever the device writes to files (i.e. not
#' `device = 'current'`). Set `options(gganimate.parallel = FALSE)` to render
#' sequentially even when daemons are running.
#'
#' Workers are separate R sessions, so it helps to know what runs where. The
#' plot is built in the main session: `aes()`, layer data and parameters,
#' stats, scales, legends and the theme (including [ggplot2::theme_set()] and
#' ggplot2's options) are all worked out before anything is sent. So is each
#' frame's data, view and labels, so scale functions under a moving view (e.g.
#' [view_follow()]) and variables used in glue labels don't need to exist on
#' the workers.
#'
#' The workers draw the frames. Before drawing they load the packages loaded in
#' the main session and attach the ones attached in it; if a package is not
#' installed for the workers, the frames are rendered sequentially with a
#' warning. Code that runs while a frame is drawn and refers to objects defined
#' in the session, such as a facet labeller, a custom geom or an S3 method, and
#' fonts registered with e.g. `systemfonts::register_font()`, must be set up on
#' the workers with [mirai::everywhere()] before calling `animate()`. Other
#' options are not copied, except that the `type` of the `png`, `jpeg`, `tiff`
#' and `bmp` devices defaults to the main session's `getOption("bitmapType")`,
#' so the output matches sequential rendering.
#'
#' Whether or not frames are rendered in parallel, each distinct warning is
#' raised once, and frames that fail to render are reported together in one
#' warning. If every frame fails, `animate()` throws an error.
#'
#' @section Layout caching:
#' Most of the time spent drawing a frame goes on parts of the plot that are
#' the same in every frame: axes, strips, legends and the theme. With
#' `options(gganimate.cache_layout = TRUE)` these are rendered once and reused,
#' and only the layers (plus the title, subtitle and caption, if their text
#' changes) are redrawn for each frame. The output is the same as without the
#' cache. It works with and without parallel rendering.
#'
#' The cache is only used with [view_static()] (the default view),
#' `facet_null()`, `facet_wrap()` or `facet_grid()`, and `coord_cartesian()`,
#' `coord_fixed()` or `coord_flip()`. Other plots, and frames in which any other
#' label changes, are rendered in full. If the panels can't be found in the
#' rendered plot, a warning is given and every frame is rendered in full.
#'
#' @section Label variables:
#' All plots have a certain set of variables available for string literal
#' interpolation within plot labels. These are:
#'
#' - **frame** gives you the frame index for the current frame
#' - **nframes** gives you the total number of frames in the animation
#' - **progress** gives you the progress of the animation at the current frame
#'   (equal to `frame/nframes`)
#' - **data** gives you the layer data for the current frame (as a list of data
#'   frames)
#'
#' Further, the transition and view in use can also make variables available.
#' Consult the documentation for these for more detail.
#'
#' @importFrom grid grid.newpage grid.draw convertWidth convertHeight
#' @importFrom grDevices png jpeg tiff bmp svg dev.off
#' @importFrom progress progress_bar
#' @importFrom ggplot2 ggplot_gtable ggplot_build
#' @export
#'
#' @examples
#' anim <- ggplot(mtcars, aes(mpg, disp)) +
#'   geom_point(aes(color = gear)) +
#'   transition_states(gear, transition_length = 2, state_length = 1) +
#'   enter_fade() +
#'   exit_fade()
#'
#' \dontrun{
#' # Explicitly animate using default (same as just printing the animation)
#' animate(anim)
#'
#' # Change duration and framerate
#' animate(anim, fps = 20, duration = 15)
#'
#' # Make the animation pause at the end and then rewind
#' animate(anim, nframes = 100, end_pause = 10, rewind = TRUE)
#'
#' # Use a different renderer
#' animate(anim, renderer = file_renderer('~/animation/'))[1:6]
#'
#' # Specify device dimensions and/or resolution
#' animate(anim, height = 2, width = 3, units = "in", res = 150)
#' }
#'
animate <- function(plot, ...) {
  UseMethod('animate')
}
#' @export
animate.default <- function(plot, ...) {
  cli::cli_abort('animation of {.cls {class(plot)}} objects are not supported')
}
#' @rdname animate
#' @export
animate.gganim <- function(plot, nframes, fps, duration, detail, renderer, device, ref_frame, start_pause, end_pause, rewind, ...) {
  args <- prepare_args(
    nframes = nframes,
    fps = fps,
    duration = duration,
    detail = detail,
    renderer = renderer,
    device = device,
    ref_frame = ref_frame,
    start_pause = start_pause,
    end_pause = end_pause,
    rewind = rewind,
    ...
  )
  if (is_knitting() && identical(def_ren$renderer, args$renderer) && !def_ren$has_proper) {
    cli::cli_warn('No renderer available. Please install the {.or {.pkg {c("gifski", "av", "magick")}}} package to create animated output')
    return(invisible(NULL))
  }
  orig_nframes <- args$nframes
  args$nframes <- args$nframes - args$start_pause - args$end_pause
  if (args$rewind) {
    args$nframes <- ceiling((args$nframes - args$start_pause) / 2)
    args$end_pause <- ceiling(args$end_pause / 2)
  }
  nframes_total <- (args$nframes - 1) * args$detail + 1
  plot <- prerender(plot, nframes_total)
  nframes_final <- get_nframes(plot)

  frame_ind <- unique0(round(seq(1, nframes_final, length.out = args$nframes)))

  if (args$device == 'current') {
    frame_ind <- c(rep(frame_ind[1], args$start_pause), frame_ind, rep(frame_ind[length(frame_ind)], args$end_pause))
    if (args$rewind) frame_ind <- c(frame_ind, rev(frame_ind))
    if (args$ref_frame < 0) {
      args$ref_frame <- args$ref_frame - args$end_pause
    } else {
      args$ref_frame <- args$ref_frame + args$start_pause
    }
  }

  if (args$nframes != length(frame_ind)) {
    cli::cli_inform('{.arg nframes} and {.arg fps} adjusted to match transition')
    args$fps <- args$fps * length(frame_ind) / args$nframes
  }

  if (args$ref_frame < 0) args$ref_frame <- nframes_final + 1 + args$ref_frame

  frames_vars <- inject(draw_frames(plot = plot, frames = frame_ind, device = args$device, ref_frame = args$ref_frame, !!!args$dev_args))
  if (args$device == 'current') return(invisible(frames_vars))

  if (args$start_pause != 0) frames_vars <- vec_rbind0(frames_vars[rep(1, args$start_pause), , drop = FALSE], frames_vars)
  if (args$end_pause != 0) frames_vars <- vec_rbind0(frames_vars, frames_vars[rep(nrow(frames_vars), args$end_pause), , drop = FALSE])
  if (args$rewind) frames_vars <- vec_rbind0(frames_vars, frames_vars[rev(seq_len(orig_nframes - nrow(frames_vars))), , drop = FALSE])

  animation <- args$renderer(frames_vars$frame_source, args$fps)
  attr(animation, 'frame_vars') <- frames_vars
  set_last_animation(animation)
  animation
}

#' Catch attempt to use the old API
#'
#' @export
#' @keywords internal
gganimate <- function(...) {
  cli::cli_abort(c(
    'It appears that you are trying to use the old API, which has been deprecated.',
    i = 'Please update your code to the new API or install the old version of gganimate from {.url https://github.com/thomasp85/gganimate/releases/tag/v0.1.1}'
  ))
}
#' @rdname gganimate
#' @export
gg_animate <- gganimate

#' @importFrom utils modifyList
prepare_args <- function(nframes, fps, duration, detail, renderer, device, ref_frame, start_pause, end_pause, rewind, ...) {
  args <- list()
  chunk_args <- if (is_knitting()) get_knitr_options(knitr::opts_current$get(), unlist = FALSE) else list(dev_args = list())
  args$nframes <- nframes %?% chunk_args$nframes %||% getOption('gganimate.nframes', 100)
  args$fps <- fps %?% chunk_args$fps %||% getOption('gganimate.fps', 10)
  duration <- duration %?% chunk_args$duration %||% getOption('gganimate.duration', NULL)
  if (!is.null(duration)) {
    if (
      !missing(fps) ||
      is.null(args$nframes) ||
      (!is.null(getOption('gganimate.fps')) && is.null(getOption('gganimate.nframes')))
    ) args$nframes <- duration * args$fps
    else args$fps <- args$nframes / duration
  }
  if (is.null(args$nframes) || is.null(args$fps)) {
    cli::cli_abort("At least 2 of {.arg nframes}, {.arg fps}, and {.arg duration} must be given")
  }
  args$detail <- detail %?% chunk_args$detail %||% getOption('gganimate.detail', 1)
  args$renderer <- renderer %?% chunk_args$renderer %||% getOption('gganimate.renderer', def_ren$renderer)
  args$device <- tolower(device %?% chunk_args$device %||% getOption('gganimate.device', 'png'))
  if (args$device == 'svglite') {
    check_installed('svglite', 'to use the svglite device')
  }
  if (args$device == 'ragg_png') {
    check_installed('ragg', 'to use the ragg_png device')
  }
  args$ref_frame <- ref_frame %?% chunk_args$ref_frame %||% getOption('gganimate.ref_frame', 1)
  args$start_pause <- start_pause %?% chunk_args$start_pause %||% getOption('gganimate.start_pause', 0)
  args$end_pause <- end_pause %?% chunk_args$end_pause %||% getOption('gganimate.end_pause', 0)
  args$rewind <- rewind %?% chunk_args$rewind %||% getOption('gganimate.rewind', FALSE)

  args$dev_args <- modifyList(getOption('gganimate.dev_args', list()), chunk_args$dev_args)
  args$dev_args <- modifyList(args$dev_args, list(...))

  args
}
# Build plot for a specific number of frames
prerender <- function(plot, nframes) {
  plot <- set_nframes(plot, nframes)
  ggplot_build(plot)
}
# Draw each frame as an image based on a specified device
# Returns a data.frame of frame metadata with image location in frame_source
# column
draw_frames <- function(plot, frames, device, ref_frame, ...) {
  stream <- device == 'current'

  dims <- capture_conditions(plot_dims(plot, ref_frame))
  if (!is.null(dims$error)) {
    cli::cli_warn(c(
      'Cannot get dimensions of plot table. Plot region might not be fixed',
      cli_values(dims$error, 'x')
    ))
  }

  dir <- tempfile(pattern = '')
  dir.create(dir, showWarnings = FALSE)
  files <- file.path(dir, sprintf('gganim_plot%04d', seq_along(frames)))
  files <- switch(
    tolower(device),
    ragg_png = ,
    png = paste0(files, '.png'),
    jpg = ,
    jpeg = paste0(files, '.jpg'),
    tif = ,
    tiff = paste0(files, '.tif'),
    bmp = paste0(files, '.bmp'),
    svglite = ,
    svg = paste0(files, '.svg'),
    current = files,
    cli::cli_abort('Unsupported device: {device}')
  )
  device_fn <- switch(
    device,
    ragg_png = ragg::agg_png,
    png = png,
    jpg = ,
    jpeg = jpeg,
    tif = ,
    tiff = tiff,
    bmp = bmp,
    svg = svg,
    svglite = svglite::svglite
  )
  # How to draw each frame, the same for sequential and parallel rendering
  render <- list(
    widths = dims$value$widths,
    heights = dims$value$heights,
    # NULL unless options(gganimate.cache_layout = TRUE) and the plot qualifies
    cache = layout_cache(plot)
  )
  if (!stream) {
    dev_args <- list(...)
    dev_args <- dev_args[names(dev_args) %in% names(formals(device_fn))]
    render$device_fn <- device_fn
    render$dev_args <- resolve_device_defaults(device_fn, dev_args)
  }

  results <- if (!stream && parallel_available()) {
    render_parallel(plot, frames, files, render)
  }
  parallel <- !is.null(results)
  if (!parallel) results <- render_sequential(plot, frames, files, render)
  report_frames(results, parallel, dims$warnings)

  frame_vars <- plot$scene$frame_vars[frames, , drop = FALSE]
  if (!stream) frame_vars$frame_source <- files
  frame_vars
}

# Draw one frame: `make_frame()` returns it, from get_frame() or, on a worker,
# from the parts computed in the main session
render_one <- function(make_frame, file, render) {
  if (!is.null(render$device_fn)) {
    inject(render$device_fn(file, !!!render$dev_args))
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  frame <- make_frame()
  frame$scene$draw_frame(frame, widths = render$widths, heights = render$heights,
                         cache = render$cache)
}

# Evaluate `expr`, keeping its value, the message of any error and the
# messages of any warnings. Both rendering paths report frames this way, so
# a warning repeated in every frame is raised once.
capture_conditions <- function(expr) {
  warnings <- character()
  result <- withCallingHandlers(
    tryCatch(
      list(value = expr, error = NULL),
      error = function(e) list(value = NULL, error = condition_text(e))
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      tryInvokeRestart('muffleWarning')
    }
  )
  result$warnings <- unique(warnings)
  result
}

render_sequential <- function(plot, frames, files, render) {
  pb <- progress_bar$new(
    'Rendering [:bar] at :fps fps ~ eta: :eta',
    total = length(frames)
  )
  start <- Sys.time()
  pb$tick(0)
  results <- vector('list', length(frames))
  for (i in seq_along(frames)) {
    results[[i]] <- capture_conditions(
      render_one(function() plot$scene$get_frame(plot, frames[i]), files[i], render)
    )
    rate <- i/as.double(Sys.time() - start, units = 'secs')
    if (is.nan(rate)) rate <- 0
    rate <- format(rate, digits = 2)
    pb$tick(tokens = list(fps = rate))
  }
  results
}

report_frames <- function(results, parallel, warnings = character()) {
  warnings <- unique(c(warnings, unlist(lapply(results, `[[`, 'warnings'))))
  for (msg in warnings) warning(msg, call. = FALSE)

  errors <- unlist(lapply(results, `[[`, 'error'))
  n_errors <- length(errors)
  if (n_errors == 0L) return(invisible())
  bullets <- cli_values(unique(errors), 'x')
  # Every frame failing points to the setup (a missing package or font, say)
  # rather than a bad frame, so don't hand back a blank animation
  if (n_errors == length(results)) {
    cli::cli_abort(c(
      'All {n_errors} frames failed to render',
      bullets,
      i = if (parallel) 'Workers get the packages loaded in this session, but not fonts or objects defined in it. Code that runs while a frame is drawn, such as a facet labeller or a custom geom, needs everything it uses set up with {.fn mirai::everywhere} before calling {.fn animate}. Or render sequentially with {.code options(gganimate.parallel = FALSE)}.'
    ))
  }
  cli::cli_warn(c('{n_errors} frame{?s} failed to render', bullets))
}

# Bullets for cli that show each message as is: braces in them must not be
# read as cli markup
cli_values <- function(x, bullet) {
  x <- gsub('}', '}}', gsub('{', '{{', x, fixed = TRUE), fixed = TRUE)
  stats::setNames(x, rep(bullet, length(x)))
}

condition_text <- function(x) {
  if (inherits(x, 'condition')) conditionMessage(x) else format(x)
}

# Parallel rendering on mirai workers ------------------------------------------
#
# The main session computes every frame (get_frame(): its data, view and
# labels), exactly as for sequential rendering, so user code that runs per
# frame (scale functions under a moving view, variables in glue labels) never
# runs on a worker. Workers only draw. They get, once each, the plot without
# its tweened data plus each frame's parts; each task is just a frame index.

parallel_available <- function() {
  isTRUE(getOption('gganimate.parallel', TRUE)) &&
    requireNamespace('mirai', quietly = TRUE) &&
    isTRUE(tryCatch(mirai::status()$connections > 0L, error = function(e) FALSE))
}

# Returns NULL, having warned, if the workers could not be set up
render_parallel <- function(plot, frames, files, render) {
  static_panel_params <- plot$layout$panel_params
  prepared <- lapply(frames, function(i) {
    capture_conditions(frame_parts(plot$scene$get_frame(plot, i), static_panel_params))
  })
  ok <- vapply(prepared, function(p) is.null(p$error), logical(1))

  skeleton <- plot
  skeleton$data <- NULL
  job <- list(
    plot = skeleton,
    parts = lapply(prepared[ok], `[[`, 'value'),
    guides = lapply(plot$layout$panel_params, `[[`, 'guides'),
    files = files[ok],
    render = render
  )
  on.exit(clear_workers(), add = TRUE)
  if (!setup_workers(job)) return(NULL)

  rendered <- mirai::mirai_map(seq_len(sum(ok)), render_frame_on_worker)[mirai::.progress]
  results <- prepared
  results[ok] <- Map(function(p, r) {
    if (mirai::is_error_value(r)) r <- list(error = condition_text(r))
    list(error = r$error, warnings = c(p$warnings, r$warnings))
  }, prepared[ok], rendered)
  results
}

# What a worker needs to rebuild a frame: its data and labels and, if the view
# moved, its panel ranges. Each moving-view frame has a new Guides object,
# which serialises to megabytes through its links to ggplot2's internals, so
# only its params (breaks, labels, positions) are sent.
frame_parts <- function(frame, static_panel_params) {
  parts <- list(data = frame$data, labels = frame$plot$labels)
  panel_params <- frame$layout$panel_params
  if (!identical(panel_params, static_panel_params)) {
    parts$coord_limits <- frame$layout$coord$limits
    parts$panel_params <- lapply(panel_params, function(p) {
      if (inherits(p$guides, 'Guides')) p$guides <- p$guides$params
      p
    })
  }
  parts
}

# Runs on a worker. `guides` holds a Guides object per panel to rebuild from.
assemble_frame <- function(plot, parts, guides) {
  plot$data <- parts$data
  plot$plot$labels <- parts$labels
  if (!is.null(parts$panel_params)) {
    plot$layout$coord$limits <- parts$coord_limits
    plot$layout$panel_params <- Map(function(p, template) {
      if (!is.null(p$guides)) p$guides <- ggproto(NULL, template, params = p$guides)
      p
    }, parts$panel_params, guides)
  }
  plot
}

# Runs on a worker. A top-level function serialises with a reference to the
# gganimate namespace, not a copy of its environment.
render_frame_on_worker <- function(k) {
  job <- get('.gganimate_job', envir = globalenv())
  capture_conditions(render_one(
    function() assemble_frame(job$plot, job$parts[[k]], job$guides),
    job$files[[k]], job$render
  ))
}

# Send the job to each worker once. Named `...` arguments to everywhere()
# persist in the worker's global environment; `.args` would not. Workers also
# load and attach the session's packages: drawing can need S3 methods (e.g.
# element_grob() for a theme element from another package) and functions
# called without `pkg::`. Fonts and objects defined in the session must be set
# up by the caller with mirai::everywhere() before animate().
# Returns FALSE, with a warning, if any worker could not be set up.
setup_workers <- function(job) {
  setup <- mirai::everywhere(
    worker_setup,
    .gganimate_job = job,
    .args = list(packages = session_packages(), libs = .libPaths())
  )[]
  failed <- Filter(mirai::is_error_value, setup)
  if (length(failed) == 0L) return(TRUE)
  cli::cli_warn(c(
    'Could not set up the mirai workers, so frames will be rendered sequentially',
    cli_values(condition_text(failed[[1]]), 'x')
  ))
  FALSE
}

# Quoted: `libs` and `packages` come from everywhere()'s `.args`
worker_setup <- quote({
  .libPaths(unique(c(libs[dir.exists(libs)], .libPaths())))
  loadNamespace('gganimate')
  get('prepare_worker', envir = asNamespace('gganimate'))(packages)
})

session_packages <- function() {
  base <- c('base', 'datasets', 'graphics', 'grDevices', 'methods', 'stats', 'utils')
  attached <- sub('^package:', '', grep('^package:', search(), value = TRUE))
  list(
    loaded = setdiff(loadedNamespaces(), base),
    # Attach from the end of the search path so the order matches the session
    attached = rev(setdiff(attached, base))
  )
}

# Runs on a worker
prepare_worker <- function(packages) {
  available <- vapply(packages$loaded, requireNamespace, logical(1), quietly = TRUE)
  if (!all(available)) {
    stop(
      'Packages loaded in the main session are not installed for the workers: ',
      paste(packages$loaded[!available], collapse = ', '),
      call. = FALSE
    )
  }
  for (pkg in packages$attached) {
    if (!paste0('package:', pkg) %in% search()) {
      suppressPackageStartupMessages(attachNamespace(pkg))
    }
  }
  TRUE
}

clear_workers <- function() {
  mirai::everywhere(suppressWarnings(rm(list = '.gganimate_job', envir = globalenv())))
}

# Workers start with fresh options, so device defaults that read the session's
# options must be resolved here. grDevices' bitmap devices (png, jpeg, tiff,
# bmp) default `type` to getOption("bitmapType"), which e.g. RStudio changes.
resolve_device_defaults <- function(device_fn, dev_args) {
  is_bitmap_device <- identical(environment(device_fn), asNamespace('grDevices')) &&
    'type' %in% names(formals(device_fn))
  if (is_bitmap_device && is.null(dev_args$type)) {
    dev_args$type <- getOption('bitmapType')
  }
  dev_args
}

# Get dimensions of plot based on a reference frame
plot_dims <- function(plot, ref_frame) {
  tmpf <- tempfile()
  png(tmpf)
  on.exit({
    dev.off()
    unlink(tmpf)
  })
  frame <- plot$scene$get_frame(plot, ref_frame)
  frame <- render_frame(frame)
  widths_rel <- frame$widths
  widths <- convertWidth(widths_rel, 'mm')
  heights_rel <- frame$heights
  heights <- convertHeight(heights_rel, 'mm')
  if (is_list(widths)) { # New unit spec
    null_widths <- vapply(unclass(widths), `[[`, numeric(1), 1L) == 0
    null_heights <- vapply(unclass(heights), `[[`, numeric(1), 1L) == 0
  } else {
    null_widths <- as.numeric(widths) == 0
    null_heights <- as.numeric(heights) == 0
  }
  widths[null_widths] <- widths_rel[null_widths]
  heights[null_heights] <- heights_rel[null_heights]
  list(widths = widths, heights = heights)
}

is_knitting <- function() isTRUE(getOption("knitr.in.progress"))

render_frame <- function(frame) {
  frame <- ggplot2::class_ggplot_built(data = frame$data, layout = frame$layout, plot = frame$plot)
  ggplot_gtable(frame)
}
