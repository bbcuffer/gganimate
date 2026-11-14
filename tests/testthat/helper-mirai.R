skip_mirai <- function() {
  skip_on_cran() # spawns background R processes
  skip_if_not_installed('mirai')
}

# Start daemons for the calling test and stop them when it finishes. Under
# devtools::test() gganimate is loaded with pkgload::load_all(), so the
# workers loading the same source (rather than an installed copy) is tested by
# every parallel test.
local_daemons <- function(n = 2, env = parent.frame()) {
  mirai::daemons(n)
  withr::defer(mirai::daemons(0), envir = env)
  invisible()
}

job_left_on_workers <- function() {
  any(unlist(mirai::everywhere(exists('.gganimate_job', envir = globalenv()))[]))
}

# Errors if called anywhere but this process, i.e. on a worker. The wrapper
# gets its own environment: this file's environment descends from the attached
# package, which R warns about when serialising it for the workers.
main_only <- function(f) {
  wrapper <- function(...) {
    if (Sys.getpid() != main_pid) stop('called on a worker')
    f(...)
  }
  environment(wrapper) <- list2env(list(main_pid = Sys.getpid(), f = f), parent = globalenv())
  wrapper
}

# Enclosed by the global environment, like a plot made in a script: this
# file's environment descends from the attached package, which R warns about
# when the plot is serialised for the workers
mirai_test_plot <- function() {
  ggplot(mtcars, aes(mpg, wt)) +
    geom_point() +
    transition_states(gear) +
    labs(title = '{frame}')
}
environment(mirai_test_plot) <- globalenv()

render_md5 <- function(p, nframes = 4, device = 'png', ...) {
  built <- prerender(p, nframes)
  frame_vars <- draw_frames(built, seq_len(nframes), device = device,
                            ref_frame = 1, width = 100, height = 100, ...)
  unname(tools::md5sum(frame_vars$frame_source))
}

render_sequential_md5 <- function(p, nframes = 4, ...) {
  withr::local_options(gganimate.parallel = FALSE)
  out <- NULL
  capture.output(out <- render_md5(p, nframes, ...))
  out
}

# Load a small package with pkgload::load_all() for the calling test. It is
# never installed. Its geom_shout() draws shout('frame') at draw time, and
# shout() appends `suffix`.
local_dev_package <- function(suffix = '!', env = parent.frame()) {
  path <- file.path(withr::local_tempdir(.local_envir = env), 'gganimatedevpkg')
  dir.create(file.path(path, 'R'), recursive = TRUE)
  writeLines(c(
    'Package: gganimatedevpkg', 'Version: 0.0.1', 'Title: Test',
    'Description: Test.', 'License: MIT', 'Encoding: UTF-8'
  ), file.path(path, 'DESCRIPTION'))
  writeLines('export(geom_shout)', file.path(path, 'NAMESPACE'))
  writeLines(c(
    "GeomShout <- ggplot2::ggproto('GeomShout', ggplot2::Geom,",
    "  draw_panel = function(data, panel_params, coord) grid::textGrob(shout('frame'))",
    ")",
    "geom_shout <- function() ggplot2::layer(geom = GeomShout, stat = 'identity',",
    "  position = 'identity', params = list())"
  ), file.path(path, 'R', 'geom.R'))
  write_dev_shout(path, suffix)
  pkgload::load_all(path, helpers = FALSE, quiet = TRUE)
  withr::defer(pkgload::unload('gganimatedevpkg'), envir = env)
  path
}

write_dev_shout <- function(path, suffix) {
  writeLines(
    sprintf("shout <- function(x) paste0(toupper(x), '%s')", suffix),
    file.path(path, 'R', 'shout.R')
  )
}
