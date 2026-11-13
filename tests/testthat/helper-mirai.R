skip_mirai <- function() {
  skip_on_cran() # spawns background R processes
  skip_if_not_installed('mirai')
}

# Start daemons for the calling test and stop them when it finishes. Workers
# load gganimate by namespace, so under devtools::test() they would otherwise
# get the installed copy rather than the code being tested.
local_daemons <- function(n = 2, env = parent.frame()) {
  mirai::daemons(n)
  withr::defer(mirai::daemons(0), envir = env)
  if (requireNamespace('pkgload', quietly = TRUE) && pkgload::is_dev_package('gganimate')) {
    path <- getNamespaceInfo('gganimate', 'path')
    mirai::everywhere(pkgload::load_all(path, quiet = TRUE), path = path)[]
  }
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
