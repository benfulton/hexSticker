local({
  app_packages <- c(
    "argonDash", "argonR", "colourpicker", "ggplot2", "hexSticker",
    "imager", "png", "shiny", "shinyWidgets", "showtext", "testthat"
  )
  missing <- app_packages[
    !vapply(app_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    message("Skipping sticker tests; missing packages: ", paste(missing, collapse = ", "))
    return(invisible(NULL))
  }

  app <- new.env()
  sys.source(system.file("polaroid", "app.R", package = "polaroid"), envir = app)
  work_dir <- tempfile("polaroid-sticker-")
  dir.create(work_dir)
  old_dir <- setwd(work_dir)
  on.exit({
    setwd(old_dir)
    unlink(work_dir, recursive = TRUE)
  })
  image <- array(1, dim = c(4, 4, 4))

  shiny::testServer(app$server, {
    for (direction in c("up", "down")) {
      for (width in c(0.1, 1.5, 3)) {
        sticker <- mysticker(
          im = image, img_x = 0.5, img_y = 0.5,
          img_width = 1, img_height = 1,
          pkg_name = "polaroid", pkg_x = 1, pkg_y = 1.6,
          pkg_color = "#FFFFFF", pkg_size = 12,
          background_color = "#172b4d", border_color = "#6772E5",
          border_width = width, url = "", urlx = 1, urly = 0.08,
          url_color = "white", url_size = 1.5, url_angle = 30,
          font_family = "sans", background_alpha = 0.5,
          hex_direction = direction
        )
        stopifnot(file.exists("temp.png"), file.size("temp.png") > 0)
        stopifnot(length(dim(png::readPNG("temp.png"))) == 3L)
        expected_margin <- 1.02 + width * 0.04
        stopifnot(isTRUE(all.equal(
          sticker$scales$get_scales("y")$limits,
          c(1 - expected_margin, 1 + expected_margin)
        )))
        unlink("temp.png")
      }
    }
  })
})
