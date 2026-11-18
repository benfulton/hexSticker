local({
  app_packages <- c(
    "argonDash", "argonR", "colourpicker", "ggplot2", "hexSticker",
    "imager", "png", "shiny", "shinyWidgets", "showtext", "yaml"
  )
  missing <- app_packages[
    !vapply(app_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    message("Skipping project tests; missing packages: ", paste(missing, collapse = ", "))
    return(invisible(NULL))
  }

  app <- new.env()
  sys.source(system.file("polaroid", "app.R", package = "polaroid"), envir = app)
  ui_html <- as.character(htmltools::renderTags(app$ui)$html)
  stopifnot(
    grepl('id="projectdn"', ui_html, fixed = TRUE),
    grepl("Save Project", ui_html, fixed = TRUE),
    grepl("Load Project", ui_html, fixed = TRUE),
    grepl('id="imgdn"', ui_html, fixed = TRUE)
  )

  settings <- list(
    background_color = "#172b4d", background_alpha = 1,
    border_color = "#6772E5", border_width = 1.5, hex_direction = "up",
    img_x = 0.5, img_y = 0.4, img_width = 1, img_height = 1,
    url = "jhk0530/polaroid", url_color = "#FFFFFF",
    url_x = 1, url_y = 0.08, url_size = 6, url_angle = "30",
    pkg_name = "polaroid", pkg_color = "#FFFFFF",
    pkg_x = 1, pkg_y = 1.6, pkg_size = 12, font = ""
  )

  shiny::testServer(app$server, {
    do.call(session$setInputs, settings)
    stopifnot(is.null(values$im), !values$font_loaded)

    path <- output$projectdn
    stopifnot(
      basename(path) == "polaroid_polaroid.yaml",
      isTRUE(all.equal(yaml::read_yaml(path), c(settings, list(background_image = NULL))))
    )

    changed_settings <- list(
      background_color = "#010203", background_alpha = 0.3,
      border_color = "#AABBCC", border_width = 3, hex_direction = "down",
      img_x = -2, img_y = 2, img_width = 10, img_height = 0,
      url = "https://example.org/#label: 'yes'\nsecond line",
      url_color = "#123456", url_x = 0, url_y = 2,
      url_size = 0, url_angle = "0",
      pkg_name = "My Package: #1", pkg_color = "#654321",
      pkg_x = 3, pkg_y = 0, pkg_size = 20, font = "Noto Sans"
    )
    do.call(session$setInputs, c(
      changed_settings,
      list(
        clear = 1, fontApply = 0, polaroid = 0,
        file = data.frame(
          name = "private.png", size = 6 * 1024 * 1024,
          type = "image/png", datapath = "/private/upload.png"
        )
      )
    ))

    path <- output$projectdn
    saved <- yaml::read_yaml(path)
    stopifnot(
      basename(path) == "polaroid_My_Package___1.yaml",
      isTRUE(all.equal(saved, c(changed_settings, list(background_image = NULL)))),
      !any(c("file", "clear", "fontApply", "polaroid") %in% names(saved)),
      is.null(values$im), !values$font_loaded
    )

    updates <- list()
    notifications <- list()
    session$sendInputMessage <- function(inputId, message) {
      updates[[inputId]] <<- message
    }
    session$sendNotification <- function(type, message) {
      notifications[[length(notifications) + 1L]] <<- list(type = type, message = message)
    }
    project_file <- tempfile(fileext = ".yaml")
    image_file <- tempfile(fileext = ".png")
    session$onSessionEnded(function() unlink(c(project_file, image_file)))
    upload_project <- function(project) {
      yaml::write_yaml(project, project_file)
      session$setInputs(project = data.frame(
        name = "project.yaml", size = file.size(project_file),
        type = "application/yaml", datapath = project_file
      ))
    }

    # Older projects restore every control without requiring an image filename.
    upload_project(settings)
    stopifnot(
      setequal(names(updates), names(settings)),
      identical(updates$hex_direction$selected, "up"),
      identical(updates$url_angle$selected, "30"),
      identical(updates$background_color$value, "#172b4d"),
      is.null(values$image_filename),
      inherits(tryCatch(output$imgdn, error = identity), "shiny.silent.error")
    )
    for (name in setdiff(names(settings), c("hex_direction", "url_angle"))) {
      stopifnot(identical(as.character(updates[[name]]$value), as.character(settings[[name]])))
    }

    project <- c(changed_settings, list(background_image = "my background.png"))
    upload_project(project)
    stopifnot(
      is.null(values$im), !values$font_loaded,
      identical(values$image_filename, "my background.png"),
      grepl("my background.png", output$background_filename, fixed = TRUE),
      identical(updates$hex_direction$selected, "down"),
      identical(updates$url_angle$selected, "0"),
      identical(updates$font$value, "Noto Sans")
    )
    do.call(session$setInputs, changed_settings)
    stopifnot(isTRUE(all.equal(yaml::read_yaml(output$projectdn), project)))

    # Invalid files must not partially update controls or the image reference.
    invalid_projects <- list(
      "not a mapping",
      project[-1],
      modifyList(project, list(background_alpha = 2)),
      modifyList(project, list(pkg_x = "wrong")),
      modifyList(project, list(url_angle = 90)),
      modifyList(project, list(hex_direction = "sideways")),
      modifyList(project, list(pkg_color = "invalid")),
      modifyList(project, list(font = list("nested", "values"))),
      modifyList(project, list(background_image = "../private.png")),
      modifyList(project, list(background_image = "C:\\private.png"))
    )
    for (invalid in invalid_projects) {
      updates <- list()
      upload_project(invalid)
      stopifnot(
        length(updates) == 0L,
        identical(values$image_filename, "my background.png"),
        tail(notifications, 1)[[1]]$message$type == "error"
      )
    }
    writeLines("font: [invalid", project_file)
    session$setInputs(project = data.frame(
      name = "broken.yaml", size = file.size(project_file), datapath = project_file
    ))
    stopifnot(tail(notifications, 1)[[1]]$message$type == "error")

    # Only successfully uploaded images change the saved filename.
    png::writePNG(array(1, dim = c(4, 4, 4)), image_file)
    session$setInputs(file = data.frame(
      name = "uploaded.png", size = file.size(image_file),
      type = "image/png", datapath = image_file
    ))
    stopifnot(
      !is.null(values$im), identical(values$image_filename, "uploaded.png"),
      yaml::read_yaml(output$projectdn)$background_image == "uploaded.png"
    )
    writeLines("not a PNG", image_file)
    session$setInputs(file = data.frame(
      name = "broken.png", size = file.size(image_file), datapath = image_file
    ))
    stopifnot(identical(values$image_filename, "uploaded.png"))
    session$setInputs(clear = 2)
    stopifnot(
      is.null(values$image_filename), is.null(values$im),
      is.null(yaml::read_yaml(output$projectdn)$background_image)
    )
  })
})
