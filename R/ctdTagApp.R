# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# GENERAL NOTES TO DEVELOPERS
#
# 1. If you add a new plot type, be sure to edit new code to locate
# the mouse, to display the plot option, and to do the plot.  There
# may be othere things you need to handle in addition.  The best
# approach is to search on `"T profile"` and to add code near all the
# spots where you find that string. Adding a new plot type is not
# trivial, and doing a partial job may cause more harm than benefit.
#
# 2. Use the formatting conventions that you see here, and do not
# reformat anything other than the specific lines you need to change
# to accomplish your ends.  In particular, PRs that contain long
# sequences of lines changed only format (as opposed to action) will
# be rejected.  The reasons for this are the same as for all
# open-source projects, so I won't belabour the point, except to say
# that I will not provide explanations in my rejection of the PR,
# apart from pointing to this paragraph.

# THE CODE FOR THIS R-SHINY APP

# Please do NOT change the next line, without adding UI elements to
# let the user make the choice, and then changing a LOT of code to
# compensate.  I cannot imagine that the benefit comes anywhere close
# to the cost of doing this. It would be like switching pressure to
# millimetres of mercury ... a lot of code would have to be changed,
# likely wrecking a lot of computation and displays and thus imposing
# a burden on users who don't happen to align with your quirk.
eos <- "gsw"

library("oce")
library("shiny")
options(oceEOS = eos)

debug <- 2

helpApp <- "<p style=\"font-size: 150%; text-align: center;\">Instructions for <tt>ocetag::ctdTagApp()</tt></p>"

helpInstallation <- "<p><b>Installation</b></p>
<pre>    remotes::install_github(\"dankelley/ocetag\", ref = \"main\")</pre>
"

helpMouse <- "<p><b>Mouse</b></p>
<ul>
<li>Click near curve to choose a focus point (drawn with a cross).</li>
</ul>"

helpKeyboard <- "<p><b>Keyboard</b></p>
<ul>
<li> <i>Application Control</i></p></li>
<ul>
<li> <b>?</b> show this message</li>
</ul>
<li> <i>Zoom and pan</i></p></li>
<ul>
<li> <b>i</b> zoom in (narrow the 'index' range) near the mouse</li>
<li> <b>o</b> zoom out (widen the 'index' range)</li>
<li> <b>=</b> reset plot to full scale</li>
<li> <b>j</b> move down in water column</li>
<li> <b>k</b> move up in water column</li>
</ul>
<li><i>Tagging</i></li>
<ul>
<li> <b>0</b> through <b>9</b> tag the focus point with given numeric code</li>
<li> <b>u</b> remove focus on last-clicked point</li>
<li> <b>x</b> delete the tag on the focus point (if there is one)</li>
</ul>
<li><i>Adding Notes</i></li>
<ul>
<li> <b>n</b> add a note for whole CTD file or, if there is a visible focus, for that index value</li>
</ul>
</ul>
"

overallHelp <- c(helpApp, helpInstallation, helpMouse, helpKeyboard)

findNearestIndex <- function(x, y, usr, data, view, debug = 0) {
    dmsg(debug, "findNearestIndex(", x, ",", y, "..., view=", view, ")\n")
    dx2 <- diff(usr[1:2])^2
    dy2 <- diff(usr[3:4])^2
    if (view == "T profile") {
        d2 <- (x - data$CT)^2 / dx2 + (y - data$yProfile)^2 / dy2
        nearest <- which.min(d2)
    } else if (view == "S profile") {
        d2 <- (x - data$SA)^2 / dx2 + (y - data$yProfile)^2 / dy2
        nearest <- which.min(d2)
    } else if (view == "sigma profile") {
        d2 <- (x - data$sigma0)^2 / dx2 + (y - data$yProfile)^2 / dy2
        nearest <- which.min(d2)
    } else if (view == "sigma-spiciness") {
        d2 <- (x - data$spiciness0)^2 / dx2 + (y - data$sigma0)^2 / dy2
        nearest <- which.min(d2)
    } else if (view == "spiciness profile") {
        d2 <- (x - data$spiciness0)^2 / dx2 + (y - data$yProfile)^2 / dy2
        nearest <- which.min(d2)
    } else if (view == "scan profile") {
        d2 <- (x - data$scan)^2 / dx2 + (y - data$yProfile)^2 / dy2
        nearest <- which.min(d2)
    } else if (view == "TS") {
        d2 <- (x - data$SA)^2 / dx2 + (y - data$CT)^2 / dy2
        nearest <- which.min(d2)
    } else {
        stop("view=\"", view, "\" is not handled yet (internal error -- please report)")
    }
    dmsg(debug, "  returning ", nearest, "\n")
    nearest
}

limitsTrim <- function(limits, ndata) {
    limits <- as.integer(limits)
    limits[1] <- max(1L, limits[1])
    limits[2] <- min(ndata, limits[2])
    limits
}

limitsToVisible <- function(limits, ndata) {
    if (2 != length(limits)) {
        stop("limits must be of length 2")
    }
    limits <- as.integer(limits)
    limits[1] <- max(1L, limits[1])
    limits[2] <- min(ndata, limits[2])
    visible <- rep(FALSE, ndata)
    visible[seq(limits[1], limits[2])] <- TRUE
    visible
}

visibleToLimits <- function(visible) {
    c(which(visible)[1L], 1L + length(visible) - which(rev(visible))[1L])
}

pinVisible <- function(v, max = NULL) {
    v[1L <= v & v <= max]
}

default <- list(
    data = list(cex = 0.6, col = "#333333A0", lwd = 1, pch = 1, type = "o"),
    Tprofile = list(cex = 0.7, col = "#333333A0", lwd = 1, pch = 1),
    Sprofile = list(cex = 0.7, col = "#333333A0", lwd = 1, pch = 1),
    sigmaprofile = list(cex = 0.7, col = "#333333A0", lwd = 1, pch = 1),
    sigmaspiciness = list(cex = 0.7, col = "#333333A0", lwd = 1, pch = 1),
    spicinessprofile = list(cex = 0.7, col = "#333333A0", lwd = 1, pch = 1),
    TS = list(cex = 0.7, col = 1, lwd = 1, pch = 1),
    highlight = list(cex = 3, col = "purple", lwd = 4, pch = 5),
    join = list(cex = 1.4, col = rgb(0.8, 0, 0.8, alpha = 0.5), pch = 0, lwd = 4.0, lwdSymbol = 4.0, type = "o"),
    profile = list(cex = 1, col = "gray", lwd = 2, pch = 20, type = "o"),
    selected = list(cex = 3, col = "purple", lwd = 4, pch = 5),
    tagged = list(cex = 1.4, col = 2, lwd = 2, pch = 20),
    cex = 1.0,
    focus = list(cex = 2, col = "purple", lwd = 2, pch = 3, minimumSpan = 5L),
    tag = list(cex = 2, lwd = 2, pch = 1)
)

#' @importFrom shiny actionButton br brushOpts column fluidPage
#' fluidRow getShinyOption HTML modalButton modalDialog observeEvent
#' plotOutput reactiveValues removeModal renderPlot renderTable
#' renderText renderUI selectInput showNotification shinyApp
#' shinyOptions showModal stopApp tagList textInput uiOutput
#' wellPanel
#'
#' @importFrom graphics axis box mtext par text
#'
#' @importFrom utils head tail
ctdTagAppUI <- fluidPage(
    # tags$head(tags$style(HTML(" .well { padding: 2px; min-height: 10px; margin: 2px;} "))),
    tags$head(uiOutput("css")),
    tags$script(paste0(
        "$(document).on(\"keypress\", function (e) {",
        "Shiny.onInputChange(\"keypress\", e.which);",
        "Shiny.onInputChange(\"keypressTrigger\", Math.random());",
        "});"
    )),
    style = "background:#e6f3ff;cursor:crosshair;",
    wellPanel(
        fluidRow(
            column(3, uiOutput("fileSelect")),
            column(1, actionButton("help", "Help")),
            column(1, actionButton("quit", "Quit")),
            column(2, selectInput("view",
                label = NULL,
                choices = c(
                    "T profile" = "T profile",
                    "S profile" = "S profile",
                    "spiciness profile" = "spiciness profile",
                    "sigma profile" = "sigma profile",
                    "sigma-spiciness" = "sigma-spiciness",
                    "scan profile" = "scan profile",
                    "TS" = "TS"
                ),
                selected = "T profile"
            )),
            conditionalPanel(
                condition = "input.view == 'T profile' || input.view == 'S profile' || input.view == 'spiciness profile' || input.view == 'sigma profile'",
                column(2, selectInput("yProfile",
                    label = NULL,
                    choices = c(
                        "pressure" = "pressure",
                        "sigma" = "sigma"
                    ),
                    selected = "pressure"
                ))
            ),
            column(2, selectInput("plotType",
                label = NULL,
                choices = c("line" = "l", "points" = "p", "line+points" = "o"),
                selected = "o"
            ))
        )
    ),
    wellPanel(
        fluidRow(
            column(12, uiOutput("indexMsg")),
            column(12, uiOutput("tagMsg"))
        )
    ),
    wellPanel(
        fluidRow(
            uiOutput("plotPanel")
        )
    ),
    wellPanel(
        uiOutput("tagDisplayHeader"),
        fluidRow(
            column(12, uiOutput("tagDisplayPanel"))
        )
    ),
    wellPanel(
        uiOutput("noteDisplayHeader"),
        fluidRow(
            column(12, uiOutput("noteDisplayPanel"))
        )
    )
)

#' @importFrom oce as.ctd numberAsPOSIXct oceSetMetadata plotTS resizableLabel vectorShow
#' @importFrom graphics contour grid
#' @importFrom gsw gsw_spiciness0
## @importFrom DT renderDT
ctdTagAppServer <- function(input, output, session) {
    requireNamespace("gsw")
    path <- shiny::getShinyOption("path", default = ".")
    suffix <- shiny::getShinyOption("suffix", default = ".cnv")
    singleFile <- shiny::getShinyOption("file", default = NULL)
    dbprefix <- shiny::getShinyOption("dbprefix", default = "~/ctdtag")
    mapping <- shiny::getShinyOption("mapping", default = list())
    plotHeight <- shiny::getShinyOption("plotHeight", default = 200)
    reader <- shiny::getShinyOption("reader", default = oce::read.oce)
    if (!is.function(reader)) {
        stop("'reader' is not a function")
        stopApp()
    }
    debug <- shiny::getShinyOption("debug", default = 0)
    # set up database
    dbname <- getDatabaseName(dbprefix = dbprefix)
    useDatabase(dbname = dbname, mapping = mapping, debug = debug - 1)
    # 'state', being reactive, creates a gateway between R and the webserver
    # that displays the app. Note that 'step' is used when one R element needs
    # to tell other elements that a change has happened.
    state <- reactiveValues(
        ignoreKeystrokes = FALSE,
        step = 0L,
        analyst = getUserName(),
        file = NULL, # set by observeEvent(input$fileSelect)
        fileWithPath = NULL, # set by observeEvent(input$fileSelect)
        ctd = NULL, # set by observeEvent(input$fileSelect)
        data = NULL, # set by observeEvent(input$fileSelect)
        ndata = NULL, # set by observeEvent(input$fileSelect)
        index = NULL, # set by observeEvent(input$fileSelect)
        scan = NULL, # set by observeEvent(input$fileSelect)
        yProfile = NULL, # set by observeEvent(input$fileSelect)
        ylabProfile = NULL, # set by observeEvent(input$fileSelect)
        visible = NULL, # set by observeEvent(input$fileSelect)
        usr = c(0, 1, 0, 1)
    )

    focusIsTagged <- function() {
        !is.null(state$index) && (state$index %in% getTags(state$fileWithPath, dbname = dbname, debug = debug - 1)$index)
    }

    focusTags <- function() {
        if (!is.null(state$index) && !is.null(state$file)) {
            tags <- getTags(state$fileWithPath, dbname = dbname, debug = debug - 1)
            tags[tags$index == state$index, "tag"]
        }
    }

    observeEvent(input$help, {
        shiny::showModal(shiny::modalDialog(
            title = NULL,
            size = "xl",
            shiny::HTML(overallHelp),
            easyClose = TRUE
        ))
    })

    observeEvent(input$quit, {
        stopApp(invisible(""))
    })

    observeEvent(input$click, {
        state$index <<- findNearestIndex(input$click$x, input$click$y, state$usr, state$data, input$view)
        state$scan <<- state$data$scan[state$index]
        dmsg(debug, "observeEvent(input$click) set state$index=", state$index, ", state$scan=", state$scan, "\n")
    })

    observeEvent(input$keypressTrigger, {
        if (state$ignoreKeystrokes) {
            return()
        }
        key <- intToUtf8(input$keypress)
        # dmsg(debug, key, "\n")
        if (key %in% as.character(0:9)) {
            if (is.null(state$index)) {
                showNotification("No focus points")
            } else {
                dmsg(debug, "responding to '", key, "' to tag the focus point\n")
                if (state$visible[state$index]) {
                    saveTag(
                        file = state$fileWithPath,
                        index = state$index,
                        tag = as.integer(key),
                        analyst = state$analyst,
                        dbname = dbname,
                        debug = debug - 1L
                    )
                    state$step <<- state$step + 1 # other shiny elements notice this
                } else {
                    showNotification("No focus points in current view")
                }
            }
        } else if (key == "i") {
            # dmsg(1 + debug, "FIRST responding to 'i': zoom in\n")
            dmsg(debug, "responding to 'i'\n")
            if (is.null(input$hover$x)) {
                showNotification("Move the cursor over the plot to make 'i' work")
            } else {
                dmsg(debug, "responding to 'i': zoom in (input$hover$x=", input$hover$x, ")\n")
                nearestIndex <- findNearestIndex(
                    input$hover$x, input$hover$y, state$usr,
                    state$data, input$view
                )
                if (is.finite(nearestIndex)) {
                    span <- sum(state$visible)
                    if (span > default$focus$minimumSpan) {
                        span <- span / 4
                        limits <- limitsTrim(nearestIndex + c(-span / 2, span / 2), state$ndata)
                        state$visible <- limitsToVisible(limits, state$ndata)
                    }
                }
            }
        } else if (key == "o") {
            dmsg(debug, "responding to 'o': zoom out\n")
            limits <- visibleToLimits(state$visible)
            span <- diff(limits)
            limits <- limitsTrim(limits + c(-span, span), state$ndata)
            state$visible <- limitsToVisible(limits, state$ndata)
        } else if (key == "=") {
            dmsg(debug, "responding to '=': view full-scale\n")
            state$visible <- rep(TRUE, state$ndata)
        } else if (key == "j") {
            dmsg(debug, "responding to 'j': move down in water column\n")
            if (!tail(state$visible, 1)) {
                limits <- visibleToLimits(state$visible)
                span <- diff(limits)
                limits <- limitsTrim(limits + (2 / 3) * span, state$ndata)
                state$visible <- limitsToVisible(limits, state$ndata)
            }
        } else if (key == "k") {
            dmsg(debug, "responding to 'k': move up in water column\n")
            if (!head(state$visible, 1)) {
                limits <- visibleToLimits(state$visible)
                span <- diff(limits)
                limits <- limitsTrim(limits - (2 / 3) * span, state$ndata)
                state$visible <- limitsToVisible(limits, state$ndata)
            }
        } else if (key == "n") {
            showModal(noteModal())
        } else if (key == "x") {
            dmsg(debug, "responding to 'x' to remove tag\n")
            if (focusIsTagged()) {
                removeTag(file = state$fileWithPath, index = state$index, dbname = dbname, debug = debug - 1)
                state$step <<- state$step + 1 # other shiny elements notice this
            }
        } else if (key == "u") {
            dmsg(debug, "responding to 'u' to remove focus point (i.e. crossed point)\n")
            state$index <<- NULL
            state$step <<- state$step + 1 # other shiny elements notice this
        } else if (key == "?") {
            shiny::showModal(shiny::modalDialog(
                title = NULL,
                size = "xl",
                shiny::HTML(overallHelp),
                easyClose = TRUE
            ))
        }
    })

    noteModal <- function(message = "") {
        state$ignoreKeystrokes <<- TRUE
        con <- dbConnect(RSQLite::SQLite(), dbname)
        notes <- dbReadTable(con, "notes")
        placeholder <- "Please write notes here."
        if (RSQLite::dbExistsTable(con, "notes")) {
            index <- as.integer(if (is.null(state$index)) 0 else state$index)
            look <- which(notes$file == state$file & notes$index == index)
            if (length(look) > 0) {
                placeholder <- paste(notes[look, "note"], collapse = "/")
            }
        }
        RSQLite::dbDisconnect(con)
        modalDialog(
            textInput("note", message, placeholder = placeholder),
            size = "xl",
            footer = tagList(
                modalButton("Cancel"),
                actionButton("noteOK", "OK")
            )
        )
    }

    observeEvent(input$noteOK, {
        con <- dbConnect(RSQLite::SQLite(), dbname)
        if (RSQLite::dbExistsTable(con, "notes")) {
            state$step <<- state$step + 1 # other shiny elements notice this
            notes <- dbReadTable(con, "notes")
            index <- as.integer(if (is.null(state$index)) 0 else state$index)
            look <- which(notes$file == state$file & notes$index == index)
            dmsg(debug, "look=", look, "\n")
            if (length(look) == 0) {
                dmsg(debug, "adding new note for file=\"", state$file, "\" at index=", index, "\n")
                dbWriteTable(con, "notes",
                    data.frame(file = state$file, index = index, note = input$note),
                    append = TRUE
                )
            } else {
                # FIXME: consider the value of 'index'
                dmsg(debug, "updating note for file=\"", state$file, "\" at index=", index, "\n")
                notes[look, "note"] <- input$note
                dbWriteTable(con, "notes", notes, overwrite = TRUE)
            }
        } else {
            showNotification("ERROR: database has no 'notes' table")
        }
        RSQLite::dbDisconnect(con)
        state$ignoreKeystrokes <<- FALSE
        removeModal()
    })

    observeEvent(input$yProfile, {
        dmsg(debug, "observing input$yProfile=\"", input$yProfile, "\"\n")
        if (input$yProfile == "pressure") {
            state$data$yProfile <<- state$data$pressure
            state$data$ylabProfile <<- resizableLabel("p")
        } else {
            state$data$yProfile <<- state$data$sigma0
            state$data$ylabProfile <<- expression(sigma[0] * " [" * kg / m^3 * "]")
        }
    })

    output$css <- renderUI({
        if (is.null(input$hover$x)) {
            # cat("TEXT\n", file=stderr())
            css <- "#shiny-plot-output {cursor: text;}"
            # css <- "cursor: text;"
        } else {
            # cat("CROSSHAIR\n", file=stderr())
            css <- "#shiny-plot-output {cursor: crosshair;}"
            # css <- "cursor: crosshair;"
        }
        tags$style(HTML(css))
    })

    output$fileSelect <- renderUI({
        if (is.null(singleFile)) {
            availableFiles <- list.files(path, paste0(suffix, "$"), ignore.case = TRUE)
        } else {
            availableFiles <- singleFile
        }
        selectInput("fileSelect", label = NULL, choices = availableFiles, selected = availableFiles[1])
    })

    observeEvent(input$fileSelect, {
        dmsg(debug, "observing input$fileSelect=\"", input$fileSelect, "\"\n")
        if (nchar(input$fileSelect) == 0L) {
            msg <- paste0("Directory \"", path, "\" has no files with names ending with \"", suffix, "\"\n")
            stop(msg)
            stopApp()
        }
        state$file <<- input$fileSelect
        # message(oce::vectorShow(path))
        # message(oce::vectorShow(state$file))
        state$fileWithPath <<- normalizePath(paste0(path, "/", input$fileSelect))
        # message(oce::vectorShow(state$fileWithPath))
        # ctd <- oce::read.oce(state$fileWithPath)
        ctd <- reader(state$fileWithPath)
        if (!inherits(ctd, "ctd")) {
            stop("cannot interpret a CTD object from this data file")
            stopApp()
        }
        if (is.na(ctd[["latitude"]])) {
            ctd <- oceSetMetadata(ctd, "latitude", 45)
            warning("added latitude=45 to CTD object\n")
        }
        if (is.na(ctd[["longitude"]])) {
            ctd <- oceSetMetadata(ctd, "longitude", -40)
            warning("added longitude=-40 to CTD object\n")
        }
        state$ctd <<- ctd
        pressure <- ctd[["pressure"]]
        if (is.null(pressure)) {
            stop("this ctd object does not contain pressure, so it cannot be analyzed")
        }
        scan <- ctd[["scan"]]
        if (is.null(scan)) {
            scan <- seq_along(pressure)
        }
        state$data <<- list(
            longitude = ctd[["longitude"]][1],
            latitude = ctd[["latitude"]][1],
            scan = scan,
            pressure = pressure,
            salinity = ctd[["salinity"]],
            temperature = ctd[["temperature"]],
            CT = ctd[["CT"]],
            SA = ctd[["SA"]],
            sigma0 = ctd[["sigma0"]],
            spiciness0 = ctd[["spiciness0"]]
        )
        ndata <- length(pressure)
        state$index <- NULL # remove focus point
        state$ndata <<- ndata
        state$visible <- rep(TRUE, ndata)
        state$data$yProfile <<- pressure
        state$data$ylabProfile <<- resizableLabel("p")
        state$longitude <- ctd[["longitude"]][1]
        state$latitude <- ctd[["latitude"]][1]
    })

    output$indexMsg <- renderText({
        state$step # to cause shiny to update this
        pvisible <- state$data$pressure[state$visible]
        msg <- ""
        if (any(is.finite(pvisible))) {
            top <- min(pvisible, na.rm = TRUE)
            bot <- max(pvisible, na.rm = TRUE)
            msg <- sprintf("CTD file \"%s\" [%.1f to %.1f dbar shown]", state$fileWithPath, top, bot)
            if (!is.null(state$index)) {
                p <- state$data$pressure[state$index]
                if (focusIsTagged()) {
                    msg <- paste(
                        msg,
                        sprintf("(focus at %.1f dbar is tagged %s)", p, paste(focusTags(), collapse = "&"))
                    )
                } else {
                    msg <- paste(
                        msg,
                        sprintf("(focus at %.1f dbar)", p)
                    )
                }
            }
        }
        msg
    })

    output$tagMsg <- renderText({
        state$step # to cause shiny to update this
        fileWithPath <- state$fileWithPath
        tags <- getTags(state$fileWithPath, dbname = dbname, debug = debug - 1)
        tags <- tags[tags$file == fileWithPath, ]
        tagMsg <- paste0("[", pluralize(length(tags$tag), "tag"), "]")
        paste0("Database \"", dbname, "\" ", tagMsg) # , " ", focusMsg)
    })

    output$tagDisplayHeader <- renderText({
        if (!is.null(state$fileWithPath)) {
            tm <- getTagMapping(dbname = dbname, debug = debug - 1L)
            msg <- "<p>"
            if (!is.null(tm) && nrow(tm) > 0L) {
                msg <- paste0(msg, "Tags and their meanings: ")
                ntm <- nrow(tm)
                for (i in seq_len(ntm)) {
                    msg <- paste0(msg, tm[i, "value"], "=\"", tm[i, "meaning"],
                        if (i < ntm) "\"; " else "\"",
                        sep = ""
                    )
                }
                msg <- paste0(msg, "</p>")
            }
            msg <- paste0(msg, paste0("Tags for CTD file \"", state$fileWithPath, "\""), "</p>")
            msg
        }
    })

    output$noteDisplayHeader <- renderText({
        if (!is.null(state$fileWithPath)) {
            "Notes:"
        }
    })

    output$plotPanel <- renderUI({
        state$step # cause a shiny update
        plotOutput("plot",
            brush = brushOpts("brush", delay = 1000, resetOnNew = TRUE),
            height = plotHeight,
            hover = "hover",
            click = "click"
        )
    })

    output$plot <- renderPlot(
        {
            state$step # cause a shiny update
            input$yProfile # cause a shiny update
            if (input$view == "T profile") {
                par(mar = c(1, 3.3, 3, 1), mgp = c(1.9, 0.5, 0))
                x <- state$data$CT[state$visible]
                y <- state$data$yProfile[state$visible]
                with(
                    default$Tprofile,
                    plot(x, y,
                        ylim = rev(range(y)), xlab = "", ylab = "", type = input$plotType,
                        axes = FALSE, yaxs = "r", cex = cex, col = col, lwd = lwd, pch = pch
                    )
                )
                state$usr <<- par("usr")
                if (!is.null(state$index)) {
                    with(
                        default$focus,
                        points(state$data$CT[state$index], state$data$yProfile[state$index],
                            cex = cex, col = col, lwd = lwd, pch = pch
                        )
                    )
                }
                tags <- getTags(state$fileWithPath, dbname = dbname, debug = debug - 1)
                if (length(tags$tag) > 0) {
                    with(
                        default$tag,
                        points(state$data$CT[tags$index], state$data$yProfile[tags$index],
                            cex = cex, pch = pch, lwd = lwd, col = 1 + tags$tag
                        )
                    )
                }
                axis(side = 2)
                axis(side = 3)
                mtext(state$data$ylab, side = 2, line = 1.5)
                mtext(resizableLabel("CT"), side = 3, line = 1.5)
                box()
            } else if (input$view == "S profile") {
                par(mar = c(1, 3, 3, 1), mgp = c(1.9, 0.5, 0))
                x <- state$data$SA[state$visible]
                y <- state$data$yProfile[state$visible]
                with(
                    default$Sprofile,
                    plot(x, y,
                        ylim = rev(range(y)), xlab = "", ylab = "", type = input$plotType,
                        axes = FALSE, yaxs = "r", cex = cex, col = col, lwd = lwd, pch = pch
                    )
                )
                state$usr <<- par("usr")
                if (!is.null(state$index)) {
                    with(
                        default$focus,
                        points(state$data$SA[state$index], state$data$yProfile[state$index],
                            cex = cex, col = col, lwd = lwd, pch = pch
                        )
                    )
                }
                tags <- getTags(state$fileWithPath, dbname = dbname, debug = debug - 1)
                if (length(tags$tag) > 0) {
                    with(
                        default$tag,
                        points(state$data$SA[tags$index], state$data$yProfile[tags$index],
                            cex = cex, pch = pch, lwd = lwd, col = 1 + tags$tag
                        )
                    )
                }
                axis(side = 2)
                axis(side = 3)
                mtext(state$data$ylab, side = 2, line = 1.5)
                mtext(resizableLabel("SA"), side = 3, line = 1.5)
                box()
            } else if (input$view == "sigma profile") {
                par(mar = c(1, 3, 3, 1), mgp = c(1.9, 0.5, 0))
                x <- state$data$sigma0[state$visible]
                y <- state$data$yProfile[state$visible]
                with(
                    default$sigmaprofile,
                    plot(x, y,
                        ylim = rev(range(y)), xlab = "", ylab = "", type = input$plotType,
                        axes = FALSE, yaxs = "r", cex = cex, col = col, lwd = lwd, pch = pch
                    )
                )
                state$usr <<- par("usr")
                if (!is.null(state$index)) {
                    dmsg(debug, "sigma profile... ", vectorShow(state$index))
                    with(
                        default$focus,
                        points(state$data$sigma0[state$index], state$data$yProfile[state$index],
                            cex = cex, col = col, lwd = lwd, pch = pch
                        )
                    )
                }
                tags <- getTags(state$fileWithPath, dbname = dbname, debug = debug - 1)
                if (length(tags$tag) > 0) {
                    with(
                        default$tag,
                        points(state$data$sigma0[tags$index], state$data$yProfile[tags$index],
                            cex = cex, pch = pch, lwd = lwd, col = 1 + tags$tag
                        )
                    )
                }
                axis(side = 2)
                axis(side = 3)
                mtext(state$data$ylab, side = 2, line = 1.5)
                mtext(resizableLabel("sigma0"), side = 3, line = 1.5)
                box()
            } else if (input$view == "sigma-spiciness") {
                par(mar = c(3, 3, 1, 1), mgp = c(1.9, 0.5, 0))
                x <- state$data$spiciness0[state$visible]
                y <- state$data$sigma0[state$visible]
                with(
                    default$sigmaspiciness,
                    plot(x, y,
                        asp = 1,
                        ylim = rev(range(y)), xlab = "", ylab = "", type = input$plotType,
                        axes = FALSE, yaxs = "r", cex = cex, col = col, lwd = lwd, pch = pch
                    )
                )
                grid()
                state$usr <<- par("usr")
                if (!is.null(state$index)) {
                    dmsg(debug, "sigma profile... ", vectorShow(state$index))
                    with(
                        default$focus,
                        points(state$data$spiciness0[state$index], state$data$sigma0[state$index],
                            cex = cex, col = col, lwd = lwd, pch = pch
                        )
                    )
                }
                tags <- getTags(state$fileWithPath, dbname = dbname, debug = debug - 1)
                if (length(tags$tag) > 0) {
                    with(
                        default$tag,
                        points(state$data$spiciness0[tags$index], state$data$sigma0[tags$index],
                            cex = cex, pch = pch, lwd = lwd, col = 1 + tags$tag
                        )
                    )
                }
                axis(side = 1)
                axis(side = 2)
                mtext(resizableLabel("spiciness0"), side = 1, line = 1.5)
                mtext(resizableLabel("sigma0"), side = 2, line = 1.5)
                box()
            } else if (input$view == "spiciness profile") {
                par(mar = c(1, 3, 3, 1), mgp = c(1.9, 0.5, 0))
                x <- state$data$spiciness0[state$visible]
                y <- state$data$yProfile[state$visible]
                with(
                    default$spicinessprofile,
                    plot(x, y,
                        ylim = rev(range(y)), xlab = "", ylab = "", type = input$plotType,
                        axes = FALSE, yaxs = "r", cex = cex, col = col, lwd = lwd, pch = pch
                    )
                )
                state$usr <<- par("usr")
                if (!is.null(state$index)) {
                    dmsg(debug, "spiciness profile... ", vectorShow(state$index))
                    with(
                        default$focus,
                        points(state$data$spiciness0[state$index], state$data$yProfile[state$index],
                            cex = cex, col = col, lwd = lwd, pch = pch
                        )
                    )
                }
                tags <- getTags(state$fileWithPath, dbname = dbname, debug = debug - 1)
                if (length(tags$tag) > 0) {
                    with(
                        default$tag,
                        points(state$data$spiciness0[tags$index], state$data$yProfile[tags$index],
                            cex = cex, pch = pch, lwd = lwd, col = 1 + tags$tag
                        )
                    )
                }
                axis(side = 2)
                axis(side = 3)
                mtext(state$data$ylab, side = 2, line = 1.5)
                mtext(resizableLabel("spiciness0"), side = 3, line = 1.5)
                box()
            } else if (input$view == "scan profile") {
                par(mar = c(1, 3.3, 3, 1), mgp = c(1.9, 0.5, 0))
                x <- state$data$scan[state$visible]
                y <- state$data$yProfile[state$visible]
                with(
                    default$Tprofile,
                    plot(x, y,
                        ylim = rev(range(y)), xlab = "", ylab = "", type = input$plotType,
                        axes = FALSE, yaxs = "r", cex = cex, col = col, lwd = lwd, pch = pch
                    )
                )
                state$usr <<- par("usr")
                if (!is.null(state$index)) {
                    with(
                        default$focus,
                        points(state$data$scan[state$index], state$data$yProfile[state$index],
                            cex = cex, col = col, lwd = lwd, pch = pch
                        )
                    )
                }
                tags <- getTags(state$fileWithPath, dbname = dbname, debug = debug - 1)
                if (length(tags$tag) > 0) {
                    with(
                        default$tag,
                        points(state$data$scan[tags$index], state$data$yProfile[tags$index],
                            cex = cex, pch = pch, lwd = lwd, col = 1 + tags$tag
                        )
                    )
                }
                axis(side = 2)
                axis(side = 3)
                mtext(state$data$ylab, side = 2, line = 1.5)
                mtext("Scan", side = 3, line = 1.5)
                box()
            } else if (input$view == "TS") {
                par(mar = c(1, 3, 3, 1), mgp = c(1.9, 0.5, 0))
                x <- state$data$salinity[state$visible]
                y <- state$data$temperature[state$visible]
                p <- state$data$pressure[state$visible]
                ctd <- as.ctd(x, y, p, longitude = state$longitude, latitude = state$latitude)
                dmsg(debug, "constructed ctd for TS plot\n")
                # Plot empty with visible data, but then add the actual full data.
                # That way, we can see tagged points even if they are in the 4%
                # within-plot buffer zone.  (I am not using xaxs="i" etc because
                # it can put intrusions on the axis.)
                plotTS(ctd, eos = eos, type = "n")
                # add spiciness0 contours
                usr <- par("usr")
                n <- 100 # increase or decrease as appropriate
                SAgrid <- seq(usr[1], usr[2], length.out = n)
                CTgrid <- seq(usr[3], usr[4], length.out = n)
                g <- expand.grid(SA = SAgrid, CT = CTgrid)
                spiciness <- matrix(gsw_spiciness0(g$SA, g$CT), nrow = n)
                contour(SAgrid, CTgrid, spiciness, lty = 2, labcex = 1, add = TRUE, col = 6)
                dmsg(debug, "completed TS plot\n")
                with(
                    default$TS,
                    points(state$data$SA, state$data$CT,
                        type = input$plotType,
                        cex = cex, col = col, lwd = lwd, pch = pch
                    )
                )
                state$usr <<- par("usr")
                if (!is.null(state$index)) {
                    with(
                        default$focus,
                        points(state$data$SA[state$index], state$data$CT[state$index],
                            cex = cex, col = col, lwd = lwd, pch = pch
                        )
                    )
                }
                tags <- getTags(state$fileWithPath, dbname = dbname, debug = debug - 1)
                if (length(tags$tag) > 0) {
                    with(
                        default$tag,
                        points(state$data$SA[tags$index], state$data$CT[tags$index],
                            cex = cex, pch = pch, lwd = lwd, col = 1 + tags$tag
                        )
                    )
                }
            } else {
                plot(0:1, 0:1, xlab = "", ylab = "", axes = FALSE, type = "n")
                text(0.5, 0.5, paste("ERROR: plot type", input$view, "not handled yet"))
            }
        },
        pointsize = 14
    )

    output$tagDisplayPanel <- renderUI({
        state$step # to cause shiny to update this
        if (!is.null(state$fileWithPath)) {
            # Show file, pressure, tag, analyst, and analysis time
            tags <- getTags(state$fileWithPath, dbname = dbname, debug = debug - 1)
            focus <- tags$file == state$fileWithPath
            tags <- tags[focus, ]
            if (!is.null(tags)) {
                tags$analysisTime <- numberAsPOSIXct(tags$analysisTime)
                tags$pressure <- state$data$pressure[tags$index]
                tags <- tags[order(tags$pressure), ]
                # if (requireNamespace("DT", quietly = TRUE)) {
                #    DT::renderDT(tags[, c("pressure", "tag", "analyst", "analysisTime")], rownames = FALSE)
                # } else {
                table <- tags[, c("index", "pressure", "tag", "analyst", "analysisTime")]
                table$analysisTime <- format(as.POSIXct(table$analysisTime))
                # print(table)
                renderTable(table)
                # }
            }
        }
    })

    output$noteDisplayPanel <- renderUI({
        state$step # to cause shiny to update this
        if (!is.null(state$fileWithPath)) {
            con <- dbConnect(RSQLite::SQLite(), dbname)
            notes <- dbReadTable(con, "notes")
            dbDisconnect(con)
            look <- notes$file == state$file
            if (length(look) > 0) {
                renderTable(notes[look, ])
            }
        }
    })
}

#' Shiny App for Tagging CTD Features
#'
#' The tags are stored in a SQLite database, for ease of processing in R or
#' other software systems.  The analyst's name and the time of analysis is
#' stored along with each time stamp, to facilitate combining the judgements
#' made by multiple analysts.  A SQLite database is used for this storage of
#' tags.
#'
#' Instructions are provided with the Help button (or by typing `?`). Tagging
#' information is stored in a SQLite database file, by default named
#' `ctdtag_USERNAME.db`, where `USERNAME` is the login name of the analyst. If
#' this file does not exist, it is created; otherwise, the existing tags (for
#' the file undergoing analysis) are displayed on the plots, as a starting
#' point.
#'
#' @param path character value naming the directory in which to search for CTD
#' files.
#'
#' @param suffix character value indicating the file suffix that is taken to
#' indicate CTD files.  This is interpreted in a case-independent manner, so the
#' default value of `"cnv"` would match both `"station1.cnv"` and
#' `"STATION2.CNV"`.
#'
#' @param file character value that names a file. If this is given, then the
#' values of `path` and `suffix` are ignored, and the app focusses on this one
#' file alone.
#'
#' @param dbprefix character value for the start of the name of the database
#' file.
#'
#' @param mapping a list that connects numerical tag codes to character values
#' indicating their meaning.  For example, `list("mixed-layer depth"=0)` might
#' be used in a study of mixed-layer depths.  By default, mappings get set in a
#' simple way, with numerical value 0 mapping to string `"0"`, etc. The value of
#' `mapping` is *only* used in creating a new database.  After that, its value
#' will be ignored if supplied, to avoid the confusion that would arise with
#' multiple intended mappings.  The mapping is stored in the database as a table
#' named `tagMapping`, the contents of which are displayed in the table that
#' appears below the plot.
#'
#' @param reader a function used to read the CTD data.  By default, this is
#' [oce::read.oce()]. This ch can read common formats such as `.cnv` files and
#' Argo-flavoured `.nc` files, but if it fails, you may supply your own function
#' here. For example, you might read a `.csv` file and then use [oce::as.ctd()]
#' to create a return value that will suit your purposes.
#'
#' @param plotHeight numeric value for the height of the plot, in pixels.
#'
#' @template debugTemplate
#'
#' @author Dan Kelley
#'
#' @export
ctdTagApp <- function(
    path = ".", suffix = "cnv", file = NULL, dbprefix = "~/ctdtag",
    mapping = list(), reader = oce::read.oce,
    plotHeight = 500, debug = 0) {
    shinyOptions(
        path = path, suffix = suffix, file = file, dbprefix = dbprefix,
        mapping = mapping, plotHeight = plotHeight, reader = reader, debug = debug
    )
    res <- shinyApp(ctdTagAppUI, ctdTagAppServer)
    message("next is return value from shinyApp()")
    print(res)
}
