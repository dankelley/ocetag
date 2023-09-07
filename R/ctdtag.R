# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# NOTES TO DEVELOPERS
#
# 1. If you add a new plot type, be sure to edit new code into
#    the spots marked BOOKMARK 1, 2 and 3.

eos <- "gsw" # do NOT change this without changing a LOT of other code, too.

requireNamespace("shiny")
requireNamespace("DT")
library(oce)
options(oceEOS=eos)

debug <- 2

helpMouse <- "<p><i>Mouse</i></p>
<ul>
<li>Click near curve to choose a focus point (drawn with a cross).</li>
</ul>"

helpKeyboard <- "<p><i>Keyboard</i></p>
<ul>
<li> <i>Application Control</i></p></li>
<ul>
<li> <b>?</b> show this message</li>
</ul>
<li> <i>Zoom and pan</i></p></li>
<ul>
<li> <b>i</b> zoom in (narrow the 'level' range) near the mouse</li>
<li> <b>o</b> zoom out (widen the 'level' range)</li>
<li> <b>O</b> (upper-case 'o') zoom all the way out</li>
<li> <b>j</b> move down in water column</li>
<li> <b>k</b> move up in water column</li>
</ul>
<li><i>Tagging</i></li>
<ul>
<li> <b>0</b> through <b>9</b> tag the focus point with given numeric code</li>
<li> <b>u</b> remove focus on last-clicked point</li>
<li> <b>x</b> delete the tag on the focus point (if there is one)</li>
</ul>
</ul>
"

overallHelp <- c(helpMouse, helpKeyboard)

findNearestLevel <- function(x, y, usr, data, view, debug=0)
{
    dmsg(debug, "findNearestLevel(", x, ",", y, "..., view=", view, ")\n")
    dx2 <- diff(usr[1:2])^2
    dy2 <- diff(usr[3:4])^2
    # start BOOKMARK 1 OF 3 (find index of data point nearest the mouse)
    if (view == "T profile") {
        d2 <- (x - data$CT)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
    } else if (view == "S profile") {
        d2 <- (x - data$SA)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
    } else if (view == "sigma profile") {
        d2 <- (x - data$sigma0)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
     } else if (view == "spiciness profile") {
        d2 <- (x - data$spiciness0)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
    } else if (view == "scan profile") {
        d2 <- (x - data$scan)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
     } else if (view == "TS") {
        d2 <- (x - data$SA)^2/dx2 + (y - data$CT)^2/dy2
        nearest <- which.min(d2)
    } else {
        stop("view=\"", view, "\" is not handled yet (internal error -- please report)")
    }
    # end BOOKMARK 1 OF 3 (find index of data point nearest the mouse)
    dmsg(debug, "  returning ", nearest, "\n")
    nearest
}

limitsTrim <- function(limits, ndata)
{
    limits <- as.integer(limits)
    limits[1] <- max(1L, limits[1])
    limits[2] <- min(ndata, limits[2])
    limits
}

limitsToVisible <- function(limits, ndata)
{
    if (2 != length(limits))
        stop("limits must be of length 2")
    limits <- as.integer(limits)
    limits[1] <- max(1L, limits[1])
    limits[2] <- min(ndata, limits[2])
    visible <- rep(FALSE, ndata)
    visible[seq(limits[1], limits[2])] <- TRUE
    visible
}

visibleToLimits <- function(visible)
{
    c(which(visible)[1L], 1L + length(visible) - which(rev(visible))[1L])
}

pinVisible <- function(v, max=NULL)
{
    v[1L <= v & v <= max]
}

default <- list(
    data=list(cex=0.6, col="#333333A0", lwd=1, pch=1, type="o"),
    Tprofile=list(cex=0.7, col="#333333A0", lwd=1, pch=1),
    Sprofile=list(cex=0.7, col="#333333A0", lwd=1, pch=1),
    sigmaprofile=list(cex=0.7, col="#333333A0", lwd=1, pch=1),
    spicinessprofile=list(cex=0.7, col="#333333A0", lwd=1, pch=1),
    TS=list(cex=0.7, col=1, lwd=1, pch=1),
    highlight=list(cex=3, col="purple", lwd=4, pch=5),
    join=list(cex=1.4, col=rgb(0.8, 0, 0.8, alpha=0.5), pch=0, lwd=4.0, lwdSymbol=4.0, type="o"),
    profile=list(cex=1, col="gray", lwd=2, pch=20, type="o"),
    selected=list(cex=3, col="purple", lwd=4, pch=5),
    tagged=list(cex=1.4, col=2, lwd=2, pch=20),
    cex=1.0,
    focus=list(cex=2, col="purple", lwd=2, pch=3, minimumSpan=5L),
    tag=list(cex=2, lwd=2, pch=1))

#' @importFrom shiny actionButton br brushOpts column fluidPage fluidRow getShinyOption p
#' plotOutput observeEvent reactiveValues renderPlot renderText renderUI selectInput
#' showNotification shinyApp shinyOptions stopApp stopApp uiOutput wellPanel
#'
#' @importFrom DT renderDT
#'
#' @importFrom graphics axis box mtext par text
#'
#' @importFrom utils head tail
uiCtdtag <- fluidPage(
    tags$head(tags$style(HTML(" .well { padding: 2px; min-height: 10px; margin: 2px;} "))),
    tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    style="background:#e6f3ff;cursor:crosshair;",
    wellPanel(
        fluidRow(
            column(3, uiOutput("fileSelect")),
            column(1, actionButton("help", "Help")),
            column(1, actionButton("quit", "Quit")),
            column(2, selectInput("view", label=NULL,
                    # start BOOKMARK 2 OF 3 (show plot option in selection menu)
                    choices=c(
                        "T profile"="T profile",
                        "S profile"="S profile",
                        "spiciness profile"="spiciness profile",
                        "sigma profile"="sigma profile",
                        "scan profile"="scan profile",
                        "TS"="TS"),
                    selected="T profile")),
            conditionalPanel(
                condition="input.view == 'T profile' || input.view == 'S profile' || input.view == 'spiciness profile' || input.view == 'sigma profile'",
                column(2, selectInput("yProfile", label=NULL,
                        choices=c(
                            "pressure"="pressure",
                            "sigma"="sigma"),
                        selected="pressure"))),
            # end BOOKMARK 2 OF 3 (show plot option in selection menu)
            column(2, selectInput("plotType", label=NULL,
                    choices=c("line"="l", "points"="p", "line+points"="o"),
                    selected="o")))),
    wellPanel(
        fluidRow(
            column(12, uiOutput("levelMsg")),
            column(12, uiOutput("tagMsg")))),
    wellPanel(
        fluidRow(
            uiOutput("plotPanel"))),
    wellPanel(
        uiOutput("databaseHeader"),
        fluidRow(
            column(12, uiOutput("databasePanel")))))

#' @importFrom oce as.ctd numberAsPOSIXct plotTS resizableLabel vectorShow
serverCtdtag <- function(input, output, session) {
    path <- shiny::getShinyOption("path", default=".")
    suffix <- shiny::getShinyOption("suffix", default=".cnv")
    dbprefix <- shiny::getShinyOption("dbprefix", default="~/ctdtag")
    mapping <- shiny::getShinyOption("mapping", default=list())
    plotHeight <- shiny::getShinyOption("plotHeight", default=200)
    debug <- shiny::getShinyOption("debug", default=0)
    # set up database
    dbname <- getDatabaseName(dbprefix=dbprefix)
    createDatabase(dbname=dbname, mapping=mapping, debug=debug-1)
    # 'state', being reactive, creates a gateway between R and the webserver
    # that displays the app. Note that 'step' is used when one R element needs
    # to tell other elements that a change has happened.
    state <- reactiveValues(
        step=0L,
        analyst=getUserName(),
        file=NULL,         # set by observeEvent(input$fileSelect)
        fileWithPath=NULL, # set by observeEvent(input$fileSelect)
        ctd=NULL,          # set by observeEvent(input$fileSelect)
        data=NULL,         # set by observeEvent(input$fileSelect)
        ndata=NULL,        # set by observeEvent(input$fileSelect)
        level=NULL,        # set by observeEvent(input$fileSelect)
        scan=NULL,         # set by observeEvent(input$fileSelect)
        yProfile=NULL,     # set by observeEvent(input$fileSelect)
        ylabProfile=NULL,  # set by observeEvent(input$fileSelect)
        visible=NULL,      # set by observeEvent(input$fileSelect)
        usr=c(0, 1, 0, 1))

    focusIsTagged <- function()
        !is.null(state$level) && (state$level %in% getTags(state$fileWithPath, dbname=dbname, debug=debug-1)$level)

    focusTags <- function() {
        if (!is.null(state$level) && !is.null(state$file)) {
            tags <- getTags(state$fileWithPath, dbname=dbname, debug=debug-1)
            tags[tags$level==state$level, "tag"]
        }
    }

    observeEvent(input$help,
        {
            shiny::showModal(shiny::modalDialog(title=NULL,
                    size="xl", shiny::HTML(overallHelp), easyClose=TRUE))
        })

    observeEvent(input$quit,
        {
            stopApp(invisible(""))
        })

    observeEvent(input$click,
        {
            state$level <<- findNearestLevel(input$click$x, input$click$y, state$usr, state$data, input$view)
            state$scan <<- state$data$scan[state$level]
            dmsg(debug, "observeEvent(input$click) set state$level=", state$level, ", state$scan=", state$scan, "\n")
        })

    observeEvent(input$keypressTrigger,
        {
            key <- intToUtf8(input$keypress)
            #dmsg(debug, key, "\n")
            if (key %in% as.character(0:9)) {
                if (is.null(state$level)) {
                    showNotification("No focus points")
                } else {
                    dmsg(debug, "responding to '", key, "' to tag the focus point\n")
                    if (state$visible[state$level]) {
                        saveTag(file=state$fileWithPath, level=state$level, scan=state$data$scan[state$level],
                            tag=as.integer(key), analyst=state$analyst, dbname=dbname, debug=debug-1)
                        state$step <<- state$step + 1 # other shiny elements notice this
                    } else {
                        showNotification("No focus points in current view")
                    }
                }
            } else if (key == "i") {
                if (!is.null(input$hover$x)) {
                    dmsg(debug, "responding to 'i': zoom in\n")
                    nearestLevel <- findNearestLevel(input$hover$x, input$hover$y, state$usr,
                        state$data, input$view)
                    span <- sum(state$visible)
                    if (span > default$focus$minimumSpan) {
                        span <- span / 4
                        limits <- limitsTrim(nearestLevel + c(-span/2, span/2), state$ndata)
                        state$visible <- limitsToVisible(limits, state$ndata)
                    }
                }
            } else if (key == "o") {
                dmsg(debug, "responding to 'o': zoom out\n")
                limits <- visibleToLimits(state$visible)
                span <- diff(limits)
                limits <- limitsTrim(limits + c(-span, span), state$ndata)
                state$visible <- limitsToVisible(limits, state$ndata)
            } else if (key == "O") {
                dmsg(debug, "responding to 'O': view full-scale\n")
                state$visible <- rep(TRUE, state$ndata)
            } else if (key == "j") {
                dmsg(debug, "responding to 'j': move down in water column\n")
                if (!tail(state$visible, 1)) {
                    limits <- visibleToLimits(state$visible)
                    span <- diff(limits)
                    limits <- limitsTrim(limits + (2/3)*span, state$ndata)
                    state$visible <- limitsToVisible(limits, state$ndata)
                }
            } else if (key == "k") {
                dmsg(debug, "responding to 'k': move up in water column\n")
                if (!head(state$visible, 1)) {
                    limits <- visibleToLimits(state$visible)
                    span <- diff(limits)
                    limits <- limitsTrim(limits - (2/3)*span, state$ndata)
                    state$visible <- limitsToVisible(limits, state$ndata)
                }
            } else if (key == "x") {
                dmsg(debug, "responding to 'x' to remove tag\n")
                if (focusIsTagged()) {
                    removeTag(file=state$fileWithPath, level=state$level, dbname=dbname, debug=debug-1)
                    state$step <<- state$step + 1 # other shiny elements notice this
                }
            } else if (key == "u") {
                dmsg(debug, "responding to 'u' to remove focus point (i.e. crossed point)\n")
                state$level <<- NULL
                state$step <<- state$step + 1 # other shiny elements notice this
            } else if (key == "?") {
                shiny::showModal(shiny::modalDialog(title=NULL,
                        size="xl", shiny::HTML(overallHelp), easyClose=TRUE))
            }
        })

    observeEvent(input$yProfile, {
        dmsg(debug, "observing input$yProfile=\"", input$yProfile, "\"\n")
        if (input$yProfile == "pressure") {
            state$data$yProfile <<- state$data$pressure
            state$data$ylabProfile <<- resizableLabel("p")
        } else {
            state$data$yProfile <<- state$data$sigma0
            state$data$ylabProfile <<- expression(sigma[0]* " ["* kg/m^3*"]")
        }
    })

    output$fileSelect <- renderUI(
        {
            availableFiles <- list.files(path, paste0(suffix, "$"), ignore.case=TRUE)
            selectInput("fileSelect", label=NULL, choices=availableFiles, selected=availableFiles[1])
        })

    observeEvent(input$fileSelect,
        {
            dmsg(debug, "observing input$fileSelect=\"", input$fileSelect, "\"\n")
            if (nchar(input$fileSelect) == 0L) {
                msg <- paste0("Directory \"", path, "\" has no files with names ending with \"", suffix, "\"\n")
                stopApp()
                stop(msg)
            }
            state$file <<- input$fileSelect
            state$fileWithPath <<- normalizePath(input$fileSelect)
            ctd <- oce::read.oce(state$file)
            state$ctd <<- ctd
            pressure <- ctd[["pressure"]]
            if (is.null(pressure))
                stop("this ctd object does not contain pressure, so it cannot be analyzed")
            scan <- ctd[["scan"]]
            if (is.null(scan))
                scan <- seq_along(pressure)
            state$data <<- list(
                longitude=ctd[["longitude"]][1],
                latitude=ctd[["latitude"]][1],
                scan=scan,
                pressure=pressure,
                salinity=ctd[["salinity"]],
                temperature=ctd[["temperature"]],
                CT=ctd[["CT"]],
                SA=ctd[["SA"]],
                sigma0=ctd[["sigma0"]],
                spiciness0=ctd[["spiciness0"]])
            ndata <- length(pressure)
            state$level <- NULL # remove focus point
            state$ndata <<- ndata
            state$visible <- rep(TRUE, ndata)
            state$data$yProfile <<- pressure
            state$data$ylabProfile <<- resizableLabel("p")
            state$longitude <- ctd[["longitude"]][1]
            state$latitude <- ctd[["latitude"]][1]
        })

    output$levelMsg <- renderText(
        {
            state$step # to cause shiny to update this
            pvisible <- state$data$pressure[state$visible]
            msg <- ""
            if (any(is.finite(pvisible))) {
                top <- min(pvisible, na.rm=TRUE)
                bot <- max(pvisible, na.rm=TRUE)
                msg <- sprintf("CTD file \"%s\" [%.1f to %.1f dbar shown]", state$fileWithPath, top, bot)
                if (!is.null(state$level)) {
                    p <- state$data$pressure[state$level]
                    if (focusIsTagged()) {
                        msg <- paste(msg,
                            sprintf("(focus at %.1f dbar is tagged %s)", p, paste(focusTags(), collapse="&")))
                    } else {
                        msg <- paste(msg,
                            sprintf("(focus at %.1f dbar)", p))
                    }
                }
            }
            msg
        })

    output$tagMsg <- renderText(
        {
            state$step # to cause shiny to update this
            fileWithPath <- state$fileWithPath
            tags <- getTags(state$fileWithPath, dbname=dbname, debug=debug-1)
            tags <- tags[tags$file == fileWithPath, ]
            tagMsg <- paste0("[", pluralize(length(tags$tag), "tag"), "]")
            paste0("Database \"", dbname, "\" ", tagMsg)#, " ", focusMsg)
        })

    output$databaseHeader <- renderText({
        if (!is.null(state$fileWithPath)) {
            tm <- getTagMapping(dbname=dbname, debug=debug-1L)
            msg <- "<p>"
            if (!is.null(tm) && nrow(tm) > 0L) {
                msg <- paste0(msg, "Tags and their meanings: ")
                ntm <- nrow(tm)
                for (i in seq_len(ntm))
                    msg <- paste0(msg, tm[i, "value"], "=\"", tm[i, "meaning"],
                        if (i < ntm) "\"; " else "\"", sep="")
                msg <- paste0(msg, "</p>")
            }
            msg <- paste0(msg, paste0("Tags for CTD file \"", state$fileWithPath, "\""), "</p>")
            msg
        }
    })

    output$plotPanel <- renderUI({
        state$step # cause a shiny update
        plotOutput("plot",
            brush=brushOpts("brush", delay=1000, resetOnNew=TRUE),
            height=plotHeight,
            hover="hover",
            click="click")
    })

    output$plot <- renderPlot({
        state$step # cause a shiny update
        input$yProfile # cause a shiny update
        # start BOOKMARK 3 OF 3 (create a plot)
        if (input$view == "T profile") {
            par(mar=c(1, 3.3, 3, 1), mgp=c(1.9, 0.5, 0))
            x <- state$data$CT[state$visible]
            y <- state$data$yProfile[state$visible]
            with(default$Tprofile,
                plot(x, y, ylim=rev(range(y)), xlab="", ylab="", type=input$plotType,
                    axes=FALSE, yaxs="r", cex=cex, col=col, lwd=lwd, pch=pch))
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$CT[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$fileWithPath, dbname=dbname, debug=debug-1)
            if (length(tags$tag) > 0) {
                with(default$tag,
                    points(state$data$CT[tags$level], state$data$yProfile[tags$level],
                        cex=cex, pch=pch, lwd=lwd, col=1+tags$tag))
            }
            axis(side=2)
            axis(side=3)
            mtext(state$data$ylab, side=2, line=1.5)
            mtext(resizableLabel("CT"), side=3, line=1.5)
            box()

        } else if (input$view == "S profile") {
            par(mar=c(1, 3, 3, 1), mgp=c(1.9, 0.5, 0))
            x <- state$data$SA[state$visible]
            y <- state$data$yProfile[state$visible]
            with(default$Sprofile,
                plot(x, y, ylim=rev(range(y)), xlab="", ylab="", type=input$plotType,
                    axes=FALSE, yaxs="r", cex=cex, col=col, lwd=lwd, pch=pch))
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$SA[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$fileWithPath, dbname=dbname, debug=debug-1)
            if (length(tags$tag) > 0) {
                with(default$tag,
                    points(state$data$SA[tags$level], state$data$yProfile[tags$level],
                        cex=cex, pch=pch, lwd=lwd, col=1+tags$tag))
            }
            axis(side=2)
            axis(side=3)
            mtext(state$data$ylab, side=2, line=1.5)
            mtext(resizableLabel("SA"), side=3, line=1.5)
            box()

        } else if (input$view == "sigma profile") {
            par(mar=c(1, 3, 3, 1), mgp=c(1.9, 0.5, 0))
            x <- state$data$sigma0[state$visible]
            y <- state$data$yProfile[state$visible]
            with(default$sigmaprofile,
                plot(x, y, ylim=rev(range(y)), xlab="", ylab="", type=input$plotType,
                    axes=FALSE, yaxs="r", cex=cex, col=col, lwd=lwd, pch=pch))
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                dmsg(debug, "sigma profile... ", vectorShow(state$level))
                with(default$focus,
                    points(state$data$sigma0[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$fileWithPath, dbname=dbname, debug=debug-1)
            if (length(tags$tag) > 0) {
                with(default$tag,
                    points(state$data$sigma0[tags$level], state$data$yProfile[tags$level],
                        cex=cex, pch=pch, lwd=lwd, col=1+tags$tag))
            }
            axis(side=2)
            axis(side=3)
            mtext(state$data$ylab, side=2, line=1.5)
            mtext(resizableLabel("sigma0"), side=3, line=1.5)
            box()

        } else if (input$view == "spiciness profile") {
            par(mar=c(1, 3, 3, 1), mgp=c(1.9, 0.5, 0))
            x <- state$data$spiciness0[state$visible]
            y <- state$data$yProfile[state$visible]
            with(default$spicinessprofile,
                plot(x, y, ylim=rev(range(y)), xlab="", ylab="", type=input$plotType,
                    axes=FALSE, yaxs="r", cex=cex, col=col, lwd=lwd, pch=pch))
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                dmsg(debug, "spiciness profile... ", vectorShow(state$level))
                with(default$focus,
                    points(state$data$spiciness0[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$fileWithPath, dbname=dbname, debug=debug-1)
            if (length(tags$tag) > 0) {
                with(default$tag,
                    points(state$data$spiciness0[tags$level], state$data$yProfile[tags$level],
                        cex=cex, pch=pch, lwd=lwd, col=1+tags$tag))
            }
            axis(side=2)
            axis(side=3)
            mtext(state$data$ylab, side=2, line=1.5)
            mtext(resizableLabel("spiciness0"), side=3, line=1.5)
            box()

        } else if (input$view == "scan profile") {
            par(mar=c(1, 3.3, 3, 1), mgp=c(1.9, 0.5, 0))
            x <- state$data$scan[state$visible]
            y <- state$data$yProfile[state$visible]
            with(default$Tprofile,
                plot(x, y, ylim=rev(range(y)), xlab="", ylab="", type=input$plotType,
                    axes=FALSE, yaxs="r", cex=cex, col=col, lwd=lwd, pch=pch))
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$scan[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$fileWithPath, dbname=dbname, debug=debug-1)
            if (length(tags$tag) > 0) {
                with(default$tag,
                    points(state$data$scan[tags$level], state$data$yProfile[tags$level],
                        cex=cex, pch=pch, lwd=lwd, col=1+tags$tag))
            }
            axis(side=2)
            axis(side=3)
            mtext(state$data$ylab, side=2, line=1.5)
            mtext("Scan", side=3, line=1.5)
            box()
 
        } else if (input$view == "TS") {
            par(mar=c(1, 3, 3, 1), mgp=c(1.9, 0.5, 0))
            x <- state$data$salinity[state$visible]
            y <- state$data$temperature[state$visible]
            p <- state$data$pressure[state$visible]
            ctd <- as.ctd(x, y, p, longitude=state$longitude, latitude=state$latitude)
            dmsg(debug, "constructed ctd for TS plot\n")
            # Plot empty with visible data, but then add the actual full data.
            # That way, we can see tagged points even if they are in the 4%
            # within-plot buffer zone.  (I am not using xaxs="i" etc because
            # it can put intrusions on the axis.)
            plotTS(ctd, eos=eos, type="n")
            dmsg(debug, "completed TS plot\n")
            with(default$TS,
                points(state$data$SA, state$data$CT, type=input$plotType,
                    cex=cex, col=col, lwd=lwd, pch=pch))
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$SA[state$level], state$data$CT[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$fileWithPath, dbname=dbname, debug=debug-1)
            if (length(tags$tag) > 0) {
                with(default$tag,
                    points(state$data$SA[tags$level], state$data$CT[tags$level],
                        cex=cex, pch=pch, lwd=lwd, col=1+tags$tag))
            }
        } else {
            plot(0:1, 0:1, xlab="", ylab="", axes=FALSE, type="n")
            text(0.5, 0.5, paste("ERROR: plot type", input$view, "not handled yet"))
        }
        # end BOOKMARK 3 OF 3 (create a plot)
    }, pointsize=14)

    output$databasePanel <- renderUI({
        state$step # to cause shiny to update this
        if (!is.null(state$fileWithPath)) {
            # Show file, pressure, tag, analyst, and analysis time
            tags <- getTags(state$fileWithPath, dbname=dbname, debug=debug-1)
            focus <- tags$file == state$fileWithPath
            tags <- tags[focus, ]
            if (!is.null(tags)) {
                tags$analysisTime <- numberAsPOSIXct(tags$analysisTime)
                tags$pressure <- state$data$pressure[tags$level]
                tags <- tags[order(tags$pressure),]
                DT::renderDT(tags[,c("pressure", "tag", "analyst", "analysisTime")], rownames=FALSE)
            }
        }
    })
}

#' Run an Shiny App for Tagging CTD Features
#'
#' The tags are stored in a SQLite database, for ease of processing in R or other
#' software systems.  The analyst's name and the time of analysis is stored along
#' with each time stamp, to facilitate combining the judgements made by multiple
#' analysts.  A SQLite database is used for this storage of tags.
#'
#' Instructions are provided with the Help button (or by typing `?`). Tagging
#' information is stored in a sqlite3 database file, by default named
#' `ctdtag_USERNAME.db`, where `USERNAME` is the login name of the analyst.
#' If this file does not exist, it is created; otherwise, the existing
#' tags (for the file undergoing analysis) are displayed on the plots,
#' as a starting point.
#'
#' @param path character value naming the directory in which to search
#' for CTD files.
#'
#' @param mapping a list that connects numerical tag codes to character values
#' indicating their meaning.  For example, `list("mixed-layer depth"=0)`
#' might be used in a study of mixed-layer depths.  By default, mappings
#' get set in a simple way, with numerical value 0 mapping to string `"0"`,
#' etc. The value of `mapping` is *only* used in creating a new database.  After that, its
#' value will be ignored if supplied, to avoid the confusion that would arise
#' with multiple intended mappings.  The mapping is
#' stored in the database as a table named `tagMapping`, the contents of
#' which are displayed in the table that appears below the plot.
#'
#' @param suffix character value indicating the file suffix that
#' is taken to indicate CTD files.  This is interpreted in a case-independent
#' manner, so the default value of `"cnv"` would match both `"station1.cnv"`
#' and `"STATION2.CNV"`.
#'
#' @param dbprefix character value for the start of the name of the database file.
#'
#' @param plotHeight numeric value for the height of the plot, in pixels.
#'
#' @template debugTemplate
#'
#' @author Dan Kelley
#'
#' @export
ctdtag <- function(path=".", suffix="cnv", dbprefix="~/ctdtag", mapping=list(), plotHeight=500, debug=0)
{
    shinyOptions(path=path, suffix=suffix, dbprefix=dbprefix, mapping=mapping, plotHeight=plotHeight, debug=debug)
    shinyApp(uiCtdtag, serverCtdtag)
}

