# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
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
     } else if (view == "TS") {
        d2 <- (x - data$SA)^2/dx2 + (y - data$CT)^2/dy2
        nearest <- which.min(d2)
    } else {
        stop("view=\"", view, "\" is not handled yet")
    }
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
    #limits[1] <- max(1L, as.integer(limits[1]))
    #limits[2] <- min(ndata, as.integer(limits[2]))
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
ui <- fluidPage(
    #headerPanel(title="", windowTitle=""),
    tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    style="background:#e6f3ff;cursor:crosshair;",
    wellPanel(
        fluidRow(
            column(1, actionButton("help", "Help")),
            column(1, actionButton("quit", "Quit")),
            column(3, selectInput("view", label=NULL,
                    choices=c(
                        "S profile"="S profile",
                        "T profile"="T profile",
                        "spiciness profile",
                        "sigma profile",
                        "TS"="TS"),
                    selected="T profile")),
            # FIXME: only show next for profile types
            column(2, selectInput("yProfile", label=NULL,
                    choices=c(
                        "pressure"="pressure",
                        "sigma"="sigma"),
                    selected="pressure")),
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
        p("Tag Table"),
        fluidRow(
            column(12, uiOutput("databasePanel")))))

#' @importFrom oce as.ctd numberAsPOSIXct plotTS resizableLabel vectorShow
server <- function(input, output, session) {
    file <- normalizePath(shiny::getShinyOption("file"))
    debug <- shiny::getShinyOption("debug", default=0)
    prefix <- shiny::getShinyOption("prefix", default="~/ctdtag")
    plotHeight <- shiny::getShinyOption("plotHeight", default=200)
    dbname <- getDatabaseName(prefix=prefix)
    createDatabase(dbname=dbname, debug=debug-1)
    directory <- shiny::getShinyOption("directory")
    suffix <- shiny::getShinyOption("suffix")
    #dmsg(debug, oce::vectorShow(directory))
    #dmsg(debug, oce::vectorShow(suffix))
    ctd <- oce::read.oce(file)
    data <- list(
        longitude=ctd[["longitude"]][1],
        latitude=ctd[["latitude"]][1],
        pressure=ctd[["pressure"]],
        salinity=ctd[["salinity"]],
        temperature=ctd[["temperature"]],
        CT=ctd[["CT"]],
        SA=ctd[["SA"]],
        sigma0=ctd[["sigma0"]],
        spiciness0=ctd[["spiciness0"]])
    # The next two get altered by user actions
    data$yProfile <- data$pressure
    data$ylabProfile <- resizableLabel("p")
    # 'state', being reactive, creates a gateway between R and the webserver
    # that displays the app. Note that 'step' is used when one R element needs
    # to tell other elements that a change has happened.
    state <- reactiveValues(
        step=0L,
        file=file,
        analyst=getUserName(),
        ctd=ctd,
        data=data,
        ndata=length(data$pressure),
        level=NULL,
        usr=c(0, 1, 0, 1),
        visible=rep(TRUE, length(data$pressure)) # all points visible at the start
    )

    focusIsTagged <- function() {
        !is.null(state$level) && (state$level %in% getTags(state$file, dbname=dbname, debug=debug-1)$level)
    }

    focusTags <- function() {
        if (!is.null(state$level) && !is.null(state$file)) {
            tags <- getTags(state$file, dbname=dbname, debug=debug-1)
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
            stopApp()
        })

    observeEvent(input$click,
        {
            state$level  <- findNearestLevel(input$click$x, input$click$y, state$usr, state$data, input$view)
            #dmsg(debug, "state$level =", state$level, "\n")
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
                        saveTag(file=state$file, level=state$level, tag=as.integer(key), analyst=state$analyst, dbname=dbname, debug=debug-1)
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
                    removeTag(file=state$file, level=state$level, dbname=dbname, debug=debug-1)
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
        #dmsg(debug, "observed input$yProfile=\"", input$yProfile, "\"\n")
        if (input$yProfile == "pressure") {
            state$data$yProfile <<- data$pressure
            state$data$ylabProfile <<- resizableLabel("p")
        } else {
            state$data$yProfile <<- data$sigma0
            state$data$ylabProfile <<- expression(sigma[0]* " ["* kg/m^3*"]")
        }
    })

    output$levelMsg <- renderText(
        {
            pvisible <- data$pressure[state$visible]
            sprintf("CTD file \"%s\" [%.1f to %.1f dbar shown]", state$file, min(pvisible), max(pvisible))
        })

    output$tagMsg <- renderText(
        {
            state$step # to cause shiny to update this
            file <- state$file
            tags <- getTags(state$file, dbname=dbname, debug=debug-1)
            tags <- tags[tags$file == file, ]
            tagMsg <- paste0("[", pluralize(length(tags$tag), "tag"), "]")
            focusMsg <- if (focusIsTagged()) {
                paste0(" (level ", state$level, " tagged: ",
                    paste(focusTags(), collapse=" & "), ")")
            } else {
                ""
            }
            paste0("Database \"", dbname, "\" ", tagMsg, " ", focusMsg)
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
        if (input$view == "T profile") {
            par(mar=c(1, 3.3, 3, 1), mgp=c(1.9, 0.5, 0))
            x <- state$data$CT[state$visible]
            y <- state$data$yProfile[state$visible]
            with(default$Tprofile,
                plot(x, y, ylim=rev(range(y)), xlab="", ylab="", type=input$plotType,
                    axes=FALSE, yaxs="i", cex=cex, col=col, lwd=lwd, pch=pch))
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$CT[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file, dbname=dbname, debug=debug-1)
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
                    axes=FALSE, yaxs="i", cex=cex, col=col, lwd=lwd, pch=pch))
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$SA[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file, dbname=dbname, debug=debug-1)
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
                    axes=FALSE, yaxs="i", cex=cex, col=col, lwd=lwd, pch=pch))
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                dmsg(debug, "sigma profile... ", vectorShow(state$level))
                with(default$focus,
                    points(state$data$sigma0[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file, dbname=dbname, debug=debug-1)
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
                    axes=FALSE, yaxs="i", cex=cex, col=col, lwd=lwd, pch=pch))
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                dmsg(debug, "spiciness profile... ", vectorShow(state$level))
                with(default$focus,
                    points(state$data$spiciness0[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file, dbname=dbname, debug=debug-1)
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

        } else if (input$view == "TS") {
            par(mar=c(1, 3, 3, 1), mgp=c(1.9, 0.5, 0))
            x <- state$data$salinity[state$visible]
            y <- state$data$temperature[state$visible]
            p <- state$data$pressure[state$visible]
            ctd <- as.ctd(x, y, p, longitude=data$longitude[1], latitude=data$latitude[1])
            # Plot empty with visible data, but then add the actual full data.
            # That way, we can see tagged points even if they are in the 4%
            # within-plot buffer zone.  (I am not using xaxs="i" etc because
            # it can put intrusions on the axis.)
            plotTS(ctd, eos=eos, type="n")
            with(default$TS,
                points(state$data$SA, state$data$CT, type=input$plotType,
                    cex=cex, col=col, lwd=lwd, pch=pch))
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$SA[state$level], state$data$CT[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file, dbname=dbname, debug=debug-1)
            if (length(tags$tag) > 0) {
                with(default$tag,
                    points(state$data$SA[tags$level], state$data$CT[tags$level],
                        cex=cex, pch=pch, lwd=lwd, col=1+tags$tag))
            }
        } else {
            plot(0:1, 0:1, xlab="", ylab="", axes=FALSE, type="n")
            text(0.5, 0.5, paste("ERROR: plot type", input$view, "not handled yet"))
        }
    #}, height=500, pointsize=14)
    }, pointsize=14)

    output$databasePanel <- renderUI({
        state$step # to cause shiny to update this
        tags <- getTags(state$file, dbname=dbname, debug=debug-1)
        if (!is.null(tags)) {
            o <- order(tags$level)
            tags <- tags[o, ]
            tags$analysisTime <- numberAsPOSIXct(tags$analysisTime)
            DT::renderDT(tags)
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
#' @param file character value naming a file to tag. NOTE: this
#' argument might go away in the future, or at least become optional,
#' if a directory-searching facility is added.
#'
#' @param prefix character value for the start of the name of the database file.
#'
#' @param plotHeight numeric value for the height of the plot, in pixels.
#'
#' @template debugTemplate
#'
#' @author Dan Kelley
#'
#' @export
ctdtag <- function(file="d201211_0048.cnv", prefix="~/ctdtag", plotHeight=500, debug=0)
{
    shinyOptions(file=file, prefix=prefix, plotHeight=plotHeight, debug=debug)
    shinyApp(ui, server)
}
