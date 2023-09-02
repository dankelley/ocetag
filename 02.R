# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
eos <- "gsw" # do NOT change this without changing a LOT of other code, too.

library(oce)
options(oceEOS=eos)
source("database.R")
debug <- 1

library(shiny)
library(shinyBS)
library(shinycssloaders)

helpMouse <- "<p><i>Mouse</i></p>
<ul>
<li>Click near curve to choose a focus point (drawn with a cross).</li>
</ul>"

helpKeyboard <- "<p><i>Keyboard</i></p>
<ul>
<li> <i>Zoom and pan</i></p></li>
<ul>
<li> <b>i</b> zoom in near mouse</li>
<li> <b>o</b> zoom out</li>
<li> <b>O</b> (upper-case 'o') zoom all the way out</li>
<li> <b>j</b> move down in water column</li>
<li> <b>k</b> move up in water column</li>
</ul>
<li><i>Tagging</i></li>
<ul>
<li> <b>0</b> through <b>9</b> tag the focus point with given numeric code.
EG: 1=top of DD layer, 2=bottom of DD layer, 3=warm-salty peak, 4=cool-fresh peak.</li>
<li> <b>x</b> remove tag on focus point</li>
<li> <b>u</b> remove focus point</li>
</ul>
</ul>
"

overallHelp <- c(helpMouse, helpKeyboard)

pluralize <- function(n=1, singular="item", plural=NULL)
{
    singular <- paste(n, singular)
    if (is.null(plural))
        plural <- paste0(singular, "s")
    if (n == 1L) singular else plural
}

msg <- function(...)
    cat(file=stderr(), ..., sep="")

dmsg <- function(...)
    if (debug > 0) cat(file=stderr(), ..., sep="")

dprint <- function(...)
    if (debug > 0) print(file=stderr(), ...)

findNearestLevel <- function(x, y, usr, data, view)
{
    dmsg("findNearestLevel(", x, ",", y, "..., view=", view, ")\n")
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
    dmsg("  returning ", nearest, "\n")
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
            column(12, uiOutput("levelMsg"))),
            column(12, uiOutput("tagMsg"))),
    fluidRow(
        uiOutput("plotPanel")))



server <- function(input, output, session) {
    createDatabase()
    file <- normalizePath(shiny::getShinyOption("file"))
    ctd <- oce::read.oce(file)
    data <- list(pressure=ctd@data$pressure, salinity=ctd@data$salinity, temperature=ctd@data$temperature)
    data$yProfile <- data$pressure
    data$ylabProfile <- resizableLabel("p")
    data$CT <- ctd[["CT"]]
    data$SA <- ctd[["SA"]]
    data$sigma0 <- ctd[["sigma0"]]
    data$spiciness0 <- ctd[["spiciness0"]]
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
        !is.null(state$level) && (state$level %in% getTags(state$file)$level)
    }

    focusTags <- function() {
        if (!is.null(state$level) && !is.null(state$file)) {
            tags <- getTags(state$file)
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
            #dmsg("state$level =", state$level, "\n")
        })

    observeEvent(input$keypressTrigger,
        {
            key <- intToUtf8(input$keypress)
            #dmsg(key, "\n")
            if (key %in% as.character(0:9)) {
                if (is.null(state$level)) {
                    showNotification("No focus points")
                } else {
                    dmsg("responding to '", key, "' click for tagging\n")
                    if (state$visible[state$level]) {
                        saveTag(file=state$file, level=state$level, tag=as.integer(key),
                            analyst=state$analyst, dbname=getDatabaseName())
                        state$step <<- state$step + 1 # other shiny elements notice this
                    } else {
                        showNotification("No focus points in current view")
                    }
                }
            } else if (key == "i") {
                if (!is.null(input$hover$x)) {
                    dmsg("responding to 'i' click for zooming in\n")
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
                dmsg("responding to 'o' click for zooming out\n")
                limits <- visibleToLimits(state$visible)
                span <- diff(limits)
                limits <- limitsTrim(limits + c(-span, span), state$ndata)
                state$visible <- limitsToVisible(limits, state$ndata)
            } else if (key == "O") {
                dmsg("responding to 'O' click to return to full-scale\n")
                state$visible <- rep(TRUE, state$ndata)
            } else if (key == "j") {
                dmsg("responding to 'j' click for moving down in water column\n")
                if (!tail(state$visible, 1)) {
                    limits <- visibleToLimits(state$visible)
                    span <- diff(limits)
                    limits <- limitsTrim(limits + (2/3)*span, state$ndata)
                    state$visible <- limitsToVisible(limits, state$ndata)
                }
            } else if (key == "k") {
                dmsg("responding to 'k' click for moving up in water column\n")
                if (!head(state$visible, 1)) {
                    limits <- visibleToLimits(state$visible)
                    span <- diff(limits)
                    limits <- limitsTrim(limits - (2/3)*span, state$ndata)
                    state$visible <- limitsToVisible(limits, state$ndata)
                }
            } else if (key == "x") {
                dmsg("responding to 'x' to remove tag if focussed\n")
                if (focusIsTagged()) {
                    removeTag(file=state$file, level=state$level, dbname=getDatabaseName())
                    state$step <<- state$step + 1 # other shiny elements notice this
                }
            } else if (key == "u") {
                dmsg("responding to 'u' to remove tag if focussed\n")
                state$level <<- NULL
            }
        })

    observeEvent(input$yProfile, {
        dmsg("observed input$yProfile=\"", input$yProfile, "\"\n")
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
            # FIXME: indent stupidly here, because the tagMsg is indented (why??)
            sprintf("&nbsp;&nbsp;&nbsp;&nbsp;CTD file \"%s\" [%.1f to %.1f dbar shown]", state$file, min(pvisible), max(pvisible))
        })

    output$tagMsg <- renderText(
        {
            state$step # to cause shiny to update this
            file <- state$file
            tags <- getTags(state$file)
            tags <- tags[tags$file == file, ]
            tagMsg <- paste0("[", pluralize(length(tags$tag), "tag"), "]")
            focusMsg <- if (focusIsTagged()) {
                paste0(" (level ", state$level, " tagged: ",
                    paste(focusTags(), collapse=" & "), ")")
            } else {
                ""
            }
            paste0("Database \"", getDatabaseName(), "\" ", tagMsg, " ", focusMsg)
        })

    output$plotPanel <- renderUI({
        state$step # cause a shiny update
        plotOutput("plot",
            brush=brushOpts("brush", delay=1000, resetOnNew=TRUE),
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
            plot(x, y, ylim=rev(range(y)), yaxs="i", type=input$plotType,
                cex=default$Tprofile$cex, col=default$Tprofile$col, lwd=default$Tprofile$lwd, pch=default$Tprofile$pch,
                axes=FALSE, xlab="", ylab="")
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$CT[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file)
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
            plot(x, y, ylim=rev(range(y)), yaxs="i", type=input$plotType,
                cex=default$Sprofile$cex, col=default$Sprofile$col, lwd=default$Sprofile$lwd, pch=default$Sprofile$pch,
                axes=FALSE, xlab="", ylab="")
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                dmsg("S profile... ", vectorShow(state$level))
                with(default$focus,
                    points(state$data$SA[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file)
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
                plot(x, y, ylim=rev(range(y)), yaxs="i", type=input$plotType,
                    cex=cex, col=col, lwd=lwd, pch=pch, axes=FALSE, xlab="", ylab=""))
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                dmsg("sigma profile... ", vectorShow(state$level))
                with(default$focus,
                    points(state$data$sigma0[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file)
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
                plot(x, y, ylim=rev(range(y)), yaxs="i", type=input$plotType,
                    cex=cex, col=col, lwd=lwd, pch=pch, axes=FALSE, xlab="", ylab=""))
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                dmsg("spiciness profile... ", vectorShow(state$level))
                with(default$focus,
                    points(state$data$spiciness0[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file)
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
            x <- state$data$SA[state$visible]
            y <- state$data$CT[state$visible]
            p <- state$data$pressure[state$visible]
            ctd <- as.ctd(x, y, p)
            # Plot empty with visible data, but then add the actual full data.
            # That way, we can see tagged points even if they are in the 4%
            # within-plot buffer zone.  (I am not using xaxs="i" etc because
            # it can put intrusions on the axis.)
            plotTS(ctd, eos=eos, type="n")
            points(state$data$SA, state$data$CT, type=input$plotType,
                cex=default$TS$cex, col=default$TS$col, lwd=default$TS$lwd, pch=default$TS$pch)
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$SA[state$level], state$data$CT[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file)
            if (length(tags$tag) > 0) {
                with(default$tag,
                    points(state$data$SA[tags$level], state$data$CT[tags$level],
                        cex=cex, pch=pch, lwd=lwd, col=1+tags$tag))
            }
        } else {
            plot(0:1, 0:1, xlab="", ylab="", axes=FALSE, type="n")
            text(0.5, 0.5, paste("ERROR: plot type", input$view, "not handled yet"))
        }
    }, height=500, pointsize=16)
}

shiny::shinyOptions(file="d201211_0048.cnv")
shinyApp(ui, server)

