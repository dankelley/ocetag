# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
eos <- "gsw" # do NOT change this without changing a LOT of other code, too.

library(shiny)
library(DT)
library(shinyBS)
library(shinycssloaders)
library(oce)
options(oceEOS=eos)

msg <- function(...)
    cat(file=stderr(), ..., sep="")

dmsg <- function(...)
    if (debug > 0) cat(file=stderr(), ..., sep="")

dprint <- function(...)
    if (debug > 0) print(file=stderr(), ...)

source("database.R") # database functions

debug <- 1

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

pluralize <- function(n=1, singular="item", plural=NULL)
{
    singular <- paste(n, singular)
    if (is.null(plural))
        plural <- paste0(singular, "s")
    if (n == 1L) singular else plural
}


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
            column(12, uiOutput("levelMsg")),
            column(12, uiOutput("tagMsg")))),
    wellPanel(
        fluidRow(
            uiOutput("plotPanel"))),
    br(),
    br(),
    p("Tag Table"),
    br(),
    wellPanel(
        fluidRow(
            column(12, uiOutput("databasePanel")))))

server <- function(input, output, session) {
    createDatabase(debug=debug)
    file <- normalizePath(shiny::getShinyOption("file"))
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
        !is.null(state$level) && (state$level %in% getTags(state$file, debug=debug)$level)
    }

    focusTags <- function() {
        if (!is.null(state$level) && !is.null(state$file)) {
            tags <- getTags(state$file, debug=debug)
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
                            analyst=state$analyst, dbname=getDatabaseName(), debug=debug)
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
                    removeTag(file=state$file, level=state$level, dbname=getDatabaseName(), debug=debug)
                    state$step <<- state$step + 1 # other shiny elements notice this
                }
            } else if (key == "u") {
                dmsg("responding to 'u' to remove focus point (i.e. crossed point)\n")
                state$level <<- NULL
                state$step <<- state$step + 1 # other shiny elements notice this
            } else if (key == "?") {
                shiny::showModal(shiny::modalDialog(title=NULL,
                        size="xl", shiny::HTML(overallHelp), easyClose=TRUE))
            }
        })

    observeEvent(input$yProfile, {
        #dmsg("observed input$yProfile=\"", input$yProfile, "\"\n")
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
            tags <- getTags(state$file, debug=debug)
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
            with(default$Tprofile,
                plot(x, y, ylim=rev(range(y)), xlab="", ylab="", type=input$plotType,
                    axes=FALSE, yaxs="i", cex=cex, col=col, lwd=lwd, pch=pch))
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$CT[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file, debug=debug)
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
            tags <- getTags(state$file, debug=debug)
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
                dmsg("sigma profile... ", vectorShow(state$level))
                with(default$focus,
                    points(state$data$sigma0[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file, debug=debug)
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
                dmsg("spiciness profile... ", vectorShow(state$level))
                with(default$focus,
                    points(state$data$spiciness0[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file, debug=debug)
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
            tags <- getTags(state$file, debug=debug)
            if (length(tags$tag) > 0) {
                with(default$tag,
                    points(state$data$SA[tags$level], state$data$CT[tags$level],
                        cex=cex, pch=pch, lwd=lwd, col=1+tags$tag))
            }
        } else {
            plot(0:1, 0:1, xlab="", ylab="", axes=FALSE, type="n")
            text(0.5, 0.5, paste("ERROR: plot type", input$view, "not handled yet"))
        }
    }, height=500, pointsize=14)

    output$databasePanel <- renderUI({
        state$step # to cause shiny to update this
        tags <- getTags(state$file, debug=debug)
        o <- order(tags$level)
        tags <- tags[o, ]
        tags$analysisTime <- numberAsPOSIXct(tags$analysisTime)
        DT::renderDT(tags)
    })
}

shiny::shinyOptions(file="d201211_0048.cnv")
shinyApp(ui, server)

