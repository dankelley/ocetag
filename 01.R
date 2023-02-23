# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

library(oce)
debug <- 1

library(shiny)
library(shinyBS)
library(shinycssloaders)
library(oce)
library(DBI)
library(RSQLite)

helpMouse <- "<p><i>Mouse</i></p>
<ul>
<li>Click near curve to choose a focus point (which gets coloured purple).</li>
</ul>"

helpKeyboard <- "<p><i>Keyboard</i></p>
<ul>
<li> <i>Zoom and pan</i></p></li>
<ul>
<li> <b>i</b> zoom in near mouse</li>
<li> <b>o</b> zoom out</li>
<!-- <li> <b>O</b> (upper-case <b>o</b>) zoom all the way out</li> -->
<li> <b>j</b> move down in water column</li>
<li> <b>k</b> move up in water column</li>
</ul>
<li><i>Tagging</i></li>
<ul>
<li> <b>0</b> through <b>9</b> tag the focus point with given numeric code</li>
<li> <b>u</b> remove tag on focus point</li>
</ul>
</ul>
"

overallHelp <- c(helpMouse, helpKeyboard)

dbname <- "initial value, which will be overwritten"

msg <- function(...) {
    cat(file=stderr(), ..., sep="")
}

dmsg <- function(...) {
    if (debug > 0) cat(file=stderr(), ..., sep="")
}

dprint <- function(...) {
    if (debug > 0) print(file=stderr(), ...)
}

createDatabase <- function(dbname=getDatabaseName())
{
    if (!file.exists(dbname)) {
        dmsg("creating '", dbname, "'\n")
        con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname)
        dmsg("dan 1\n")
        RSQLite::dbCreateTable(con, "tags",
            c("file"="TEXT",
                level="INT",
                tag="INT",
                analyst="TEXT",
                analysisTime="TIMESTAMP"))
        dmsg("dan 2\n")
        #RSQLite::dbWriteTable(con, "tags")
        RSQLite::dbDisconnect(con)
        dmsg("dan 3\n")
    }
}

getTags <- function(file=NULL, dbname=getDatabaseName())
{
    if (file.exists(dbname)) {
        con <- dbConnect(RSQLite::SQLite(), dbname)
        tags <- RSQLite::dbReadTable(con, "tags")
        if (!is.null(file)) {
            dmsg("should focus only on file \"", state$file, "\"\n")
        }
        RSQLite::dbDisconnect(con)
        tags
    } else {
        NULL
    }
}

saveTag <- function(file=NULL, level=NULL, tag=NULL, analyst=NULL, dbname=NULL)
{
    # no checking on NULL; add that if we want to generalize
    df <- data.frame(file=file, level=level, tag=tag, analyst=analyst, analysisTime=Sys.time())
    dprint(df)
    #dmsg("saveTag(file=", file, ", level=", level, ", tag=", tag, ", analyst=", analyst, ", dbname=", dbname, ")")
    con <- dbConnect(RSQLite::SQLite(), dbname)
    RSQLite::dbAppendTable(con, "tags", df)
    RSQLite::dbDisconnect(con)
}

findNearestLevel <- function(x, y, usr, data, view)
{
    dmsg("findNearestLevel(", x, ",", y, "...)\n")
    dmsg("  ", vectorShow(usr))
    dx2 <- diff(usr[1:2])^2
    dy2 <- diff(usr[3:4])^2
    dmsg("  ", vectorShow(dx2))
    dmsg("  ", vectorShow(dy2))
    if (view == "T profile") {
        d2 <- (x - data$temperature)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
        dmsg(sprintf("  T=%.3f, p=%.3f -> index=%d\n", x, y, nearest))
    } else if (view == "S profile") {
        d2 <- (x - data$salinity)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
        dmsg(sprintf("  S=%.3f, p=%.3f -> index=%d\n", x, y, nearest))
    } else if (view == "TS") {
        d2 <- (x - data$salinity)^2/dx2 + (y - data$temperature)^2/dy2
        nearest <- which.min(d2)
        dmsg(sprintf("  S=%.3f, p=%.3f -> index=%d\n", x, y, nearest))
    } else {
        stop("view=\"", view, "\" is not handled yet")
    }
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
    highlight=list(cex=3, col="purple", lwd=4, pch=5),
    join=list(cex=1.4, col=rgb(0.8,0,0.8,alpha=0.5), pch=0, lwd=4.0,lwdSymbol=4.0, type="o"),
    profile=list(cex=1, col="gray", lwd=2, pch=20, type="o"),
    selected=list(cex=3, col="purple", lwd=4, pch=5),
    tagged=list(cex=1.4, col=2, lwd=2, pch=20),
    #TS=list(cex=1, col="gray", lwd=2, pch=20, type="o"),
    cex=1.0,
    focus=list(cex=2, col="purple", lwd=2, pch=1, minimumSpan=10),
    lwdRect=1)

ui <- fluidPage(
    headerPanel(title="", windowTitle=""),
    tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    style="background:#e6f3ff;cursor:crosshair;",
    wellPanel(
        fluidRow(
            column(1, actionButton("help", "Help")),
            column(1, actionButton("quit", "Quit")),
            column(2, selectInput("view", label=NULL,
                    choices=c("S profile"="S profile", "T profile"="T profile", "TS"="TS"),
                    selected="T profile")),
            column(2, selectInput("yProfile", label=NULL,
                    choices=c("pressure"="pressure", "sigma-theta"="sigmaTheta"),
                    selected="pressure")),
            column(3, selectInput("plotType", label=NULL,
                    choices=c("line"="l", "points"="p", "line+points"="o"),
                selected="o")))),
    wellPanel(
        fluidRow(
            column(12, uiOutput("tagMsg")),
            column(12, uiOutput("levelMsg")))),
    fluidRow(
        uiOutput("plotPanel")))

getUserName <- function()
{
    # FIXME: maybe use Sys.info()[["user"]] ???
    res <- if (.Platform$OS.type == "windows") Sys.getenv("USERNAME") else Sys.getenv("USER")
    if (is.null(res) || 0L == nchar(res))
        res <- "unknown"
    res
}

getDatabaseName <- function(prefix="~/ctd_tag")
{
    file.path(paste0(prefix, "_", getUserName(), ".db"))
}

server <- function(input, output, session) {
    createDatabase()
    file <- "~/data/arctic/beaufort/2012/d201211_0048.cnv"
    ctd <- read.oce(file)
    dbname <<- "ctd.db"
    data <- with(ctd@data, list(pressure=pressure, salinity=salinity, temperature=temperature))
    data$yProfile <- data$pressure
    data$ylabProfile <- resizableLabel("p")
    data$sigmaTheta <- swSigmaTheta(ctd, eos="unesco")
    state <- reactiveValues(
        file=file,
        analyst=getUserName(),
        ctd=ctd,
        data=data,
        ndata=length(data$pressure),
        level=NULL,
        usr=c(0, 1, 0, 1),
        visible=rep(TRUE, length(data$pressure)) # all points visible at the start
        )

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
            message("state$level =", state$level )
            #dmsg(sprintf("clicked at x=%.4f, y=%.4f\n", input$click$x, input$click$y))
            #<><><> if (input$view == "T profile") {
            #<><><>     d2 <- (input$click$x-state$data$temperature)^2 + (input$click$y-state$data$pressure)^2
            #<><><>     state$level <- which.min(d2)
            #<><><>     #sprintf("T=%.3f degC, p=%.3f dbar\n", input$hover$x, input$hover$y)
            #<><><> } else if (input$view == "S profile") {
            #<><><>     d2 <- (input$click$x-state$data$salinity)^2 + (input$click$y-state$data$pressure)^2
            #<><><>     state$level <- which.min(d2)
            #<><><>     #sprintf("S=%.3f, p=%.3f dbar\n", input$hover$x, input$hover$y)
            #<><><> } else if (input$view == "N(z)") {
            #<><><>     # FIXME: code for N2
            #<><><>     d2 <- (input$click$x-state$data$N2)^2 + (input$click$y-state$data$pressure)^2
            #<><><>     state$level <- which.min(d2)
            #<><><>     #sprintf("N=%.3g, p=%.3f dbar\n", input$hover$x, input$hover$y)
            #<><><> } else {
            #<><><>     message("this view, ", input$view, "is not handled")
            #<><><> }
        })

    observeEvent(input$keypressTrigger,
        {
            key <- intToUtf8(input$keypress)
            dmsg(key, "\n")
            if (key %in% as.character(0:9)) {
                if (is.null(state$level )) {
                    showNotification("No focus points")
                } else {
                    if (state$visible[state$level]) {
                        dmsg("  visible. should tag at level ", state$level, "\n")
                        dmsg("  analystName=\"", state$analystName, "\"\n")
                        dmsg("  file=\"", state$file, "\"\n")
                        saveTag(file=state$file, level=state$level, tag=as.integer(key),
                            analyst=state$analyst, dbname=getDatabaseName())
                    } else {
                        showNotification("No focus points in current view")
                    }
                }
            } else if (key == "i") {
                if (!is.null(input$hover$x)) {
                    nearestLevel <- findNearestLevel(input$hover$x, input$hover$y, state$usr, state$data, input$view)
                    span <- sum(state$visible)
                    if (span > default$focus$minimumSpan) {
                        span <- span / 4
                        limits <- limitsTrim(nearestLevel + c(-span/2, span/2), state$ndata)
                        state$visible <- limitsToVisible(limits, state$ndata)
                    }
                }
            } else if (key == "o") {
                limits <- visibleToLimits(state$visible)
                span <- diff(limits)
                limits <- limitsTrim(limits + c(-span, span), state$ndata)
                state$visible <- limitsToVisible(limits, state$ndata)
                #} else if (key == "O") {
                #    state$visible <- rep(TRUE, state$ndata)
            } else if (key == "j") { # move down in water column
                if (!tail(state$visible, 1)) {
                    limits <- visibleToLimits(state$visible)
                    span <- diff(limits)
                    limits <- limitsTrim(limits + (2/3)*span, state$ndata)
                    state$visible <- limitsToVisible(limits, state$ndata)
                }
            } else if (key == "k") { # move up in water column
                if (!head(state$visible, 1)) {
                    limits <- visibleToLimits(state$visible)
                    span <- diff(limits)
                    limits <- limitsTrim(limits - (2/3)*span, state$ndata)
                    state$visible <- limitsToVisible(limits, state$ndata)
                }
            } else if (key == "u") {
                state$level <- NULL
            }
        })

    observeEvent(input$yProfile, {
        #dmsg("observed input$yProfile=\"", input$yProfile, "\"\n")
        if (input$yProfile == "pressure") {
            data$yProfile <<- data$pressure
            data$ylabProfile <<- resizableLabel("p")
        } else {
            data$yProfile <<- data$sigmaTheta
            data$ylabProfile <<- expression(sigma[theta]* " ["* kg/m^3*"]")
        }
    })

    output$levelMsg <- renderText(
        {
            if (!is.null(input$hover$x)) {
                if (input$view == "T profile") {
                    sprintf("T=%.3f degC, p=%.3f dbar\n", input$hover$x, input$hover$y)
                } else if (input$view == "S profile") {
                    sprintf("S=%.3f, p=%.3f dbar\n", input$hover$x, input$hover$y)
                } else if (input$view == "TS") {
                    sprintf("S=%.3g C, T=%.3f\n", input$hover$x, input$hover$y)
                } else if (input$view == "N(z)") {
                    sprintf("N=%.3g, p=%.3f dbar\n", input$hover$x, input$hover$y)
                } else {
                    "FIXME"
                }
            } else {
                "Move cursor over plot to inspect data"
            }
        })

    output$tagMsg <- renderText(
        {
            tags <- getTags()
            if (length(tags$tag) > 0L) {
                paste(length(tags$tag), "tags")
            } else {
                "no tags yet"
            }
        })

    output$plotPanel <- renderUI({
        plotOutput("plot",
            brush=brushOpts("brush", delay=1000, resetOnNew=TRUE),
            hover="hover",
            click="click")
    })

    output$plot <- renderPlot({
        if (input$view == "T profile") {
            par(mar=c(1, 3.3, 3, 1), mgp=c(1.9, 0.5, 0))
            x <- state$data$temperature[state$visible]
            if (input$yProfile == "pressure") {
                y <- state$data$pressure[state$visible]
                ylab <- resizableLabel("p")
            } else if (input$yProfile == "sigmaTheta") {
                y <- state$data$sigmaTheta[state$visible]
                ylab <- expression(sigma[theta] * " [" * kg/m^3*"]")
            }
            #msg("1...T ylab:");print(file=stderr(), ylab)
            plot(x, y, ylim=rev(range(y)),
                type=input$plotType, cex=default$data$cex, col=default$data$col,
                axes=FALSE, xlab="", ylab="")
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$temperature[state$level],
                        if (input$yProfile == "pressure") state$data$pressure[state$level]
                        else state$data$sigmaTheta[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            axis(side=2)
            axis(side=3)
            #msg("2...T ylab:");print(file=stderr(), ylab)
            mtext(ylab, side=2, line=1.9)
            mtext(resizableLabel("T"), side=3, line=1.9)
            box()
        } else if (input$view == "S profile") {
            par(mar=c(1, 3.3, 3, 1), mgp=c(1.9, 0.5, 0))
            x <- state$data$salinity[state$visible]
            if (input$yProfile == "pressure") {
                y <- state$data$pressure[state$visible]
                ylab <- resizableLabel("p")
            } else if (input$yProfile == "sigmaTheta") {
                y <- state$data$sigmaTheta[state$visible]
                ylab <- expression(sigma[theta] * " [" * kg/m^3*"]")
            }
            #msg("1...S ylab:");print(file=stderr(), ylab)
            plot(x, y, ylim=rev(range(y)),
                type=input$plotType, cex=default$data$cex, col=default$data$col,
                axes=FALSE, xlab="", ylab="")
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$salinity[state$level],
                        if (input$yProfile == "pressure") state$data$pressure[state$level]
                        else state$data$sigmaTheta[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            axis(side=2)
            axis(side=3)
            #msg("2...S ylab:");print(file=stderr(), ylab)
            mtext(ylab, side=2, line=1.9)
            mtext(resizableLabel("S"), side=3, line=1.9)
            box()
        } else if (input$view == "TS") {
            par(mar=c(1, 3, 3, 1), mgp=c(1.9, 0.5, 0))
            x <- state$data$salinity[state$visible]
            y <- state$data$temperature[state$visible]
            p <- state$data$pressure[state$visible]
            plotTS(as.ctd(x, y, p), eos="unesco", type=input$plotType)
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$salinity[state$level], state$data$temperature[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
        } else {
            plot(0:1, 0:1, xlab="", ylab="", axes=FALSE, type="n")
            text(0.5, 0.5, paste("ERROR: plot type", input$view, "not handled yet"))
        }
    })
}

shinyApp(ui, server)

