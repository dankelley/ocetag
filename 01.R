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
<li> <b>0</b> return to full scale</li>
<li> <b>j</b> move down in water column</li>
<li> <b>k</b> move up in water column</li>
</ul>
<li><i>Tagging</i></li>
<ul>
<li> <b>u</b> remove focus</li>
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
                analysisName="TEXT",
                analysisTime="TIMESTAMP"))
        dmsg("dan 2\n")
        #RSQLite::dbWriteTable(con, "tags")
        dmsg("dan 3\n")
        RSQLite::dbDisconnect(con)
        dmsg("dan 4\n")
    }
}


getTags <- function(dbname=getDatabaseName())
{
    con <- dbConnect(RSQLite::SQLite(), dbname)
    tags <- RSQLite::dbReadTable(con, "tags")
    RSQLite::dbDisconnect(con)
    tags
}

findNearestIndex <- function(hover, data, view)
{
    if (view == "T profile") {
        d2 <- (hover$x - data$temperature)^2 + (hover$y - data$pressure)^2
        nearest <- which.min(d2)
        dmsg(sprintf("hovering at T=%.3f, p=%.3f -> index=%d\n",
                hover$x, hover$y, nearest))
    } else if (view == "S profile") {
        d2 <- (hover$x - data$salinity)^2 + (hover$y - data$pressure)^2
        nearest <- which.min(d2)
        dmsg(sprintf("hovering at S=%.3f, p=%.3f -> index=%d\n",
                hover$x, hover$y, nearest))
    } else {
        stop("FIXME: key=i, plot type not S or T")
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
    #minBrush=0.01,                    # ignore brush regions smaller than scale times this
    #first=TRUE,
    #pch=20,
    #map=list(cex=1.0, col="#2297E6A0", lwd=1, pch=20, type="o"),
    #OLD data=list(cex=1.0, col="#2297E6A0", lwd=1, pch=20, type="o"),
    data=list(cex=0.6, col="#333333A0", lwd=1, pch=1, type="o"),
    highlight=list(cex=3, col="purple", lwd=4, pch=5),
    join=list(cex=1.4, col=rgb(0.8,0,0.8,alpha=0.5), pch=0, lwd=4.0,lwdSymbol=4.0, type="o"),
    bad=list(bg="white", cex=1.2, col=2, pch=21, lwd=2),
    profile=list(cex=1, col="gray", lwd=2, pch=20, type="o"),
    selected=list(cex=3, col="purple", lwd=4, pch=5),
    tagged=list(cex=1.4, col=2, lwd=2, pch=20),
    #TS=list(cex=1, col="gray", lwd=2, pch=20, type="o"),
    depth=list(col=gray(rep(0.3,4), alpha=seq(1,0.7,length.out=4)),
        lwd=seq(4,2,length.out=4),
        lty=rep(1,4)), #lty=1:4),#c(1,2,1,3)),
    spice=list(col="olivedrab", lty="dotted", lwd=1.4),
    cex=1.0,
    focus=list(cex=2, col="purple", lwd=2, pch=1, minimumSpan=10),
    lwdRect=1)

ui <- fluidPage(
    headerPanel(
        title="",
        windowTitle=""),
    #p(strong(paste("ctdTag"))),
    tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    style="background:#e6f3ff;cursor:crosshair;",
    wellPanel(
        fluidRow(
            column(1, actionButton("help", "Help")),
            column(1, actionButton("quit", "Quit")),
            column(2, selectInput("view", label=NULL,
                    choices=c("S profile"="S profile", "T profile"="T profile", "TS"="TS"),
                    selected="T profile")),
            column(2, selectInput("yaxis", label=NULL,
                    choices=c("pressure"="pressure", "sigma-theta"="sigthe"),
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
    file <- "d201211_0048.cnv"
    ctd <- read.oce(file)
    dbname <<- "ctd.db"
    data <- with(ctd@data, list(pressure=pressure, salinity=salinity, temperature=temperature))
    data$sigthe <- swSigmaTheta(ctd, eos="unesco")
    state <- reactiveValues(
        ctd=ctd,
        data=data,
        ndata=length(data$pressure),
        nearestIndex=NULL,
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
            #dmsg(sprintf("clicked at x=%.4f, y=%.4f\n", input$click$x, input$click$y))
            if (input$view == "T profile") {
                d2 <- (input$click$x-state$data$temperature)^2 + (input$click$y-state$data$pressure)^2
                state$nearestIndex <- which.min(d2)
                #sprintf("T=%.3f degC, p=%.3f dbar\n", input$hover$x, input$hover$y)
            } else if (input$view == "S profile") {
                d2 <- (input$click$x-state$data$salinity)^2 + (input$click$y-state$data$pressure)^2
                state$nearestIndex <- which.min(d2)
                #sprintf("S=%.3f, p=%.3f dbar\n", input$hover$x, input$hover$y)
            } else if (input$view == "N(z)") {
                # FIXME: code for N2
                d2 <- (input$click$x-state$data$N2)^2 + (input$click$y-state$data$pressure)^2
                state$nearestIndex <- which.min(d2)
                #sprintf("N=%.3g, p=%.3f dbar\n", input$hover$x, input$hover$y)
            } else {
                message("this view, ", input$view, "is not handled")
            }
        })

    observeEvent(input$keypressTrigger,
        {
            key <- intToUtf8(input$keypress)
            if (key == "i") {
                if (!is.null(input$hover$x)) {
                    nearestIndex <- findNearestIndex(input$hover, state$data, input$view)
                    span <- sum(state$visible)
                    if (span > default$focus$minimumSpan) {
                        span <- span / 4
                        limits <- limitsTrim(nearestIndex + c(-span/2, span/2), state$ndata)
                        state$visible <- limitsToVisible(limits, state$ndata)
                    }
                }
            } else if (key == "o") {
                #dmsg("'o'")
                #dprint(state$visible)
                limits <- visibleToLimits(state$visible)
                #dmsg(vectorShow(limits, postscript="original"))
                span <- diff(limits)
                limits <- limitsTrim(limits + c(-span, span), state$ndata)
                #dmsg(vectorShow(limits, postscript="updated"))
                state$visible <- limitsToVisible(limits, state$ndata)
                #?span <- sum(state$visible)
                #?span <- span * 4
                #?limits <- as.integer(state$nearestIndex + c(-span/2, span/2))
                #?limits[1] <- max(1L, limits[1])
                #?limits[2] <- min(state$ndata, limits[2])
                #state$visible <- rep(FALSE, state$ndata)
                #state$visible[seq(limits[1], limits[2])] <- TRUE
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
            } else if (key == "0") {
                state$visible <- rep(TRUE, state$ndata)
            } else if (key == "u") {
                state$nearestIndex <- NULL
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
            if (input$yaxis == "pressure") {
                y <- state$data$pressure[state$visible]
                ylab <- resizableLabel("p")
            } else if (input$yaxis == "sigthe") {
                y <- state$data$sigthe[state$visible]
                ylab <- expression(sigma[theta] * " [" * kg/m^3*"]")
            }
            #msg("1...T ylab:");print(file=stderr(), ylab)
            plot(x, y, ylim=rev(range(y)),
                type=input$plotType, cex=default$data$cex, col=default$data$col,
                axes=FALSE, xlab="", ylab="")
            if (!is.null(state$nearestIndex)) {
                with(default$focus,
                    points(state$data$temperature[state$nearestIndex],
                        if (input$yaxis == "pressure") state$data$pressure[state$nearestIndex] else state$data$sigthe[state$nearestIndex],
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
            if (input$yaxis == "pressure") {
                y <- state$data$pressure[state$visible]
                ylab <- resizableLabel("p")
            } else if (input$yaxis == "sigthe") {
                y <- state$data$sigthe[state$visible]
                ylab <- expression(sigma[theta] * " [" * kg/m^3*"]")
            }
            #msg("1...S ylab:");print(file=stderr(), ylab)
            plot(x, y, ylim=rev(range(y)),
                type=input$plotType, cex=default$data$cex, col=default$data$col,
                axes=FALSE, xlab="", ylab="")
            if (!is.null(state$nearestIndex)) {
                with(default$focus,
                    points(state$data$salinity[state$nearestIndex],
                        if (input$yaxis == "pressure") state$data$pressure[state$nearestIndex] else state$data$sigthe[state$nearestIndex],
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
            if (!is.null(state$nearestIndex)) {
                with(default$focus,
                    points(state$data$salinity[state$nearestIndex], state$data$temperature[state$nearestIndex],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
        } else {
            plot(0:1, 0:1, xlab="", ylab="", axes=FALSE, type="n")
            text(0.5, 0.5, paste("ERROR: plot type", input$view, "not handled yet"))
        }
    })
}

shinyApp(ui, server)

