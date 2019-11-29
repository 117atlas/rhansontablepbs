#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("RHandsonTable Problems"),

    mainPanel(
        tabBox(width = "100%",
            tabPanel(
                h5(strong("First table")),
                br(),
                fluidRow(
                    column(width = 8,
                        numericInput("nbr_col", "Number of columns in the tables", min = 3, max = 17, value = 8)
                    ),
                    column(width = 2,
                        actionButton("decrease", "Decrease number", icon = icon("minus"))
                    ),
                    column(width = 2,
                        actionButton("increase", "Increase number", icon = icon("plus"))
                    ),
                    tags$style(type='text/css', "#decrease {margin-top:14%} #increase {margin-top:14%}")
                ),
                br(),
                fluidRow(
                    column(width = 12,
                        rHandsontableOutput("first")
                    )
                )
            ),
            tabPanel(
                h5(strong("Second table")),
                br(),
                fluidRow(
                    column(width = 12,
                           rHandsontableOutput("second")
                    )
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    buildTab1 <- function(intervals){
        if(!is.null(intervals)){
            output$first <-rhandsontable::renderRHandsontable({
                StrEval1= "dtf = data.frame("
                StrEval2="ColHeaders = c("
                StrEval3="colWidths = c("
                
                colwidths = 0
                if (length(intervals) <= 8) colwidths = 100
                else if (length(intervals) <= 11) colwidths = 90
                else colwidths = 1005/length(intervals)
                if (colwidths < 70) colwidths = 70
                
                oldData = NULL
                if (!is.null(input$first)){
                    CTR = hot_to_r(input$first)
                    if (!is.null(CTR) && length(CTR) > 0){
                        for (iter in 1:length(CTR)){
                            oldData[iter] = as.numeric(CTR[,iter])
                        }
                    }
                }
                
                for(iter in 1:(length(intervals)-1)){
                    StrEval1 = paste0(StrEval1, 'classe', iter,'=c(', ifelse(iter<=length(oldData) && !is.na(oldData[iter]), paste0('"', oldData[iter], '"'), '"100"'),'), ')
                    StrEval2 = paste0(StrEval2, '"[', intervals[iter],', ', intervals[iter+1], '[", ')
                    StrEval3 = paste0(StrEval3, paste0(colwidths,', '))
                }
                StrEval1 = paste0(StrEval1, 'classe', length(intervals),'=c(', ifelse(length(intervals)<=length(oldData) && !is.na(oldData[length(intervals)]), paste0('"', oldData[length(intervals)], '"'), '"100"'),'), stringsAsFactors = FALSE)')
                eval(parse(text = StrEval1))
                StrEval2 = paste0(StrEval2, '"[', intervals[length(intervals)],', Inf[")')
                eval(parse(text = StrEval2))
                StrEval3 = paste0(StrEval3, paste0(colwidths,')'))
                eval(parse(text = StrEval3))
                
                
                rhandsontable::rhandsontable(dtf, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "1000px") %>%
                    hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE)%>%
                    hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=TRUE, fixedColumnsLeft=1) %>%
                    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)%>%
                    hot_col(col = 1:length(ColHeaders), halign = "htCenter")
            })
        }
    }
    
    buildTab2 <- function(intervals){
        if(!is.null(intervals)){
            output$second <-rhandsontable::renderRHandsontable({
                StrEval1= "dtf = data.frame("
                StrEval2="ColHeaders = c("
                StrEval3="colWidths = c("
                
                colwidths = 0
                if (length(intervals) <= 8) colwidths = 100
                else if (length(intervals) <= 11) colwidths = 90
                else colwidths = 1005/length(intervals)
                if (colwidths < 70) colwidths = 70
                
                oldData = NULL
                if (!is.null(input$second)){
                    NDC = hot_to_r(input$second)
                    if (!is.null(NDC) && length(NDC) > 0){
                        for (iter in 1:length(NDC)){
                            oldData[iter] = as.numeric(NDC[,iter])
                        }
                    }
                }
                
                for(iter in 1:(length(intervals)-1)){
                    StrEval1 = paste0(StrEval1, 'classe', iter,'=c(', ifelse(iter<=length(oldData) && !is.na(oldData[iter]) && oldData[iter]!=0, paste0('"', oldData[iter], '"'), '""'),'), ')
                    StrEval2 = paste0(StrEval2, '"[', intervals[iter],', ', intervals[iter+1], '[", ')
                    StrEval3 = paste0(StrEval3, paste0(colwidths,', '))
                }
                StrEval1 = paste0(StrEval1, 'classe', length(intervals),'=c(', ifelse(length(intervals) <= length(oldData) && !is.na(oldData[length(intervals)]) && oldData[length(intervals)]!=0, paste0('"', oldData[length(intervals)], '"'), '""'),'), stringsAsFactors = FALSE)')
                eval(parse(text = StrEval1))
                StrEval2 = paste0(StrEval2, '"[', intervals[length(intervals)],', Inf[")')
                eval(parse(text = StrEval2))
                StrEval3 = paste0(StrEval3, paste0(colwidths,')'))
                eval(parse(text = StrEval3))
                
                
                rhandsontable::rhandsontable(dtf, colHeaders = ColHeaders, selectCallback = TRUE, useTypes = TRUE, width = "1000px") %>%
                    hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = TRUE)%>%
                    hot_cols(columnSorting = TRUE, colWidths= colWidths, manualColumnResize=FALSE, fixedColumnsLeft=0) %>%
                    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)%>%
                    hot_col(col = 1:length(ColHeaders), halign = "htCenter")
            })
        }
    }
    
    showDialog <- function(title, message){
        showModal(modalDialog(
            title=title,
            size = "m",
            footer = modalButton("Fermer"),
            message
            )
        )
    }
    
    observeEvent(input$nbr_col, {
        nbrCol = ifelse(is.null(input$nbr_col) || is.na(input$nbr_col), 3, input$nbr_col)
        if (nbrCol < 3){
            updateNumericInput(session, "nbr_col", value = 3)
            showDialog("Erreur", "La valeur minimale du nombre de colonnes est de 3")
        }
        else if (nbrCol > 17){
            updateNumericInput(session, "nbr_col", value = 17)
            showDialog("Erreur", "La valeur maximale du nombre de colonnes est de 17")
        }
        else{
            intervals = buildIntervals(nbrCol)
            buildTab1(intervals)
            buildTab2(intervals)
        }
    })
    
    observeEvent(input$decrease, {
        nbrCol = ifelse(is.null(input$nbr_col) || is.na(input$nbr_col), 3, input$nbr_col)
        if (nbrCol > 3){
            updateNumericInput(session, "nbr_col", value = nbrCol-1)
        }
        else{
            showDialog("Erreur", "La valeur minimale du nombre de colonnes est de 3")
        }
    })
    
    observeEvent(input$increase, {
        nbrCol = ifelse(is.null(input$nbr_col) || is.na(input$nbr_col), 3, input$nbr_col)
        if (nbrCol < 17){
            updateNumericInput(session, "nbr_col", value = nbrCol+1)
        }
        else{
            showDialog("Erreur", "La valeur maximale du nombre de colonnes est de 17")
        }
    })
    
    buildIntervals <- function(nbrCol){
        intervals = seq(10, 10*nbrCol, by=10)
        return(intervals)
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
