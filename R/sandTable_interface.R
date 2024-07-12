#' @title sandTable interface function (shiny)
#'
#' @description Function to create shiny interface
#'
#' @import shiny miniUI dplyr
#'
#' @return none
#'
#' @keywords internal
#'




#not user facing, but still document

sandTable_interface = function() {

    getScenarios = function(sampleData=FALSE) {

        validChoices=lapply(ls(.GlobalEnv), function(df) names(eval(parse(text=df))) %in% c("scenarioName")) #scan through every name of every object
        validChoices=lapply(validChoices, function(myRow) any(myRow)) #reduce this to one entry for each object
        validChoices=unlist(validChoices)


        if (any(validChoices)){
            return(ls(.GlobalEnv)[validChoices])
        } else { #if NO valid choices, and sampleData==TRUE, return a sample dataset

            if (sampleData==TRUE){
                return(c("scenarioMaker::example1_scenario"))
            } else {
                return()
            }

        }

    }






    sandTableUI=shinyUI(
        fluidPage(
            gadgetTitleBar("sandTable"),
            sidebarPanel(
                selectInput('scenario',"Select Scenario",c("None", getScenarios(sampleData=TRUE))),
                htmlOutput("summarizeScenario"),
                width=3
            ),
            mainPanel(
                tabsetPanel(

                    ######################
                    ### Parameters Tab ###
                    ######################
                    tabPanel("Overview", icon = icon("map-o"),
                             checkboxInput("defaultColorsOverview","Use Default Colors",value=TRUE),
                             plotly::plotlyOutput("overviewPlot",width="860px", height="690px")
                    ),

                    tabPanel("Relative Space", icon = icon("bar-chart"),
                             plotOutput(outputId="moneyPlot")
                    ),
                    tabPanel("Relative Distance", icon = icon("line-chart"),
                             checkboxInput("defaultColorsDistance","Use Default Colors",value=TRUE),
                             plotly::plotlyOutput(outputId="distancePlot",width="860px", height="690px")
                    ),
                    tabPanel("Weapon Engagements", icon = icon("dot-circle-o"),
                             #DT::dataTableOutput("engagementTable")
                             rhandsontable::rHandsontableOutput("engagementTable"),
                             actionButton("saveEngagements", "Save Changes to Scenario")
                    )
                )
            )
        )
    )


    sandTableServer <- function(input, output, session) {

        values = reactiveValues(scenario = 'None')

        ################################################################################
        ### When these values are selected, translate them from text into dataframes ###
        ################################################################################

        observeEvent(input$scenario,{
            if (input$scenario != "None"){
                values$scenario=eval(parse(text=input$scenario))
            } else {
                values$scenario="None"

            }
        })






        ###################
        ### Do the plots ###
        ###################

        observe({ ### Plot truth and engagements ###
            req(is.list(values$scenario))

            output$overviewPlot=plotly::renderPlotly({
                overviewPlot=plot_add_engagements(scenario=values$scenario,useDefaultColors=input$defaultColorsOverview)
                overviewPlot$elementId=NULL
                overviewPlot
                })

        })

        observe({ ### Plot money plot ###

            req(is.list(values$scenario))


            output$moneyPlot = renderPlot({plot_money_chart(scenario=values$scenario)})

        })

        observe({ ### Plot relative distance ###

            req(is.list(values$scenario))

            output$distancePlot=plotly::renderPlotly({
                distancePlot=scenarioMaker::plot_distance_data_plotly(scenario=values$scenario,useDefaultColors = input$defaultColorsDistance)
                distancePlot$elementId=NULL #the elementId thing suppresses a meaningless warning
                distancePlot

            })

        })


        output$engagementTable <- rhandsontable::renderRHandsontable({

            rhandsontable::rhandsontable(data=values$scenario$engagementData, stretchH = "all")
        })


        observeEvent(input$saveEngagements,{ ### save data on click
            saveEngagementData()
        })

        saveEngagementData = eventReactive(input$saveEngagements, {
            req(is.list(values$scenario))
            cat("Saving Changes to Scenario\n")
            engagementData=rhandsontable::hot_to_r(input$engagementTable)
            values$scenario$engagementData=engagementData
            assign(as.character(input$scenario),values$scenario,envir=.GlobalEnv)
        })

        output$summarizeScenario=renderText({
            if (is.list(values$scenario)){
                checkTruth=is.data.frame(values$scenario$targetTruth)
                checkOwnShip=is.data.frame(values$scenario$ownShipTruth)
                checkEngagement=is.data.frame(values$scenario$engagementData)

                changesSaved=identical(eval(parse(text=input$scenario)),values$scenario)

                s=sprintf('<b>Scenario Contains:</b><br>Target Truth: %s<br>OwnShip Truth: %s<br>Engagement Data: %s<br>',
                          checkTruth,checkOwnShip,checkEngagement)


                HTML(s)
            }

        })

        observeEvent(input$done, {
            stopApp()
        })

    }
    runGadget(shinyApp(sandTableUI, sandTableServer),viewer = dialogViewer("",width=1200,height=1200))

}

#runGadget(shinyApp(sandTableUI, sandTableServer),viewer = dialogViewer("",width=1200,height=1200))
