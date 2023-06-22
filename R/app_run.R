#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
## The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @import shiny
#' @importFrom magrittr `%>%`
## usethis namespace: end

q_values = c(talking = 1535, quiet = 20)
q_choices = names(q_values)

mask_values = c(`no masks` = 0, `surgical masks` = 0.5, `(K)N95 masks` = 0.9)
mask_choices = names(mask_values)

exert_values = c(`sleeping` = 0.011, `sitting` =  0.01795, 
                 `slow walking` = 0.0280, `light exercise` = 0.0434,
                 `moderate exercise` = 0.0689, `heavy exercise` = 0.0840)
exert_choices = names(exert_values)
CO2_v = seq(410, 2500, 10)

# Probability of infection model

runApp = function(){
    
    # Front End
    ui <- fluidPage(
        sidebarLayout(
            sidebarPanel = sidebarPanel(
                numericInput('n', 
                             label = paste('How many people will be in the',
                                           'room with you?'),
                             value = 5, min = 1, step = 1),
                numericInput('t', 
                             label = paste('How long (in minutes) will you be',
                                           'at the event?'),
                             value = 60, min = 1, step = 1),
                numericInput('Ip', 
                             label = paste('What is the COVID prevalence in',
                                           'your area (in percent)?'),
                             value = 3, min = 0, step = 1, max = 100),
                selectInput('q', 
                            label = 'Will people mostly be talking or quiet?',
                            choices = q_choices),
                selectInput('mask_out', 
                            label = paste('What, if any, masks will',
                                          'others be wearing?'),
                            choices = mask_choices),
                selectInput('mask_in', 
                            label = paste('What, if any, masks will you',
                                                     'be wearing?'),
                            choices = mask_choices),
                selectInput('exert', 
                            label = paste('What level of exertion will',
                                                   'most people be at?'),
                            choices = exert_choices, selected = 'sitting'),
                numericInput('CO2', 
                             label = paste('If you know the CO2 value of',
                                           'the location, enter it here. If',
                                           'not leave it blank.'),
                             value = NA, min = 450, step = 1)
            ),
            mainPanel = mainPanel(
                plotOutput('infplt'),
                textOutput('disclaimer'),
                htmlOutput('citation'),
            )
        )
    )
    
    # Back End
    
    server <- function(input, output){
        
        # Create the data frame to plot
        df_plt = reactive({
            pi_df(CO2_v = CO2_v, 
                  M = exert_values[input$exert], 
                  q = q_values[input$q], 
                  t = input$t/60, 
                  n = input$n, 
                  nu_out = mask_values[input$mask_out], 
                  nu_in = mask_values[input$mask_in],
                  Ip = input$Ip)
        })
        
        
        # Create the plot
        
        plt = reactive({
            plot_pi(df_plt(), 
                    C = input$CO2,
                    M = exert_values[input$exert],
                    q = q_values[input$q],
                    t = input$t/60,
                    n = input$n,
                    nu_out = mask_values[input$mask_out],
                    nu_in = mask_values[input$mask_in],
                    Ip = input$Ip)
        })
        
        output$infplt <- renderPlot({
            plt()
        })
        
        output$disclaimer <- renderText(paste('This tool has not been tested',
                                              'or validated. It is based on',
                                              'one recently published paper', 
                                              'and should not be relied on',
                                              'for safety or infection',
                                              'control. I am presenting it',
                                              'here merely as a curiosity.'))
        output$citation <- renderUI({
            HTML(paste('<br/>The modeling used in this app is based on the', 
                       'paper <a',
       'href="https://link.springer.com/article/10.1007/s11356-023-27944-9")>',
                       'SARS-CoV-2 airborne infection probability estimated',
                       'by using indoor carbon dioxide</a> by Iwamura &',
                       'Tsutsumi, 2023'))
        })
    }
    
    shinyApp(ui = ui, server = server)
    
}

# To Do

# gender/age correction (0.9 female, 0.5 child for M)