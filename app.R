#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)


q_values = c(talking = 1535, quiet = 20)
q_choices = names(q_values)

mask_values = c(`no masks` = 0, `surgical masks` = 0.5, `(K)N95 masks` = 0.9)
mask_choices = names(mask_values)

exert_values = c(`sleeping` = 0.011, `sitting` =  0.01795, 
                 `slow walking` = 0.0280, `light exercise` = 0.0434,
                 `moderate exercise` = 0.0689, `heavy exercise` = 0.0840)
exert_choices = names(exert_values)

# Probability of infection model
prob_inf = function(C, M, I, q, t, n, nu_out, nu_in){
    # C: indoor CO2 concentration in ppm
    # C0: outdoor CO2 concentration in ppm
    # I: number of infectious people
    # M: emission rate of CO2 (m^3/h/person)
    # q: generation rate of infectious quanta (/h)
    # p: pulmonary ventilations rate of a person (m^3/h)
    # t: exposure time (h)
    # n: number of individuals
    # nu_out: exhalation filtration efficiency
    # nu_in: inhalation filtration efficiency
    
    C0 = 415 * 1e-6
    p = 0.48
    C = C * 1e-6
    
    # CO2 correction:
    cc = -(C-C0)/M
    
    # base Wells-Riley
    wr = (I * q * p * t)/n
    
    # Mask correction
    mc = (1 - nu_out)*(1 - nu_in)
    
    #Propability of infection
    p_i = 1 - exp(cc * wr * mc)
    
    return(p_i)
}

make_Pv = function(p, n, Iv){
    v = choose(n, Iv) * (p/100)^Iv *(1-p/100)^(n - Iv)
    names(v) = Iv
    return(v)
}

make_combos = function(CO2_v, Iv, Pv){
    df = expand.grid(CO2_v, Iv)
    df = cbind(df, Pv[as.character(df[,2])])
    colnames(df) = c('CO2', 'Iv', 'Pv')
    df
}

# Front End
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            numericInput('n', label = paste('How many people will be in the',
                                            'room with you?'),
                         value = 5, min = 1, step = 1),
            numericInput('t', label = paste('How long (in minutes) will you be',
                                            'at the event?'),
                         value = 60, min = 1, step = 1),
            numericInput('Ip', label = paste('What is the COVID prevalence in',
                                             'your area (in percent)?'),
                         value = 3, min = 0, step = 1, max = 100),
            selectInput('q', label = 'Will people mostly be talking or quiet?',
                        choices = q_choices),
            selectInput('mask_out', label = paste('What, if any, masks will',
                                                  'others be wearing?'),
                        choices = mask_choices),
            selectInput('mask_in', label = paste('What, if any, masks will you',
                                                 'be wearing?'),
                        choices = mask_choices),
            selectInput('exert', label = paste('What level of exertion will',
                                               'most people be at?'),
                        choices = exert_choices, selected = 'sitting'),
            numericInput('CO2', label = paste('If you know the CO2 value of',
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
    # How many people are infectious?
    CO2_v = seq(450, 2500, 10)
    Iv = reactive({
        seq(0,input$n, 1)
    })
    
    
    # Associated probabilities
    Pv = reactive({
        make_Pv(input$Ip, input$n, Iv())
    })

    # # Find the infection probability for each value of CO2
    combos = reactive({
        make_combos(CO2_v, Iv(), Pv())
    })

    
    df_plt = reactive({
        df = data.frame(combos(),
              PI = prob_inf(C = combos()[,'CO2'],
                       M = exert_values[input$exert],
                       I = combos()[,'Iv'],
                       q = q_values[input$q],
                       t = input$t/60,
                       n = input$n,
                       nu_out = mask_values[input$mask_out],
                       nu_in = mask_values[input$mask_in]))
        (df
            %>% group_by(CO2)
            %>% summarize(PI = sum(PI*Pv)))
    })

    plt = reactive({
        ln1 = NULL
        ln2 = NULL
        ln3 = NULL
        ln4 = scale_y_continuous(labels = function(x) paste0(x*100, '%'))
        if (!is.na(input$CO2)){
            my_p = sum(Pv()* prob_inf(input$CO2,
                            M = exert_values[input$exert],
                            I = Iv(),
                            q = q_values[input$q],
                            t = input$t/60,
                            n = input$n,
                            nu_out = mask_values[input$mask_out],
                            nu_in = mask_values[input$mask_in]))
            my_p = round(as.numeric(my_p), 2)
            ln1 = geom_vline(xintercept = input$CO2, linetype = 3)
            ln2 = geom_hline(yintercept = my_p, linetype = 3)
            ln3 = scale_x_continuous(breaks = c(input$CO2,pretty(df_plt()$CO2)))
            ln4 = scale_y_continuous(breaks = c(my_p,
                                                pretty(df_plt()$PI)),
                                     labels = function(x) paste0(x, '%'))
        }
        ggplot(df_plt(), aes(CO2, PI)) + geom_line() +
            ln1 +
            ln2 +
            ln3 +
            ln4 +
            ylab('Calculated probability of infection') +
            ggtitle('Modified Wells-Riley Probability of Infection') +
            theme_bw()
    })

    output$infplt <- renderPlot({
        plt()
    })

    output$disclaimer <- renderText(paste('This tool has not been tested or',
                                    'validated. It is based on one recently',
                                    'published paper and should not be relied',
                                    'on for safety or infection control. I am',
                                'presenting it here merely as a curiosity.'))
    output$citation <- renderUI({
        HTML(paste('<br/>The modeling used in this app is based on the paper',
    '<a href="https://link.springer.com/article/10.1007/s11356-023-27944-9")>',
    'SARS-CoV-2 airborne infection probability estimated by using indoor',
    'carbon dioxide</a> by Iwamura & Tsutsumi, 2023'))
    })
}

shinyApp(ui = ui, server = server)

# To Do

# gender/age correction (0.9 female, 0.5 child for M)