# plot_pi() ================

#' Plot the risk line graph
#' 
#' @param df A data frame with columns CO2 and p_i
#' @param C Current CO2 value
#' @param M Emission rate of CO2 (m^3/h/person)
#' @param q Generation rate of infectious quanta (/h/person)
#' @param t Length of exposure (h)
#' @param n Number of individuals in the room (person)
#' @param nu_out Exhalation filtration efficiency (no units)
#' @param nu_in Inhalation filtration efficiency (no units)
#' @param Ip the prevalence of covid in the community in percent (0 - 100)
#' @param C0 Outdoor CO2 concentration (ppm)
#' @param p pulmonary ventilation rate of a person (m^3/h)
plot_pi = function(df_plt, C, M, q, t, n, nu_out, nu_in, Ip, C0 = 410, 
                   p = 0.48){
    ln1 = NULL
    ln2 = NULL
    ln3 = NULL
    ln4 = ggplot2::scale_y_continuous(labels = calc_lab)
                                        
    if (!is.na(C)){
        p_i = wt_pi(C = C, M = M, q = q, t = t, n = n, nu_out = nu_out,
                    nu_in = nu_in, Ip = Ip, C0 = C0, p = p)
        ln1 = ggplot2::geom_vline(xintercept = C, linetype = 3)
        ln2 = ggplot2::geom_hline(yintercept = p_i, linetype = 3)
        ln3 = ggplot2::scale_x_continuous(breaks = c(C,pretty(df_plt$CO2)))
        ln4 = ggplot2::scale_y_continuous(breaks = c(p_i,
                                      pretty(df_plt$p_i)),
                           labels = calc_lab)
    }
    plt = ggplot2::ggplot(df_plt, ggplot2::aes(x = CO2, y = p_i)) +
        ggplot2::geom_line() +
        ln1 +
        ln2 +
        ln3 +
        ln4 +
        ggplot2::ylab('Calculated probability of infection') +
        ggplot2::ggtitle('Modified Wells-Riley Probability of Infection') +
        ggplot2::theme_bw()
    return(plt)
}

# calc_lab() =========================

#' Calculate the numbers and generate the y-axis label
#' 
#' @param x a vector of numbers between 0 and 1
calc_lab = function(x) {
    n = ifelse(x == 0, 0, round(100/(x*100), 1))
    labs = ifelse(n == 0, '0', paste('1/',n, sep = ''))
    return(labs)
}

# pi_df() ==========================

#' Create a data frame of pi values at different CO2 concentrations
#' 
#' @param CO2_v The vector of possible CO2 concentrations
#' @param M Emission rate of CO2 (m^3/h/person)
#' @param q Generation rate of infectious quanta (/h/person)
#' @param t Length of exposure (h)
#' @param n Number of individuals in the room (person)
#' @param nu_out Exhalation filtration efficiency (no units)
#' @param nu_in Inhalation filtration efficiency (no units)
#' @param Ip the prevalence of covid in the community in percent (0 - 100)
#' @param C0 Outdoor CO2 concentration (ppm)
#' @param p pulmonary ventilation rate of a person (m^3/h)
pi_df = function(CO2_v, M, q, t, n, nu_out, nu_in, Ip, C0 = 415, p = 0.48){
    pi = c()
    for (C in CO2_v){
        pi = c(pi,wt_pi(C = C, M = M, q = q, t = t, n = n, nu_out = nu_out, 
                        nu_in = nu_in, C0 = C0, p = p, Ip = Ip))
    }
    df_plt = data.frame(CO2 = CO2_v, p_i = pi)
    return(df_plt)
}

# wt_pi() ================

#' Calculate the probability of infection at a given CO2 weighted by possible I
#'
#' @param C The indoor CO2 concentration (ppm)
#' @param M Emission rate of CO2 (m^3/h/person)
#' @param q Generation rate of infectious quanta (/h/person)
#' @param t Length of exposure (h)
#' @param n Number of individuals in the room (person)
#' @param nu_out Exhalation filtration efficiency (no units)
#' @param nu_in Inhalation filtration efficiency (no units)
#' @param C0 Outdoor CO2 concentration (ppm)
#' @param p pulmonary ventilation rate of a person (m^3/h)
#' @param Ip the prevalence of covid in the community in percent (0 - 100)
wt_pi = function(C, M, q, t, n, nu_out, nu_in, C0, p, Ip){
    # Start by getting all possible values of I
    Iv = seq(0, n, 1)
    
    # Get a vector of probabilities of infection for each I
    pi_v = c()
    for (I in Iv){
        pi_v = c(pi_v,
                 prob_inf(C = C, M = M, I = I, q = q, t = t, n = n, 
                          nu_out = nu_out, nu_in = nu_in, C0 = C0, p = p))
    }
    
    # Weight the probabilities of infection by the probability of I
    p_i = sum(pi_v * dbinom(Iv, n, Ip/100))
    return(p_i)
}

# prob_inf() ======================

#' Calculate the probability of infection at a given I and CO2
#' 
#' @param C The indoor CO2 concentration (ppm)
#' @param M Emission rate of CO2 (m^3/h/person)
#' @param I Number of infectious individuals in the room (person)
#' @param q Generation rate of infectious quanta (/h/person)
#' @param t Length of exposure (h)
#' @param n Number of individuals in the room (person)
#' @param nu_out Exhalation filtration efficiency (no units)
#' @param nu_in Inhalation filtration efficiency (no units)
#' @param C0 Outdoor CO2 concentration (ppm)
#' @param p pulmonary ventilation rate of a person (m^3/h)
prob_inf = function(C, M, I, q, t, n, nu_out, nu_in, C0, p){
    
    # Convert ppm to pp1
    C0 = C0 * 1e-6
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

# make_combos() ==============

#' Make a comprehensive data frame with all combinations of CO2 and I
#' 
#' @param CO2_v a vector of CO2 values
#' @param n the number of individuals
#' @param p the prevalence in percent of infections in your area
make_combos = function(CO2_v, n, p){
    df = expand.grid(CO2_v, seq(0,n,1))
    colnames(df) = c('CO2','I')
    df = (df
          %>% dplyr::mutate(Pv = dbinom(I, n, p/100)))
    return(df)
}
