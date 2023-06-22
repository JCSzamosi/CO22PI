
test_that("make_combos() works", {
    CO2_v = seq(450,700,50)
    n = 10
    p = 0.1
    load('./data/good_combo.RData')
    
    expect_equal(make_combos(CO2_v, n, p), good_combo)
  
})

test_that('prob_inf() works',{
    s = 0.07408171
    expect_equal(prob_inf(C = 1000, M = 0.0280, I = 2, q = 1535, t = 0.5,
                          n = 10, nu_out = 0.5, nu_in = 0.9, C0 = 415,
                          p = 0.48),
    s)
})

test_that('wt_prob() works',{
    s = 0.03711854
    expect_equal(wt_pi(C = 1000, M = 0.0280, q = 1535, t = 0.5,
                          n = 10, nu_out = 0.5, nu_in = 0.9, C0 = 415,
                          p = 0.48, Ip = 10),
                 s)
})

test_that('pi_df() works',{
    CO2_v = seq(450,1000,50)
    s = data.frame(CO2 = CO2_v, p_i = c(0.002297473, 0.005562209, 0.008806655,
                                        0.012030962, 0.015235286, 0.018419776,
                                        0.021584583, 0.024729856, 0.027855742, 
                                        0.030962388, 0.034049938, 0.037118535))
    
    expect_equal(pi_df(CO2_v = CO2_v, M = 0.0280, q = 1535, t = 0.5,
                       n = 10, nu_out = 0.5, nu_in = 0.9, Ip = 10, 
                       C0 = 415, p = 0.48),
                 s)
    
})