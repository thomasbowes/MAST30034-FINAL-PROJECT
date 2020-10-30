
binom_cauchit = glm(formula = domain1_score/12 ~ word_count + Mistakes + reading_ease, 
                    +     family = binomial(link = "cauchit"))

sapply(gamma_pred, function(y) min(max(y,0),12))

library(irr)
library(MLmetrics)

binom_probit_pred = predict(binom_probit, df, type = "response")*12
binom_probit_pred_matrix = matrix(c(round(binom_probit_pred), df$domain1_score), nrow = 1783, ncol = 2)
kappa2(binom_probit_pred_matrix)


MSE(binom_cauchit_pred, domain1_score)