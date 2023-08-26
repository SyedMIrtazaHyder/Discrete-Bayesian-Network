#a)
age_cpt2 <- array(c(young, adult, old),
                 dim = c(3),
                 dimnames = list(A = levels(age)))

sex_cpt2 <- array(c(male, female),
                 dim = c(2),
                 dimnames = list(S = levels(sex)))

education_cpt2 <- array(
  data = c(high_given_young_male, uni_given_young_male,
           high_given_adult_male, uni_given_adult_male, 
           high_given_old_male, uni_given_old_male,
           high_given_young_female, uni_given_young_female,
           high_given_adult_female, uni_given_adult_female,
           high_given_old_female, uni_given_old_female),
  dim = c(2, 3, 2),
  dimnames = list(
    E = levels(education),
    A = levels(age),
    S = levels(sex)
  )
)

residence_cpt2 <- array(
  data = c(small_given_high, big_given_high,
           small_given_uni, big_given_uni),
  dim = c(2, 2),
  dimnames = list(
    R = levels(residence),
    E = levels(education)
  )
)

occupation_cpt2 <- array(
  data = c(self_given_high, emp_given_high,
           self_given_uni, emp_given_uni),
  dim = c(2, 2),
  dimnames = list(
    O = levels(occupation),
    E = levels(education)
  )
)

transport_cpt2 <- array(
  data = c(car_given_self_and_small, train_given_self_and_small, other_given_self_and_small,
           car_given_emp_and_small, train_given_emp_and_small, other_given_emp_and_small,
           car_given_self_and_big, train_given_self_and_big, other_given_self_and_big, 
           car_given_emp_and_big, train_given_emp_and_big, other_given_emp_and_big),
  dim = c(3, 2, 2),
  dimnames = list(
    T = levels(transport),
    O = levels(occupation),
    R = levels(residence)
  )
)

cpts <- list(A = age_cpt2, S = sex_cpt2,
             E = education_cpt2, R = residence_cpt2,
             O = occupation_cpt2, T = transport_cpt2)

#b)
#making dag
newDag <- empty.graph(nodes = c('A', 'S', 'E', 'R', 'O', 'T'))
arcs <- matrix(c("A", "E",
                 "S", "E",
                 "E", "R",
                 "E", "O",
                 "R", "T",
                 "O", "T"), byrow=TRUE, ncol = 2)
arcs(newDag) <- arcs
#making bn
bn <- custom.fit(newDag, cpts)
nparams(bn) #local parameters in BN
nodes(bn) #nodes in BN
arcs(bn) #arcs in BN

#c)
#1) Due to unknown number of unique values in categorical features and Data cleaning
# and lack of classification as categorical variables are char type not factor type
#2)
survey_df <- as.data.frame(survey) #i
library(dplyr) #ii
survey_df <- survey_df %>%
  mutate_if(is.character, as.factor) #iii
otherBn <- bn.fit(newDag, survey_df) #iv
modelstring(otherBn) #v