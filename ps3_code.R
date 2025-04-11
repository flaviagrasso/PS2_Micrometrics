# Template of R script to answer problem set
# Group number: 
# Group composition: A, B, and C

# Get the username
user <- Sys.info()["user"]
print(user)

# Define file path conditionally
if (user == "erick") {
  filepath <- "/home/erick/TEMP/"
} else if (user == "titouanrenault") {
  filepath <- "/Users/titouanrenault/Desktop/Master/micrometrics/Problem set 2/files"
} else if (user == "B") {
  filepath <- "/FILE/PATH/B/"
} else if (user == "C") {
  filepath <- "/FILE/PATH/C/"
} else {
  filepath <- ""  # Default case if user is not listed
}

# Print the selected file path
print(paste("File path set to:", filepath))

#Library
library(grf)
library(dplyr)
library(labelled)
library(ggplot2)
#Download data
ps3 = read.csv(file.path(filepath, "expanded_data.csv"))

#Create label for name of variables in plots
var_label(ps3$education_rate) = "Education Rate (%)"
var_label(ps3$unemployment_rate) = "Unemployment Rate (%)"
var_label(ps3$religious_adherence) = "Religious Adherence (%)"
var_label(ps3$women_labor_force_participation) = "Women participating in labor market (%)"

label_names <- c("education_rate" = "Education (%)",
                 "religious_adherence"= "Religious Adherence (%)",
                "women_labor_force_participation" = "Women in labor market (%)"
)


#-------------------------------------------------------------------------------
#EXERCISE 2
#-------------------------------------------------------------------------------

# Question 1.a

#Create treatment: 1 if year>lfdivlaw
ps3$treated = ifelse(ps3$year>ps3$lfdivlaw, 1, 0)

#Create binary for urbanization (tau forest does not accept non-numeric variable)
ps3$urban_bin =ifelse(ps3$urbanization == "Urban", 1, 0)

# Outcome variable
Y <- ps3$div_rate_sim

# Treatment indicator
W <- ps3$treated

#Covariates
X <- ps3[, c("education_rate", "childcare_availability", "unemployment_rate", "median_income", 
             "marriage_rate", "religious_adherence", "alcohol_consumption",
             "domestic_violence_rate", "women_labor_force_participation",
             "housing_cost", "crime_rate", "social_services_spending", "urban_bin")] ##tbc urbanisation



#estimate causal forest
tau.forest <- causal_forest(X, Y, W, num.trees = 2000)
#Compute  ATE
ate <- average_treatment_effect(tau.forest)
print(ate)
#estimate    std.err 
#0.11065390 0.03587485 

# Comment: The average treatment effect (ATE) estimated using the causal forest is 0.11, 
# and it is statistically significantly different from zero at the 5% level. In question 1.c), 
# using the DiD specification, we obtained an estimate of 0.03 for the treatment effect, 
# which was not statistically significant. Compared to DiD, the causal forest estimate 
# is notably higher, highlighting a discrepancy between the two approaches. 
# This inconsistency may arise due to differences in model specification. Causal 
#forests accommodate various functional forms between treatment and outcome, 
#allowing for flexible estimation of heterogeneous treatment effects.


#Question 1.b 


#Most important variable is religious adherence (6). Then women_labor_force_participation (9), education_rate (1). 
#Other variables seem significantly less important. 

# Question 1.b

#Compute best-linear projection (BLP) of conditionnal average treatment effect
blp <- best_linear_projection(tau.forest, X)
blp
# The best linear projection fits a linear approximation of the conditional average treatment effects (CATEs) 
# on the covariates in our dataset. This is useful for identifying which variables are most strongly associated 
# with heterogeneity in the treatment effect.
# 
# In our case, the most relevant covariates are: education rate, religious adherence, and women’s labor force 
# participation. All three are statistically significantly different from zero. This suggests that higher levels 
# of education, stronger religious adherence, and greater female labor force participation are associated 
# with higher treatment effects.
#
# In the next step, we compute variable importance to quantify how frequently each covariate is used in tree splits. 
# We also visualize the relative importance of each variable compared to the most influential one.


#Obtain variable importance
vi = variable_importance(tau.forest)
#Set variable importance relative to the largest (and rescale to 100)
vi_relative = (vi /max(vi))*100

#Create df to plot variable importance
vi_df <- data.frame(
  Variable = colnames(X),
  Importance = vi_relative
) %>% arrange(-Importance)

#Final Plot using ggplot
ggplot(vi_df, aes(x = Importance, y = reorder(Variable, -Importance))) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Variable Importance", y = NULL,
       title = "Variable Importance (relative to max)") +theme_classic()


#Compute Targeting Operator Characteristic;

# Split sample into train and eval
n <- nrow(X)
set.seed(123)
train <- sample(1:n, n / 2)

# Train causal forest on training set
train.forest <- causal_forest(X[train, ], Y[train], W[train])

# Train separate forest on evaluation set
eval.forest <- causal_forest(X[-train, ], Y[-train], W[-train])

# Get predicted CATEs from training forest on the evaluation data
rate <- rank_average_treatment_effect(eval.forest,
                                      predict(train.forest, X[-train, ])$predictions)
rate
plot(rate, main = "Targeting Operator Characteristic (TOC)")

# Comment: The TOC (Targeting Operator Characteristic) is the area under the curve of the Rank-Weighted Average Treatment Effect (RATE).
# The RATE is a metric that measures how well the CATE estimator ranks units according to their estimated treatment benefit.
# Specifically, the TOC quantifies, for each quantile q, the incremental benefit of treating only the q% of units with the largest 
# estimated CATEs, compared to the overall average treatment effect (ATE).
# 
# We observe that targeting the most effective states (based on their estimated CATE) significantly increases the treatment effect.
# For example, treating the top 20% of states with the largest estimated CATEs would lead to an ATE of 0.7, 
# whereas treating the bottom 80% would yield a much lower ATE of only 0.1.

#Plot CATE by distribution of variables that could drive heterogeneity:

library(ggplot2)
library(dplyr)
library(tidyr)

# Predict CATEs using out-of-bag estimates
tau.hat.oob <- predict(tau.forest)
# Add the CATE predictions to your original data
ps3$CATE <- tau.hat.oob$predictions

# Pick variables chosen in the first step (variable importance) to explore heterogeneity
hetero_vars <- c("education_rate", "religious_adherence", "women_labor_force_participation")

# Reshape data to long format for faceted plotting
long_data <- ps3 %>%
  select("CATE", "education_rate", "religious_adherence", "women_labor_force_participation")%>%
  pivot_longer(cols = all_of(hetero_vars), names_to = "Variable", values_to = "Value")

# Plot: one plot, multiple facets
ggplot(long_data, aes(x = Value, y = CATE)) +
  geom_smooth(method = "loess", color = "blue")+
  facet_wrap(~ Variable, scales = "free_x", labeller = labeller(Variable = label_names)) +
  theme_minimal() +
  labs(title = "CATEs by Potential Heterogeneity Drivers",
       x = "Value of Variable",
       y = "Estimated CATE")+
  theme_classic()

# Question 1.c

# We found strong evidence of heterogeneous treatment effects in the previous analysis.
# The Best Linear Projection (BLP) showed that key variables—such as education levels,
# religious adherence, and the share of women in the labor market significantly explain
# the variation in treatment effects across observations.
#
# The Targeting Operator Characteristic (TOC) curve further supports this finding. It
# demonstrates that the average treatment effect varies across different fractions of the
# population, ranked by their predicted responsiveness to the treatment. In particular,
# states in the top 10%—those expected to increase the most unilateral divorce as a result
#of the law—experience the highest treatment effects, with the effect decreasing 
#gradually across lower-ranked groups.This highlights the presence of heterogeneity 
#and suggests that the unilateral divorce law policy had varying effects on states 
#depending on the composition of their population.
#
# While the Average Treatment Effect (ATE) estimated in Question 1.a) was approximately
# 0.11, the Conditional Average Treatment Effects (CATEs) vary considerably across
# different values of key covariates. For instance, in counties where 90% of individuals
# hold a university degree, the CATE reaches around 0.25. This indicates that the treatment
# has a stronger impact in areas with higher education levels.

#Question 1.d

tau.forest_2 = causal_forest(X, Y, W, honesty = FALSE)
#Compute new ATE
ate_2 = average_treatment_effect(tau.forest_2)
ate_2

#estimate    std.err 
#0.11529298 0.03562229 
#The estimate of the ate and standard error is almost unchanged. 

#Compute BLP
blp_2 <- best_linear_projection(tau.forest_2, X)
blp_2

#The best linear remains unchanged. Education_rate, religous adherence and women labor
#force participation are still the three variables statistically different from 0. 

# Comment: 
# We obtain the same ATE. When we do not use honest causal trees, we do not split the
# training data into separate subsets for determining the tree structure and estimating the treatment effect
# within each leaf.
# This makes the model appear "more precise" in-sample, but may lead to bias in the 
# average treatment effect due to overfitting. 
# We would expect this bias to be more important in small samples. 
# This is because, in small samples, the model is more likely to fit spurious correlation, leading to 
# overfitting. In contrast, in large samples, random noise tends to cancel out, reducing 
# the impact of overfitting on the estimated treatment effects.
# It is particularly relevant for our estimation of CATE, since we care mostly
# of credible inference and not only prediction. 




