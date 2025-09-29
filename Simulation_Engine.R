library(readxl)
library(dplyr)
library(car)
library(MASS)
library(fitdistrplus)
library(VGAM)
library(ggplot2)
library(cvTools)
library(evir)
library(writexl)
library(ggplot2)
library(gridExtra)
#-----------------------------------------------------------------------------------------
data_file <- "/Users/garrett/Desktop/MSU/MSU_Spring_2025/Capstone/Code/HospitalData.xlsx"
reins_data <- read_excel(data_file, sheet = "Reinsurance Data")
loss_data <- read_excel(data_file, sheet = "Losses 2014-2023")
loss_data_supplemental <- read_excel(data_file, sheet = "Supplemental Losses")
hospitals <- read_excel(data_file, sheet = "Location Data")
bridge <- read_excel(data_file, "Loc to Arr ID")
#-----------------------------------------------------------------------------------------
# Prepare data with appropriate columns and transformations. We cannot filter out attachment points that are equal to zero.
#they make up far to much of the reinsurance data - not including would reduce quality of simulation substantially. 
reins_data <- reins_data %>% filter(`Reins Premium` > 0, `Layer Size Amt USD` > 0, `Reins Capacity` > 0)

#add small trivial constant to attachment point so natural log transform will work.
reins_data$`Attachment Pt Amt USD` <- reins_data$`Attachment Pt Amt USD` + 0.00001
#-----------------------------------------------------------------------------------------
# Fit initial regression model
model <- lm(log(`Reins Premium`) ~ log(`Attachment Pt Amt USD`) + log(`Layer Size Amt USD`) + `Reins Share`, 
            data = reins_data)
summary(model) # initial assessment
#-----------------------------------------------------------------------------------------
# Compute Cook's Distance for outlier detection
cooksD <- cooks.distance(model)

# Define high-influence points using standard cutoff 4/n
high_influence <- which(cooksD > (4 / nrow(reins_data)))

# Remove identified high-influence points
reins_data_clean <- reins_data[-high_influence, ]

# Confirm removal of outliers
cat("Removed", length(high_influence), "outliers based on Cook's Distance.\n")

model_clean <- lm(log(`Reins Premium`) ~ log(`Attachment Pt Amt USD`) + log(`Layer Size Amt USD`) + `Reins Share`,
                  data = reins_data_clean)
summary(model_clean)
plot(model_clean)

#-----------------------------------------------------------------------------------------
vif_values <- vif(model_clean)
print(vif_values)

# Check if any VIF values exceed the threshold of 5 (common practice)
if(any(vif_values > 5)) {
  cat("Warning: High multicollinearity detected. Consider dropping or combining predictors.\n")
} else {
  cat("No problematic multicollinearity detected.\n")
}
# Perform 10-fold cross-validation to assess model robustness
cv_results <- cvFit(model_clean, 
                    y = log(reins_data_clean$`Reins Premium`), 
                    data = reins_data_clean, 
                    K = 10)

# Display Cross-Validated Mean Squared Error (MSE)
cat("Cross-Validated MSE:", cv_results$cv, "\n")

#-----------------------------------------------------------------------------------------
set.seed(123)  # reproducibility
residuals <- model_clean$residuals

gen_premium <- function(layer_features,n_years) {
  pred_mean_log <- predict(model_clean, newdata = layer_features)
  sampled_resid <- sample(residuals, n_years, replace = TRUE)
  return(exp(pred_mean_log + sampled_resid))
}

layer_features <- data.frame(
  "Attachment Pt Amt USD" = (1),
  "Layer Size Amt USD" = 10000000,
  "Reins Share" = 0.5,
  check.names = FALSE
)

prem <- c()
for(i in 1:1000){
  prems <- gen_premium(layer_features,100)
  prem <- append(prem, mean(prems))
}
summary(prem)
#-----------------------------------------------------------------------------------------
# Extract residuals and actual premium
reins_data_clean$residuals <- residuals(model_clean)
reins_data_clean$actual_premium <- reins_data_clean$`Reins Premium`

# Plot residuals vs actual premium

ggplot(reins_data_clean, aes(x = actual_premium, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_x_log10(labels = scales::dollar) +
  labs(
    title = "Residuals vs. Reinsurance Premium",
    x = "Reinsurance Premium (log scale)",
    y = "Residuals from log-linear model"
  ) +
  theme_minimal()
#-----------------------------------------------------------------------------------------
#Clean data by pooling losses by year and index-rec to approximate 
#loss per property distribution on a yearly basis 
agg_data <- loss_data
agg_data <- agg_data[!( is.na(agg_data$`Gross Total - Trended`) | is.na(agg_data$`Index Rec`) | 
                       is.na(agg_data$`Occurred Year`) | (agg_data$`Index Rec` %in% c("-"))) ,]

agg_data$key <- paste(agg_data$`Occurred Year`, agg_data$`Index Rec`, sep = "")
agg_data <- aggregate(`Gross Total - Trended` ~ key, data = agg_data, FUN = sum)

#Merge Dave's Supplemental Losses with existing data
individual_losses <- agg_data$`Gross Total - Trended`
summary(individual_losses)
supplemental_losses <- loss_data_supplemental$`Gross Reserve`
individual_losses <- append(individual_losses, supplemental_losses)
individual_losses <- individual_losses[individual_losses > 0]
individual_losses <- individual_losses[!is.na(individual_losses)]
summary(individual_losses)

#Fit a log normal distribution to cleaned loss data using a maximum likelihood estimate
fit_ln <- fitdist(individual_losses, "lnorm")
cat("Lognormal distribution parameters:\n")
print(fit_ln)

#Find suitable threshold for generalized pareto distribution. 
#---------------------------------------------------------------------------------------------------------
# Define a range of thresholds (e.g., from the 80th to the 99th percentile)
thresholds <- seq(quantile(individual_losses, 0.80),
                  quantile(individual_losses, 0.99),
                  length.out = 100)

# Initialize vectors to store the estimated shape parameters and mean residual life values
shape_estimates <- numeric(length(thresholds))
mrl_values <- numeric(length(thresholds))

# Loop over each threshold, fit a GPD for the tail losses, and record the shape parameter and MRL
for (i in 1:length(thresholds)) {
  tail_losses <- individual_losses[individual_losses > thresholds[i]]
  
  # Only compute if there are exceedances
  if (length(tail_losses) > 0) {
    # Fit the GPD to the excesses over the threshold
    fit_pareto <- vgam((tail_losses - thresholds[i]) ~ 1, paretoII(location = 0), trace = FALSE)
    shape_estimates[i] <- Coef(fit_pareto)["shape"]
    
    # Compute the Mean Residual Life (MRL) for this threshold
    mrl_values[i] <- mean(tail_losses - thresholds[i])
  } else {
    shape_estimates[i] <- NA
    mrl_values[i] <- NA
  }
}

# Set up the plotting area to display two plots side by side
par(mfrow = c(1, 2))

# Threshold Stability Plot: Estimated shape parameter versus threshold
plot(thresholds, shape_estimates, type = "b",
     xlab = "Threshold", 
     ylab = "Estimated Shape Parameter (xi)",
     main = "Threshold Stability Plot")

# Mean Residual Life Plot: MRL versus threshold
plot(thresholds, mrl_values, type = "b",
     xlab = "Threshold",
     ylab = "Mean Residual Life",
     main = "Mean Residual Life Plot")


#Determine MFL for hospitals as of 2023.
#---------------------------------------------------------------------------------------------------------
hospitals <- hospitals[hospitals$`Index Rec` %in% bridge$`Index Rec`, ]
hospitals <- merge(hospitals, bridge, by = "Index Rec", all.x = TRUE)
MFL <- aggregate(`Cumulative Amt USD` ~ `Arr ID`, data = reins_data, FUN = max)
hospitals <- merge(hospitals, MFL, by = "Arr ID", all.x = TRUE)

#remove NA values from MFLs. This will be used to create a capped generalized pareto later on in the simulation 
for (i in 1:length(hospitals$`Cumulative Amt USD`)){
  if(is.na(hospitals[i,"Cumulative Amt USD"])){
    hospitals[i, "Cumulative Amt USD"] <- mean(hospitals$`Cumulative Amt USD`, na.rm = TRUE)
  }
}

# Define the negative log-likelihood function for the truncated and untruncated GPD
#---------------------------------------------------------------------------------------------------------
tgpd_nllik <- function(params, y, T_excess) {
  sigma <- params[1]
  xi <- params[2]

  # Check for invalid parameters
  if (sigma <= 0 || any(1 + xi * (y/sigma) <= 0) || (1 + xi * (T_excess)/sigma) <= 0) {
    return(Inf)
  }
  
  F_T <- 1 - (1 + xi * T_excess/sigma)^(-1/xi)
  n <- length(y)
  
  nllik <- n * log(F_T) + n * log(sigma) + (1/xi + 1) * sum(log(1 + xi * y/sigma))
  return(nllik)
}

gpd_nllik <- function(params, y) {
  sigma <- params[1]
  xi <- params[2]
  
  # Validity checks
  if (sigma <= 0 || any(1 + xi * y / sigma <= 0)) return(Inf)
  
  n <- length(y)
  nllik <- n * log(sigma) + (1/xi + 1) * sum(log(1 + xi * y / sigma))
  return(nllik)
}


#Fit custom Generalized pareto distribution for losses for each property 
#---------------------------------------------------------------------------------------------------------
# Initialize vectors to store parameters

custom_generalized_pareto <- function(population, threshold){
  
  shape_param <- numeric(length(population$`Cumulative Amt USD`))
  scale_param <- numeric(length(population$`Cumulative Amt USD`))
  is_truncated <- numeric(length(population$`Cumulative Amt USD`))
 
  tail_losses <- individual_losses[individual_losses > threshold]
  
  for (i in 1:length(population$`Cumulative Amt USD`)){
   
    T = population$`Cumulative Amt USD`[i]
    
    if (T <= max(individual_losses)) {
      
      # Fit Truncated GPD
      T_excess <- T - threshold
      truncated_losses <- tail_losses[tail_losses <= T] - threshold
      
      # perform optimization with truncation...
      init_params <- c(sigma = max(sd(truncated_losses),1), xi = 0.05)
      #print(i)
      
      #sometimes the truncated is so close to the un-truncated they are indistinguishable from one another. 
      tryCatch({
        fit <- optim(init_params,
              tgpd_nllik,
              y = truncated_losses,
              T_excess = T_excess,
              method = "L-BFGS-B",
              lower = c(1e-6, -Inf),
              upper = c(Inf, Inf))
        
        is_truncated[i] <- 1
      }, 
      error = function(e) {
        # If there is an error default back to standard GPD.
        truncated_losses <- tail_losses - threshold
        init_params <- c(sigma = max(sd(truncated_losses),1), xi = 0.05)
        fit <- optim(init_params,
                     gpd_nllik,
                     y = truncated_losses,
                     method = "L-BFGS-B",
                     lower = c(1e-6, -5),
                     upper = c(Inf, 5))
        is_truncated[i] <- 0
      })
      
    } 
    else{
          # Fit Standard GPD without truncation
          truncated_losses <- tail_losses - threshold
          init_params <- c(sigma = max(sd(truncated_losses),1), xi = 0.05)
          fit <- optim(init_params,
                       gpd_nllik,
                       y = truncated_losses,
                       method = "L-BFGS-B",
                       lower = c(1e-6, -5),
                       upper = c(Inf, 5))
          is_truncated[i] <- 0
       }
       fitted_sigma <- fit$par[1]
       fitted_xi <- fit$par[2]
       shape_param[i] = fitted_xi
       scale_param[i] = fitted_sigma
       #print(fitted_xi)
  }
  population["shape_param"] <- shape_param
  population["scale_param"] <- scale_param
  population["is_truncated"] <- is_truncated
  
  return(population)
}

#population <- custom_generalized_pareto(population,3000000)
#-----------------------------------------------------------------------------------------
rtrunc_gpd <- function(n, sigma, xi, lower = 0, upper) {
  # Ensure numeric stability for CDF and quantile functions
  pgpd <- function(x) {
    if (xi == 0) {
      return(1 - exp(-(x - lower) / sigma))
    } else {
      return(1 - (1 + xi * (x - lower) / sigma)^(-1 / xi))
    }
  }
  qgpd <- function(p) {
    p <- pmin(p, 1 - 1e-12)  # clamp to avoid infinite tail
    if (xi == 0) {
      return(lower - sigma * log(1 - p))
    } else {
      return(lower + sigma * ((1 - p)^(-xi) - 1) / xi)
    }
  }
  
  # Compute truncated CDF bounds
  Pl <- pgpd(lower)
  Pu <- pgpd(upper)
  
  # Sample uniformly from truncated CDF interval
  u <- runif(n, Pl, Pu)
  
  # Transform back using robust quantile function
  samples <- qgpd(u)
  
  # Final safeguard against overshoot due to float error
  samples[samples > upper] <- upper
  
  return(samples)
}

#-----------------------------------------------------------------------------------------
#threshold_trunc used in rSeverity refers to the maximum possible loss that can occur, and is contextually different from the threshold used in the code above
#for fitting a truncated Pareto distribution. 
rSeverity <- function(n, risk_prob, shape_param, scale_param, is_truncated, threshold_trunc, threshold) {
  
  # Risk probability (probability that a loss occurs)
  # This must be defined outside the function or passed in
  # e.g., risk_prob <- 0.95
  loss_occured <- rbinom(n, 1, risk_prob)
  
  # Uniform to split body and tail
  u <- runif(n)
  p_tail <- 0.05  # Top 5% = tail
  severities <- numeric(n)
  
  # Body: Lognormal for bottom 95%
  n_ln <- sum(u <= (1 - p_tail))
  severities[u <= (1 - p_tail)] <- rlnorm(n_ln,
                                          meanlog = fit_ln$estimate["meanlog"],
                                          sdlog = fit_ln$estimate["sdlog"])
  
  # Tail: Generalized Pareto
  n_tail <- sum(u > (1 - p_tail))
  
  if (n_tail > 0) {
    if (!is_truncated) {
      # Regular GPD (i.e., Pareto-type tail)
      tail_samples <- scale_param * ((1 - runif(n_tail))^(-1 / shape_param) - 1)
    } else {
      # Truncated GPD
      tail_samples <- rtrunc_gpd(n_tail, scale_param, shape_param, lower = 0, threshold_trunc)
    }
    # Add threshold to tail samples
    severities[u > (1 - p_tail)] <- threshold + tail_samples
  }
  
  # Set to 0 if no loss occurred
  severities[loss_occured == 0] <- 0
  severities[severities > threshold_trunc] <- threshold_trunc
  return(severities)
}

#Quick Validation Snack.
losses <- rSeverity(1000000,0.04,18.7,7071031, FALSE, 30000000,3000000)
summary(losses)
#-----------------------------------------------------------------------------------------
# Example Reinsurance Structure (adjustable)
attachments <- c(0, 10e6, 50e6)   # Layer attachments at $0, $10M, and $50M
limits <- c(10e6, 40e6, Inf)      # Corresponding limits: 0-10M, 10-40M, 50M+ unlimited
ceded <- c(TRUE, FALSE, FALSE)     # First layer re-insured, next two layers retained by FM
shares <- c(0.4,0,0) #Reinsurance shares (0 implies they share non of the layer which is equivelant to ceded = FALSE)
#-----------------------------------------------------------------------------------------
#Applies a reinsurance structure to simulated losses. 
#Computes what actual insurance company pays and finds difference between the total loss to determine recovery.
apply_reinsurance <- function(individual_losses, attachments, limits, ceded, shares) {
  net_loss_per_occ <- numeric(length(individual_losses))
  
  for(i in seq_along(individual_losses)) {
    loss <- individual_losses[i]
    paid_by_insurer <- 0
    remaining_loss <- loss
    
    for(layer in seq_along(attachments)) {
      attach_point <- attachments[layer]
      layer_limit <- limits[layer]
      is_ceded <- ceded[layer]
      share <- shares[layer]
      
      # Loss exceeding attachment point
      layer_excess <- max(0, remaining_loss - attach_point)
      
      if(layer_excess <= 0) next
      
      # Loss amount within the layer limit
      layer_pay <- if(is.finite(layer_limit)) {
        min(layer_excess, layer_limit)
      } else {
        layer_excess
      }
      
      # if is_ceded is true reinsurance pays otherwise FM retains responsibility.
      if(!is_ceded) {
        paid_by_insurer <- paid_by_insurer + layer_pay
      }
      #Partially ceded to reinsurance based on quota share agreement.
      else if(is_ceded && share < 1){
        paid_by_insurer <- paid_by_insurer + layer_pay * (1-share)
      }
      
      remaining_loss <- remaining_loss - layer_pay
    }
    
    # Residual loss above all layers assumed by insurer
    paid_by_insurer <- paid_by_insurer + max(0, remaining_loss)
    net_loss_per_occ[i] <- paid_by_insurer
  }
  
  reinsuranceRecovery = individual_losses - net_loss_per_occ

  return(reinsuranceRecovery)
}

#-----------------------------------------------------------------------------------------
#Simulates potential losses and applies reinsurance program to the losses. 
simulate_program <- function(n_years, population, attachments, limits, ceded, shares, risk_probs, threshold) {
  
    losses <- matrix(NA, nrow = nrow(population), ncol = n_years)
    recoveries <- matrix(NA, nrow = nrow(population), ncol = n_years)
    risk_prob <- NA
    
    for(i in 1:nrow(population)){
      
      risk <- population[i,]$`Risk Quality`
      if(risk %in% c("A")){
        risk_prob <- risk_probs[1]
      }
      else if(risk %in% c("B")){
        risk_prob <- risk_probs[2]
      }
      else if(risk %in% c("C")){
        risk_prob <- risk_probs[3]
      }
      else if(risk %in% c("D")){
        risk_prob <- risk_probs[4]
      }
      else if(risk %in% c("E")){
        risk_prob <- risk_probs[5]
      }
      else if(risk %in% c("F")){
        risk_prob <- risk_probs[6]
      }
      #when the risk category doesn't match the ones specified above default back to average.
      else{
        risk_prob <- risk_probs[7]
      }
      
      #Simulate Losses
      n_year_loss <- rSeverity(n_years,risk_prob,population[i,]$`shape_param`, population[i,]$`scale_param`, population[i,]$`is_truncated`, 
                               population[i,]$`Cumulative Amt USD`, threshold)
      losses[i,] <- n_year_loss
      
      #Simulate Recoveries
      recoveries[i,] <- apply_reinsurance(n_year_loss, attachments, limits, ceded, shares)

    }
    
  results <- list(losses = losses, recoveries = recoveries)
  
  return(results)
}
#-----------------------------------------------------------------------------------------

# Simulate 10,000 years of losses under defined reinsurance program
simulated_results <- simulate_program(
  n_years = 100,
  population = hospitals,
  attachments = attachments,
  limits = limits,
  ceded = ceded,
  shares = shares
)

gross_losses_by_year <- colSums(simulated_results$losses)
recoveries_by_year <- colSums(simulated_results$recoveries)


# Calculate net loss related metrics
mean_gross_loss <- mean(gross_losses_by_year)
mean_recovery <- mean(recoveries_by_year)

print(mean_gross_loss)
print(mean_recovery)
#-----------------------------------------------------------------------------------------
n_years <- 1000

# Define a named list of risk-tower structures
risk_structures <- list(
  ABC_50Mil = list(
    risks      = c("A","B","C"),
    attachments= c(0, 0.5e8, 4.5e8, 8.5e8, 11.5e8, 14.5e8),
    limits     = c(0.5e8, 4e8,   4e8,   3e8,    3e8,    Inf),
    ceded      = c(TRUE, FALSE, TRUE,  FALSE,  TRUE,   FALSE),
    shares     = c(1,    0,     1,     0,      1,      0)
  ),
  ABC = list(
    risks      = c("A","B","C"),
    attachments= c(0, 4.5e8, 8.5e8, 11.5e8, 14.5e8),
    limits     = c(4.5e8,   4e8,   3e8,    3e8,    Inf),
    ceded      = c(FALSE, TRUE,  FALSE,  TRUE,   FALSE),
    shares     = c(0,     1,     0,      1,      0)
  ),
  D_50Mil = list(
    risks      = "D",
    attachments= c(0, 0.5e8, 2.8e8,  6.8e8,  9.5e8, 12.5e8),
    limits     = c(0.5e8, 2.3e8, 4e8,   2.7e8, 3e8,   Inf),
    ceded      = c(TRUE,   FALSE, TRUE,  FALSE, TRUE, FALSE),
    shares     = c(1,      0,     0.5,    0,    1,      0)
  ),
  D = list(
    risks      = "D",
    attachments= c(0, 2.8e8,  6.8e8,  9.5e8, 12.5e8),
    limits     = c(2.8e8, 4e8,   2.7e8, 3e8,   Inf),
    ceded      = c(FALSE, TRUE,  FALSE, TRUE, FALSE),
    shares     = c(0,     0.5,    0,    1,      0)
  ),
  E_50Mil = list(
    risks      = "E",
    attachments= c(0, 0.5e8, 2.35e8, 4.35e8, 9.5e8, 12e8),
    limits     = c(0.5e8, 1.85e8, 2e8,   5.15e8, 2.5e8, Inf),
    ceded      = c(TRUE,   FALSE, TRUE,  FALSE,  TRUE,  FALSE),
    shares     = c(1,      0,     1,      0,      1,      0)
  ),
  E = list(
    risks      = "E",
    attachments= c(0, 2.35e8, 4.35e8, 9.5e8, 12e8),
    limits     = c(2.35e8, 2e8,   5.15e8, 2.5e8, Inf),
    ceded      = c(FALSE, TRUE,  FALSE,  TRUE,  FALSE),
    shares     = c(0,     1,      0,      1,      0)
  ),
  F_ex50Mil = list(
    risks      = "F",
    attachments= c(0, 4.5e7, 5.5e7, 3.25e8),
    limits     = c(4.5e7, 1e7, 2.70e8),
    ceded      = c(FALSE, FALSE, TRUE, FALSE),
    shares     = c(0,     0,     1,     0)
  ),
  F_ = list(
    risks      = "F",
    attachments= c(0, 4.5e7, 5.5e7, 3.25e8),
    limits     = c(4.5e7, 1e7, 2.70e8),
    ceded      = c(TRUE, FALSE, TRUE, FALSE),
    shares     = c(0.666667,     0,     1,     0)
  )
)

structure_list <- list()
risk_probs_base <- c(0.034,0.0403,0.0404,0.0329,0.0491,0.104,0.0385)
threshold_base <- 3000000

risk_prob_multiplier <- seq(0.5,1.5,0.5)
shape_multiplier <- seq(0.5,1.5,0.5)
scale_multiplier <- seq(0.5,1.5,0.5)
threshold_multiplier <- seq(0.5,1.5,0.5)
reins_prem_multiplier <- seq(0.5,1.5,0.5)

for(risk_mult in risk_prob_multiplier){
  risk_probs <- risk_probs_base * risk_mult
  for(shape_mult in shape_multiplier){
    for(scale_mult in scale_multiplier){
      for(thresh_mult in threshold_multiplier){
        threshold <- threshold_base * thresh_mult
        for(reins_mult in reins_prem_multiplier){
          for (grp in names(risk_structures)) {
            print(risk_mult)
            params <- risk_structures[[grp]]
            
            # filter the hospitals for this risk group
            population  <- hospitals[hospitals$`Risk Quality` %in% params$risks, ]
            population <- custom_generalized_pareto(population, threshold)
            
            population$shape_param <- population$shape_param * shape_mult
            population$scale_param <- population$scale_param * scale_mult
            
            
            # pull out the other vectors
            attachments <- params$attachments
            limits      <- params$limits
            ceded       <- params$ceded
            shares      <- params$shares
          
          #-----------------------------------------------------------------------------------------
                
          # Simulate net losses under the given structure (1,000 iterations for stability)
          results <- simulate_program(
                n_years = n_years,
                population = population,
                attachments = attachments,
                limits = limits,
                ceded = ceded,
                shares = shares,
                risk_probs = risk_probs,
                threshold = threshold
                )
               
                # Estimate reinsurance premium using the OLS model.
                
          yearly_reins_prem <- 0
          for( i in 1:nrow(population)){
               if(any(ceded)) {
                     
                  for(j in which(ceded)) {
                      layer_cap <- population[i,]$`Cumulative Amt USD`
                    
                      if(layer_cap < attachments[j]){
                         ceded_premium <- 0
                       }
                       else if(layer_cap < attachments[j] + limits[j]){
                         
                         layer_features <- data.frame(
                           "Attachment Pt Amt USD" = (attachments[j] + 1),
                           "Layer Size Amt USD" = layer_cap - attachments[j],
                           "Reins Share" = shares[j],
                           check.names = FALSE
                         )
              
                         
                         sim_premiums <- gen_premium(layer_features,n_years)
                         ceded_premium <- mean(sim_premiums)
                       }
                       else{
                         layer_features <- data.frame(
                           "Attachment Pt Amt USD" = (attachments[j] + 1),
                           "Layer Size Amt USD" = limits[j],
                           "Reins Share" = shares[j],
                           check.names = FALSE
                         )
                         sim_premiums <- gen_premium(layer_features,n_years)
                         ceded_premium <- mean(sim_premiums)
                         
                       }
                       yearly_reins_prem <- ceded_premium + yearly_reins_prem
                       #print(layer_features)
                     }
                    
                  }
                }
                #print(yearly_reins_prem)
                yearly_reins_prem <- yearly_reins_prem * reins_mult
                
                # Calculate performance metrics
                yearly_recoveries <- colSums(results$recoveries)
                recovery_ratios <- yearly_recoveries / yearly_reins_prem
                yearly_losses <- colSums(results$losses)
                yearly_net_losses <- yearly_losses - yearly_recoveries + yearly_reins_prem
                net_loss_volatility <- sd(yearly_net_losses)
                gross_loss_volatility <- sd(yearly_losses)
                reins_vol_reduction <- 1- ( (gross_loss_volatility - net_loss_volatility) / gross_loss_volatility)
                
                # Store current structure
                curr_structure <- data.frame(
                  name = grp,
                  risk_prob_mult = risk_mult,
                  scale_mult = scale_mult,
                  shape_mult = shape_mult,
                  reins_mult = reins_mult,
                  threshold_mult = thresh_mult,
                  num_locations = nrow(population),
                  mean_yearly_recovery = mean(yearly_recoveries), 
                  mean_yearly_reisnurance_prem = mean(yearly_reins_prem),
                  mean_yearly_recovery_ratio = mean(recovery_ratios),
                  mean_yearly_loss = mean(colSums(results$losses)),
                  gl_volatility = gross_loss_volatility,
                  nl_volatility = net_loss_volatility,
                  gross_loss_100_year = quantile(yearly_losses,0.99), 
                  net_loss_100_year = quantile(yearly_net_losses, 0.99),
                  vol_reduc = reins_vol_reduction
                )
                structure_list[[length(structure_list) + 1]] <- curr_structure
              }
            }
          }
        }
      }
}
# Combine accumulated results
structures <- do.call(rbind, structure_list)
write_xlsx(structures, "~/Desktop/reins_strucutres.xlsx")

