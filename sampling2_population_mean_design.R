# ============================================================================
# OPTIMAL SAMPLE SIZE DETERMINATION WITH SUBGROUP ANALYSIS
# Considering: Bias, Cost, Time, and Multiple Strata
# ============================================================================

# Load required libraries
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(reshape2)) install.packages("reshape2")

library(ggplot2)
library(dplyr)
library(gridExtra)
library(reshape2)

set.seed(456)

# ============================================================================
# SECTION 1: DEFINE POPULATION AND SUBGROUPS
# ============================================================================

cat("="*80, "\n")
cat("OPTIMAL SAMPLE SIZE DETERMINATION FOR STRATIFIED SAMPLING\n")
cat("="*80, "\n\n")

# Define subgroups (strata)
n_subgroups <- 5
subgroup_names <- c("Department A", "Department B", "Department C", 
                    "Department D", "Department E")

# Population characteristics per subgroup
population_data <- data.frame(
  Subgroup = subgroup_names,
  Population_Size = c(500, 800, 1200, 600, 900),  # N_h
  Population_Mean = c(45, 52, 48, 55, 50),        # μ_h
  Population_SD = c(8, 10, 9, 12, 7),             # σ_h
  Cost_Per_Unit = c(10, 15, 12, 20, 8),           # c_h (dollars)
  Time_Per_Unit = c(0.5, 0.8, 0.6, 1.0, 0.4),     # t_h (hours)
  Response_Rate = c(0.90, 0.85, 0.88, 0.80, 0.92) # Expected response rate
)

# Calculate total population
N_total <- sum(population_data$Population_Size)
population_data$Weight <- population_data$Population_Size / N_total

cat("POPULATION STRUCTURE:\n")
print(population_data)
cat("\nTotal Population Size (N):", N_total, "\n\n")

# ============================================================================
# SECTION 2: SAMPLE SIZE DETERMINATION FUNCTIONS
# ============================================================================

# Function 1: Simple Random Sampling (SRS) - Required sample size
calculate_srs_sample_size <- function(N, sigma, margin_error, confidence_level = 0.95) {
  z_alpha <- qnorm(1 - (1 - confidence_level)/2)
  
  # Initial estimate (infinite population)
  n0 <- (z_alpha * sigma / margin_error)^2
  
  # Finite population correction
  n <- n0 / (1 + (n0 - 1) / N)
  
  return(ceiling(n))
}

# Function 2: Stratified Random Sampling - Neyman Allocation
neyman_allocation <- function(N_h, sigma_h, total_sample_size) {
  # Optimal allocation proportional to N_h * sigma_h
  allocation_weights <- N_h * sigma_h
  n_h <- total_sample_size * allocation_weights / sum(allocation_weights)
  
  # Ensure at least 2 units per stratum
  n_h <- pmax(n_h, 2)
  
  return(ceiling(n_h))
}

# Function 3: Proportional Allocation
proportional_allocation <- function(N_h, total_sample_size) {
  weights <- N_h / sum(N_h)
  n_h <- total_sample_size * weights
  n_h <- pmax(n_h, 2)
  return(ceiling(n_h))
}

# Function 4: Optimal Allocation with Cost Constraints
optimal_allocation_with_cost <- function(N_h, sigma_h, cost_h, total_budget) {
  # Optimal allocation: n_h proportional to (N_h * sigma_h / sqrt(cost_h))
  allocation_weights <- N_h * sigma_h / sqrt(cost_h)
  
  # Calculate optimal total sample size given budget
  sum_weights <- sum(allocation_weights * sqrt(cost_h))
  n_total <- total_budget / sum_weights
  
  n_h <- n_total * allocation_weights / sum(allocation_weights)
  n_h <- pmax(n_h, 2)
  
  return(list(
    sample_sizes = ceiling(n_h),
    total_sample = sum(ceiling(n_h)),
    total_cost = sum(ceiling(n_h) * cost_h)
  ))
}

# Function 5: Sample size for desired precision
calculate_stratified_sample_size <- function(N_h, sigma_h, W_h, margin_error, 
                                            confidence_level = 0.95, 
                                            allocation = "neyman") {
  z_alpha <- qnorm(1 - (1 - confidence_level)/2)
  N <- sum(N_h)
  
  if(allocation == "neyman") {
    # Neyman allocation formula
    numerator <- (sum(W_h * sigma_h))^2
    denominator <- (margin_error / z_alpha)^2 + (1/N) * sum(W_h * sigma_h^2)
    n_total <- numerator / denominator
  } else {
    # Proportional allocation
    numerator <- sum(W_h * sigma_h^2)
    denominator <- (margin_error / z_alpha)^2 + (1/N) * sum(W_h * sigma_h^2)
    n_total <- numerator / denominator
  }
  
  return(ceiling(n_total))
}

# ============================================================================
# SECTION 3: CALCULATE SAMPLE SIZES UNDER DIFFERENT SCENARIOS
# ============================================================================

cat("="*80, "\n")
cat("SAMPLE SIZE CALCULATIONS\n")
cat("="*80, "\n\n")

# Scenario parameters
margin_error <- 2.0  # Desired margin of error
confidence_level <- 0.95
total_budget <- 5000  # dollars
max_time <- 200  # hours
acceptable_bias_percent <- 1  # 1% acceptable bias

# Overall population variance (weighted)
overall_sigma <- sqrt(sum(population_data$Weight * population_data$Population_SD^2 + 
                          population_data$Weight * (population_data$Population_Mean - 
                          sum(population_data$Weight * population_data$Population_Mean))^2))

cat("SCENARIO PARAMETERS:\n")
cat("Desired Margin of Error:", margin_error, "\n")
cat("Confidence Level:", confidence_level * 100, "%\n")
cat("Total Budget: $", total_budget, "\n")
cat("Maximum Time:", max_time, "hours\n")
cat("Acceptable Bias:", acceptable_bias_percent, "%\n\n")

# Calculate sample sizes for different approaches
results <- list()

# 1. Simple Random Sampling (ignoring strata)
n_srs <- calculate_srs_sample_size(N_total, overall_sigma, margin_error, confidence_level)
results$SRS <- list(
  total = n_srs,
  cost = n_srs * mean(population_data$Cost_Per_Unit),
  time = n_srs * mean(population_data$Time_Per_Unit),
  method = "Simple Random Sampling"
)

# 2. Stratified - Neyman Allocation
n_neyman_total <- calculate_stratified_sample_size(
  population_data$Population_Size,
  population_data$Population_SD,
  population_data$Weight,
  margin_error,
  confidence_level,
  allocation = "neyman"
)

n_neyman <- neyman_allocation(
  population_data$Population_Size,
  population_data$Population_SD,
  n_neyman_total
)

results$Neyman <- list(
  total = sum(n_neyman),
  by_stratum = n_neyman,
  cost = sum(n_neyman * population_data$Cost_Per_Unit),
  time = sum(n_neyman * population_data$Time_Per_Unit),
  method = "Neyman Allocation"
)

# 3. Stratified - Proportional Allocation
n_prop <- proportional_allocation(population_data$Population_Size, n_neyman_total)
results$Proportional <- list(
  total = sum(n_prop),
  by_stratum = n_prop,
  cost = sum(n_prop * population_data$Cost_Per_Unit),
  time = sum(n_prop * population_data$Time_Per_Unit),
  method = "Proportional Allocation"
)

# 4. Optimal with Cost Constraint
opt_cost <- optimal_allocation_with_cost(
  population_data$Population_Size,
  population_data$Population_SD,
  population_data$Cost_Per_Unit,
  total_budget
)

results$Cost_Optimal <- list(
  total = opt_cost$total_sample,
  by_stratum = opt_cost$sample_sizes,
  cost = opt_cost$total_cost,
  time = sum(opt_cost$sample_sizes * population_data$Time_Per_Unit),
  method = "Cost-Optimal Allocation"
)

# 5. Time-Constrained Allocation
time_weights <- population_data$Population_Size * population_data$Population_SD / 
                sqrt(population_data$Time_Per_Unit)
max_samples_by_time <- max_time / population_data$Time_Per_Unit
total_time_samples <- min(sum(max_samples_by_time), 
                          max_time / mean(population_data$Time_Per_Unit))

n_time <- ceiling(total_time_samples * time_weights / sum(time_weights))
n_time <- pmin(n_time, floor(max_samples_by_time))

results$Time_Optimal <- list(
  total = sum(n_time),
  by_stratum = n_time,
  cost = sum(n_time * population_data$Cost_Per_Unit),
  time = sum(n_time * population_data$Time_Per_Unit),
  method = "Time-Optimal Allocation"
)

# ============================================================================
# SECTION 4: COMPARE ALL METHODS
# ============================================================================

cat("COMPARISON OF SAMPLING METHODS:\n\n")

comparison_df <- data.frame(
  Method = character(),
  Total_Sample = numeric(),
  Total_Cost = numeric(),
  Total_Time = numeric(),
  Efficiency = numeric(),
  stringsAsFactors = FALSE
)

for(method_name in names(results)) {
  method <- results[[method_name]]
  
  # Calculate variance of estimator for comparison
  if(method_name == "SRS") {
    variance_estimator <- overall_sigma^2 / method$total * (1 - method$total / N_total)
  } else {
    # Stratified variance
    variance_estimator <- sum(population_data$Weight^2 * 
                              population_data$Population_SD^2 / 
                              method$by_stratum * 
                              (1 - method$by_stratum / population_data$Population_Size))
  }
  
  efficiency <- 1 / variance_estimator  # Higher is better
  
  comparison_df <- rbind(comparison_df, data.frame(
    Method = method$method,
    Total_Sample = method$total,
    Total_Cost = round(method$cost, 2),
    Total_Time = round(method$time, 2),
    Efficiency = round(efficiency, 2)
  ))
}

print(comparison_df)
cat("\n")

# Detailed allocation by subgroup
cat("DETAILED ALLOCATION BY SUBGROUP:\n\n")

allocation_detail <- population_data %>%
  select(Subgroup, Population_Size, Population_SD, Cost_Per_Unit, Time_Per_Unit) %>%
  mutate(
    Neyman = results$Neyman$by_stratum,
    Proportional = results$Proportional$by_stratum,
    Cost_Optimal = results$Cost_Optimal$by_stratum,
    Time_Optimal = results$Time_Optimal$by_stratum
  )

print(allocation_detail)
cat("\n")

# ============================================================================
# SECTION 5: BIAS ANALYSIS
# ============================================================================

cat("="*80, "\n")
cat("BIAS ANALYSIS\n")
cat("="*80, "\n\n")

# Function to calculate expected bias
calculate_expected_bias <- function(sample_sizes, N_h, response_rates) {
  # Non-response bias estimation
  effective_sample <- sample_sizes * response_rates
  effective_rate <- effective_sample / sample_sizes
  
  # Bias increases with lower response rates and smaller samples
  bias_factor <- (1 - mean(effective_rate)) * 100
  
  return(bias_factor)
}

bias_results <- data.frame(
  Method = names(results),
  Expected_Bias_Percent = numeric(length(results)),
  Within_Acceptable = logical(length(results))
)

for(i in seq_along(results)) {
  method_name <- names(results)[i]
  method <- results[[method_name]]
  
  if(method_name == "SRS") {
    # Assume average response rate
    expected_bias <- calculate_expected_bias(
      rep(method$total / n_subgroups, n_subgroups),
      population_data$Population_Size,
      population_data$Response_Rate
    )
  } else {
    expected_bias <- calculate_expected_bias(
      method$by_stratum,
      population_data$Population_Size,
      population_data$Response_Rate
    )
  }
  
  bias_results$Expected_Bias_Percent[i] <- round(expected_bias, 2)
  bias_results$Within_Acceptable[i] <- expected_bias <= acceptable_bias_percent
}

print(bias_results)
cat("\n")

# ============================================================================
# SECTION 6: VISUALIZATION - EXPERIMENTAL DESIGN
# ============================================================================

cat("Generating visualizations...\n\n")

# Plot 1: Sample Size Comparison
p1 <- ggplot(comparison_df, aes(x = reorder(Method, -Total_Sample), y = Total_Sample)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = Total_Sample), vjust = -0.5, size = 4) +
  labs(title = "Total Sample Size by Allocation Method",
       x = "Method", y = "Total Sample Size") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# Plot 2: Cost Comparison
p2 <- ggplot(comparison_df, aes(x = reorder(Method, -Total_Cost), y = Total_Cost)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.8) +
  geom_hline(yintercept = total_budget, linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = paste0("$", round(Total_Cost))), vjust = -0.5, size = 4) +
  annotate("text", x = 3, y = total_budget + 200, 
           label = paste0("Budget: $", total_budget), color = "red", size = 4) +
  labs(title = "Total Cost by Allocation Method",
       x = "Method", y = "Total Cost ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# Plot 3: Time Comparison
p3 <- ggplot(comparison_df, aes(x = reorder(Method, -Total_Time), y = Total_Time)) +
  geom_bar(stat = "identity", fill = "darkorange", alpha = 0.8) +
  geom_hline(yintercept = max_time, linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = round(Total_Time, 1)), vjust = -0.5, size = 4) +
  annotate("text", x = 3, y = max_time + 10, 
           label = paste0("Max Time: ", max_time, " hrs"), color = "red", size = 4) +
  labs(title = "Total Time by Allocation Method",
       x = "Method", y = "Total Time (hours)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# Plot 4: Allocation by Subgroup (Stacked Bar)
allocation_long <- allocation_detail %>%
  select(Subgroup, Neyman, Proportional, Cost_Optimal, Time_Optimal) %>%
  melt(id.vars = "Subgroup", variable.name = "Method", value.name = "Sample_Size")

p4 <- ggplot(allocation_long, aes(x = Method, y = Sample_Size, fill = Subgroup)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  geom_text(aes(label = Sample_Size), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Sample Allocation Across Subgroups",
       x = "Allocation Method", y = "Sample Size",
       fill = "Subgroup") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.position = "right")

# Plot 5: Efficiency vs Cost Trade-off
p5 <- ggplot(comparison_df, aes(x = Total_Cost, y = Efficiency)) +
  geom_point(size = 5, color = "darkblue", alpha = 0.7) +
  geom_text(aes(label = Method), vjust = -1, hjust = 0.5, size = 3) +
  labs(title = "Efficiency vs Cost Trade-off",
       x = "Total Cost ($)", y = "Efficiency (1/Variance)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# Plot 6: Population Structure and Sample Distribution (Neyman)
pop_sample_comparison <- population_data %>%
  mutate(Sample_Neyman = results$Neyman$by_stratum,
         Pop_Percent = Population_Size / sum(Population_Size) * 100,
         Sample_Percent = Sample_Neyman / sum(Sample_Neyman) * 100) %>%
  select(Subgroup, Pop_Percent, Sample_Percent) %>%
  melt(id.vars = "Subgroup", variable.name = "Type", value.name = "Percentage")

p6 <- ggplot(pop_sample_comparison, aes(x = Subgroup, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "Population vs Sample Distribution (Neyman Allocation)",
       x = "Subgroup", y = "Percentage (%)",
       fill = "Distribution") +
  scale_fill_manual(values = c("Pop_Percent" = "lightblue", 
                               "Sample_Percent" = "coral"),
                    labels = c("Population", "Sample")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# Combine plots
grid.arrange(p1, p2, p3, ncol = 2)
grid.arrange(p4, p5, p6, ncol = 2)

# ============================================================================
# SECTION 7: EXPERIMENTAL DESIGN DIAGRAM
# ============================================================================

# Create a visual representation of the sampling design
design_data <- allocation_detail %>%
  mutate(
    x_pos = 1:n(),
    y_pop = Population_Size,
    y_sample = Neyman
  )

p_design <- ggplot(design_data) +
  # Population bars
  geom_rect(aes(xmin = x_pos - 0.4, xmax = x_pos + 0.4, 
                ymin = 0, ymax = Population_Size),
            fill = "lightblue", alpha = 0.5, color = "black") +
  # Sample bars
  geom_rect(aes(xmin = x_pos - 0.3, xmax = x_pos + 0.3, 
                ymin = 0, ymax = Neyman),
            fill = "darkblue", alpha = 0.8, color = "black") +
  geom_text(aes(x = x_pos, y = Population_Size + 50, label = Subgroup),
            angle = 0, hjust = 0.5, size = 4, fontface = "bold") +
  geom_text(aes(x = x_pos, y = Population_Size / 2, 
                label = paste0("N=", Population_Size)),
            size = 3, color = "darkblue") +
  geom_text(aes(x = x_pos, y = Neyman / 2, 
                label = paste0("n=", Neyman)),
            size = 3.5, color = "white", fontface = "bold") +
  labs(title = "Experimental Design: Population and Sample Structure",
       subtitle = "Neyman Optimal Allocation",
       x = "", y = "Size") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

print(p_design)

# ============================================================================
# SECTION 8: RECOMMENDATION AND SUMMARY
# ============================================================================

cat("="*80, "\n")
cat("RECOMMENDATION\n")
cat("="*80, "\n\n")

# Find best method based on constraints
best_methods <- comparison_df %>%
  filter(Total_Cost <= total_budget, Total_Time <= max_time) %>%
  arrange(desc(Efficiency))

if(nrow(best_methods) > 0) {
  cat("RECOMMENDED METHOD:", best_methods$Method[1], "\n")
  cat("Total Sample Size:", best_methods$Total_Sample[1], "\n")
  cat("Total Cost: $", best_methods$Total_Cost[1], "\n")
  cat("Total Time:", best_methods$Total_Time[1], "hours\n")
  cat("Efficiency Score:", best_methods$Efficiency[1], "\n\n")
  
  recommended_allocation <- switch(
    as.character(best_methods$Method[1]),
    "Neyman Allocation" = results$Neyman$by_stratum,
    "Proportional Allocation" = results$Proportional$by_stratum,
    "Cost-Optimal Allocation" = results$Cost_Optimal$by_stratum,
    "Time-Optimal Allocation" = results$Time_Optimal$by_stratum,
    rep(ceiling(best_methods$Total_Sample[1] / n_subgroups), n_subgroups)
  )
  
  cat("RECOMMENDED ALLOCATION BY SUBGROUP:\n")
  for(i in 1:n_subgroups) {
    cat(sprintf("  %s: %d samples\n", subgroup_names[i], recommended_allocation[i]))
  }
  
} else {
  cat("WARNING: No method satisfies both budget and time constraints!\n")
  cat("Consider increasing budget or time, or accepting lower precision.\n")
}

cat("\n")
cat("="*80, "\n")
cat("ANALYSIS COMPLETE\n")
cat("="*80, "\n")

# ============================================================================
# SECTION 9: EXPORT RESULTS
# ============================================================================

# Save all results to CSV files
write.csv(comparison_df, "sample_size_comparison.csv", row.names = FALSE)
write.csv(allocation_detail, "allocation_by_subgroup.csv", row.names = FALSE)
write.csv(bias_results, "bias_analysis.csv", row.names = FALSE)

cat("\nResults exported to CSV files:\n")
cat("  - sample_size_comparison.csv\n")
cat("  - allocation_by_subgroup.csv\n")
cat("  - bias_analysis.csv\n\n")

cat("Script execution completed successfully!\n")
