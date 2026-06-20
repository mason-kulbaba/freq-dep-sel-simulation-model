# Example scenarios for NFDS simulation
# ===============================================

source("nfds_simulation.R")

# ============================================================================
# Scenario 1: Strong phenological variation (flower timing)
# ============================================================================

cat("\n=== SCENARIO 1: Phenological Variation ===\n")

scenario1 <- run_nfds_simulation(
  n_generations = 20,
  n_plants = 100,
  season_length = 180,
  mutation_rate = 0.1,
  mutation_effects = list(
    n_flowers = 1,           # Small mutation
    male_phase_length = 1.0, # Larger mutation: timing variation
    female_phase_length = 1.0,
    anthers_per_flower = 1,
    ovules_per_flower = 1
  ),
  selfing_rate = 0.1
)

# Plot phenological trait evolution
par(mfrow = c(2, 2))

plot(scenario1$statistics$generation, scenario1$statistics$mean_male_phase,
     main = "Male Phase Length Evolution",
     xlab = "Generation", ylab = "Days in Male Phase",
     type = "l", col = "steelblue", lwd = 2)

plot(scenario1$statistics$generation, scenario1$statistics$mean_female_phase,
     main = "Female Phase Length Evolution",
     xlab = "Generation", ylab = "Days in Female Phase",
     type = "l", col = "coral", lwd = 2)

# Boxplot of final population phenotypes
final_pop_1 <- data.frame(
  n_flowers = sapply(scenario1$population, \(p) p$n_flowers),
  male_phase = sapply(scenario1$population, \(p) p$male_phase_length),
  female_phase = sapply(scenario1$population, \(p) p$female_phase_length)
)

boxplot(final_pop_1$male_phase ~ 1, main = "Final Male Phase Distribution",
        ylab = "Days", names = "Male Phase")

boxplot(final_pop_1$female_phase ~ 1, main = "Final Female Phase Distribution",
        ylab = "Days", names = "Female Phase")

par(mfrow = c(1, 1))

cat("Scenario 1 Summary:\n")
cat("  Male phase mean (gen 1):", 
    scenario1$statistics$mean_male_phase[1], "\n")
cat("  Male phase mean (gen 20):", 
    scenario1$statistics$mean_male_phase[20], "\n")
cat("  Female phase mean (gen 1):", 
    scenario1$statistics$mean_female_phase[1], "\n")
cat("  Female phase mean (gen 20):", 
    scenario1$statistics$mean_female_phase[20], "\n")

# ============================================================================
# Scenario 2: Floral reproductive investment variation
# ============================================================================

cat("\n=== SCENARIO 2: Reproductive Investment Variation ===\n")

scenario2 <- run_nfds_simulation(
  n_generations = 20,
  n_plants = 100,
  season_length = 180,
  mutation_rate = 0.1,
  mutation_effects = list(
    n_flowers = 3,           # Larger mutation
    male_phase_length = 0.3,
    female_phase_length = 0.3,
    anthers_per_flower = 3,  # Larger mutation
    ovules_per_flower = 3    # Larger mutation
  ),
  selfing_rate = 0.1
)

par(mfrow = c(2, 2))

plot(scenario2$statistics$generation, scenario2$statistics$mean_flowers,
     main = "Flower Number Evolution",
     xlab = "Generation", ylab = "Number of Flowers",
     type = "l", col = "darkgreen", lwd = 2)

plot(scenario2$statistics$generation, scenario2$statistics$mean_anthers,
     main = "Total Anthers Evolution",
     xlab = "Generation", ylab = "Anthers per Plant",
     type = "l", col = "purple", lwd = 2)

plot(scenario2$statistics$generation, scenario2$statistics$mean_ovules,
     main = "Total Ovules Evolution",
     xlab = "Generation", ylab = "Ovules per Plant",
     type = "l", col = "orange", lwd = 2)

# Final population
final_pop_2 <- data.frame(
  n_flowers = sapply(scenario2$population, \(p) p$n_flowers),
  total_anthers = sapply(scenario2$population, \(p) p$total_anthers),
  total_ovules = sapply(scenario2$population, \(p) p$total_ovules)
)

plot(final_pop_2$total_anthers, final_pop_2$total_ovules,
     main = "Anther-Ovule Correlation (Final Gen)",
     xlab = "Total Anthers", ylab = "Total Ovules",
     pch = 16, col = rgb(0, 0, 0, 0.4))

par(mfrow = c(1, 1))

cat("Scenario 2 Summary:\n")
cat("  Flower number mean (gen 1):", 
    scenario2$statistics$mean_flowers[1], "\n")
cat("  Flower number mean (gen 20):", 
    scenario2$statistics$mean_flowers[20], "\n")
cat("  Corr(anthers, ovules):", 
    round(cor(final_pop_2$total_anthers, final_pop_2$total_ovules), 3), "\n")

# ============================================================================
# Scenario 3: No mutation (baseline)
# ============================================================================

cat("\n=== SCENARIO 3: No Mutation (Baseline) ===\n")

scenario3 <- run_nfds_simulation(
  n_generations = 20,
  n_plants = 100,
  mutation_rate = 0.0,  # No mutation
  verbose = FALSE
)

cat("Scenario 3 Summary:\n")
cat("  Population remains monomorphic (no variation introduced)\n")
cat("  Mean flowers all generations:", 
    unique(scenario3$statistics$mean_flowers), "\n")

# ============================================================================
# Compare mutation rates
# ============================================================================

cat("\n=== COMPARING MUTATION RATES ===\n")

compare_scenarios <- lapply(c(0.0, 0.05, 0.1, 0.2), function(mut_rate) {
  results <- run_nfds_simulation(
    n_generations = 15,
    n_plants = 100,
    mutation_rate = mut_rate,
    mutation_effects = list(
      n_flowers = 2,
      male_phase_length = 0.8,
      female_phase_length = 0.8,
      anthers_per_flower = 2,
      ovules_per_flower = 2
    ),
    verbose = FALSE
  )
  
  final_flowers <- sapply(results$population, \(p) p$n_flowers)
  final_male_phase <- sapply(results$population, \(p) p$male_phase_length)
  
  list(
    mutation_rate = mut_rate,
    final_flower_var = var(final_flowers),
    final_phase_var = var(final_male_phase),
    final_flower_mean = mean(final_flowers),
    final_phase_mean = mean(final_male_phase)
  )
})

comparison_df <- do.call(rbind, lapply(compare_scenarios, as.data.frame))
print(comparison_df)

cat("\nKey insights:\n")
cat("  - Higher mutation rates lead to greater phenotypic variance\n")
cat("  - Phenotypic variance scales with mutation rate\n")
cat("  - Low mutation rates may not maintain variation\n")
