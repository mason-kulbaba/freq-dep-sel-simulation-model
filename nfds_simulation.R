# Negative Frequency-Dependent Selection Simulation Model
# Protandrous plants with phenological variation
# ================================================

# tidyverse is optional; used only for visualization
# library(tidyverse)

# ============================================================================
# Plant Class and Helper Functions
# ============================================================================

#' Plant object with reproductive traits
#' @param id Unique plant identifier
#' @param n_flowers Number of flowers
#' @param male_phase_length Days in male phase
#' @param female_phase_length Days in female phase
#' @param anthers_per_flower Anthers produced per flower
#' @param ovules_per_flower Ovules produced per flower
create_plant <- function(
    id = 1,
    n_flowers = 30,
    male_phase_length = 3,
    female_phase_length = 3,
    anthers_per_flower = 20,
    ovules_per_flower = 30
) {
  list(
    id = id,
    n_flowers = n_flowers,
    male_phase_length = male_phase_length,
    female_phase_length = female_phase_length,
    anthers_per_flower = anthers_per_flower,
    ovules_per_flower = ovules_per_flower,
    total_anthers = n_flowers * anthers_per_flower,
    total_ovules = n_flowers * ovules_per_flower,
    flowering_schedule = NULL,  # Will be filled during simulation
    male_pollen = 0,
    female_receptivity = 0
  )
}

#' Initialize a population of plants
#' @param n_plants Number of plants to create
#' @param default_params List of default parameters for all plants
initialize_population <- function(n_plants = 100, default_params = list()) {
  params <- modifyList(
    list(
      n_flowers = 30,
      male_phase_length = 3,
      female_phase_length = 3,
      anthers_per_flower = 20,
      ovules_per_flower = 30
    ),
    default_params
  )
  
  lapply(1:n_plants, function(i) {
    do.call(create_plant, c(id = i, params))
  })
}

#' Generate flowering schedule with optional variation
#' Creates a schedule showing which flowers are in which phase on each day
#' @param plant A plant object
#' @param season_length Number of days in flowering season
#' @param stagger_flowers Logical; if TRUE, staggers flower opening
#' @param stagger_sd Standard deviation of stagger effect (in days)
generate_flowering_schedule <- function(
    plant,
    season_length = 180,
    stagger_flowers = TRUE,
    stagger_sd = 2
) {
  
  cycle_length <- plant$male_phase_length + plant$female_phase_length
  
  # Create schedule matrix: rows = flowers, cols = days
  schedule <- matrix(0, nrow = plant$n_flowers, ncol = season_length)
  
  for (flower_id in 1:plant$n_flowers) {
    # Stagger flower opening
    start_day <- if (stagger_flowers) {
      max(1, round(rnorm(1, flower_id * 2, stagger_sd)))
    } else {
      flower_id * 2
    }
    
    # Fill in flowering days
    day <- start_day
    while (day <= season_length) {
      # Male phase
      for (m_day in 0:(plant$male_phase_length - 1)) {
        if (day + m_day <= season_length) {
          schedule[flower_id, day + m_day] <- 1  # 1 = male
        }
      }
      # Female phase
      for (f_day in 0:(plant$female_phase_length - 1)) {
        if (day + plant$male_phase_length + f_day <= season_length) {
          schedule[flower_id, day + plant$male_phase_length + f_day] <- 2  # 2 = female
        }
      }
      day <- day + cycle_length
    }
  }
  
  schedule
}

#' Introduce mutations in a population
#' Randomly modifies plant traits with specified probability
#' @param population List of plant objects
#' @param mutation_rate Probability of mutation per plant
#' @param mutation_effects List with sd for each trait to mutate
introduce_mutations <- function(
    population,
    mutation_rate = 0.05,
    mutation_effects = list(
      n_flowers = 2,
      male_phase_length = 0.5,
      female_phase_length = 0.5,
      anthers_per_flower = 2,
      ovules_per_flower = 2
    )
) {
  
  lapply(population, function(plant) {
    if (runif(1) < mutation_rate) {
      # Randomly select which trait to mutate
      trait <- sample(names(mutation_effects), 1)
      effect <- mutation_effects[[trait]]
      
      old_val <- plant[[trait]]
      new_val <- max(1, round(rnorm(1, old_val, effect)))
      plant[[trait]] <- new_val
      
      # Update derived traits
      if (trait == "n_flowers" || trait == "anthers_per_flower") {
        plant$total_anthers <- plant$n_flowers * plant$anthers_per_flower
      }
      if (trait == "n_flowers" || trait == "ovules_per_flower") {
        plant$total_ovules <- plant$n_flowers * plant$ovules_per_flower
      }
    }
    plant
  })
}

# ============================================================================
# Mating and Fitness Functions
# ============================================================================

#' Calculate pollen compatibility based on phenology overlap
#' Measures the overlap in female receptive period with male donor period
#' @param donor_schedule Flowering schedule of pollen donor
#' @param recipient_schedule Flowering schedule of pollen recipient
calculate_pollen_pool <- function(
    donor_schedule,
    recipient_schedule,
    season_length = 180
) {
  
  # Find days when donor is in male phase (phase = 1)
  donor_male_days <- which(colSums(donor_schedule == 1) > 0)
  
  # Find days when recipient is in female phase (phase = 2)
  recipient_female_days <- which(colSums(recipient_schedule == 2) > 0)
  
  # Calculate overlap
  overlap <- length(intersect(donor_male_days, recipient_female_days))
  
  overlap
}

#' Simulate mating round in the population
#' Each female flower receives pollen from available males
#' @param population List of plant objects
#' @param season_length Length of flowering season
#' @param selfing_rate Proportion of pollen that is self-pollen (0-1)
simulate_mating <- function(
    population,
    season_length = 180,
    selfing_rate = 0.1
) {
  
  n_plants <- length(population)
  
  # Generate flowering schedules
  population <- lapply(population, function(plant) {
    plant$flowering_schedule <- generate_flowering_schedule(
      plant,
      season_length = season_length
    )
    plant
  })
  
  # Initialize results
  results <- list()
  
  for (i in 1:n_plants) {
    recipient <- population[[i]]
    n_female_flowers <- sum(colSums(recipient$flowering_schedule == 2) > 0)
    
    if (n_female_flowers == 0) next
    
    # Pollen pool from all males (frequency-dependent component)
    pollen_pool <- numeric(n_plants)
    
    for (j in 1:n_plants) {
      donor <- population[[j]]
      
      # Compatibility based on phenology overlap
      overlap <- calculate_pollen_pool(
        donor$flowering_schedule,
        recipient$flowering_schedule,
        season_length
      )
      
      # Pollen contribution = overlap * pollen availability
      pollen_pool[j] <- overlap * donor$total_anthers
    }
    
    # Normalize pollen pool
    pollen_total <- sum(pollen_pool)
    if (pollen_total > 0) {
      pollen_freq <- pollen_pool / pollen_total
      
      # Apply selfing rate
      pollen_freq[i] <- pollen_freq[i] * (1 + selfing_rate)
      pollen_freq <- pollen_freq / sum(pollen_freq)
      
      results[[i]] <- list(
        plant_id = i,
        n_female_flowers = n_female_flowers,
        total_ovules = n_female_flowers * recipient$ovules_per_flower,
        pollen_frequencies = pollen_freq,
        pollen_total = pollen_total
      )
    }
  }
  
  results
}

# ============================================================================
# Simulation Runner
# ============================================================================

#' Run complete NFDS simulation
#' @param n_generations Number of generations to simulate
#' @param n_plants Initial population size
#' @param season_length Days in flowering season
#' @param mutation_rate Probability of mutation per plant
#' @param mutation_effects Named list with sd for each trait
#' @param selfing_rate Proportion of self-pollen
run_nfds_simulation <- function(
    n_generations = 10,
    n_plants = 100,
    season_length = 180,
    mutation_rate = 0.05,
    mutation_effects = list(
      n_flowers = 2,
      male_phase_length = 0.5,
      female_phase_length = 0.5,
      anthers_per_flower = 2,
      ovules_per_flower = 2
    ),
    selfing_rate = 0.1,
    verbose = TRUE
) {
  
  # Initialize
  population <- initialize_population(n_plants)
  
  # Storage for results
  generation_stats <- data.frame(
    generation = integer(),
    mean_flowers = numeric(),
    sd_flowers = numeric(),
    mean_male_phase = numeric(),
    sd_male_phase = numeric(),
    mean_female_phase = numeric(),
    sd_female_phase = numeric(),
    mean_anthers = numeric(),
    sd_anthers = numeric(),
    mean_ovules = numeric(),
    sd_ovules = numeric()
  )
  
  for (gen in 1:n_generations) {
    if (verbose) cat("Generation", gen, "of", n_generations, "\n")
    
    # Record statistics
    flowers <- sapply(population, \(p) p$n_flowers)
    male_phase <- sapply(population, \(p) p$male_phase_length)
    female_phase <- sapply(population, \(p) p$female_phase_length)
    anthers <- sapply(population, \(p) p$total_anthers)
    ovules <- sapply(population, \(p) p$total_ovules)
    
    generation_stats <- rbind(
      generation_stats,
      data.frame(
        generation = gen,
        mean_flowers = mean(flowers),
        sd_flowers = sd(flowers),
        mean_male_phase = mean(male_phase),
        sd_male_phase = sd(male_phase),
        mean_female_phase = mean(female_phase),
        sd_female_phase = sd(female_phase),
        mean_anthers = mean(anthers),
        sd_anthers = sd(anthers),
        mean_ovules = mean(ovules),
        sd_ovules = sd(ovules)
      )
    )
    
    # Simulate mating
    mating_results <- simulate_mating(
      population,
      season_length = season_length,
      selfing_rate = selfing_rate
    )
    
    # Create next generation (simplified: assume equal fitness, selection via mating)
    population <- introduce_mutations(
      population,
      mutation_rate = mutation_rate,
      mutation_effects = mutation_effects
    )
  }
  
  list(
    population = population,
    statistics = generation_stats
  )
}

# ============================================================================
# Example Usage
# ============================================================================

if (FALSE) {
  # Run simulation with defaults
  results <- run_nfds_simulation(
    n_generations = 20,
    n_plants = 100,
    season_length = 180,
    mutation_rate = 0.05
  )
  
  # Visualize trait evolution
  results$statistics |>
    ggplot(aes(x = generation)) +
    geom_line(aes(y = mean_flowers), color = "steelblue", linewidth = 1) +
    geom_ribbon(aes(ymin = mean_flowers - sd_flowers,
                    ymax = mean_flowers + sd_flowers),
                alpha = 0.2, fill = "steelblue") +
    labs(title = "Flower Number Evolution",
         x = "Generation", y = "Mean Number of Flowers") +
    theme_minimal()
  
  # View final population traits
  tibble(
    n_flowers = sapply(results$population, \(p) p$n_flowers),
    male_phase = sapply(results$population, \(p) p$male_phase_length),
    female_phase = sapply(results$population, \(p) p$female_phase_length),
    anthers_per_flower = sapply(results$population, \(p) p$anthers_per_flower),
    ovules_per_flower = sapply(results$population, \(p) p$ovules_per_flower)
  )
}
