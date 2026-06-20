# Negative Frequency-Dependent Selection Simulation Model

## Overview

This R package implements a population genetics simulation model to explore negative frequency-dependent selection (NFDS) in protandrous flowering plants. The model tracks phenotypic evolution in reproductive traits when mating success depends on phenological synchrony with available mates.

## Key Features

### Plant Biology
- **Protandry**: Each flower has a male phase followed by a female phase
- **Temporal staggering**: Flowers open at different times during the season
- **Configurable phenology**: Male and female phase durations are evolvable traits
- **Reproductive investment**: Number of anthers and ovules per flower are evolvable

### Evolutionary Dynamics
- **Mutation-driven variation**: Introduces variation in phenological and reproductive traits
- **Frequency-dependent mating**: Pollen compatibility based on phenological overlap
- **Tracking**: Records population statistics across generations

### Core Plant Traits
Each plant is initialized with:
- **30 flowers** (configurable)
- **Male phase**: 3 days (configurable, subject to mutation)
- **Female phase**: 3 days (configurable, subject to mutation)
- **20 anthers per flower** (configurable)
- **30 ovules per flower** (configurable)

## Files

### `nfds_simulation.R`
Main simulation code containing:
- `create_plant()` - Initialize a single plant
- `initialize_population()` - Create initial population
- `generate_flowering_schedule()` - Create flower phenology schedule
- `introduce_mutations()` - Add heritable variation
- `calculate_pollen_pool()` - Assess phenological compatibility
- `simulate_mating()` - Execute mating round
- `run_nfds_simulation()` - Complete simulation pipeline

### `demo_scenarios.R`
Example usage with multiple ecological scenarios:
1. **Phenological variation**: Evolution of flower timing
2. **Reproductive investment**: Evolution of flower number and gametete production
3. **Baseline (no mutation)**: Control scenario
4. **Mutation rate comparison**: How mutation rate affects trait variance

## Quick Start

```r
# Source the simulation code
source("nfds_simulation.R")

# Run a basic simulation
results <- run_nfds_simulation(
  n_generations = 20,
  n_plants = 100,
  season_length = 180,
  mutation_rate = 0.05
)

# View trait statistics across generations
results$statistics

# Examine final population
sapply(results$population, function(p) c(
  flowers = p$n_flowers,
  male_phase = p$male_phase_length,
  female_phase = p$female_phase_length
))
```

## API Reference

### Main Function: `run_nfds_simulation()`

```r
run_nfds_simulation(
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
)
```

**Arguments:**
- `n_generations`: Number of generations to simulate
- `n_plants`: Initial population size
- `season_length`: Days of flowering season
- `mutation_rate`: Probability of mutation per individual per generation
- `mutation_effects`: Named list with standard deviations for normally-distributed mutations
  - Values represent SD of mutation effect on each trait
  - Larger values = larger mutations
  - Set to 0 to disable mutation in a trait
- `selfing_rate`: Proportion of self-fertilization (0-1)
- `verbose`: Print progress messages

**Returns:**
A list with two elements:
- `population`: List of final plant objects
- `statistics`: Data frame with generation-by-generation statistics

### Plant Object Structure

Each plant is a list with:
```
$id                    - Plant identifier
$n_flowers             - Number of flowers
$male_phase_length     - Duration of male phase (days)
$female_phase_length   - Duration of female phase (days)
$anthers_per_flower    - Number of anthers per flower
$ovules_per_flower     - Number of ovules per flower
$total_anthers         - Total pollen production
$total_ovules          - Total ovule production
$flowering_schedule    - Matrix: flowers × days (phase at each time point)
```

## Understanding the Model

### Phenology and Mating
- A flower's phenological stage is either male (1), female (2), or not flowering (0)
- Pollen can only fertilize female flowers
- Fertilization probability increases with phenological overlap
- A plant with high flower overlap with a donor receives more pollen from that donor

### Mutation Mechanism
- Each generation, each plant has probability `mutation_rate` of mutating
- When a mutation occurs, a random trait is selected
- The new trait value is drawn from `Normal(old_value, mutation_effect_sd)`
- Mutations are permanent and heritable

### Evolutionary Dynamics
The simulation assumes:
- No selection against mutations (neutral evolution with mutation)
- Mating is proportional to phenological compatibility
- All plants produce seeds and contribute equally to next generation (for now)
- Trait correlations arise from mutation and mating patterns

## Example Scenarios

### Scenario 1: Phenological Evolution
Explore how flower timing (male/female phase duration) evolves:
```r
results <- run_nfds_simulation(
  n_generations = 20,
  mutation_rate = 0.1,
  mutation_effects = list(
    male_phase_length = 1.0,   # Strong effect
    female_phase_length = 1.0,
    n_flowers = 0.5,           # Weak effect
    anthers_per_flower = 0.5,
    ovules_per_flower = 0.5
  )
)
```

### Scenario 2: Reproductive Investment
Explore evolution of flower and gametete production:
```r
results <- run_nfds_simulation(
  n_generations = 20,
  mutation_rate = 0.1,
  mutation_effects = list(
    n_flowers = 3,             # Strong effect
    anthers_per_flower = 3,
    ovules_per_flower = 3,
    male_phase_length = 0.3,   # Weak effect
    female_phase_length = 0.3
  )
)
```

## Future Extensions

Potential improvements to the model:
1. **Explicit fitness**: Add selection on reproductive success (number of seeds set)
2. **Genetic architecture**: Implement linkage and pleiotropy
3. **Population genetics**: Track allele frequencies and effective population size
4. **Resource constraints**: Model trade-offs between flower number and phase duration
5. **Environmental variation**: Seasonal variation in flowering conditions
6. **Pollen limitation**: Explicit model of pollen availability
7. **Viability selection**: Mortality as a function of phenotype

## References

Negative frequency-dependent selection occurs when rare phenotypes have higher fitness, maintaining polymorphism in populations. This model specifically addresses the case where mating compatibility depends on temporal synchrony.

Key papers on NFDS and protandry:
- Charlesworth & Charlesworth (1978, 1981) - Theory of NFDS in plants
- Lloyd & Webb (1986) - Evolution of protandry
- Schoen (1982) - Self-fertilization and flower protandry
