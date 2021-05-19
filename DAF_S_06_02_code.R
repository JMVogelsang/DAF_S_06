# Package installation ####
install.packages("decisionSupport")
install.packages("DiagrammeR")
library(DiagrammeR)

# Plot the impact pathway ####
?mermaid
# mermaid function from DiagrammeR package creates graphical impact pathways.
# LR makes arrows in graph go from left to right.
mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        MP(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->FR(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Cost labour)-->FR; linkStyle 3 stroke: red, stroke-width:1.5px
        CM(Cost management)-->FR;linkStyle 4 stroke: blue, stroke-width:1.5px")

# RL makes arrows in graph go from right to left
mermaid("graph RL
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        MP(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->FR(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Cost labour)-->FR; linkStyle 3 stroke: red, stroke-width:1.5px
        CM(Cost management)-->FR;linkStyle 4 stroke: blue, stroke-width:1.5px")

# TD makes arrows in graph go from right to left
mermaid("graph TB
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        MP(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->FR(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Cost labour)-->FR; linkStyle 3 stroke: red, stroke-width:1.5px
        CM(Cost management)-->FR;linkStyle 4 stroke: blue, stroke-width:1.5px")

# BT makes arrows in graph go from right to left
mermaid("graph BT
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        MP(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->FR(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Cost labour)-->FR; linkStyle 3 stroke: red, stroke-width:1.5px
        CM(Cost management)-->FR;linkStyle 4 stroke: blue, stroke-width:1.5px")

# Changing colour and size of arrows and changing colour of boxes.
mermaid("graph BT
        Y(Yield)-->I(Income); linkStyle 0 stroke:blue, stroke-width:2px
        MP(Market price)-->I; linkStyle 1 stroke: blue, stroke-width:2px
        I-->FR(Final result); linkStyle 2 stroke: brown, stroke-width:3px
        CL(Cost labour)-->FR; linkStyle 3 stroke: brown, stroke-width:3px
        CM(Cost management)-->FR;linkStyle 4 stroke: brown, stroke-width:3px
        style Y fill:orange
        style MP fill:orange
        style I fill:green
        style CL fill:green
        style CM fill:green
        style FR fill:red")

# Build the model ####
# Creates a table with the variables yield, market price... and their caracteristics.
input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost", "Management_cost"),
                              lower = c(6000, 3, 500, 100),
                              median = NA,
                              upper = c(14000, 8, 1000, 2000),
                              distribution = c("posnorm", "posnorm", "posnorm", "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)", "Management cost (USD/ha)"),
                              Description = c("Yield in a sweet cherry farm under normal conditions",
                                              "Price of sweet cherry in a normal season",
                                              "Labor costs in a normal season", "Management costs in a normal season"))
input_estimates


# Implementation of the model ####
model_function <- function(){
  
  # Estimate the income in a normal season.
  income <- Yield * Market_price
  
  # Estimate the overall costs in a normal season.
  overall_costs <- Labor_cost + Management_cost
  
  # Estimate the final results from the model.
  final_result <- income - overall_costs
  
  # Generate the list of outputs from the Monte Carlo simulation.
  return(list(final_result = final_result))
}

library(decisionSupport)
# Run the Monte Carlo simulation using the model function and data from input_estimates.
chile_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                    model_function = model_function,
                                    numberOfModelRuns = 800,
                                    functionSyntax = "plainNames")

chile_mc_simulation


# Visualize model output as graph.  
plot_distributions(mcSimulation_object = chile_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")

# Visualize model output as different graph. 
plot_distributions(mcSimulation_object = chile_mc_simulation,
                   vars = "final_result",
                   method = "hist_simple_overlay",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")

# Visualize model output as boxplot.. 
plot_distributions(mcSimulation_object = chile_mc_simulation,
                   vars = "final_result",
                   method = "boxplot",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")

# Testing ####
# Creates possible values for the input variables.
make_variables(as.estimate(input_estimates))

# creates a possible output for the costs, different in each run due to make-variables before.
Labor_cost + Management_cost


AAA
