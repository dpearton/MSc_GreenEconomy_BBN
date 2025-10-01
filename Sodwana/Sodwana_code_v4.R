# BBNet Model for Sodwana Bay
# Version 4.0

# Code to run the BBNet model and scenarios for the MSc Green Economy Thesis.
# Dr David Pearton
# 2025/09/26

# This will load an run the versions of the scenarios associated with the thesis:
# Using Bayesian belief network models to assess potential conservation interventions in the iSimangaliso Wetland Park. 
# Award for which degree is submitted: Master of Science 
# Supervisor: Prof Rick Stafford Faculty of Science and Technology 
# Bournemouth University 
# Submission: September 2025 


# Install and load bbnet and associated packages
bbnet_packages <- c('bbnet', 'tidyverse', 'patchwork')

# Install packages not yet installed
installed_packages <- bbnet_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
install.packages(bbnet_packages[!installed_packages])
                   }
# Packages loading
invisible(lapply(bbnet_packages, library, character.only = TRUE))
  
# Set up working directory data directory, etc                   
setwd("~/BBN/Sodwana")
dir.create('./plots')
plot_path = './plots/'
model_path = './models/'
scenario_path = './scenarios/'
#############################
# Run Sodwana model version 4.0
# Load data

Sodwana_model <- read.csv(paste0(model_path,"sodwana_cc_v4_model.csv"), header = TRUE)

head(Sodwana_model)

########################
# Load Scenarios


sodwana_scenario_diveres_low <- read.csv(paste0(scenario_path,"sodwana_scenario_diveres_low.csv"), header = TRUE)
sodwana_scenario_diveres_high <- read.csv(paste0(scenario_path,"sodwana_scenario_diveres_high.csv"), header = TRUE)
sodwana_scenario_levy_low <- read.csv(paste0(scenario_path,"sodwana_scenario_levy_low.csv"), header = TRUE)
sodwana_scenario_levy_high <- read.csv(paste0(scenario_path,"sodwana_scenario_levy_high.csv"), header = TRUE)

# Scenario names
scenario1 = "Low dive restrictions 2MR"
scenario2 = "High dive restrictions 2MR"
scenario3 = "Low conservation levy"
scenario4 = "High conservation levy"

##############################
# Visualise network to check connections

Sodwana_networkdiagram <- read.csv(paste0(model_path,"sodwana_cc_v4_networkdiagram.csv"), header = T)

bbn.network.diagram(Sodwana_networkdiagram, arrange = layout.fruchterman.reingold, font.size = 1)

bbn.network.diagram(Sodwana_networkdiagram, arrange = layout.sphere, font.size = 1.5)
bbn.network.diagram(Sodwana_networkdiagram, arrange = layout.random, font.size = 1.5)
bbn.network.diagram(Sodwana_networkdiagram, arrange = layout.circle, font.size = 1)

# Save circular layout network
png(paste0(plot_path, "Sodwana_networkdiagram_circular.png"), units = "cm", height = 48, width = 48, res = 300)
bbn.network.diagram(Sodwana_networkdiagram, arrange = layout.circle, font.size = 1, arrow.size = 4)
dev.off()


######################
# check sensitivity
Sodwana_nodes <- Sodwana_model$X
Sodwana_nodes[[1]]

# Test sensitivity of the first node 
Sodwana_sensitivity_Sodwana_1 <- bbn.sensitivity(Sodwana_model, boot_max = 1000, Sodwana_nodes[[1]])


# Method 1 for testing sensitivity of all the nodes
results_Sodwana <- lapply(Sodwana_nodes, function(node) {
  bbn.sensitivity(Sodwana_model, node, boot_max = 1000)
})

names(results_Sodwana) <- Sodwana_nodes
results_Sodwana

saveRDS(results_Sodwana, "Sodwana_model_sensitivity.RDS")

Sodwana_sensitivity_bar <- results_Sodwana$coral_reef_2MR %>% ggplot() +
    geom_col(aes(x= sens.output, y=Freq), fill = "darkgreen") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, color = "black")) +
    labs(title = "Sensitivity analysis: Sodwana Bay\ncoral_reef_2MR") 

Sodwana_sensitivity_bar

ggsave(paste0(plot_path, "Sodwana_sensitivity_bar.png"), 
       Sodwana_sensitivity_bar,
       units = "cm", height = 24, width = 48, dpi = 300)



# Alternative method of testing sensitivity of all the nodes. 
# Runs a bit quicker than method 1 and output prettier.

results_df <- map_dfr(Sodwana_nodes, ~{
  out <- bbn.sensitivity(Sodwana_model, .x, boot_max = 1000)
  data.frame(node = .x, out)
})


write_csv(results_df, "Sodwana_model_sensitivity.csv")

Sodwana_model_sensitivity <- results_df %>% ggplot(aes(x=sens.output, y = Freq, fill = node)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~ node)

Sodwana_model_sensitivity

ggsave(paste0(plot_path, "Sodwana_model_sensitivity.png"),
       Sodwana_model_sensitivity,
       units = "cm",
       height = 48,
       width = 76,
       dpi = 400
       )


#######################
# Run Scenarios

# Test scenario1 with 100 bootstraps

Sodwana_models_1_b100_run1 <- bbn.predict(bbn.model = Sodwana_model, 
            priors1 = sodwana_scenario_diveres_low,
            boot_max = 100, 
            figure = 2,
            font.size = 10)


model_b100_fig1 <- Sodwana_models_1_b100_run1[[1]]$plot + labs(title = scenario1)

model_b100_fig1

ggsave(paste0(plot_path, "Sodwana_models_1_b100_run1.png"), 
       model_b100_fig1,
       units = "cm", height = 24, width = 36, dpi = 300)




# Test all 4 models with 100 bootstraps

Sodwana_models_1234_b100_run1 <- bbn.predict(bbn.model = Sodwana_model, 
            priors1 = sodwana_scenario_diveres_low, 
            priors2 = sodwana_scenario_diveres_high,
            priors3 = sodwana_scenario_levy_low,
            priors4 = sodwana_scenario_levy_high,
            boot_max = 100, 
            figure = 2,
            font.size = 10)


model_b100_fig1 <- Sodwana_models_1234_b100_run1[[1]]$plot + labs(title = scenario1)
model_b100_fig2 <- Sodwana_models_1234_b100_run1[[2]]$plot + labs(title = scenario2)
model_b100_fig3 <- Sodwana_models_1234_b100_run1[[3]]$plot + labs(title = scenario3)
model_b100_fig4 <- Sodwana_models_1234_b100_run1[[4]]$plot + labs(title = scenario4)

(model_b100_fig1 +  model_b100_fig2) / (model_b100_fig3 + model_b100_fig4) 

ggsave(paste0(plot_path, "Sodwana_models_1234_b100_run1.png"), 
(model_b100_fig1 +  model_b100_fig2) / (model_b100_fig3 + model_b100_fig4),
       units = "cm", height = 30, width = 42, dpi = 300)


# Run all 4 scenarios at 10,000 bootstraps
 
Sodwana_models_1234_b10000_run1 <- bbn.predict(bbn.model = Sodwana_model, 
            priors1 = sodwana_scenario_diveres_low, 
            priors2 = sodwana_scenario_diveres_high,
            priors3 = sodwana_scenario_levy_low,
            priors4 = sodwana_scenario_levy_high,
            boot_max = 10000, 
            figure = 2,
            font.size = 10)


# Save results
saveRDS(Sodwana_models_1234_b10000_run1, "Sodwana_models_1234_b10000_run1.RDS")


# Label and print each output graph
model_b10000_fig1 <- Sodwana_models_1234_b10000_run1[[1]]$plot + labs(title = scenario1)
model_b10000_fig2 <- Sodwana_models_1234_b10000_run1[[2]]$plot + labs(title = scenario2)
model_b10000_fig3 <- Sodwana_models_1234_b10000_run1[[3]]$plot + labs(title = scenario3)
model_b10000_fig4 <- Sodwana_models_1234_b10000_run1[[4]]$plot + labs(title = scenario4)

(model_b10000_fig1 +  model_b10000_fig3) / (model_b10000_fig2 + model_b10000_fig4) 

ggsave(paste0(plot_path, "Sodwana_models_1234_b10000_run1.png"), 
(model_b10000_fig1 +  model_b10000_fig3) / (model_b10000_fig2 + model_b10000_fig4),
       units = "cm", height = 30, width = 42, dpi = 300)


# Create and save combined barplot
Sodwana_models_1234_b10000_run1[[1]]$summary

Scenario1_summary <- Sodwana_models_1234_b10000_run1[[1]]$summary %>% 
    mutate(scenario = "scenario1") %>% 
    relocate()

summary_tbl <- map_dfr(
  seq_along(Sodwana_models_1234_b10000_run1),
  ~ Sodwana_models_1234_b10000_run1[[.x]]$summary %>%
    as_tibble() %>%
    mutate(scenario = paste0("scenario_", .x), .before = 1) %>% 
      relocate(name, .after = scenario)
)


Sodwana_scenario_comp_all <- ggplot(summary_tbl, aes(x = name, y = Increase, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = LowerCI, ymax = UpperCI),
    position = position_dodge(width = 0.8),
    width = 0.3
  ) +
  labs(x = "Variable", y = "Increase", title = "BBN Scenario Comparisons") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
       scale_fill_discrete(labels=c(scenario1, scenario2, scenario3, scenario4))

Sodwana_scenario_comp_all

ggsave(paste0(plot_path, "Sodwana_scenario_comp_all.png"), 
        Sodwana_scenario_comp_all,
       units = "cm", height = 18, width = 24, dpi = 300)

#####################
# Visualize timeseries for selected scenarios

bbn.timeseries(bbn.model = Sodwana_model, 
               priors1 = sodwana_scenario_levy_low, 
               timesteps = 5, 
               disturbance = 1)


bbn.visualise(bbn.model = Sodwana_model, 
               priors1 = sodwana_scenario_levy_high, 
               timesteps = 5, 
               disturbance = 1,
              threshold = 0.05,
              arrow.size = 2)

################

