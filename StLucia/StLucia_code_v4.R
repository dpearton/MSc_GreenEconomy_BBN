# BBNet Model for Lake St Lucia
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
setwd("~/BBN/StLucia")
dir.create('./plots')
plot_path = './plots/'
model_path = './models/'
scenario_path = './scenarios/'
#############################
# Run StLucia model version 4 
# Load data

StLucia_model <- read.csv(paste0(model_path,"StLucia_model_v5.csv"), header = TRUE)

head(StLucia_model)

########################
# Load Scenarios


StLucia_scenario_managedmouth <- read.csv(paste0(scenario_path,"StLucia_scenario_managedmouth_v1.csv"), header = TRUE)
StLucia_scenario_natmouth <- read.csv(paste0(scenario_path,"StLucia_scenario_natmouth_v1.csv"), header = TRUE)
StLucia_scenario_rewild <- read.csv(paste0(scenario_path,"StLucia_scenario_rewild_v1.csv"), header = TRUE)
StLucia_scenario_catchment <- read.csv(paste0(scenario_path,"StLucia_scenario_catchment_v1.csv"), header = TRUE)
StLucia_scenario_comb_natmouth_rewild <- read.csv(paste0(scenario_path,"StLucia_scenario_comb_natmouth_rewild_v1.csv"), header = TRUE)
StLucia_scenario_comb_allnat <- read.csv(paste0(scenario_path,"StLucia_scenario_comb_allnat_v1.csv"), header = TRUE)

#Scenario names
scenario1 = "Artificial mouth management"
scenario2 = "Minimal mouth management"
scenario3 = "Rewilding & Invasive species managment"
scenario4 = "Catchment rehabilitation"
scenario5 = "Combination: Minimal mouth management + Rewilding"
scenario6 = "Combination: Minimal mouth management + Rewilding + Catchment rehabilitation"

##############################
# Visualise network to check connections

StLucia_networkdiagram <- read.csv(paste0(model_path,"StLucia_model_networkdiagram_v4.csv"), header = T)

bbn.network.diagram(StLucia_networkdiagram, arrange = layout.fruchterman.reingold, font.size = 1)

bbn.network.diagram(StLucia_networkdiagram, arrange = layout.sphere, font.size = 1.5)
bbn.network.diagram(StLucia_networkdiagram, arrange = layout.random, font.size = 1.5)
bbn.network.diagram(StLucia_networkdiagram, arrange = layout.circle, font.size = 1)


png(paste0(plot_path, "StLucia_networkdiagram_circular.png"), units = "cm", height = 48, width = 48, res = 300)
bbn.network.diagram(StLucia_networkdiagram, arrange = layout.circle, font.size = 1, arrow.size = 4)
dev.off()


######################
# check sensitivity
StLucia_nodes <- StLucia_model$X
StLucia_nodes[[1]]
# short test that sensitivity analysis works
StLucia_sensitivity_StLucia_1 <- bbn.sensitivity(StLucia_model, boot_max = 100, StLucia_nodes[[1]])

# Testing sensitivity of all the nodes. 
results_df <- map_dfr(StLucia_nodes, ~{
  out <- bbn.sensitivity(StLucia_model, .x, boot_max = 1000)
  data.frame(node = .x, out)
})

write_csv(results_df, "StLucia_model_sensitivity_results_df.csv")

StLucia_model_sensitivity <- results_df %>% ggplot(aes(x=sens.output, y = Freq, fill = node)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~ node) +
    theme(legend.position = "none", axis.text.x = element_blank())

StLucia_model_sensitivity


ggsave(paste0(plot_path, "StLucia_model_sensitivity_noxaxis.png"),
       StLucia_model_sensitivity,
       units = "cm",
       height = 48,
       width = 76,
       dpi = 400
       )


#######################
# Run Scenarios

# Test scenario1 with 100 bootstraps

StLucia_models_1_b100_run1 <- bbn.predict(bbn.model = StLucia_model, 
            priors1 = StLucia_scenario_diveres_low,
            boot_max = 100, 
            figure = 2,
            font.size = 10)


model_b100_fig1 <- StLucia_models_1_b100_run1[[1]]$plot + labs(title = scenario1)

model_b100_fig1

ggsave(paste0(plot_path, "StLucia_models_1_b100_run1.png"), 
       model_b100_fig1,
       units = "cm", height = 24, width = 36, dpi = 300)

# Run all six scenarios at 10,000 bootstraps
 
StLucia_models_123456_b10000_run1 <- bbn.predict(bbn.model = StLucia_model, 
            priors1 = StLucia_scenario_managedmouth, 
            priors2 = StLucia_scenario_natmouth,
            priors3 = StLucia_scenario_rewild,
            priors4 = StLucia_scenario_catchment,
            priors4 = StLucia_scenario_comb_natmouth_rewild,
            priors4 = StLucia_scenario_comb_allnat,
            boot_max = 10000, 
            figure = 2,
            font.size = 10)


# Save results for figures and comparisons
saveRDS(StLucia_models_123456_b10000_run1, "StLucia_models_123456_b10000_run1.RDS")

# Create figures with labels and save using patchwork layout
model_b10000_fig1 <- StLucia_models_123456_b10000_run1[[1]]$plot + labs(title = scenario1)
model_b10000_fig2 <- StLucia_models_123456_b10000_run1[[2]]$plot + labs(title = scenario2)
model_b10000_fig3 <- StLucia_models_123456_b10000_run1[[3]]$plot + labs(title = scenario3)
model_b10000_fig4 <- StLucia_models_123456_b10000_run1[[4]]$plot + labs(title = scenario4)
model_b10000_fig5 <- StLucia_models_123456_b10000_run1[[5]]$plot + labs(title = scenario5)
model_b10000_fig6 <- StLucia_models_123456_b10000_run1[[6]]$plot + labs(title = scenario6)


(model_b10000_fig1 +  model_b10000_fig2) / (model_b10000_fig3 + model_b10000_fig4) / (model_b10000_fig5 + model_b10000_fig6) 

ggsave(paste0(plot_path, "StLucia_models_123456_b10000_run1_reordered.png"), 
(model_b10000_fig1 +  model_b10000_fig2) / (model_b10000_fig3 + model_b10000_fig4) / (model_b10000_fig5 + model_b10000_fig6),
       units = "cm", height = 30, width = 42, dpi = 300)


# Create and save consolidated barplot
StLucia_models_123456_b10000_run1[[1]]$summary

Scenario1_summary <- StLucia_models_123456_b10000_run1[[1]]$summary %>% 
    mutate(scenario = "scenario1") %>% 
    relocate()

summary_tbl <- map_dfr(
  seq_along(StLucia_models_123456_b10000_run1),
  ~ StLucia_models_123456_b10000_run1[[.x]]$summary %>%
    as_tibble() %>%
    mutate(scenario = paste0("scenario_", .x), .before = 1) %>% 
      relocate(name, .after = scenario)
)


StLucia_scenario_comp_all <- ggplot(summary_tbl, aes(x = name, y = Increase, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = LowerCI, ymax = UpperCI),
    position = position_dodge(width = 0.8),
    width = 0.3
  ) +
  labs(x = "Variable", y = "Increase", title = "BBN Scenario Comparisons") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
StLucia_scenario_comp_all

ggsave(paste0(plot_path, "StLucia_scenario_comp_all.png"), 
        StLucia_scenario_comp_all,
       units = "cm", height = 18, width = 24, dpi = 300)

#####################
# Visualize timeseries for selected scenarios


# StLucia_scenario_natmouth

bbn.timeseries(bbn.model = StLucia_model, 
               priors1 = StLucia_scenario_natmouth, 
               timesteps = 5, 
               disturbance = 1)

# StLucia_scenario_comb_natmouth_rewild

bbn.visualise(bbn.model = StLucia_model, 
               priors1 = StLucia_scenario_comb_natmouth_rewild, 
               timesteps = 5, 
               disturbance = 1,
              threshold = 0.05,
              arrow.size = 2)

################

