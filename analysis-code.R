library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(patchwork)
library(readxl)

# Commenting the following chunk, which anonymizes the survey data
# # There are some students we need to exclude from analyses,
# # because they submitted Quiz 1 after they might have used the apps.
# exclude <- read_csv("teaching-survey/data/names-to-exclude.csv")
# survey <- read_csv("teaching-survey/data/missouri-round2-all.csv") %>%
#   select(-date)
# survey <-
#   survey %>%
#   filter(!(name %in% exclude$name))
# only_one_quiz <-
#   survey %>%
#   group_by(name) %>%
#   summarize(how_many = n()) %>%
#   filter(how_many == 31)
#
#
# survey <-
#   survey %>%
#   filter(!(name %in% only_one_quiz$name)) %>%
#   filter(!(is.na(response))) %>%
#   # after thinking more about the controls, the following three questions
#   # don't seem like true controls bc they were part of the curriculum throughout
#   # the semester and are generally reflected in the apps themselves too.
#   # this may muddy the results a little bit, so I am excluding them here.
#   filter(!(topic == "structured_popgrowth" & question == "Population growth rate")) %>%
#   filter(!(topic == "structured_popgrowth" & question == "Exponential vs. Logistic growth")) %>%
#   filter(!(topic == "structured_popgrowth" & question ==
#              "Fecundity effects on population dynamics"))
#
# name_id <- bind_cols(
#   real_name = unique(survey$name),
#   random_name = stringi::stri_rand_strings(length = 5,
#                                            n = length(unique(survey$name)))
# )
#
# to_export <-
#   left_join(survey, name_id, by = c("name" = "real_name")) %>%
#   select(-name) %>%
#   select(random_name, everything())
#
# write_csv(to_export, file = "teaching-survey/data/missouri_scrubbed_data.csv")
################

# Begin analysis of anonymized data
missouri_data <-
  read_xls("Survey_data.xls", sheet = 1)

# for Lotka-Volterra analysis ---------------------------------------------

for_lv_analysis <-
  missouri_data %>%
  filter(quiz_no %in% c("first", "second"),
         topic %in% c("lotka_volterra", "structured_popgrowth")) %>%
  pivot_wider(names_from = quiz_no, values_from = response) %>%
  mutate(difference = second - first) %>%
  filter(!(is.na(difference))) %>%
  mutate(normalized_gain = ifelse(second > first,
                                  difference/(7-first),
                                  ifelse(second == first,
                                         0,
                                         (second-first)/first)))

# Now that we have normalized change scores, check whether normalized
# scores are less correlated with response to pre-quiz than the raw differences
# (as we expect them to be).

cor_pre_diff_lv <- ggplot(for_lv_analysis) +
  geom_point(aes(x = first, y = difference))
cor_pre_ndiff_lv <- ggplot(for_lv_analysis) +
  geom_point(aes(x = first, y = normalized_gain))
cor_pre_diff_lv + cor_pre_ndiff_lv


# That looks better– less correlation between normalized change and pre-survey score.
# Now, we can start evaluating how these normalized differences vary across different
# questions, or categories of questions.

lv_popg_cat_comparison <-
  for_lv_analysis %>%
  group_by(topic) %>%
  summarize(mean_ndiff = mean(normalized_gain),
            sd_ndiff = sd(normalized_gain),
            sem_ndiff = sd_ndiff/sqrt(n())) %>%
  arrange(topic)
lv_popg_cat_comparison$topic <-
  factor(lv_popg_cat_comparison$topic,
         levels = c("structured_popgrowth", "lotka_volterra"))

(lv_popg_cat_comparison_plot <-
    ggplot(lv_popg_cat_comparison) +
    geom_pointrange(aes(x = topic, y = mean_ndiff,
                        ymin = mean_ndiff-sem_ndiff,
                        ymax = mean_ndiff+sem_ndiff,
                        fill = topic, shape = topic),
                    size = 0.8, stroke = 1) +
    scale_fill_manual(values = c("grey", "orange")) +
    scale_shape_manual(values = c(22,21)) +
    scale_y_continuous(limits = c(0, 0.45), expand = c(0,0)) +
    scale_x_discrete(labels = c("Control (Structured\npopulation growth)",
                                "Lotka-Volterra Model")) +
    geom_hline(yintercept = 0, linetype = 1) +
    ecoevoapps::theme_apps() +
    ylab("normalized change\nin confidence") + xlab("") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
          axis.title = element_text(size = 12),
          axis.line.x = element_blank(),
          panel.grid.major.y = element_line(size = .05, color = "grey25")))

# The graph above shows differences across the whole category;
# let’s now split it up by each question within the category.

# Summarize across the questions within each category
lv_popg_q_comparison <-
  for_lv_analysis %>%
  mutate(topicq = paste0(topic,"_", question)) %>%
  group_by(topic, topicq) %>%
  summarize(mean_ndiff = mean(normalized_gain),
            sd_ndiff = sd(normalized_gain),
            sem_ndiff = sd_ndiff/sqrt(n())) %>%
  arrange(desc(topic), mean_ndiff) %>% ungroup %>%
  mutate(x_val = c(1:nrow(.)))


x_labels <- sub( ".*_(.*)", "\\1", lv_popg_q_comparison$topicq)
x_labels[2] <- "Sampling populations\nin the field"
x_labels[7] <- "Inter- vs. Intra-specific\ninteractions"
x_labels[9] <- "Exponential vs.\nLogistic growth"
x_labels[12] <- "Lotka-Volterra\ncompetition model"
x_labels[13] <- "Positive or negative\nspecies interactions"
(lv_popg_q_comparison_plot <-
    ggplot(lv_popg_q_comparison) +
    geom_pointrange(aes(x = x_val, y = mean_ndiff,
                        ymin = mean_ndiff - sem_ndiff,
                        max = mean_ndiff + sem_ndiff,
                        fill = topic, shape = topic), size = 1, stroke = 1) +
    scale_fill_manual(values = c("orange", "grey")) +
    scale_shape_manual(values = c(21,22)) +
    ylab("normalized change\nin confidence") + xlab("") +
    scale_y_continuous(limits = c(0, 0.45), expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0),
                       limits = c(0.5,nrow(lv_popg_q_comparison)+0.5),
                       breaks = 1:nrow(lv_popg_q_comparison), labels = x_labels)+
    geom_hline(yintercept = 0, linetype = 1) +
    geom_segment(x = 4.5, xend = 4.5, y = 0, yend = Inf, linetype = 3) +
    annotate(geom = "text", x = 1, y = 0.4, hjust = 0, size = 5,
             label = "Structured pop.\ngrowth (Control)") +
    annotate(geom = "text", x = 5, y = 0.4, hjust = 0, size = 5,
             label = "Lotka-Volterra model") +
    ecoevoapps::theme_apps() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
          axis.title = element_text(size = 12),
          axis.line.x = element_blank(),
          panel.grid.major.y = element_line(size = .05, color = "grey25")))

# Now, let’s evaluate whether the change in student confidence is related at all
# to how interested students were when they took the initial survey,
# i.e. before doing the activity:

interest_commEcol <- missouri_data %>%
  filter(topic == "interest", quiz_no == "first", question == "Community ecology")
# Take a look at the distribution of interest levels
ggplot(interest_commEcol) +
  geom_jitter(aes(x = question, y = response, color = quiz_no), height = 0, width = .1)

# From this, we can reasonably categorize an interest of 1,2,3 as "low",
# interest of 4 or 5 as "medium", and interest of 6 or 7 as "high":

interest_commEcol <- interest_commEcol %>%
  mutate(response2 = response,
         response2 = ifelse(response2 < 4, "AA_low",
                            ifelse(response2 < 6, "AB_medium", "AC_high"))) %>%
  # rename some columns for easier merging in the future, and remove some
  # columns that are no longer necessary
  select(-quiz_no, -topic, question2 = question)

# Figure out how many students indicated different levels of interest
num_interest_level_lv <-
  interest_commEcol %>%
  group_by(response2) %>%
  summarize(num = n())

# Join the interest in with the bigger LV question confidence dataset
for_lv_interest <-
  left_join(for_lv_analysis, interest_commEcol) %>%
  filter(!(is.na(response2)))


for_lv_interest_sum <- for_lv_interest %>%
  group_by(topic, response2) %>%
  summarize(mean_diff = mean(normalized_gain),
            sem_diff = sd(normalized_gain)/sqrt(n()),
            total = n())

(lv_int <-
    ggplot(for_lv_interest_sum) +
    geom_pointrange(aes(x = response2, y = mean_diff,
                        ymin = mean_diff - sem_diff, ymax = mean_diff + sem_diff,
                        fill = topic, shape = topic),
                    position = position_dodge(width = .1), size = 1) +
    scale_fill_manual(values = c("orange", "grey")) +
    scale_shape_manual(values = c(21,22)) +
    scale_x_discrete(labels = c("low\n(1, 2, or 3)", "med\n(4 or 5)", "high\n(6 or 7)")) +
    ylab("normalized change\nin confidence") +
    xlab("Pre-activity interest in Community ecology") +
    scale_y_continuous(expand = c(0,0), limits = c(0,.45)) +
    annotate("text", x = 1, y = Inf,
             label = paste0("n = ", num_interest_level_lv$num[1], " students"), vjust = 1) +
    annotate("text", x = 2, y = Inf,
             label = paste0(num_interest_level_lv$num[2]), vjust = 1) +
    annotate("text", x = 3, y = Inf,
             label = paste0(num_interest_level_lv$num[3]), vjust = 1) +
    ecoevoapps::theme_apps() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
          axis.title = element_text(size = 12),
          axis.title.x = element_text(margin=margin(-50,0,0,0)),
          panel.grid.major.y = element_line(size = .05, color = "grey25"))
)

#################
#################
#################

# For SIR analysis --------------------------------------------------------


for_sir_analysis <-
  missouri_data %>%
  filter(quiz_no %in% c("third", "second"),
         topic %in% c("structured_popgrowth", "SIR")) %>%
  pivot_wider(names_from = quiz_no, values_from = response) %>%
  mutate(difference = third - second) %>%
  filter(!(is.na(difference))) %>%
  mutate(normalized_gain = ifelse(third > second,
                                  difference/(7-second),
                                  ifelse(third == second,
                                         0,
                                         (third-second)/second)))
cor_pre_diff_si <- ggplot(for_sir_analysis) +
  geom_point(aes(x = second, y = difference))
cor_pre_ndiff_si <-
  ggplot(for_sir_analysis) +
  geom_point(aes(x = second, y =
                   normalized_gain))
cor_pre_diff_si + cor_pre_ndiff_si

sir_popg_cat_comparison <-
  for_sir_analysis %>%
  group_by(topic) %>%
  summarize(mean_ndiff = mean(normalized_gain),
            sd_ndiff = sd(normalized_gain),
            sem_ndiff = sd_ndiff/sqrt(n())) %>%
  arrange(topic)
sir_popg_cat_comparison$topic <- factor(sir_popg_cat_comparison$topic,
                                        levels = c("structured_popgrowth", "SIR"))
# Make a plot to compare normalized gains across categories
(sir_popg_cat_comparison_plot <-
    ggplot(sir_popg_cat_comparison) +
    geom_pointrange(aes(x = topic, y = mean_ndiff,
                        ymin = mean_ndiff-sem_ndiff,
                        ymax = mean_ndiff+sem_ndiff,
                        fill = topic, shape = topic), size = 1, stroke = 1) +
    scale_fill_manual(values = c("grey", "#009E73")) +
    scale_shape_manual(values = c(22,21)) +
    scale_x_discrete(labels = c("Control (Structured\npopulation growth)",
                                "SIR Disease \n dynamics model")) +
    ecoevoapps::theme_apps() +
    ylab("normalized change\nin confidence") + xlab("") +
    scale_y_continuous(limits = c(0, 0.45), expand = c(0,0)) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
          axis.title = element_text(size = 12),
          panel.grid.major.y = element_line(size = .05, color = "grey25")))
sir_popg_q_comparison <-
  for_sir_analysis %>%
  mutate(topicq = paste0(topic,"_", question)) %>%
  group_by(topic, topicq) %>%
  summarize(mean_ndiff = mean(normalized_gain),
            sd_ndiff = sd(normalized_gain),
            sem_ndiff = sd_ndiff/sqrt(n())) %>%
  arrange(desc(topic), mean_ndiff) %>% ungroup %>%
  mutate(x_val = 1:nrow(.))


x_labels2 <- sub( ".*_(.*)", "\\1", sir_popg_q_comparison$topicq)
x_labels2[3] <- "Sampling populations\nin the field"
x_labels2[5] <- "SIR Disease\ndynamics model"
x_labels2[9] <- "Vaccination effects\non disease spread"


(sir_popg_q_comparison_plot <- ggplot(sir_popg_q_comparison) +
    geom_pointrange(aes(x = x_val, y = mean_ndiff,
                        ymin = mean_ndiff - sem_ndiff,
                        max = mean_ndiff + sem_ndiff,
                        fill = topic, shape = topic),
                    size = 1, stroke = 1) +
    scale_shape_manual(values = c(21,22)) +
    scale_fill_manual(values = c("#009E73", "grey")) +
    ylab("normalized change\nin confidence") + xlab("") +
    scale_y_continuous(limits = c(0, 0.45), expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0),
                       limits = c(0.5,nrow(sir_popg_q_comparison)+0.5),
                       breaks = 1:nrow(sir_popg_q_comparison),
                       labels = x_labels2)+
    geom_hline(yintercept = 0, linetype = 1) +
    geom_segment(x = 4.5, xend = 4.5, y = 0, yend = Inf, linetype = 3) +
    annotate(geom = "text", x = 1, y = 0.4, hjust = 0,
             size = 5, label = "Structured pop. growth\n(Control)") +
    annotate(geom = "text", x = 5, y = 0.4, hjust = 0,
             size = 5, label = "SIR model") +
    ecoevoapps::theme_apps() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
          axis.title = element_text(size = 12),
          axis.line.x = element_blank(),
          panel.grid.major.y = element_line(size = .05, color = "grey25")))




interest_diseaseEcol <- missouri_data %>%
  filter(topic == "interest", quiz_no == "second", question == "Disease ecology")
# Take a look at the distribution of interest levels
ggplot(interest_diseaseEcol) +
  geom_jitter(aes(x = question, y = response, color = quiz_no), height = 0, width = .1)



# From this, we can reasonably categorize an interest of 1,2,3 as "low",
# interest of 4 or 5 as "medium", and interest of 6 or 7 as "high":
interest_diseaseEcol <-
  interest_diseaseEcol %>%
  mutate(response2 = response,
         response2 = ifelse(response2 < 4, "AA_low",
                            ifelse(response2 < 6, "AB_medium", "AC_high"))) %>%
  # rename some columns for easier merging in the future, and remove some
  # columns that are no longer necessary
  select(-quiz_no, -topic, question2 = question)
# Figure out how many students indicated different levels of interest...
num_interest_level_si <-
  interest_diseaseEcol %>%
  group_by(response2) %>%
  summarize(num = n())
# Join the interest in with the bigger LV question confidence dataset
for_si_interest <-
  left_join(for_sir_analysis, interest_diseaseEcol) %>%
  filter(!(is.na(response2)))



for_si_interest_sum <-
  for_si_interest %>%
  group_by(topic, response2) %>%
  summarize(mean_diff = mean(normalized_gain),
            sem_diff = sd(normalized_gain)/sqrt(n()),
            total = n())


(si_int <-
    ggplot(for_si_interest_sum) +
    geom_pointrange(aes(x = response2, y = mean_diff,
                        ymin = mean_diff - sem_diff, ymax = mean_diff + sem_diff,
                        fill = topic, shape = topic),
                    position = position_dodge(width = .1), size = 1) +
    scale_fill_manual(values = c("#009E73", "grey")) +
    scale_shape_manual(values = c(21,22)) +
    scale_x_discrete(labels = c("low\n(1, 2, or 3)", "med\n(4 or 5)", "high\n(6 or 7)")) +
    ylab("normalized change\nin confidence") +
    xlab("Pre-activity interest in Disease ecology") +
    scale_y_continuous(expand = c(0,0), limits = c(0,.45)) +
    annotate("text", x = 1, y = Inf,
             label = paste0("n = ", num_interest_level_lv$num[1], " students"), vjust = 1) +
    annotate("text", x = 2, y = Inf,
             label = paste0(num_interest_level_si$num[2]), vjust = 1) +
    annotate("text", x = 3, y = Inf,
             label = paste0(num_interest_level_si$num[3]), vjust = 1) +
    ecoevoapps::theme_apps() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
          axis.title = element_text(size = 12),
          axis.title.x = element_text(margin=margin(-50,0,0,0)),
          panel.grid.major.y = element_line(size = .05, color = "grey25"))
)


(missouri_prepost <-  lv_popg_cat_comparison_plot + lv_int + #lv_popg_q_comparison_plot +
  sir_popg_cat_comparison_plot + si_int + #sir_popg_q_comparison_plot +
  plot_layout(widths = c(2,2)) +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = 'none')
)


# -------------------------------- #
#  That concludes the MU analysis  #
# -------------------------------- #



# Analysis of UCLA survey data ----------------

# Read in the data and reshape it to be more useful
ucla_post <- read_xls("Survey_data.xls", sheet = 3)
ucla_post_long <- ucla_post %>%
  pivot_longer(help_lv:conf_island_dyn)
ucla_post_long <- ucla_post_long %>%
  # replace values of < 1 with 1 (some students entered 0 though they were asked not to)
  mutate(value = ifelse(value < 1, 1, value))
# Rename some values for better plotting
ucla_post_long_h <- ucla_post_long %>%
  filter(str_detect(name, "help")) %>%
  mutate(name = ifelse(name == "help_lv", "Lotka Volterra model",name),
         name = ifelse(name == "help_ib", "Island Biogeography",name))
# Change the levels for better plotting
ucla_post_long_h$name <- factor(ucla_post_long_h$name,
                                levels = c("Lotka Volterra model",
                                           "Island Biogeography"))
panels_name <- data.frame(name = as.factor(levels(ucla_post_long_h$name)))

# Now that we have a workable data frame, we can start plotting.


(plot_overall <-
  ggplot(ucla_post_long_h) +
  geom_histogram(aes(x = value, fill = name)) +
  scale_fill_manual(values = c("#009E73", "darkmagenta")) +
  facet_wrap(.~name, scales = "free", nrow = 2) +
  labs(subtitle = "From a scale of 1 (not helpful) to 7 (very helpful)\nhow helpful were the interactive apps?") +
  theme_bw() +
  geom_text(data = panels_name,
            mapping = aes(x = .52, y = Inf, label = name),
            hjust = 0, vjust = 1.5) +
  scale_x_continuous(breaks = 1:7, labels = 1:7, limits = c(0.5,7.5)) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none",
        plot.subtitle = element_text(size = 9.5)))


# Next, the goal is to make a similar histogram of responses to questions about individual concepts. This takes
# some more code wrangling

ucla_post_long_q <- ucla_post_long %>%
  filter(!(str_detect(name, "help")))
ucla_post_long_q$name <- str_replace(ucla_post_long_q$name, pattern = "conf_", replacement = "")
ucla_post_long_q$name <- str_replace(ucla_post_long_q$name, pattern = "con_", replacement = "")
ucla_post_long_q <-
  ucla_post_long_q %>%
  mutate(name = ifelse(name == "growth", "Exponential vs. Logistic\ngrowth (LV)",name),
         name = ifelse(name == "K", "Carrying capacity\n(LV)",name),
         name = ifelse(name == "popdyn", "Population dynamics\nand time series (LV)",name),
         name = ifelse(name == "mol_eco", "Molecular ecology\n(control)",name),
         name = ifelse(name == "popgrowth", "Population growth rates\n(LV)",name),
         name = ifelse(name == "comp_int", "Competitive interactions\n(LV)",name),
         name = ifelse(name == "comp_coeff", "Competition coefficients\n(LV)",name),
         name = ifelse(name == "comp_ex", "Competitive exclusion\n(LV)",name),
         name = ifelse(name == "biogeochem", "Biogeochemical cycles\n(control)",name),
         name = ifelse(name == "coexistence", "Coexistence\n(LV)",name),
         name = ifelse(name == "lvcomp", "Lotka-Volterra\ncompetition model (LV)",name),
         name = ifelse(name == "islandbio", "Island Biogeography\n(Biogeo)",name),
         name = ifelse(name == "im_rate", "Immigration rate\n(Biogeo)",name),
         name = ifelse(name == "ext_rate", "Extinction rate\n(Biogeo)",name),
         name = ifelse(name == "island_dyn", "Mainland/Island\nDynamics (Biogeo)", name)
  )
ucla_post_long_q <-
  ucla_post_long_q %>%
  mutate(category = ifelse(str_detect(name, "LV"), "Lotka Volterra Model",
                           ifelse(str_detect(name, "control"), "Control",
                                  "Island Biogeography")))

ucla_post_long_q$name <- factor(ucla_post_long_q$name,
                                levels = c("Molecular ecology\n(control)",
                                           "Biogeochemical cycles\n(control)",
                                           "Exponential vs. Logistic\ngrowth (LV)",
                                           "Carrying capacity\n(LV)",
                                           "Population dynamics\nand time series (LV)",
                                           "Population growth rates\n(LV)",
                                           "Competitive interactions\n(LV)",
                                           "Competitive exclusion\n(LV)",
                                           "Competition coefficients\n(LV)",
                                           "Coexistence\n(LV)",
                                           "Lotka-Volterra\ncompetition model (LV)",
                                           "Island Biogeography\n(Biogeo)",
                                           "Immigration rate\n(Biogeo)",
                                           "Extinction rate\n(Biogeo)",
                                           "Mainland/Island\nDynamics (Biogeo)"))
panels_name <- data.frame(name = as.character(as.factor(levels(ucla_post_long_q$name))))


plot_specifics <-
  ggplot(ucla_post_long_q) +
  geom_histogram(aes(x = value, fill = category)) +
  facet_wrap(.~name, nrow = 3) +
  theme_bw() +
  # geom_text(data = panels_name,
  # mapping = aes(x = .52, y = Inf, label = name), hjust = 0, vjust = 1.5, size = 3.25) +
  scale_fill_manual(values = c("grey", "darkmagenta", "#009E73")) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        #strip.text.x = element_blank(),
        legend.position = "none",
        plot.subtitle = element_text(size = 9.5)) +
  scale_x_continuous(breaks = 1:7, labels = 1:7, limits = c(0.5,7.5)) +
  labs(subtitle = "From a scale of 1 (not helpful) to 7 (very helpful), please rate how much\nthe interactive activity helped you learn the following concepts")


plot_overall + plot_specifics + plot_layout(widths = c(2,5)) +
  plot_annotation(tag_level = "a")
