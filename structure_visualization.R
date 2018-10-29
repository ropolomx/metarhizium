
# Load packages -----------------------------------------------------------

library(tidyverse)

# Mrob Habitat ------------------------------------------------------------

mrob_metadata <- read_csv('data_for_structure/mrob_meta.csv')

mrob_metadata <-
  mrob_metadata %>%
  mutate(Habitat = str_replace(Habitat, "Ag", "Agricultural")) %>%
  mutate(Habitat = str_replace(Habitat, "Nat", "Natural")) %>%
  mutate(Glaciation = str_replace(Glaciation, "I", "Inside")) %>%
  mutate(Glaciation = str_replace(Glaciation, "O", "Outside"))

mrob_habitat <- read_csv('data_for_structure/harvest_mrob_habitat/structure_plot_mrob_habitat.csv')

# mrob_habitat <-
#   mrob_habitat %>%
#   rename(SampleID = Ind)

mrob_habitat <- gather(mrob_habitat, key=Cluster, value = Q, 3:5)

mrob_habitat$SampleID <- as.factor(mrob_habitat$SampleID)
mrob_metadata$SampleID <- as.factor(mrob_metadata$SampleID)
mrob_habitat$`LRC ID #` <- as.factor(mrob_habitat$`LRC ID #`)

mrob_habitat <- inner_join(mrob_habitat, mrob_metadata, by="SampleID")

# mrob_habitat$SampleID <- 

mrob_habitat_viz <- 
  ggplot(data = mrob_habitat, aes(
    # x=fct_reorder(.,.f=SampleID,.x = Q,.desc=F),
    x=`LRC ID #`,
    y=Q, 
    fill=Cluster)
    ) +
  geom_bar(stat="Identity", width = 1) +
  scale_fill_viridis_d(option="magma", direction = -1, begin = 0.2, end = 0.9) +
  facet_grid(. ~ Habitat, space = "free", scales = "free") +
  theme_classic() +
  theme(
    panel.spacing.x = unit(0.15, "lines"),
    strip.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size=12,angle=90),
    axis.text.y = element_text(size=12)
  ) +
  xlab("\nSample ID #") + 
  ylab("Ancestry Coefficient (Q)\n")

mrob_habitat_viz

ggsave(
  filename = 'mrob_habitat_viz.png',
  plot = mrob_habitat_viz,
  height = 6.5,
  width = 10.5,
  units = "in",
  dpi = 600
)


# Mrob glaciation ---------------------------------------------------------

mrob_glaciation <- read_csv('data_for_structure/harvest_mrob_glaciation/structure_plot_mrob_glaciation.csv')

mrob_glaciation <-
  mrob_glaciation %>%
  rename(SampleID = Ind)

mrob_glaciation <- gather(mrob_glaciation, key=Cluster, value = Q, 3:5)


mrob_glaciation <- inner_join(mrob_glaciation, mrob_metadata, by="SampleID")

mrob_glaciation$SampleID <- as.factor(mrob_glaciation$SampleID)
mrob_glaciation$`LRC ID #` <- as.factor(mrob_glaciation$`LRC ID #`)

mrob_glaciation_viz <- 
  mrob_glaciation %>%
  ggplot(aes(
    # x=fct_reorder(.f=SampleID,.x = Q,.desc=T),
    x=`LRC ID #`,
    y=Q, fill=Cluster)
    ) +
  geom_bar(stat="Identity", width = 1) +
  scale_fill_viridis_d(option="magma", direction = -1, begin = 0.2, end = 0.9) +
  facet_grid(. ~ Glaciation, space = "free", scales = "free") +
  theme_classic() +
  theme(
    panel.spacing.x = unit(0.15, "lines"),
    strip.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size=12,angle=90, hjust=0),
    axis.text.y = element_text(size=12)
  ) +
  xlab("\nSample ID #") + 
  ylab("Ancestry Coefficient (Q)\n")

mrob_glaciation_viz

ggsave(
  filename = 'mrob_glaciation_viz.png',
  plot = mrob_glaciation_viz,
  height = 6.5,
  width = 10.5,
  units = "in",
  dpi = 600
)


# Mbru habitat ------------------------------------------------------------

mbru_metadata <- read_csv('data_for_structure/mbru_meta.csv')

mbru_metadata <-
  mbru_metadata %>%
  mutate(Habitat = str_replace(Habitat, "Ag", "Agricultural")) %>%
  mutate(Habitat = str_replace(Habitat, "Nat", "Natural")) %>%
  mutate(Glaciation = str_replace(Glaciation, "I", "Inside")) %>%
  mutate(Glaciation = str_replace(Glaciation, "O", "Outside"))

mbru_habitat <- read_csv('data_for_structure/structure_plot_mbru_habitat.csv')

mbru_habitat <-
  mbru_habitat %>%
  rename(SampleID = Ind)

mbru_habitat <- gather(mbru_habitat, key=Cluster, value = Q, 3:4)

mbru_habitat$SampleID <- as.factor(mbru_habitat$SampleID)
mbru_metadata$SampleID <- as.factor(mbru_metadata$SampleID)

mbru_habitat <- inner_join(mbru_habitat, mbru_metadata, by="SampleID")

# mbru_habitat$SampleID <- 

mbru_habitat_viz <- 
  mbru_habitat %>%
  ggplot(aes(x=SampleID, y=Q, fill=Cluster)) +
  geom_bar(stat="Identity", width=1) +
  scale_fill_viridis_d(option="inferno", direction = -1, end=0.9) +
  facet_grid(. ~ Ecozone, space = "free", scales = "free") +
  theme_classic() +
  theme(
    panel.spacing.x = unit(0.25, "lines"),
    strip.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size=7,angle=90),
    axis.text.y = element_text(size=12),
    panel.grid = element_blank()
  ) +
  xlab("\nSample ID #") + 
  ylab("Ancestry Coefficient (Q)\n")

mbru_habitat_viz

ggsave(
  filename = 'mbru_habitat_viz.png',
  plot = mbru_habitat_viz,
  height = 6.5,
  width = 10.5,
  units = "in",
  dpi = 600
)
