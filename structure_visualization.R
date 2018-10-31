
# Load packages -----------------------------------------------------------

library(tidyverse)
library(gridExtra)
library(pagas)

# Mrob Habitat ------------------------------------------------------------

mrob_metadata <- read_csv('data_for_structure/mrob_meta.csv')

mrob_metadata <-
  mrob_metadata %>%
  mutate(Habitat = str_replace(Habitat, "Ag", "Agricultural")) %>%
  mutate(Habitat = str_replace(Habitat, "Nat", "Natural")) %>%
  mutate(Glaciation = str_replace(Glaciation, "I", "Glaciated")) %>%
  mutate(Glaciation = str_replace(Glaciation, "O", "Not glaciated"))

mrob_habitat <- read_csv('data_for_structure/harvest_mrob_habitat/structure_plot_mrob_habitat.csv')

mrob_habitat <- gather(mrob_habitat, key=Cluster, value = Q, 3:5)

mrob_habitat$SampleID <- as.factor(mrob_habitat$SampleID)
mrob_metadata$SampleID <- as.factor(mrob_metadata$SampleID)

mrob_habitat <- inner_join(mrob_habitat, mrob_metadata, by="SampleID")

mrob_habitat$`LRC ID #` <- as.factor(mrob_habitat$`LRC ID #`)

mrob_habitat_viz <- 
  ggplot(data = mrob_habitat, aes(
    # x=fct_reorder(.,.f=SampleID,.x = Q,.desc=F),
    x=`LRC ID #`,
    y=Q, 
    fill=Cluster)
    ) +
  geom_bar(stat="Identity", width = 1) +
  # scale_fill_viridis_d(option="cividis", direction = -1,
  #                      begin = 0.1,
  #                      end = 0.9
  #                      ) +
  scale_fill_grey() +
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
  ylab("Ancestry Coefficient (Q)\n") +
  ggtitle("B")

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

mrob_glaciation$SampleID <- as.factor(mrob_glaciation$SampleID)
mrob_glaciation <- inner_join(mrob_glaciation, mrob_metadata, by="SampleID")

mrob_glaciation$`LRC ID #` <- as.factor(mrob_glaciation$`LRC ID #`)

mrob_glaciation_viz <- 
  mrob_glaciation %>%
  ggplot(aes(
    # x=fct_reorder(.f=SampleID,.x = Q,.desc=T),
    x=`LRC ID #`,
    y=Q, fill=Cluster)
    ) +
  geom_bar(stat="Identity", width = 1) +
  scale_fill_grey() +
  # scale_fill_viridis_d(option="cividis", 
                       # direction = -1, 
                       # begin = 0.1, 
                       # end = 0.9
                       # ) +
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
  ylab("Ancestry Coefficient (Q)\n") +
  ggtitle("B")

mrob_glaciation_viz

ggsave(
  filename = 'mrob_glaciation_viz.png',
  plot = mrob_glaciation_viz,
  height = 6.5,
  width = 10.5,
  units = "in",
  dpi = 600
)

ggsave(
  filename = 'mrob_glaciation_viz.tiff',
  plot = mrob_glaciation_viz,
  height = 6.5,
  width = 10.5,
  units = "in",
  dpi = 300
)

# Mbru habitat ------------------------------------------------------------

mbru_metadata <- read_csv('data_for_structure/mbru_meta.csv')

mbru_metadata <-
  mbru_metadata %>%
  mutate(Habitat = str_replace(Habitat, "Ag", "Agricultural")) %>%
  mutate(Habitat = str_replace(Habitat, "Nat", "Natural")) %>%
  mutate(Glaciation = str_replace(Glaciation, "I", "Glaciated")) %>%
  mutate(Glaciation = str_replace(Glaciation, "O", "Not glaciated"))

mbru_habitat <- read_csv('data_for_structure/harvest_mbru_habitat/structure_plot_mbru_habitat.csv')

mbru_habitat <-
  mbru_habitat %>%
  rename(SampleID = Ind)

mbru_habitat <- gather(mbru_habitat, key=Cluster, value = Q, 3:4)

mbru_habitat <- inner_join(mbru_habitat, mbru_metadata, by="SampleID")

mbru_habitat$SampleID <- as.factor(mbru_habitat$SampleID)
mbru_habitat$`LRC ID #` <- as.factor(mbru_habitat$`LRC ID #`)

mbru_habitat_viz <- 
  mbru_habitat %>%
  ggplot(aes(x=SampleID, y=Q, fill=Cluster)) +
  geom_bar(stat="Identity", width=1) +
  scale_fill_grey() +
#  scale_fill_viridis_d(option="cividis", 
#                       direction = -1, 
#                       begin = 0.1, 
#                       end=0.9
#                       ) +
  facet_grid(. ~ Habitat, space = "free", scales = "free") +
  theme_classic() +
  theme(
    panel.spacing.x = unit(0.25, "lines"),
    strip.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size=6.5,angle=90),
    axis.text.y = element_text(size=12),
    panel.grid = element_blank()
  ) +
  xlab("\nSample ID #") + 
  ylab("Ancestry Coefficient (Q)\n") +
  ggtitle("A")

mbru_habitat_viz

ggsave(
  filename = 'mbru_habitat_viz.png',
  plot = mbru_habitat_viz,
  height = 6.5,
  width = 10.5,
  units = "in",
  dpi = 600
)

# Mbru glaciation ---------------------------------------------------------

mbru_glaciation <- read_csv('data_for_structure/harvest_mbru_glaciation/structure_plot_mbru_glaciation.csv')

mbru_glaciation <-
  mbru_glaciation %>%
  rename(SampleID = Ind)

mbru_glaciation <- gather(mbru_glaciation, key=Cluster, value = Q, 3:4)

mbru_glaciation <- inner_join(mbru_glaciation, mbru_metadata, by="SampleID")

mbru_glaciation$SampleID <- as.factor(mbru_glaciation$SampleID)
mbru_glaciation$`LRC ID #` <- as.factor(mbru_glaciation$`LRC ID #`)

mbru_glaciation_viz <- 
  mbru_glaciation %>%
  ggplot(aes(x=SampleID, y=Q, fill=Cluster)) +
  geom_bar(stat="Identity", width=1) +
  # scale_fill_viridis_d(option="cividis", 
  #                      direction = -1, 
  #                      begin = 0.1, 
  #                      end=0.9
  #                      ) +
  scale_fill_grey()+
  facet_grid(. ~ Glaciation, space = "free", scales = "free") +
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
  ylab("Ancestry Coefficient (Q)\n") +
  ggtitle("A")

mbru_glaciation_viz

ggsave(
  filename = 'mbru_glaciation_viz.png',
  plot = mbru_glaciation_viz,
  height = 6.5,
  width = 10.5,
  units = "in",
  dpi = 600
)

# Combined figures --------------------------------------------------------

# Habitat

habitat_grob <- grid.arrange(mbru_habitat_viz, mrob_habitat_viz, ncol =1)

ggsave(
  filename = 'habitat_structure.png',
  plot = habitat_grob,
  height = 6.5,
  width = 11.5,
  units = "in",
  dpi = 600
)

ggsave(
  filename = 'habitat_structure.tiff',
  plot = habitat_grob,
  height = 7,
  width = 11,
  units = "in",
  dpi = 200
)

glaciation_grob <- grid.arrange(mbru_glaciation_viz, mrob_glaciation_viz, ncol =1)

ggsave(
  filename = 'glaciation_structure.png',
  plot = glaciation_grob,
  height = 6.5,
  width = 11.5,
  units = "in",
  dpi = 600
)

ggsave(
  filename = 'glaciation_structure.tiff',
  plot = glaciation_grob,
  height = 7,
  width = 10.5,
  units = "in",
  dpi = 200
)
