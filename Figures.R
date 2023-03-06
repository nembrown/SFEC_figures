library(ggplot2)
library(tidyverse)
library(patchwork)

"%notin%"<- Negate("%in%")

chinook_sfec<-read.csv("Chinook_SFEC.csv") %>% as_tibble()
coho_sfec<-read.csv("Coho_SFEC.csv") %>% as_tibble()

theme_set(theme_bw())


chinook_sfec_sum<- chinook_sfec %>%
                   filter(PSC.Fishery %notin% c("Dead Fish Survey", "Hatchery",
                                                "Spawning Ground", "Mixed Wild Broodstock and Hatchery Returns",
                                                "Wild Broodstock Collection",
                                                "Groundfish Observer (Bering Sea/Aleutians)",
                                                "Groundfish Observer (Gulf of Alaska)")) %>%
                   filter(Fis.Gear %notin%  c("Juvenile", "Juvenile Sampling - Beach Seine",
                                              "Juvenile Sampling - Seine", "Juvenile Sampling - Trawl",
                                              "Hatchery Miscellaneous", "PNP Hatchery Cost Recovery")) %>%
                   group_by(Recovery.Year, Region, Detection.Method) %>%
                   summarise(sum_estimated = sum(SUM.Estimated, na.rm=TRUE)) %>%
                   pivot_wider(names_from=Detection.Method, values_from = sum_estimated) %>%
                   mutate(Electronic = case_when(
                     is.na(Electronic) ~ 0,
                     TRUE ~ as.numeric(Electronic)))%>%
                      mutate(Visual = case_when(
                            is.na(Visual) ~ 0,
                       TRUE ~ as.numeric(Visual))) %>%
                   mutate(percent_electronic = Electronic/(Electronic + Visual)*100) %>%
                   filter(Region %notin% c(0, "CA", ""))

chinook_SFEC_plot <- ggplot(chinook_sfec_sum, aes(x=Recovery.Year, y=percent_electronic, col=Region)) +
                     geom_point() +
                     geom_line()+
                     scale_x_continuous(breaks=seq(2010, 2020, 1))+
                     xlab("Year")+
                     ylab("Estimated number CWTs sampled electronically (%)")+
                     ggtitle("Chinook adult fishery sampling")
chinook_SFEC_plot

coho_sfec_sum<- coho_sfec %>%
                  filter(PSC.Fishery %notin% c("Dead Fish Survey", "Hatchery",
                               "Spawning Ground", "Mixed Wild Broodstock and Hatchery Returns",
                               "Wild Broodstock Collection",
                               "Groundfish Observer (Bering Sea/Aleutians)",
                               "Groundfish Observer (Gulf of Alaska)")) %>%
                filter(Fis.Gear %notin%  c("Juvenile", "Juvenile Sampling - Beach Seine",
                             "Juvenile Sampling - Seine", "Juvenile Sampling - Trawl",
                             "Hatchery Miscellaneous", "PNP Hatchery Cost Recovery")) %>%
                   group_by(Recovery.Year, Region, Detection.Method) %>%
                   summarise(sum_estimated = sum(SUM.Estimated, na.rm=TRUE)) %>%
                   pivot_wider(names_from=Detection.Method, values_from = sum_estimated) %>%
                   mutate(Electronic = case_when(
                               is.na(Electronic) ~ 0,
                                TRUE ~ as.numeric(Electronic))) %>%
                   mutate(Visual = case_when(
                            is.na(Visual) ~ 0,
                            TRUE ~ as.numeric(Visual))) %>%
                   mutate(percent_electronic = Electronic/(Electronic + Visual)*100) %>%
                     filter(Region %notin% c("", "CA", 0))

coho_SFEC_plot <- ggplot(coho_sfec_sum, aes(x=Recovery.Year, y=percent_electronic, col=Region)) +
  geom_point() +
  geom_line()+
  scale_x_continuous(breaks=seq(2010, 2020, 1))+
  xlab("Year")+
  ylab("Estimated number CWTs sampled electronically (%)")+
  ggtitle("Coho adult fishery sampling")
coho_SFEC_plot


detection_methods_plot<-chinook_SFEC_plot+coho_SFEC_plot +   plot_layout(guides = 'collect')
detection_methods_plot
ggsave("Detection_methods_plot.jpeg")

?ggsave

