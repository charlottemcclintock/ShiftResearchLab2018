

parcels <- read_csv("parcels.csv") %>% select(homevalue, nbhd, county) %>% filter(!nbhd=="Kennedy")


ggplot(parcels, aes(x=homevalue, fill=county)) + geom_density(aes(alpha=.5)) + xlim(0, 1000000)











