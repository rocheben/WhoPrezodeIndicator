
#Colors palette
all_values<-c(seq(0,9),NA)
wildCirculationShape$Area <- factor(wildCirculationShape$Area, levels = all_values)
livestockCirculationShape$Area <- factor(livestockCirculationShape$Area, levels = all_values)
compositeShape$Area <- factor(compositeShape$Area, levels = all_values)
palette1 <- colorRampPalette(brewer.pal(9, "Reds"))(length(all_values))
palette2 <- colorRampPalette(brewer.pal(9, "Blues"))(length(all_values))
palette3 <- colorRampPalette(brewer.pal(9, "Purples"))(length(all_values))

#Plotting summary indices 
wildlifePlot <- ggplot() +
	geom_sf(data = wildCirculationShape, aes(fill = as.factor(Area))) +
	theme_minimal()+labs(fill = "Wildlife aggregated risk")+
	scale_fill_manual(values = palette1,breaks = all_values)
ggsave(paste0("./figures/",pathogen[j], "_wildlifePlot.pdf"),wildlifePlot,width = 10, height = 5, dpi = 150, units = "in");
st_write(obj = wildCirculationShape, dsn=paste0("./figures/resultsWildlife_",pathogen[j],".shp"),delete_dsn=T)

liveStockPlot <- ggplot() +
	geom_sf(data = livestockCirculationShape, aes(fill = as.factor(Area))) +
	scale_fill_manual(values = palette2,breaks = all_values) +
	theme_minimal()+labs(fill = "Livestock aggregated risk")
ggsave(paste0("./figures/",pathogen[j],"_liveStockPlot.pdf"),liveStockPlot,width = 10, height = 5, dpi = 150, units = "in");
st_write(obj = livestockCirculationShape, dsn=paste0("./figures/resultslivestock_",pathogen[j],".shp"),delete_dsn=T)

compositePlot <- ggplot() +
	geom_sf(data = compositeShape, aes(fill = as.factor(Area))) +
	scale_fill_manual(values = palette3, breaks = all_values) +
	labs(fill = "Overall aggregated risk")+ theme_minimal()
ggsave(paste0("./figures/",pathogen[j], "_compositePlot_.pdf"),compositePlot,width = 10, height = 5, dpi = 150, units = "in");
st_write(obj = compositeShape, dsn=paste0("./figures/resultsComposite_",pathogen[j],".shp"),delete_dsn=T)

# Create a composite plot
temp <- plot_grid(wildlifePlot, liveStockPlot, compositePlot, 
			   nrow = 2, ncol = 2, align = "v", hjust = c(0, 0, 0.5))
ggsave(paste0("./figures/",pathogen[j], "_AllPlots.pdf"), temp,width = 10, height = 5, dpi = 150, units = "in");
