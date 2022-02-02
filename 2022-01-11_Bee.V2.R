x <- c("tidyverse", "broom", "tidytuesdayR", "rgeos", "geojsonio", "ggtext", "showtext")
(function(x){
  sapply(x, function(x) if(!x %in% installed.packages()){
    install.packages(x, dependencies = T)
  })
  sapply(x, library, character.only=T)
})(x)



##Add font para o gráfico
font_add_google(name = "Red Hat Display", family = "Red Hat Display")
font_add_google(name = "Red Hat Text", family = "Red Hat Text")
showtext_auto()

## Add most  frequent stressor when the population reduced
#Idea
download.file("https://raw.githubusercontent.com/ncruickshank/nc_r_tidytuesday/master/2022/2022-01-11%20-%20Bees/Bees%20Analysis.Rmd", "idea.rmd")

#Loading data
Bee_data <- tt_load("2022-01-11")
Bee_data$Colony <- Bee_data$colony
Bee_data$Stressor <- Bee_data$stressor

#Making stressor pivor wider
Bee_data$Wide_Stressor <- Bee_data$Stressor %>% pivot_wider(names_from = stressor, values_from= stress_pct)

#Merging data
Bee_data$Merge_bee_b <- Bee_data$Colony %>% left_join(Bee_data$Wide_Stressor)

#Calculando o quanto de colônias foram perdidas e adicionadas

Bee_data$Merge_bee_c <- Bee_data$Merge_bee_b %>% mutate(colony_var_n= colony_added- colony_lost,
                                                        colony_var_pct= (100*colony_var_n)/colony_n)


#Verificando se os meses fazem diferença no turnover de colméias
Bee_data$Merge_bee_c %>% group_by(months) %>% summarise(mean=mean(colony_var_pct, na.rm=T))
#Em abril-junho o número de colônias tende a aumentar



#Calculaa média da porcentagem de variação ao longo dos anos
Bee_data$Summary_colony_var <- Bee_data$Merge_bee_c %>% group_by(state) %>% summarise(mean_col_variation_pct= mean(colony_var_pct, na.rm=T))


#Calcula a principal variável associada a redução
Bee_data$Summary_colony_var <- Bee_data$Summary_colony_var %>%  right_join(Bee_data$Merge_bee_c %>%  group_by(state) %>% do(tidy(lm(colony_var_pct~`Varroa mites`+
                                                           `Other pests/parasites`+
                                                           Disesases+
                                                           Pesticides+
                                                           Other+
                                                           Unknown,., na.action = "na.exclude"))) %>% 
  filter(., !grepl("Intercept", term)) %>% 
  slice_min(n=1, estimate))


#Download hexabin map https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
Graph <- list()
Graph$hexabin_us <- geojsonio::geojson_read("us_states_hexgrid.geojson", what="sp") #Para carregar o arquivo em formato espacial
#Remover United States do nome dos estados
Graph$hexabin_us@data <- Graph$hexabin_us@data %>% mutate(google_name= google_name %>% gsub(" \\(United States\\)", "",.))

#Fortify graph data
Graph$Hexabin_tidy <- Graph$hexabin_us %>% broom::tidy( region="google_name") %>% rename(state=id)

#Add information about variation in colony numbers (%)
Graph$Hexabin_tidy <- Graph$Hexabin_tidy %>% left_join(y=Bee_data$Summary_colony_var)


#Calcular região centroide de cada hexabin para poder adicionar os nomes
Graph$centers <- data.frame(Graph$hexabin_us %>% rgeos::gCentroid(byid = T), id=Graph$hexabin_us@data$iso3166_2)


#Add siglas dos estados
download.file("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv", "siglas.csv")
Graph$Hexabin_tidy <- Graph$Hexabin_tidy %>% left_join(read_csv("siglas.csv") %>% rename(state=1, id=2))


##Adc coordenadas centrais dos estados
Graph$Hexabin_tidy <- Graph$Hexabin_tidy %>% left_join(Graph$centers)


ggplot(data=Graph$Hexabin_tidy)+ geom_polygon(aes(long, lat, group=state, fill=mean_col_variation_pct), color="#490C0C", size=2)+
  geom_polygon(aes(long, lat, group=state), fill=NA, color="lightyellow2", size=1)+
  geom_text(aes(x, y, label=id), family= "Red Hat Display")+
  #Adc o estressor e arruma o nome deles
  geom_text(aes(x, y, label=term %>% gsub("`", "",.) %>% replace(term== "`Other pests/parasites`", "Other")),
            family= "Red Hat Text", fontface="italic", vjust=2)+
  theme_void()+
  coord_map()+
  labs(title = "Redução média no número de colônias de abelhas e estressor mais comum", subtitle = "2015-2020")+
  scale_fill_gradientn(colors=c("yellowgreen", "yellow1", "darkorange1","darkorange4"), name="  %")+
  theme(panel.background = element_rect("#0C1017"),
        plot.background = element_rect("#0C1017"),
        plot.title = element_text(family = "Red Hat Display", size = 22, hjust = 0.5, colour = "#EBEBEC"),
        plot.subtitle = element_text(family = "Red Hat Text", size = 18, hjust = 0.5, colour = "#EBEBEC"),
        legend.text = element_text(family="Red Hat Text", colour="#EBEBEC"),
        legend.title = element_text(family="Red Hat Display", colour="#EBEBEC"))
  
ggsave("Bee.tiff", width=7, height=5, device="tiff")




###Brincadeira com estatística
a <- Bee_data$Merge_bee_c %>% group_by(state, months) %>% 
  select(11:18) %>% summarise(across(where(is.numeric), function(x) mean(x,na.rm=T))) %>% ungroup() %>% group_by(state) %>% 
  do(tidy(lm(colony_var_pct~ `Varroa mites`+
                                    `Other pests/parasites`+
                                    Disesases+
                                    Pesticides+
                                    Other+
                                    Unknown, data=., na.action="na.exclude", ))) %>%  filter(., !grepl("Intercept", term)) %>% slice_min(n=1, estimate)




Bee_data$Merge_bee_c %>%  lm(colony_var_pct~`Varroa mites`+
                                                                                                       `Other pests/parasites`+
                                                                                                       Disesases+
                                                                                                       Pesticides+
                                                                                                       Other+
                                                                                                       Unknown ,., na.action = "na.exclude") %>% plot()




Bee_data$Merge_bee_c %>%  lm(colony_var_pct ~ as.factor(state) + as.factor(months), .) %>% summary()
plot(Bee_data$Merge_bee_c$year, Bee_data$Merge_bee_c$colony_var_pct)
abline(v=Bee_data$Merge_bee_c$colony_var_pct)

plot(mod)

