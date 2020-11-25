library(tidyverse)
library(readxl)
library(patchwork)
library(ggthemes)

#source : https://data.humdata.org/dataset/city-product-per-capita
if(!file.exists("file.xlsx"))
  download.file("https://data.humdata.org/dataset/e4324ba3-910a-4f69-aad8-677e96b8234a/resource/23a97bd4-24af-47ee-8339-1409ba9e785b/download/city_product_capita.xlsx","file.xlsx")

data <- read_xlsx("./file.xlsx")

colnames(data) <- c("Country","City","x",as.data.frame(data)[2,4:length(data)])

data <- data[4:nrow(data),] %>% 
          select(-x) %>% 
          fill(Country,.direction="down") %>%
          filter(!is.na(City)) %>%  mutate_all(as.character) %>%
          pivot_longer(c(-Country,-City),values_to="GDP_PC",names_to="Year") %>%
          mutate(GDP_PC = as.numeric(GDP_PC),
                 Year=as.numeric(Year))

data <- data %>% left_join(data %>% filter(Year==2000) %>% select(Country,City,GDP_2000=GDP_PC),
                   by=c("Country","City")) %>%
        mutate(GDP_PC2000=GDP_PC/GDP_2000)

p1 <- data %>% filter(Country %in% c("Austria") &
                  !is.na(GDP_PC)) %>%
        ggplot(aes(x=Year,y=GDP_PC2000,colour=City)) +
          geom_line() +
          labs(title="Austria",
                x="Year",
               y="GDP Per Capita - Normalised Yr 2000")

p2 <- data %>% filter(Country %in% c("Australia") &
                        !is.na(GDP_PC)) %>%
  ggplot(aes(x=Year,y=GDP_PC2000,colour=City)) +
  geom_line() +
  labs(title="Australia",
       x="Year",
       y="Normalised GDP Per Capita")

p3 <- data %>% filter(Country %in% c("Chile") &
                        !is.na(GDP_PC)) %>%
  ggplot(aes(x=Year,y=GDP_PC2000,colour=City)) +
  geom_line() +
  labs(title="Chile",
       x="Year",
       y="Normalised GDP Per Capita")

text_size<-9 
combined_plot <- p2 / p3 + plot_annotation(title = 'GDP per Capita (compared to Year 2000)',
                          caption="Data Source : https://data.humdata.org/dataset/city-product-per-capita") & 
              theme_hc() &
              theme(legend.position = "right",
                    plot.title = element_text(size=12),
                    axis.title.x = element_text(size = text_size),
                    axis.text.x = element_text(angle = 0, hjust = 1,size = text_size),
                    axis.title.y = element_text(size = text_size),
                    axis.text.y = element_text(size = text_size),
                    strip.text.x = element_text(size = text_size),
                    strip.text.y = element_text(size = text_size, angle = 90),
                    legend.title=element_text(size=text_size),
                    legend.text=element_text(size=text_size),
                    legend.justification = "left") 
combined_plot
ggsave("city_gdp_au_at_cl.png",combined_plot,dpi=600)

 
  
