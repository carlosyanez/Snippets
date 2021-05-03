# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-20/readme.md

# make sure that {librarian} and {tidytuesdayR} are there
if(!require(librarian)) install.packages("librarian")


# load other libraries

librarian::shelf("tidyverse",
                 "here",
                 "arrow",
                 "flextable",
                 "xkcd",
                 "showtext",
                 "carlosyanez/customthemes")

# folder reference for here()
tt_date<-"2021-04-27"


# load tt data
if(!file.exists(here(tt_date,"tidytuesday.parquet"))){
  
  if(!require(tidytuesdayR)) install.packages("tidytuesdayR")
  
  tuesdata <- tidytuesdayR::tt_load(tt_date)
  write_parquet(tuesdata$departures, here(tt_date,"tidytuesday.parquet"),compression = "gzip", compression_level = 7)
  departures <- tuesdata$departures
}else{
  departures <- read_parquet(here(tt_date,"tidytuesday.parquet"))
}

# from xkcd vignette, adapted for macos
download.file("http://simonsoftware.se/other/xkcd.ttf",
                             dest=here(tt_date,"xkcd.ttf"), mode="wb")

font_add("xkcd",here(tt_date,"xkcd.ttf"))
showtext_auto()

#dismissals <- departures %>% count(fyear,departure_code) %>%
#              group_by(fyear) %>%
#              mutate(n_perc=n/sum(n)) %>%
#              ungroup() %>%
#              filter(fyear==2015) %>%
#              filter(!is.na(departure_code)|!(departure_code==9))

dismissals <- departures %>% 
              replace_na(list(departure_code=7)) %>%
              mutate(departure_code=if_else(departure_code==8,7,departure_code)) %>%
              filter(!(departure_code==9)) %>%
              count(departure_code) %>%
              arrange(n) %>%
              mutate(new_code=row_number(),
                     n_perc=100*n/sum(n))

y_labels <- seq(0,ceiling(max(dismissals$n_perc)/10)*10,10)

xrange <- c(min(dismissals$new_code)-.2,max(dismissals$new_code))
yrange <- c(0,max(y_labels))
bar_width <- 0.3
x_labels <- tibble(departure_code=1:7,
                   label=c("Death","Illness","Performance Issues","Legal Violations or Concerns",
                           "Retired","New Opportunity","Other")) %>%
                  left_join(dismissals,by="departure_code") %>%
                  arrange(new_code) %>%
                  pull(label)

y_labels <- seq(0,ceiling(yrange[2]/10)*10,10)

p<- ggplot() + xkcdrect(aes(xmin=new_code-bar_width,xmax=new_code+bar_width,
                         ymin=0,ymax=n_perc),
                     data=dismissals,
                     fill="gray80",color="gray80")+
  xkcdaxis(xrange,yrange) +
  theme_xkcd() +
   labs(x="",y="Percentage of total",
        title="Is a tough job?",
        subtitle="Reasons for CEO departures in SP 5000 companies between 2000 and 2018",
        caption="Source :  Gentry et al. via DataIsPlural and tidytuesday -- twitter: at carlosyanez") + 
   scale_x_continuous(breaks=1:7,labels=x_labels)+
   scale_y_continuous(breaks = y_labels,labels = y_labels)+
   coord_flip() +
  theme(plot.background=element_rect(colour="black"))

wv <- 12
save_image(p,here(tt_date,"ceo_departures.png"),width=wv,height=wv/2) 


