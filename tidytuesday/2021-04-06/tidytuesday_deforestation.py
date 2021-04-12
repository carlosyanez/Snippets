# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-06/readme.md

#load libraries

import pandas as pd
from bs4 import BeautifulSoup
import requests as r
import geopandas 
import geoplot
import mapclassify as mc
import matplotlib.pyplot as plt

#load data

forest_area = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest_area.csv')

#get data from wikipedia
url = "https://en.m.wikipedia.org/wiki/List_of_countries_by_forest_area"
wiki_page_request = r.get(url)
wiki_page_text = wiki_page_request.text

soup = BeautifulSoup(wiki_page_text, 'html.parser')
table =soup.find('table',{'class':"wikitable"})

forest_area_year=pd.read_html(str(table))
# convert list to dataframe
forest_area_year=pd.DataFrame(forest_area_year[0])

#new_header = forest_area_year.iloc[0] #grab the first row for the header
#forest_area_year = forest_area_year[1:] #take the data less the header row
#forest_area_year.columns = new_header #set the header row as the df header

forest_area_year.columns = forest_area_year.columns.map(str)
cols = forest_area_year.columns[1:5]

forest_area_year[cols]=forest_area_year[cols].transform(lambda x : x*1000)   # values in hectares

forest_area_year=forest_area_year[forest_area_year.Region=="World"]
forest_area_year = forest_area_year.melt(id_vars=["Region"],var_name="year",value_name="area")
forest_area_year["year"]=pd.to_numeric(forest_area_year["year"])
forest_area_year = forest_area_year[(forest_area_year.year==2000) | (forest_area_year.year ==2020)]
forest_area_year =forest_area_year.drop(columns=["Region"])


del url,wiki_page_request,wiki_page_text,soup,table,cols

#population data

pop = pd.read_csv('2021-04-06/pop.csv')  ###got file from R wpp2019 package!
pop['entity']=pop['name']
pop=pop[['entity','2000','2020']]

pop=pop.melt(id_vars=["entity"],var_name="year",value_name="pop")
pop["pop"]=pd.to_numeric(pop["pop"])
pop["year"]=pd.to_numeric(pop["year"])


forest_area["year"]=pd.to_numeric(forest_area["year"])
forest_area  = forest_area[(forest_area.year==2000) | (forest_area.year==2020)]
forest_area = forest_area.dropna(subset=["code"])


forest_area = forest_area.merge(forest_area_year,how="left",on=["year"])
forest_area = forest_area.merge(pop,how="left", on=["entity","year"])
forest_area = forest_area.dropna(subset=["pop"])
forest_area["area"]=pd.to_numeric(forest_area["area"])
forest_area["pop"]=pd.to_numeric(forest_area["pop"])
forest_area["areapc"]=forest_area["area"]*forest_area["forest_area"]/forest_area["pop"]

area_pc = forest_area[["entity","year","areapc"]]
area_pc['year'] = area_pc['year'].astype(str)
area_pc=area_pc.pivot_table(index=['entity'], columns='year', values='areapc').reset_index()
area_pc["difference"] = area_pc["2020"] -area_pc["2000"]
area_pc = area_pc.dropna(subset=["difference"])

#world map

world = geopandas.read_file(
    geopandas.datasets.get_path('naturalearth_lowres')
)

world=world.merge(area_pc,how="left",left_on=['name'], right_on=['entity'])


# from https://www.python-graph-gallery.com/choropleth-map-geopandas-python


# Initialize the figure
fig, ax = plt.subplots(1, 1, figsize=(16, 12))

scheme = mc.Quantiles(world['2020'], k=10)


# Map
a= geoplot.choropleth(world, 
    hue="2020", 
    linewidth=.1,
    scheme=scheme, cmap='viridis',
    legend=True,
    edgecolor='black',
    ax=ax
);

ax.set_title('Forest area per capita in 202', fontsize=13);

plt.show(a)

