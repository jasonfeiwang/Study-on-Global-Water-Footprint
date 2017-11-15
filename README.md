# data-512-Final Project

The objective of this project is to illustrate the problem of imbalanced water consumption among the world and raise the awareness and conscience of general public to take actions on a more sustainable water consumption pattern.

### License of the source data

Datasets from the Water Footprint Network :

* National Water Footprint dataset: http://waterfootprint.org/en/resources/water-footprint-statistics/#CP3
* Internal Virtual Water Flow Statistics: http://waterfootprint.org/en/resources/water-footprint-statistics/#CP4
* Water Scarcity Statistics: http://waterfootprint.org/en/resources/water-footprint-statistics/#CP5
* Water Pollution Level Statistics: http://waterfootprint.org/en/resources/water-footprint-statistics/#CP6
* Others


### Project Plan

The issue of the world\'s fresshwater consumption has been tradititonally studied only at a national level based on a basic principle of national demand is less than or equal to the national supply. But recent studies by Prof. Arjen Hoekstra shows that to reduce the global freshwater consumption in aggregate, not only do water policy decision makers need to put their water consumption within a global context, but also they need to adopt a more holistic view by taking into account of the interaction between one country\'s water policy and policies in other sectors like energy, trade, agriculture and so on. At the individual level, the general public should be more informed with the diversity of water intensity among all the products they consume each day. To study the issue of freshwater consumption within a global context, Prof. Arjen Hoekstra coined the term \"Water Footprint\", which extended the traditional definition of water usage by incorporating new dimentions like time, location, water type etc. 

With the establishment of the Watef Footprint Network, public can get access to numerous studies on global water consumption over the last decade and see the vast imbalance of water usage accross the world. But the study results are mostly presented via a form of exploratory analyses, indicating key areas of extreme water footprint imbalance or the water resrouce scarcity. More hidden patterns of global water footprint woudl be discovered by applying more data science oriented approaches (time series analysis, anomaly detection for example).

There is also no optimization model which aims at reducing global water consumption in total and mediating water resource exploitation disparity. Therefore in this project I plan to accomplish the following:
  - synthesize the datasets of water footprint, water scarcity, international trade, water intentensity of products 
  - extend the studies of Prof. Arjen Hoekstra by building visualization dashboards using R Shiny app to give the user a more flexible way to discover global water footprint patterns.
  - establish models to automatically detect outlier countries/regions of high water footprint or rapid growth thereof.
  - make recommendations to the change of trading patterns at the country - product category level.
