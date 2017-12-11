# The Need For Balanced Global Freshwater Usage
Final Project of DATA 512 - Human Centered Data Science of the M.S. Program in Data Science, UW

Abstract:
The objective of this project is to illustrate the issue of imbalanced water consumption in the world and raise the awareness and conscience of public to take actions on a more sustainable water consumption pattern. Under the framework of Global Water Footprint, I formulated research questions to study how the global flow of freshwater resources was distributed and whether the transfer of water resources and pollutions was fair between the developed and developing countries. The data and visualizations show that there is indeed a hugely imbalanced distribution of global water usage and transfer, and the freshwater usage of a country not only depend on the volume of aggregated consumption, but also the water intensity of its inhabitants' consumption pattern, and the water scarcity and technology of water usage of its trading partners. As a result, to reduce the global freshwater consumption, not only do water policy decision makers need to put their water consumption within a global context, also they need to adopt a more holistic view by considering of the interaction between one country's water policy and policies in related sectors like energy, trade, technology, agriculture and so on. At the individual level, the public should be more informed with the diversity of water intensity among all the products they consume every day.

### License of the source data

Datasets from the Water Footprint Network, licensed under [CC BY-SA 3.0](https://creativecommons.org/licenses/by-sa/3.0/): 

* National Water Footprint dataset: http://waterfootprint.org/en/resources/water-footprint-statistics/#CP3
* International Virtual Water Flow Statistics: http://waterfootprint.org/en/resources/water-footprint-statistics/#CP4


### Datasets

* Original Datasets are saved under Data/original datasets/
  - Dataset of Global Virtual Water Flow (1996-2005 annual averages): Report50-Appendix-II&III.xls, the associated paper is [1]
  - Dataset of Global Water Footprint Per Capita by country (1996-2005 annual averages): Report50-Appendix-VIII&IX.xls

* Transformed Datasets are saved under Data/
  - Dataset of Global Virtual Water Flow (1996-2005 annual averages): dfWaterFlow.RData and GlobalVirtualWaterFlow.csv
  - Dataset of Global Water Footprint Per Capita by country (1996-2005 annual averages): dfWFPC.RData and WFPerCapita.csv

###  References

* [1]. Mekonnen, M.M. and Hoekstra, A.Y. (2011) National water footprint accounts: [The green, blue and grey water footprint of production and consumption](http://www.waterfootprint.org/Reports/Report50-NationalWaterFootprints-Vol1.pdf), Value of Water Research Report Series No. 50, UNESCO-IHE, Delft, the Netherlands
* 2. Hoekstra, A.Y. and Mekonnen, M.M. (2012) [The water footprint of humanity](http://waterfootprint.org/media/downloads/Hoekstra-Mekonnen-2012-WaterFootprint-of-Humanity.pdf), Proceedings of the National Academy of Sciences, 109(9): 3232–3237.
* 3. Mekonnen, M.M. and Hoekstra, A.Y. (2011) [The green, blue and grey water footprint of crops and derived crop products](http://waterfootprint.org/media/downloads/Mekonnen-Hoekstra-2011-WaterFootprintCrops.pdf), Hydrology and Earth System Sciences, 15(5): 1577-1600.
* 4. Arjen Hoekstra et al.(2011), [The Water Footprint Assessment Manual](http://waterfootprint.org/media/downloads/TheWaterFootprintAssessmentManual_2.pdf)
