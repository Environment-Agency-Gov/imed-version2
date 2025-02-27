# Introduction #
Awareness of the human health consequences resulting from some environmental harms has increased over recent years, for example the mental health impacts from a lack of access to green space, whereas for other areas the relationship has been known for many decades, for example air pollution. There is not yet an established dataset that looks at cumulative environmental harms within neighbourhoods. 

The development of a prototype Index of Multiple Environmental Deprivation (IMED) has been a collaborative independent project by the Environmental Data Network involving staff from Friends of the Earth, The Environment Agency, Natural England, DEFRA and academics. We all have a shared view that it is a worthwhile endeavour to create a prototype for discussion with a wider community to explore its potential usefulness, as well as receive feedback on the choices of indicators, domains, and analytical approach. If the idea of an IMED is thought to be useful by a range of stakeholders, we will then collaboratively explore how best to develop the IMED, who should be involved, set a timeframe, and then commence work on refining the methodology and publishing a new version. 

This document describes the process used to derive the first version of the Index for Multiple Environmental Deprivation. We consider this iteration to be a proof of concept rather than a complete version of the index and are publishing this with the intention of allowing a wider set of interested people to review, feedback and provide additional ideas of how we might evolve future versions of the IMED so that it is both more comprehensive and useful for a wider audience.

The IMED has been calculated at Lower Super Output Area (LSOA) level, using the 2011 LSOA boundaries. We have used 2011 boundaries because several of the data sources we have used were published using the 2011 boundaries. Additional data that is useful to correlate with this index, such as the current Index of Deprivation for England, also use 2011 boundaries. However, future iterations of the IMED should consider moving to the 2021 LSOA boundaries.

For this iteration, the index only covers England.

This document lists the data sources used, the approaches used to derive the IMED and presents LSOA-level maps of the IMED and the component IMED domains showing the most environmentally deprived deciles.

## Potential applications of the IMED ##
The Index of Multiple Environmental Deprivation (IMED) is a composite measure used to assess cumulative environmental quality using indicators that are important to community health and well-being. It is therefore human-centric and does not attempt to capture environmental harms that are more significant for other species (e.g. light or water pollution).
The IMED currently addresses environmental deprivation in three domains: exposure to pollution (as air quality and noise); exposure to climate risk (flooding and heat); and access to a salutogenic environment (tree canopy cover, greenspace, and public rights of way). Mapping the index allows areas experiencing the greatest overall environmental deprivation to be identified, whilst mapping individual domains allows for development of more targeted investigations, such as identification of communities most vulnerable to climate change. 
The IMED can be used to understand environmental disparities at national, regional, and local scales, where it has a range of potential applications, including: 
 - **Policy development and resource allocation.** Environmental deprivation contributes to social deprivation, as recognised in the Index of Multiple Deprivation (IMD) (which includes only a simple living environment domain). The IMED provides a more substantive environmental index and so can better identify those communities most in need of environmental improvement, for health and climate resilience. This evidence can support development of national to local policy. Resources aimed at improving living conditions can be targeted to maximise impact from expenditure. The IMED uses publicly available data sets and so there is good scope for periodic updates. This would support monitoring and evaluation of policy and other interventions. 
 - **Health improvement.** There is already strong evidence of a relationship between environmental quality and health outcomes. Aggregate and domain level IMED data, mapped at fine spatial scale (LSOA) will allow analyses of the role of cumulative environmental factors on a range of health outcomes, leading to the design of health improvement interventions. 
 - **Understanding environmental and climate justice.** Social metrics are excluded from the IMED offering scope to explore the relationships between environmental deprivation, social deprivation, and health outcomes. Better understanding the ‘triple jeopardy’ of income deprivation, environmental deprivation and poor health is needed to better understand how social and political processes shape environmental inequality and climate vulnerability. 
 - **Inclusive spatial planning.** The IMED has been developed at fine spatial scale (LSOA level) and so can assist in local area planning and development to ensure equitable access to healthy and climate resilient environments for all residents. 
 - **Community Advocacy.** The IMED can enables community groups to advocate for better environmental conditions and climate resilience. The data-driven evidence can support those communities facing significant environmental deprivation and climate risk to argue for additional support (e.g. grants, technical assistance). 
 - **Supporting inter-agency action.** In addition to national and local government, numerous UK bodies have responsibilities relating to the environment, climate, health and inequality. The IMED, with national coverage of fine spatial scale data, supports dialogue between these bodies and their development of collaborative action to promote common interest. 
 -** Demonstrating commitment to environmental democracy.** The IMED is a powerful but readily understood tool that government and agencies can use to convey action on environmental and climate justice. The audience can range from the public to the international community. For example, the IMED represents a practical measure to guide action and build capacity needed to meet commitments under the UNECE Aarhus Convention on Access to Information, Public Participation in Decision-making and Access to Justice in Environmental Matters.

## Version 1 data caveats ##
We acknowledge that this first iteration of an IMED is not comprehensive in terms of covering all aspects of environmental deprivation. Firstly, there are a limited number of indicators in each domain. The number of indicators was kept intentionally small to ensure a manageable level of work for this proof-of-concept iteration. However, we also recognise that the pollution domain currently only considers the impacts of noise and air pollution; there are other causes of and impacts from pollution that we should look to include in future versions. It is also possible that the nature domain could be enhanced with additional data sources. And while flooding and heatwaves are significant climate risks, it is possible the climate domain could be strengthened by additional indicators.

Secondly, the data processing of some of the data sets to produce indicators could be more sophisticated. For some indicators, the data processing approach is sufficient. For others, particularly those with non-normal distributions, further processing options may produce a more optimal indicator. With some indicators having skewed distributions, not all indicators will have made a comparable contribution to the final index.

For some indicators we have used thresholds to produce an indicator, for example, air pollution guidelines. While the different indicators and the data that underpins them do not always allow for thresholds to be used, this could be seen as an inconsistency across the different indicators. Further discussion on alternative options for processing data can be found in the section on ‘Future development of the IMED’ on page 19.

Finally, when developing the Index we considered having a fourth domain covering ‘Living Environment’ which could include data such as road accidents, housing quality, and how well maintained the LSOA is (e.g. absence of litter, boarded-up properties, etc.). We choose not to for this iteration but are interested in feedback of whether such a domain should be included in the next iteration.

**Based on these caveats, this version of the Index should be considered ‘experimental’, and we do not recommend wider use of this version of the indicator other than for demonstration purposes.**

# Methodology and data #
## Overview ##
The first iteration of the IMED was calculated from six indicators in three domains covering pollution, nature, and climate risk. The indicators mostly draw on publicly available data (Table 1). A score was calculated for each domain, and the overall IMED score calculated by combining the three domain scores.

Each domain was calculated from two indicators, some of these indicators being composites of more than one data set (e.g. two air pollutants). Indicator data was normalised on a 0 to 1 scale, with 1 representing the most polluted, most nature deprived or highest climate risk score. Some indicators naturally lent themselves to this scale (i.e. source data was expressed as a percentage), whilst others with highly skewed distributions needed to be processed further in order for them to have an influence on the overall score equivalent to the other indicators.

**Table 1. Data sources used in IMED construction and mapping**
Domain      | Indicator and data source
------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Pollution   |  Air pollution: Neighbourhoods with NO2 and PM2.5 levels above WHO guidelines: Friends of the Earth analysis of Defra’s modelled background air pollution data, 2022. uk-air.defra.gov.uk/data/pcm-data
Pollution   |  Noise pollution: Road and rail noise: Strategic noise mapping, Defra 2019. https://www.gov.uk/government/publications/strategic-noise-mapping-2019Pollution
Pollution   |  Noise pollution: Aircraft noise: Aircraft Noise Map (data provided on request) https://noise-map.com/home/.
Nature      |  Greenspace: Access to green space in England: Scenario 2 (All green space with rights of way), Defra, Official Statistic in Development, 2024: https://www.gov.uk/government/statistics/access-to-green-space-in-england; https://www.gov.uk/government/statistics/access-to-green-space-in-england/access-to-green-space-in-england
Nature      |  Tree canopy cover: Terra Sulis on behalf of Friends of the Earth, 2022. https://policy.friendsoftheearth.uk/insight/mapping-english-tree-cover-results-ranking-and-methodology
Climate risks |  Flood risk: Risk of Flooding from Rivers and Sea, Low to High Risk Extent. Environment Agency (2024) https://www.data.gov.uk/dataset/bad20199-6d39-4aad-8564-26a46778fd94/risk-of-flooding-from-rivers-and-sea; Risk of Flooding from Surface Water – 1 in 100 year event extent. Environment Agency (2015). https://environment.data.gov.uk/dataset/51a5c4e7-10d3-4f34-bb0e-558835ab8c85
Climate risks |  Heat exposure risk: Twenty-year mean-monthly (Jan-Dec) near-surface daily maximum air temperature 2020-40 for RCP 8.5. CHESS-SCAPE: Future projections of meteorological variables at 1 km resolution for the United Kingdom 1980-2080 derived from UK Climate Projections 2018. https://catalogue.ceda.ac.uk/uuid/8194b416cbee482b89e0dfbe17c5786c
Other |	LSOA boundary files: Lower layer Super Output Areas (December 2011) Boundaries EW BFC (V3). https://geoportal.statistics.gov.uk/datasets/ons::lower-layer-super-output-areas-december-2011-boundaries-ew-bfc-v3/about

In calculating the domain scores from individual indicators, and the overall IMED score from the domains, we have incorporated a weighting system that enables different indicators or domains to have more or less significance in the final IMED score. However, for this first iteration of the IMED we have used an equal weighting for all indicators and domains, so that each has an equal impact on the final IMED score. The IMED calculation process is summarised in Table 2 and Figure 1. 

**Table 2. Summary of data processing to derive IMED indicators**
Domain       | Indicator       | Processing summary
-------------|-----------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Pollution    | Air pollution   | NO2 and PM2.5 annual average background concentrations each rescaled to a value from 0 to 1, with 0.5 representing WHO guidelines, then both combined into one air pollution indicator (0 = lowest levels of air pollution; 0.5 air pollution at WHO guidelines; 1 = highest air pollution from NO2 and PM2.5). 
Pollution    | Noise pollution   | Proportion of an LSOA with noise levels from road and rail above 55 dB and from aircraft above 45 dB (1 = 100% of LSOA impacted by noise pollution).
Nature       | Tree cover   | Tree canopy cover (% LSOA), inverted to between 0 and 1 (1 = minimum observed tree canopy cover).
Nature       | Greenspace   | Percentage of the population within 1 km of accessible greenspace (Defra Scenario 2, which included at least 2 hectares of accessible greenspace and rural rights of way). The percentage access was inverted to a value of between 0 and 1 (1 = least amount of accessible greenspace, i.e. none).
Climate risks    | Flood risk   | Proportion of LSOA at risk of flooding from rivers and seas, combined with proportion of LSOA at risk of surface water flooding. This data was  normalised by using a log transformation and expressed on 0-1 scale (0 = no part of LSOA at risk of flooding, 1 = all LSOA at risk of flooding).
Climate risks   | Heat exposure risk   | 20 year modelled monthly average maximum temperature for 2020-2040 under UKCIP18 RCP 8.5 (high emission/least emission mitigation) climate scenario, rescaled to a value of 0-1 (0 = lowest average maximum monthly temp, 1 = highest average maximum monthly temp).

## Calculating the overall IMED score ##
The IMED score was calculated by summing the domain scores to produce an overall index of multiple environmental deprivation (IMED):  

IMED score = 	[pollution domain score  * pollution weighting]   +  [nature domain score     *  nature weighting] + [climate domain score    *  climate weighting]

IMED deciles were then calculated from the IMED score, with decile 1 representing the most environmentally deprived LSOAs.  Figure 2 summarises the index construction process. 

**Figure 1.  Process diagram summarising the calculation of the IMED.** (Note: In this first IMED iteration, indicator and domain weights are all equal)
![imed1-data-process-diagram](https://github.com/user-attachments/assets/669d71a0-5d15-49ab-a61d-5b052a3e9de6)

# IMED version 1: Maps of IMED and domain deciles across England #

Label                                               | Map
----------------------------------------------------|----------------------------------------------------------------------------
**Figure 2. Map of IMED deciles**                   | ![imed](https://github.com/user-attachments/assets/9ea5492d-d3cc-435d-bff6-d695939c9fdf)
**Figure 4. Map of IMED pollution domain deciles**  | ![pollution-domain](https://github.com/user-attachments/assets/b3e150d5-797d-49f4-be81-910d22a41494)
**Figure 5. Map of IMED nature domain deciles**     | ![nature-domain](https://github.com/user-attachments/assets/96a51458-16c3-4e7d-88ff-fd35392cfa47)
**Figure 6. Map of IMED climate domain deciles**    | ![climate-domain](https://github.com/user-attachments/assets/a5ce94a8-d525-40cd-bd0b-256bc3f7aed9)

