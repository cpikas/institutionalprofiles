# institutionalprofiles
Scripts for ASIST MET17 paper on bibliometric methods for profiling institutions

Poster available at: https://speakerdeck.com/cpikas/methods-for-bibliometric-institutional-profiles-for-the-practitioner

Files included:
* FractionalTimesCited.R - imports Clarivate Web of Science data and calculates fractional times cited per Leydesdorff and Opthof(2010)
* geocodeaffil.R - takes author affiliation addresses and number of articles - both available many ways but most easily from bibliometrix, and maps them as sized circles on a world map
* kmlShapeCiteTraj.R - takes output of WoS (or Scopus) citation report, and performs longitudinal k-means clustering to find clusters of citation trajectories (separate article on this in preparation)
* topartLDA.R - takes abstracts and performs LDA on them and uses a visualization to help name the clusters. Figure 2 on the poster was actually done using Excel :(
