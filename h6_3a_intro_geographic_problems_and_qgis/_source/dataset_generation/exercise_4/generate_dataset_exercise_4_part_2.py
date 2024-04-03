import geopandas
import pandas as pd

stats19_counts_by_msoa_2018_2022  = pd.read_csv("h6_3a_intro_geographic_problems_and_qgis/_source/dataset_generation/exercise_3/stats19_counts_by_msoa_2018_2022.csv")

msoa_gdf = geopandas.read_file("h6_3a_intro_geographic_problems_and_qgis/_source/dataset_generation/helpers/MSOA_casualties_collisions_3857.gpkg")

msoa_gdf = msoa_gdf.drop(columns=["casualty_counts_5_years_n", "collision_counts_5_years_n"])

msoa_gdf_counts = pd.merge(
    left=msoa_gdf,
    right=stats19_counts_by_msoa_2018_2022,
    left_on="MSOA11CD",
    right_on="msoa11cd",
    how="left"
).drop(columns="msoa11cd")

msoa_gdf_counts.to_file("h6_3a_intro_geographic_problems_and_qgis/_source/dataset_generation/exercise_4/stats_19_counts_by_msoa_3857.geojson", driver='GeoJSON')
