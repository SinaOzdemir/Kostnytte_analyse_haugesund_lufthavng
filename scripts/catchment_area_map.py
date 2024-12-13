# -*- coding: utf-8 -*-
"""
Created on Fri Dec 13 12:32:02 2024

@author: sioz
"""

import folium
from folium.plugins import HeatMap

# Create a base map
base_map = folium.Map(location=[59.5, 5.5], zoom_start=7)

# Add the airports as markers
for airport, coords in airports.items():
    folium.Marker(location=coords, popup=f"{airport}").add_to(base_map)

# Create heat maps for primary and secondary catchment areas
primary_locations = []
secondary_locations = []

for _, row in df.iterrows():
    location_coords = nearby_locations[row['Location']]
    if row['Catchment Area'] == "Primary":
        primary_locations.append(location_coords)
    elif row['Catchment Area'] == "Secondary":
        secondary_locations.append(location_coords)

# Add heat layers
HeatMap(primary_locations, radius=15, gradient={0.4: "blue", 0.65: "lime", 1: "red"}).add_to(base_map)
HeatMap(secondary_locations, radius=15, gradient={0.4: "blue", 0.65: "orange", 1: "yellow"}).add_to(base_map)

# Save the map to a file
map_file = "/mnt/data/Airport_Catchment_Areas_Map.html"
base_map.save(map_file)

map_file
