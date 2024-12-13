# -*- coding: utf-8 -*-
"""
Created on Fri Dec 13 12:29:45 2024

@author: sioz
"""

import pandas as pd
from geopy.distance import geodesic
import folium
from folium.plugins import HeatMap
from matplotlib.patches import Circle
from matplotlib.collections import PatchCollection
import ace_tools as tools


# Define the coordinates of the airports
airports = {
    "Stavanger Airport": (58.8766, 5.6378),
    "Haugesund Airport": (59.3450, 5.2084),
    "Stord Airport": (59.7919, 5.3409),
    "Bergen Airport": (60.2936, 5.2181),
}

# Define travel radii in kilometers for catchment areas
primary_radius = 60  # Approximately 1 hour by car
secondary_radius = 90  # Approximately 1.5 hours by car

# Placeholder data for nearby towns and cities with their coordinates
# (Sample data, adjust with actual nearby towns for real-world precision)
nearby_locations = {
    "Stavanger": (58.9699, 5.7331),
    "Haugesund": (59.4136, 5.2680),
    "Stord": (59.7793, 5.5000),
    "Bergen": (60.3913, 5.3221),
    "Sandnes": (58.8524, 5.7352),
    "Kopervik": (59.2820, 5.3033),
    "Odda": (60.0706, 6.5460),
    "Voss": (60.6214, 6.4243),
}

# Calculate distances and determine inclusion in catchment areas
results = []
for airport, airport_coords in airports.items():
    for location, location_coords in nearby_locations.items():
        distance_km = geodesic(airport_coords, location_coords).kilometers
        if distance_km <= primary_radius:
            catchment = "Primary"
        elif distance_km <= secondary_radius:
            catchment = "Secondary"
        else:
            catchment = "Outside"
        results.append({
            "Airport": airport,
            "Location": location,
            "Distance (km)": round(distance_km, 2),
            "Catchment Area": catchment
        })

# Create a DataFrame
df = pd.DataFrame(results)

# Save the results to an Excel file
output_file = "/mnt/data/Airport_Catchment_Areas.xlsx"
df.to_excel(output_file, index=False)


tools.display_dataframe_to_user(name="Airport Catchment Areas", dataframe=df)




#Create a base map
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


# Define a function to create circular patches
def create_circles(centers, radii, color, alpha):
    circles = []
    for center, radius in zip(centers, radii):
        circles.append(Circle(center, radius / 111))  # Convert km to degrees (~111km per degree)
    return PatchCollection(circles, color=color, alpha=alpha, edgecolor="black", linewidths=0.5)

# Coordinates of airports for mapping
airport_coords = {airport: (coords[1], coords[0]) for airport, coords in airports.items()}

# Separate primary and secondary circles for each airport
primary_coords = list(airport_coords.values())
secondary_coords = list(airport_coords.values())

# Initialize the plot
fig, ax = plt.subplots(figsize=(12, 12))
ax.set_aspect("equal")
ax.set_title("Airport Catchment Areas", fontsize=16)
ax.set_xlabel("Longitude", fontsize=12)
ax.set_ylabel("Latitude", fontsize=12)

# Draw primary catchment areas (blue)
primary_circles = create_circles(primary_coords, [primary_radius] * len(primary_coords), color="blue", alpha=0.5)
ax.add_collection(primary_circles)

# Draw secondary catchment areas (orange)
secondary_circles = create_circles(secondary_coords, [secondary_radius] * len(secondary_coords), color="orange", alpha=0.3)
ax.add_collection(secondary_circles)

# Plot airport points
for name, coord in airport_coords.items():
    ax.plot(coord[0], coord[1], "ro", markersize=8, label=name)

# Add legend and grid
handles = [
    plt.Line2D([0], [0], color="blue", lw=4, alpha=0.5, label="Primary Catchment"),
    plt.Line2D([0], [0], color="orange", lw=4, alpha=0.3, label="Secondary Catchment"),
    plt.Line2D([0], [0], color="red", marker="o", markersize=8, linestyle="", label="Airports"),
]
ax.legend(handles=handles, loc="lower right")
ax.grid(True)

# Set the bounds of the map
longitudes, latitudes = zip(*airport_coords.values())
ax.set_xlim(min(longitudes) - 1, max(longitudes) + 1)
ax.set_ylim(min(latitudes) - 1, max(latitudes) + 1)

# Save the static map
static_map_file = "/mnt/data/Airport_Catchment_Areas_Static_Map_Final.png"
plt.savefig(static_map_file, bbox_inches="tight")
plt.show()

static_map_file