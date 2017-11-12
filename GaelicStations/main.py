import pandas as pd
import utm
from fuzzywuzzy import fuzz
import folium
import numpy as np
from geopy.geocoders import Nominatim
from geopy.distance import vincenty
import seaborn as sns
from scipy.stats import spearmanr

def main():
    # DATA PREPARATION

    ## Collect
    names = pd.read_csv('names.csv')
    locations = pd.read_csv('locations.csv')

    ## Clean
    names = names.loc[names['Gaelic'] != names.loc[6].Gaelic]

    ## Combine
    data = pd.merge(names, locations, how='inner', on=['English'])

    ## Transform
    GQS_latlon = (55.8623,-4.2511)
    GQS_ref = utm.from_latlon(*GQS_latlon)
    GQS_row = locations.loc[locations['English'] == "Glasgow Queen Street"]
    E_off = int(GQS_ref[0] - GQS_row['Easting'].values[0])
    N_off = int(GQS_ref[1] - GQS_row['Northing'].values[0])
    data['Lat'] = data.apply(lambda row: latitude(row,GQS_ref[2],E_off,N_off), axis=1)
    data['Lon'] = data.apply(lambda row: longitude(row,GQS_ref[2],E_off,N_off), axis=1)
    data = data[['English','Gaelic','Lat','Lon']]

    # ANALYSIS

    # Visual Analysis
    mapOps = {
        'location' : (56.4907,-4.2026),
        'zoom_start' : 7,
        'tiles' : 'Stamen Terrain'
    }
    m = folium.Map(**mapOps)

    for index, row in data.iterrows():
        ratio = fuzz.ratio(row['English'],row['Gaelic'])
        colour = 'red' if ratio < 50 else 'blue'
        folium.Marker(
            location=(row['Lat'],row['Lon']),
            icon=folium.Icon(color=colour,icon='cloud')
        ).add_to(m)
    m.save('index.html')

    # Feature Extraction
    locate = Nominatim().geocode
    city = ['Glasgow','Edinburgh','Aberdeen','Dundee','Inverness','Stirling']
    city_loc = list(map(lambda x:(x.latitude, x.longitude), map(locate,city)))

    X = data.apply(lambda row: fuzz.ratio(row['English'],row['Gaelic']) , axis=1)
    Y = data.apply(lambda row: rscore(city_loc,(row['Lat'],row['Lon'])), axis=1)

    # Statistical Analysis
    XY = pd.DataFrame({'Name Similarity':X, 'Rural Score':Y})
    plotOps = {
                'x':'Name Similarity',
                'y':'Rural Score',
                'data':XY,
                'stat_func':spearmanr
               }
    plot = sns.jointplot(**plotOps)
    plot.savefig('analysis.png')

# HELPER FUNCTIONS
def rscore(city_loc,station_loc):
    return(np.min(list(map(lambda x: vincenty(x,station_loc).miles,city_loc))))

def latitude(row,zone,E_off,N_off):
    EastNorth = (row['Easting']+E_off, row['Northing']+N_off)
    latlon = utm.to_latlon(*EastNorth, zone, northern=True)
    return(round(latlon[0],4))

def longitude(row,zone,E_off,N_off):
    EastNorth = (row['Easting']+E_off, row['Northing']+N_off)
    latlon = utm.to_latlon(*EastNorth, zone, northern=True)
    return(round(latlon[1],4))

if __name__ == '__main__':
    main()
