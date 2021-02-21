import requests
import numpy as np
import pandas as pd
import herepy
# store your herepy API key in a file "config.py" as api_key = "xyz"
import config
import json
import re

# tell the API to use your key
geocoderApi = herepy.GeocoderApi(config.api_key)

# read in data to be geocoded (in this case voter data)
voters = pd.read_csv("voter_list.csv")

# make addresses a list
addresses = voters['Address'].tolist()
# remove line breaks within addresses
addresses = [re.sub('\\n',', ',a) for a in addresses]

# run the geocoder on addresses, currently set to only run on 10 addresses
geocode_responses = []
#for i in range(0,len(addresses):
for i in range(0,10):
    response = geocoderApi.free_form(addresses[i])
    geocode_responses.append(response.as_dict())

# dump these responses to a json file so you won't have to rerun the geocoder api if you need more info from them
with open(r'geocode_api_responses.json', 'w') as f:
    json.dump(geocode_responses, f)

# code to read it back in, in case you need it later
with open(r"geocode_api_responses.json", "r") as read_file:
    data = json.load(read_file)


#create blank lists to store results, also store the address as the geocoder recognized it
#in case you need to diagnose errors later
lng_list = []
lat_list = []
recognized_addresses = []
# parse geocode responses. This retrieves 
for x in geocode_responses:
    # extract the lat and lon, and append them to the list
    try:
        lng_list.append(x['items'][0]['position']['lng'])
        lat_list.append(x['items'][0]['position']['lat'])
        recognized_addresses.append(x['items'][0]['address']['label'])
        
    except:
        # if the geocoder did not return a result, just append 0
        lng_list.append(0)
        lat_list.append(0)
        recognized_addresses.append('N/A')

# add these lists as columns to your original dataset
voters['lon'] = lng_list
voters['lat'] = lat_list
voters['geocoder_recognized_address'] = recognized_addresses

# export updated voters file
voters.to_csv("geocoded_voter_data.csv")