#!/usr/bin/env python
import requests
import json


# global variable initialization
metadata_server = "http://169.254.169.254/latest/"
#metadata_server = "http://localhost:3000/latest/"
path_dict = {"meta-data/" : ''}
temp_dict = {}

# function that returns a key and a list of value/values from the metadata server
def api_gen(key, url):
    value = requests.get(url).text
    path_list = [x for x in value.splitlines()]
    parent_key = key
    return parent_key, path_list


# recursive function that goes through each folder level in the metadata api
def met_gen(path_list, url):
    for p in path_list:
        if p[-1] != '/':
           api_call = api_gen(p, url+p)
           update(path_dict, api_call[0], api_call[1][0])
        else:
           api_call = api_gen(p, url+p)
           temp_dict = { k:[] for k in api_call[1]}
           update(path_dict, api_call[0], temp_dict)
           met_gen(api_call[1], url+api_call[0])
    return

# recursive function that finds a key in a nested dictionary and updates its value
def update(dic, key, value):
    for k,v in dic.items():
        if k == key:
            dic[k] = value
        elif isinstance(v, dict):
            update(dic.get(k), key, value)
        elif not k in dic.keys():
            dic.update({ key : value })
    return

#  initializing path_list and calling met_gen function
path_list = ["meta-data/"]
met_gen(path_list, metadata_server)

# Deleting sensitive information from the dictionary
del path_dict["meta-data/"]["identity-credentials/"]
del path_dict["meta-data/"]["public-keys/"]

json_data = json.dumps(path_dict, indent=4)
print(json_data)
