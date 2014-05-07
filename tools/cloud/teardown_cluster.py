#!/usr/bin/env python2
# Script to teardown entire cluster

from common import *
import json

# Return a list of instance names
def list_instances():
  result = json.loads(run(['gcutil', 'listinstances', '--format=json']))
  my_zone = result['items']['zones/' + ZONE]
  if 'instances' in my_zone:
    return map(lambda i: i['name'], my_zone['instances'])
  else:
    return []

ls = list_instances()
print 'Currently running:', ls
if ls:
  run(['gcutil', 'deleteinstance', '--force', '--zone=' + ZONE] + ls)
