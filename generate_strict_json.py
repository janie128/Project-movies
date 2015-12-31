"""
Raw data source for reviews is not in strict JSON format and cannot be parsed by jsonlite package in R.
This script converts to strict JSON format.
"""

import gzip
import json
import sys

if len(sys.argv) != 3:
  print("usage: python <py file> <input file> <output file>")
  sys.exit()

input = gzip.open(sys.argv[1], 'r')
output = open(sys.argv[2], 'w')

for line in input:
  processed = json.dumps(eval(line))
  output.write(processed + '\n')
