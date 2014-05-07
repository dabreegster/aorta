#import os
import subprocess

ZONE = 'us-central1-a'

# Runs the command and returns the stdout
def run(argv):
  proc = subprocess.Popen(argv, stdout=subprocess.PIPE) # stderr=open(os.devnull, 'wb')
  return proc.communicate()[0]
