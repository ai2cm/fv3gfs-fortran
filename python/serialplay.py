import sys
import numpy as np
import os

# On MacOS, remember to set the environment variable DYLD_LIBRARY_PATH to contain
# the path to the SerialBox /lib directory

os.environ["DYLD_LIBRARY_PATH"]="/Users/apauling/Documents/work/fv3gfs-fortran-orig/serialbox/"

SERIALBOX_DIR = "/Users/ckung/Documents/Code/serialbox/install"
sys.path.append(SERIALBOX_DIR + "/python")
import serialbox as ser