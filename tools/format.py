#!/usr/bin/env python3

import glob
import os


valid_dirs = ["src", "tests", "app"]
files = []
for d in valid_dirs:
    os.system("fourmolu -i {0}".format(d))
    files += glob.glob(d + "/**/*.hs", recursive=True)

for f in files:
    os.system("stylish-haskell -i {0}".format(f))
