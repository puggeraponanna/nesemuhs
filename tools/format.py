#!/usr/bin/env python3

import glob
import os


valid_dirs = ["src", "tests", "app"]
files = []
for d in valid_dirs:
    print("Formatting directory: {0}".format(d))
    os.system("fourmolu -i {0}".format(d))
    files += glob.glob(d + "/**/*.hs", recursive=True)

for f in files:
    print("Styling file: {0}".format(f))
    os.system("stylish-haskell -i {0}".format(f))
