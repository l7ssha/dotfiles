from collections import deque
import os
import sys

def fix_line(filepath):
    try:
        with open(filepath, "r+") as f:
            last_line = deque(f, 1)
            if last_line[0][-1] != "\n":
                f.write("\n")
    except:
        pass

def check_dir(path):
    if ".git" in path:
        return

    for entry in os.scandir(path):
        if entry.is_file():
            fix_line(entry.path)
        elif entry.is_dir():
            check_dir(entry.path)

check_dir(sys.argv[1])
