import os
import sys
from datetime import datetime, timedelta
from time import sleep
from requests import Session

CYAN = "\x1b[36m"
RED = "\x1b[31m"
YELLOW = "\x1b[33m"
RESET = "\x1b[0m"

AOC_BASE_URL = "https://adventofcode.com/%(year)s/day/%(day)s/input"

date = datetime.utcnow()
YEAR = date.year
MONTH = date.month
DAY = date.day

START_TIME = datetime(YEAR, 12, DAY, 6, 0, 1, 0, datetime.utcnow().tzinfo)
TIME_OFFSET = timedelta(0, 20, 0, 0, 0, 0, 0)

print(RESET + f"Today: {DAY}.{MONTH}.{YEAR}") 
if START_TIME - date <= TIME_OFFSET and (START_TIME - date).seconds > 0:
    print(CYAN + "INFO: Waiting for start...")
    while START_TIME - date <= TIME_OFFSET and (START_TIME - date).seconds > 0:
        print("\r" + CYAN + "INFO: Only " + str((START_TIME - date).seconds) + "s left" + RESET, end="")
        date = datetime.utcnow()
        sleep(.9)
    print()

    
if MONTH != 12:
    print(RED + "ERROR: Not december yet" + RESET)
    raise SystemExit(1)
elif START_TIME - date >= TIME_OFFSET:
    print(RED + "ERROR: Assignment opens at 6am" + RESET)
    raise SystemExit(1)

DAY_REPR = str(DAY).zfill(2)
FOLDER_NAME = "day" + DAY_REPR

if not os.access(FOLDER_NAME,  os.R_OK | os.W_OK):
    print(CYAN + "INFO: Creating folder %r" % FOLDER_NAME)
    os.mkdir(FOLDER_NAME)
else:
    print(CYAN + "INFO: Folder alreay exists")
    
args = sys.argv[1:]
filetype = ""   # filetype needs to be extension
if "h" in args or "haskell" in args or "hs" in args:
    filetype = "hs"
else:
    filetype = "py"
    
FILENAME = os.path.join(FOLDER_NAME, "day" + DAY_REPR + "." + filetype)
DATA_FILE = os.path.join(FOLDER_NAME, "data_" + FOLDER_NAME.lower() + ".txt")
if not os.access(FILENAME, os.R_OK | os.W_OK) or "-f" in args:
    print(CYAN + "INFO: Created file for solution...")
    has_templ = False
    try:
        with open(filetype + ".templ", "r") as tf:
            template = tf.read().replace("$$FILE$$", DATA_FILE.replace("\\", "\\\\"))
    except FileNotFoundError:
        print(YELLOW + "WARNING: No template for file type %r found" % filetype)
    else:
        has_templ = True

    with open(FILENAME, "w") as f:
        if has_templ:
            f.write(template)  # type: ignore

elif "-f" not in args:
    print(YELLOW + "WARNING: File already exists. Use -f override")
    
print(CYAN + "INFO: Fetching todays puzzle input...")
day_url = AOC_BASE_URL % {"year": YEAR, "day": DAY}

SESSION_FILE = ".session_id"
try:
    with open(SESSION_FILE) as sess_file:
        sess_id =  sess_file.read().strip()
        
except FileNotFoundError:
    print(RED + "ERROR: Could not read session id")
    print(RED + "ERROR: Is the file %r present?" % SESSION_FILE)
    print(RED + "ERROR: If creating, don't forget to put the file into your .gitignore" + RESET)
    raise SystemExit(1)


print(CYAN + "INFO: Trying to access URL %r..." % day_url)
with Session() as s:
    s.cookies.update({"session": sess_id})
    response = s.get(day_url)
    if not response.ok:
        print(YELLOW + "WARNING: Server responded with %s" % response.status_code + RESET)
        raise SystemExit(1)
    
    data = response.text

print(CYAN + "INFO: Creating data file...")

if not os.access(DATA_FILE, os.W_OK | os.R_OK) or "-d" in args:
    with open(DATA_FILE, "w") as f:
        f.write(data)

elif not "-d" in args:
    print(YELLOW + "WARNING: Data file already exists")
    print("WARNING: Use -d to override")
    
print(CYAN + "INFO: Done" + RESET)
