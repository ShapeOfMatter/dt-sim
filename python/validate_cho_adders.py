## This is a dump of my repl history; i've made no effort to clean it up. If you need to use it for anything, first re-work it into a working script.

import csv
with open("gitignore/adder17gmw0000.views.csv", newline='') as f:
  data = list(csv.DictReader(f))
len(data)
data[0].keys()
from pprint import pp
pp(data[0].keys())
pp(list(data[0].keys()))
def bin2dec(bs):
  return sum(b * (2**i) for (i, b) in enumerate(bs))
def dict2tuple(row):
honest_secret_keys = sorted(k for k in data.keys() if k.startswith('h_'))
honest_secret_keys = sorted(k for k in data[0].keys() if k.startswith('h_'))
honest_secret_keys
len(honest_secret_keys
)
corrupt_secret_keys = sorted(k for k in data[0].keys() if k.startswith('i_x'))
len(corrupt_secret_keys)
corrupt_secret_keys
def keynum(k):
  return int(k.strip('hiv_xyrz'))
corrupt_secret_keys = sorted(k for k in data[0].keys() if k.startswith('i_x'), key=keynum)
corrupt_secret_keys = sorted((k for k in data[0].keys() if k.startswith('i_x')), key=keynum)
len(corrupt_secret_keys)
corrupt_secret_keys
answer_secret_keys = sorted((k for k in data[0].keys() if k.startswith('i_r')), key=keynum)
len(answer_secret_keys)
answer_secret_keys
def row2tuple(row):
from dataclasses import dataclass
@dataclass(frozen=true)
class Datum
@dataclass(frozen=true)
class Datum:
  x: int
  y: int
  r: int
@dataclass(frozen=True)
class Datum:
  x: int
  y: int
  r: int
def row2datum(row):
  return Datum(x=bin2dec(row[k] for k in corrupt_secret_keys), y=bin2dec(row[k] for k in honest_secret_keys), r=bin2dec(row[k] for k in answer_secret_ke
ys))
row2datum(data[0])
def bin2dec(bs):
  return sum(int(b) * (2**i) for (i, b) in enumerate(bs))
def bin2dec(bs):
row2datum(data[0])
def correct(d):
  return d.r == d.x + d.y
correct(row2datum(data[0]))
example = row2datum(data[0])
example
example.x+example.y
def row2datum(row):
  return Datum(x=bin2dec(row[k] for k in reversed(corrupt_secret_keys)), y=bin2dec(row[k] for k in reversed(honest_secret_keys)), r=bin2dec(row[k] for k
 in answer_secret_keys))
example = row2datum(data[0])
example
example.x+example.y
example
print(example)
correct(example)
data2 = [row2datum(r) for r in data]
failing = [d for d in data2 if not correct(d)]
failing
import readline
for i in range(readline.get_current_history_length()):
  print (readline.get_history_item(i + 1))
