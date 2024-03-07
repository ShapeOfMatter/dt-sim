from argparse import (ArgumentParser, FileType)
from dataclasses import (fields)

# https://stackoverflow.com/a/3414096/10135377
import sys
import os
path = os.path.dirname(sys.modules[__name__].__file__)
sys.path.insert(0, os.path.join(path, '..'))

from cho_builder import gmw
from cho_builder import beaver
from cho_builder.core import ImplementationDetails, GateGenerators



argp = ArgumentParser(description="Implement a Bristol Fashion circuit as a .cho protocol.")
argp.add_argument("implementation", type=str, choices=["gmw", "beaver"], help="How to implement the circuit.")
argp.add_argument("input_file", type=FileType('r', 1, encoding='utf_8', errors='strict'))
argp.add_argument("output_file", type=FileType('w', 1, encoding='utf_8', errors='strict'))
for f in fields(ImplementationDetails):
    argp.add_argument(f"--{f.name}", type=f.type, default=f.default, **{k:v for k,v in f.metadata.items() if k in ("help", "default", "choices")})
args = argp.parse_args()
config = ImplementationDetails.from_args(args)
main = {"gmw": gmw.main,
        "beaver": beaver.main
        }[args.implementation]
main(args.input_file, args.output_file, config)

