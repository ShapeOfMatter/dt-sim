from dataclasses import (dataclass, field, fields)
from typing import Callable

output_options = {
    "values": "(the default) causes the .cho to reveal and output the circuit's final values normally.",
    "shares": "will cause the .cho to `OUPTUT` the wire shares that would be revealed to form the final values.",
    "none":   "will skip all reveal and output stages."
}

suppress_output_help = "\n".join(("What the .cho should do with the terminal wires of the circuit.",
                                  *(f'"{k}" {v}' for k,v in output_options.items())))

@dataclass(frozen=True, order=True)
class ImplementationDetails:
    bias_sharing: int = field(default=0, metadata={'help': "Bias randomness used in secret sharing",
                                                   'defaults': [0,1,2,3]})
    bias_and: int = field(default=0, metadata={'help': "Bias randomness used for AND gates",
                                               'defaults': [0,1,2,3]})
    accidental_secret: int = field(default=0, metadata={'help': "Rate of accidentally sending secret inputs to corrupt party",
                                                        'defaults': [0,1,2,3]})
    accidental_gate: int = field(default=0, metadata={'help': "Rate of accidentally sending shares of and-gate outputs to corrupt party",
                                                      'defaults': [0,1,2,3]})
    outputs: str = field(default="values", metadata={'help': suppress_output_help,
                                                     'defaults': ["values"],
                                                     'choices': output_options.keys()})

    @classmethod
    def from_dict(cls, d):
        return cls(**{f.name: f.type(d[f.name]) for f in fields(cls)})
    @classmethod
    def from_args(cls, args):
        return cls.from_dict(vars(args))
    def as_csv_data(self):
        return f"{self.bias_sharing},{self.bias_and},{self.accidental_secret},{self.accidental_gate},{self.outputs}"
    def short(self):
        return ",".join(f"{f.name}={getattr(self, f.name)}"
                        for f in fields(self)
                        if getattr(self, f.name) != f.default
                       ) or "normal settings"

@dataclass(frozen=True)
class GateGenerators:
    and_gate: Callable[[str, str], str]
    xor_gate: Callable[[str, str], str]
    inv_gate: Callable[[str], str]
