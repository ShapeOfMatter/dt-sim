from .circuit_generator import gensym, emit, gen_circuit, gen_randomness
import random
from .gmw import gen_xor, gen_inv, share_macro, reveal_macro

from .core import GateGenerators


def gen_and(a, b): # this could be merged back up to gmw, but it's fine here.
    out = gensym('g')
    emit(f'DO and_beaver(P1({a}_1, {b}_1), P2({a}_2, {b}_2)) GET({out}_1=out1, {out}_2=out2)')
    return out

generators = GateGenerators(and_gate=gen_and, xor_gate=gen_xor, inv_gate=gen_inv)

and_macro = """
MACRO and_beaver(P1(x2, y2), P2(x1, y1)) AS
  -- generate beaver triple
  {a1_defs}
  a1 = {a1}
  a2 = FLIP @P3
  b1 = FLIP @P3
  {b2_defs}
  b2 = {b2}
  c1 = FLIP @P3

  c2 = ((a1 + a2) ^ (b1 + b2)) + c1

  -- deal shares of beaver triple
  SEND a1 TO P1
  SEND b1 TO P1
  SEND c1 TO P1

  SEND a2 TO P2
  SEND b2 TO P2
  SEND c2 TO P2

  -- use beaver triple to evaluate the gate
  d1 = x2 + a1
  d2 = x1 + a2
  SEND d1 TO P2
  SEND d2 TO P1
  d = d1 + d2
  e1 = y2 + b1
  e2 = y1 + b2
  SEND e1 TO P2
  SEND e2 TO P1
  e = e1 + e2

  out1 = (d ^ e) + (d ^ b1) + (e ^ a1) + c1
  -- only one party does addition-w-constant.
  out2 =           (d ^ b2) + (e ^ a2) + c2

  {and_leakage_defs}
  leakage = out2 ^(~({and_leakage_var}))
  SEND leakage TO P1
ENDMACRO
"""

header_comment = """
{{-
A GMW implementation of {input_file} using beaver triples.
Broken acording to: {config}
-}}
"""

def main(input_file, output_file, config):
    share_randomness_var, share_randomness_defs = gen_randomness(config.bias_sharing, 'P1')
    # we only bias one of the dealer flips, to try to keep the scale of this break on par with the other implementation.
    a1, a1_defs = gen_randomness(config.bias_and, 'P3', globalizer='a')
    b2, b2_defs = gen_randomness(config.bias_and, 'P3', globalizer='b')
    and_leakage_var, and_leakage_defs = gen_randomness(config.accidental_gate, 'P2', baseline=0)

    header = "\n".join((header_comment.format(input_file=input_file, config=config),
                        share_macro.format(share_randomness_var=share_randomness_var,
                                           share_randomness_defs=share_randomness_defs),
                        and_macro.format(a1=a1,
                                         a1_defs=a1_defs,
                                         b2=b2,
                                         b2_defs=b2_defs,
                                         and_leakage_var=and_leakage_var,
                                         and_leakage_defs=and_leakage_defs),
                        reveal_macro.format()))

    gen_circuit(config, generators, header, input_file, output_file)
