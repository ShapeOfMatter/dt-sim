from .circuit_generator import gensym, emit, gen_circuit, gen_randomness
import random
from .core import GateGenerators


def gen_xor(a, b):
    out = gensym('g')
    emit(f'{out}_1 = {a}_1 + {b}_1')
    emit(f'{out}_2 = {a}_2 + {b}_2')
    return out

def gen_inv(a):
    out = gensym('g')
    emit(f'{out}_1 = ~{a}_1')
    emit(f'{out}_2 = {a}_2')
    return out

def gen_and(a, b):
    out = gensym('g')
    emit(f'DO and_gmw(P1({a}_1, {b}_1), P2({a}_2, {b}_2)) GET({out}_1=out1, {out}_2=out2)')
    return out

generators = GateGenerators(and_gate=gen_and, xor_gate=gen_xor, inv_gate=gen_inv)

share_macro = """
MACRO secret_share(P1(x), P2()) AS
  {share_randomness_defs}
  s1 = {share_randomness_var}
  s2 = x + s1
  SEND s2 TO P2
ENDMACRO
"""

and_macro = """
MACRO and_gmw(P1(x2, y2), P2(x1, y1)) AS
  {and_randomness_defs}
  out2 = {and_randomness_var}
  {and_leakage_defs}
  leakage = out2 ^(~({and_leakage_var}))
  SEND leakage TO P1
  g1_s2_00 = out2 + ((x1 + 0) ^ (y1 + 0))
  g1_s2_01 = out2 + ((x1 + 0) ^ (y1 + 1))
  g1_s2_10 = out2 + ((x1 + 1) ^ (y1 + 0))
  g1_s2_11 = out2 + ((x1 + 1) ^ (y1 + 1))
  out1 = OBLIVIOUSLY [[g1_s2_00, g1_s2_01]?y2, [g1_s2_10, g1_s2_11]?y2]?x2 FOR P1
ENDMACRO
"""

reveal_macro = """
MACRO reveal(P1(x1), P2(x2)) AS
  SEND x1 TO P2
  SEND x2 TO P1
  y = x1 + x2
ENDMACRO
"""

header_comment = """
{{-
A GMW implementation of {input_file} using oblivious transfer.
Broken acording to: {config}
-}}
"""

def main(input_file, output_file, config):
    share_randomness_var, share_randomness_defs = gen_randomness(config.bias_sharing, 'P1')
    and_randomness_var, and_randomness_defs = gen_randomness(config.bias_and, 'P2')
    and_leakage_var, and_leakage_defs = gen_randomness(config.accidental_gate, 'P2', baseline=0)

    header = "\n".join((header_comment.format(input_file=input_file, config=config),
                        share_macro.format(share_randomness_var=share_randomness_var,
                                           share_randomness_defs=share_randomness_defs),
                        and_macro.format(and_randomness_var=and_randomness_var,
                                         and_randomness_defs=and_randomness_defs,
                                         and_leakage_var=and_leakage_var,
                                         and_leakage_defs=and_leakage_defs),
                        reveal_macro.format()))

    gen_circuit(config,
                generators,
                header,
                input_file,
                output_file)
