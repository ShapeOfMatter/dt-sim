from argparse import (ArgumentParser, FileType)
from sys import stdout

argp = ArgumentParser(description="Generate a .cho file for a 2-party GMW adder n bits wide.")
argp.add_argument("n", type=int, help="the bit-width of the adder")
argp.add_argument("-t", "--target", nargs="?", type=FileType('w'), default=stdout,
                  help="the file to write to (default stdout)")
argp.add_argument("-p", "--parties", nargs=2, default=("P1", "P2"), type=str,
                  help="the names to use for the (exactly two) parties (default P1 and P2)")
args = argp.parse_args()

BITS = args.n
f = args.target
p1, p2 = args.parties


gn = 0
def gensym(x):
    global gn
    gn += 1
    return f'{x}{gn}'

def emit(s=''):
    f.write(s + '\n')

def gen_xor(a, b):
    out = gensym('g')
    emit(f'{out}_1 = {a}_1 + {b}_1')
    emit(f'{out}_2 = {a}_2 + {b}_2')
    return out

def gen_and(a, b):
    out = gensym('g')
    emit(f'DO and_gmw({p1}({a}_1, {b}_1), {p2}({a}_2, {b}_2)) GET({out}_1=out1, {out}_2=out2)')
    return out

def gen_adder(xs, ys):
    outs = []
    carry = None

    for a, b in zip(reversed(xs), reversed(ys)):
        # ab = a + b
        gab = gen_xor(a, b)

        # out = ab + carry % 2
        if carry is None:
            out = gab
        else:
            out = gen_xor(gab, carry)

        outs.append(out)
        
        # carry = ((a + carry) * (b + carry) + carry) % 2
        if carry is None:
            carry1 = a
            carry2 = b
        else:
            carry1 = gen_xor(a, carry)
            carry2 = gen_xor(b, carry)
        carry3 = gen_and(carry1, carry2)
        if carry is None:
            carry = carry3
        else:
            carry = gen_xor(carry3, carry)

    return outs

header = """
MACRO secret_share(P1(x), P2()) AS
  s1 = FLIP @P1
  s2 = x + s1
  SEND s1 TO P2
ENDMACRO

MACRO and_gmw(P1(x1, y1), P2(x2, y2)) AS
  out1 = FLIP @P1
  g1_s2_00 = out1 + ((x1 + 0) ^ (y1 + 0))
  g1_s2_01 = out1 + ((x1 + 0) ^ (y1 + 1))
  g1_s2_10 = out1 + ((x1 + 1) ^ (y1 + 0))
  g1_s2_11 = out1 + ((x1 + 1) ^ (y1 + 1))
  out2 = OBLIVIOUSLY [[g1_s2_00, g1_s2_01]?y2, [g1_s2_10, g1_s2_11]?y2]?x2 FOR P2
ENDMACRO

MACRO reveal(P1(x1), P2(x2)) AS
  SEND x1 TO P2
  SEND x2 TO P1
  y = x1 + x2
ENDMACRO
"""
emit(header)

xs = [f'x{i}' for i in range(BITS)]
ys = [f'y{i}' for i in range(BITS)]

emit('-- Read secrets')
for xi in xs:
    emit(f'{xi} = SECRET @{p1}')

emit()
for yi in ys:
    emit(f'{yi} = SECRET @{p2}')

emit()
emit('-- Set up shares')
for xi in xs:
    emit(f'DO secret_share({p1}({xi}), {p2}()) GET({xi}_1=s2, {xi}_2=s1)')

emit()
for yi in ys:
    emit(f'DO secret_share({p2}({yi}), {p1}()) GET({yi}_1=s1, {yi}_2=s2)')

emit()
emit('-- Adder circuit')
outs = gen_adder(xs, ys)

emit()
emit('-- Reveal output')
rs = [f'r{i}' for i in range(BITS)]
for o, r in zip(outs, rs):
    emit(f'DO reveal({p1}({o}_1), {p2}({o}_2)) GET({r}=y)')
    emit(f'OUTPUT {r}')

f.close()
