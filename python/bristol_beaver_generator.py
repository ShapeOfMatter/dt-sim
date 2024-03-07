import sys
from io import StringIO

assert len(sys.argv) == 2

NUM_TRIPLES = int(sys.argv[1])

f = StringIO()

current_wire = 0
def next_wire():
    global current_wire
    w = current_wire
    current_wire += 1
    return w

def emit(s=''):
    f.write(s + '\n')

def gen_xor(a, b):
    out = next_wire()
    emit(f'2 1 {a} {b} {out} XOR')
    return out

def gen_and(a, b):
    out = next_wire()
    emit(f'2 1 {a} {b} {out} AND')
    return out


xs = [next_wire() for i in range(NUM_TRIPLES)]
ys = [next_wire() for i in range(NUM_TRIPLES)]

output_wires = [gen_and(x, y) for x, y in zip(xs, ys)]

# w = next_wire()
# output_wires.append(w)
# emit(f'1 1 {carry} {w} EQW')


num_gates = f.getvalue().count('\n')

f2 = open(f"beaver_triple_gen_{NUM_TRIPLES}.txt", "w")
f2.write(f'{num_gates} {current_wire}\n')
f2.write(f'2 {len(xs)} {len(ys)}\n')
f2.write(f'1 {len(output_wires)}\n\n')
f2.write(f.getvalue())
f2.close()
