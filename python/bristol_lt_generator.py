import sys
from io import StringIO

assert len(sys.argv) == 2

BITS = int(sys.argv[1])

f = StringIO()

current_wire = 0
def next_wire():
    global current_wire
    w = current_wire
    current_wire += 1
    return w

def emit(s=''):
    f.write(s + '\n')

def gen_not(a):
    out = next_wire()
    emit(f'1 1 {a} {out} INV')
    return out
    
def gen_xor(a, b):
    out = next_wire()
    emit(f'2 1 {a} {b} {out} XOR')
    return out

def gen_or(a, b):
    out1 = next_wire()
    out2 = next_wire()
    out = next_wire()
    emit(f'2 1 {a} {b} {out1} AND')
    emit(f'2 1 {a} {b} {out2} XOR')
    emit(f'2 1 {out1} {out2} {out} XOR')
    return out

def gen_and(a, b):
    out = next_wire()
    emit(f'2 1 {a} {b} {out} AND')
    return out

def gen_less_than(a_bits, b_bits):
    a_greater = None
    b_greater = None

    for a, b in zip(a_bits, b_bits):
        if a_greater is None:
            assert b_greater == None
            new_a_greater = gen_and(a, gen_not(b))
            new_b_greater = gen_and(b, gen_not(a))
        else:
            new_a_greater = gen_or(a_greater, gen_and(a, gen_and(gen_not(b), gen_not(b_greater))))
            new_b_greater = gen_or(b_greater, gen_and(b, gen_and(gen_not(a), gen_not(a_greater))))
        a_greater = new_a_greater
        b_greater = new_b_greater
    return a_greater


xs = [next_wire() for i in range(BITS)]
ys = [next_wire() for i in range(BITS)]

out = gen_less_than(xs, ys)

w = next_wire()
emit(f'1 1 {out} {w} EQW')
output_wires = [w]

num_gates = f.getvalue().count('\n')

f2 = open(f"less_than_{BITS}.txt", "w")
f2.write(f'{num_gates} {current_wire}\n')
f2.write(f'2 {len(xs)} {len(ys)}\n')
f2.write(f'1 {len(output_wires)}\n\n')
f2.write(f.getvalue())
f2.close()
