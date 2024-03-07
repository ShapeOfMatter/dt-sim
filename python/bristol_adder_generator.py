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

def gen_xor(a, b):
    out = next_wire()
    emit(f'2 1 {a} {b} {out} XOR')
    return out

def gen_and(a, b):
    out = next_wire()
    emit(f'2 1 {a} {b} {out} AND')
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

    return outs, carry

xs = [next_wire() for i in range(BITS)]
ys = [next_wire() for i in range(BITS)]

outs, carry = gen_adder(xs, ys)

output_wires = []
for outv in outs:
    w = next_wire()
    emit(f'1 1 {outv} {w} EQW')
    output_wires.append(w)

# w = next_wire()
# output_wires.append(w)
# emit(f'1 1 {carry} {w} EQW')


num_gates = f.getvalue().count('\n')

f2 = open(f"adder_{BITS}.txt", "w")
f2.write(f'{num_gates} {current_wire}\n')
f2.write(f'2 {len(xs)} {len(ys)}\n')
f2.write(f'1 {len(output_wires)}\n\n')
f2.write(f.getvalue())
f2.close()
