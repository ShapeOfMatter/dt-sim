import random
import sys

def gen_randomness(bias_level, party, baseline=1, globalizer=''):
    output = ""
    for i in range(bias_level + baseline):
        output += f'f{globalizer}_{i} = FLIP @{party}\n  '
    exp = ' ^ '.join([f'f{globalizer}_{i}' for i in range(bias_level + baseline)]) or ' 1 '
    # if baseline=bias_level=0, then you'll always use the value 1 instead of anding together the (nonexistant) flips.
    return exp, output

gn = 0  # TODO: remove global state!
def gensym(x):
    global gn
    gn += 1
    return f'{x}{gn}'

global_f = None  # TODO: remove global state!
def emit(s=''):
    global_f.write(s + '\n')

def gen_circuit(config, generators, header, circuit_file, output_file):
    global global_f

    global_f = output_file

    circuit_lines = circuit_file.readlines()

    num_gates, num_wires = [int(x) for x in circuit_lines[0].strip().split(' ')]
    inputs_line = circuit_lines[1].strip().split(' ')
    outputs_line = circuit_lines[2].strip().split(' ')

    input_bits = [int(x) for x in inputs_line[1:]]
    output_bits = [int(x) for x in outputs_line[1:]]

    input_wires = []
    current_wire = 0
    for bw in input_bits:
        input_wires.append(list(range(current_wire, current_wire+bw)))
        current_wire += bw

    assert len(input_wires) == 2
    assert len(output_bits) == 1

    gate_lines = circuit_lines[4:]


    emit(header)

    xs = [f'x{i}' for i in input_wires[0]]
    ys = [f'y{i}' for i in input_wires[1]]

    emit('-- Read secrets')
    for xi in xs:
        emit(f'{xi} = SECRET @P1')
        #emit(f'SEND {xi} TO P2')

    emit()
    for yi in ys:
        emit(f'{yi} = SECRET @P2')
        value, setup = gen_randomness(config.accidental_secret, 'P2', baseline=0, globalizer=gensym(""))
        emit(setup)
        emit(f'leak_{yi} = {yi} ^(~({value}))')
        emit(f'SEND leak_{yi} TO P1 -- accidental send secret to corrupt')

    emit()
    emit('-- Set up shares')
    for xi in xs:
        emit(f'DO secret_share(P1({xi}), P2()) GET({xi}_1=s1, {xi}_2=s2)')

    emit()
    for yi in ys:
        emit(f'DO secret_share(P2({yi}), P1()) GET({yi}_1=s2, {yi}_2=s1)')

    ##################################################
    # Gates
    ##################################################
    emit()
    emit('-- Circuit evaluation')

    wire_names = {}
    for x, i in zip(xs, input_wires[0]):
        wire_names[i] = x
    for y, i in zip(ys, input_wires[1]):
        wire_names[i] = y

    for gl in gate_lines:
        gate_params = gl.strip().split(' ')
        gate_type = gate_params[-1]

        if gate_type == 'INV':
            inputs, outputs, in1, out, t = gate_params
            out_name = generators.inv_gate(wire_names[int(in1)])
            wire_names[int(out)] = out_name

        elif gate_type == 'EQW':
            inputs, outputs, in1, out, t = gate_params
            in_name = wire_names[int(in1)]
            out_name = gensym('g')
            emit(f'{out_name}_1 = {in_name}_1')
            emit(f'{out_name}_2 = {in_name}_2')
            wire_names[int(out)] = out_name

        elif gate_type == 'XOR':
            inputs, outputs, in1, in2, out, t = gate_params
            out_name = generators.xor_gate(wire_names[int(in1)],
                                           wire_names[int(in2)])
            wire_names[int(out)] = out_name

        elif gate_type == 'AND':
            inputs, outputs, in1, in2, out, t = gate_params
            out_name = generators.and_gate(wire_names[int(in1)],
                                           wire_names[int(in2)])
            wire_names[int(out)] = out_name

        elif gate_type == '':
            pass
        else:
            raise Exception('unknown gate', gate_params)

    output_wires= list(sorted(wire_names.keys()))[-output_bits[0]:]
    output_names = [wire_names[k] for k in output_wires]

    emit()
    emit('-- Reveal output?')
    for r, o in enumerate(output_names):
        if 'values' == config.outputs:
            emit(f'DO reveal(P1({o}_1), P2({o}_2)) GET(r{r}=y)')
            emit(f'OUTPUT r{r}')
        elif 'shares' == config.outputs:
            emit(f'OUTPUT {o}_1')
            emit(f'OUTPUT {o}_2')
        elif 'none' == config.outputs:
            emit(f'-- ignoring output {o}.')
        else:
            raise Exception(f"Unknown config.", config)

    global_f.close()
