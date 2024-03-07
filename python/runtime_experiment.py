import sys
import os
import time

pp = 200

with open('results.csv', 'w') as results_file:
    results_file.write('bits, sample gen time, tree training time\n')

    for bits_exp in range(8, 12):
        bits = 2**bits_exp
        print('==================================================')
        print(f'BITS: {bits}')
        print('==================================================')

        adder_filename = f'adder_{bits}.cho'
        os.system(f'python examples/adder_generator.py {bits}')

        start = time.time()
        os.system(f'cabal exec d-tree-data adder_{bits}.cho {pp} {pp} {pp} > samples.csv')
        end = time.time()
        time_samples = end - start

        start = time.time()
        os.system(f'python examples/d-tree-csv.py samples.csv {pp} {pp} {pp}')
        end = time.time()
        time_trees = end - start

        results_file.write(f'{bits}, {time_samples}, {time_trees}\n')
        os.system(f'rm {adder_filename}')
