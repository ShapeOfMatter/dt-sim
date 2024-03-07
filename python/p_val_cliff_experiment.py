from subprocess import check_output, Popen, PIPE
from datetime import datetime
import time

cho_names = ['a306','a364','a399','a466','a504','a562','135','a214',
             'a234','a299','a359','a36','a454','a488','a512','a94']

data_size_grid = [2**e for e in range(7, 14)]
#data_size_grid = [2**e for e in range(7, 10)]
iter_grid = range(10, 55, 5)
#iter_grid = range(5, 25, 5)
trials = 3

for cho_name in cho_names:
    now = datetime.now()
 
    dt_string = now.strftime("%d-%m-%Y_%H:%M:%S")
    filename = f'results/cliff_results_{cho_name}_{dt_string}.csv'
    with open(filename, 'w') as f:
        f.write('iters, train_size, test_size, pval, time\n')

    cho_file = f'examples/search/{cho_name}.cho'

    for train_size in data_size_grid:
        for test_size in data_size_grid:
            for iters in iter_grid:
                for _ in range(trials):
                    #print(f'Running {iters}, {train_size}, {test_size}')
                    start_time = time.time()

                    data_gen = Popen(['cabal', 'exec', 'd-tree-data', '--',
                                    str(iters), str(train_size), str(test_size),
                                    '-f', cho_file],
                                    stdout=PIPE)
                    dtree = Popen(['python', 'python/d-tree-csv.py', '-',
                                str(iters), str(train_size), str(test_size)],
                                stdin=data_gen.stdout, stdout=PIPE)
                    data_gen.stdout.close()
                    out,err = dtree.communicate()
                    pval = out.decode('utf-8').strip()
                    total_time = time.time() - start_time

                    result = f'{iters}, {train_size}, {test_size}, {pval}, {total_time}'
                    print(result)

                    with open(filename, 'a') as f:
                        f.write(result + '\n')
