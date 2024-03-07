from argparse import (ArgumentParser, FileType)
from collections import Counter
from csv import DictReader
from dataclasses import dataclass, fields, replace
from datetime import datetime
from itertools import chain, product
import math
import os
from pprint import PrettyPrinter
import subprocess
import sys

from cho_builder.core import ImplementationDetails


launch_time = datetime.now()


help_message = """Build various circuits and test them at various powers.
Note the behavior of the breakage arguments: They have defaults, some of which are lists.
If one of them ends up with only a single value, then that one value will be used for all experiments, but lists will not get cross-product-ed.
"""

argp = ArgumentParser(description=help_message)
# We don't use `FileType` arguments because we don't want these handles open all the time.
argp.add_argument("--iters", "-i", type=int, action="extend", nargs="+", help="Values of \"iterations\" at which to test the circuits.")
argp.add_argument("--trains", "-t", type=int, action="extend", nargs="+", help="Values of \"training N\" at which to test the circuits.")
argp.add_argument("--testing-ratio", type=int, default=2,
                  help="Circuits will be tested with \"testing N\" set to t/testing-ratio for each t in --trains.")
argp.add_argument("--circuits", "-c", type=str, action="extend", nargs="+", help="Circuit files to parse into CHO and then test."
                                                                                 +" A \"circuits/\" prefix will be added!")
argp.add_argument("--protocols", "-p", type=str, action="extend", nargs="+", help="The protocols with which to implement each circuit.")
argp.add_argument('--output_file', "-o", type=str, default="results/circuit_results_{t}.csv",
                  help="Path to the CSV file to write data to. Will be overwritten if it already exists."
                       +" The pattern \"\{t\}\" will be replaced with the current time.")
argp.add_argument("--repititions", "-R", type=int, default=3, help="How many times to repeat each test on each circuit.")
argp.add_argument("--unpad", action="store_false", help="Don't pad the ideal-world views with random noise to match the dimensions of the real-world views.")
argp.add_argument("--temp-dir", type=str, default=".", help="A temp dir in which to stash data from child processes.")
argp.add_argument("--pre-existing", "-P", type=str, action="extend", nargs="+", default=[],
                  help="CSV files containing data from prior runs, which can be skipped.")
argp.add_argument("--log", type=FileType('w', 1, encoding='utf_8', errors='strict'), default="-", help="An extra log file to use instead of STD_OUT.")
for f in fields(ImplementationDetails):
    argp.add_argument(f"--{f.name}", action="extend", nargs="+", type=f.type, **{k:v for k,v in f.metadata.items() if k in ("help", "choices")})
args = argp.parse_args()

log = PrettyPrinter(width=120, stream=args.log).pprint
log(args)
iters = args.iters or [64, 128]
trains = args.trains or [2**i for i in range(7, 10)]
testing_ratio = args.testing_ratio
circuit_names = args.circuits or ['adder_1.txt',
                                  'adder_2.txt',
                                  'adder_3.txt',
                                  'adder_4.txt',
                                  'adder_5.txt',
                                  'adder_6.txt',
                                  'adder_7.txt',
                                  'adder_8.txt',
                                  'adder_9.txt',
                                  'adder_10.txt',
                                  'adder_11.txt',
                                  'adder_12.txt',
                                  'adder_13.txt',
                                  'adder_14.txt',
                                  'adder_15.txt',
                                  'adder_16.txt',
                                  'adder_17.txt',
                                  'adder_18.txt',
                                  'adder_19.txt',
                                  'adder_20.txt',
                                  'adder_21.txt',
                                  'adder_22.txt',
                                  'adder_23.txt',
                                  'adder_24.txt',
                                  'adder_25.txt',
                                  'adder_26.txt',
                                  'adder_27.txt',
                                  'adder_28.txt',
                                  'adder_29.txt',
                                  'adder_30.txt',
                                  'adder_31.txt',
                                  'adder_32.txt',
                                  'adder64.txt',
                                  'mult64.txt',
                                  'aes_128.txt',
                                  'divide64.txt',
                                  'FP-add.txt',
                                  'FP-mul.txt',
                                  'sha256.txt',
                                  'FP-div.txt']
protocol_types = args.protocols or ['gmw', 'beaver']
filename = args.output_file.format(t=launch_time.strftime("%d-%m-%Y_%H:%M:%S"))
TRIALS = args.repititions
pad_views = args.unpad
temp_dir = args.temp_dir
cho_file = os.path.join(temp_dir, 'tmp.cho')
config_choices = {f.name: vars(args)[f.name] or f.metadata['defaults']
                  for f in fields(ImplementationDetails)}
basic_config = ImplementationDetails(**{f: vs[0]
                                        for f,vs in config_choices.items()
                                        if 1 == len(vs)
                                        })
configs = sorted(set(
    replace(basic_config, **{f: v})
    for f,vs in config_choices.items()
    for v in vs
    if 1 != len(vs)
))
log((iters, trains, testing_ratio, pad_views, circuit_names, protocol_types, filename, TRIALS, cho_file, configs))


def scrape_prior(csv_name):
    with open(csv_name, newline='') as f:
        dr = DictReader(f)
        for row in dr:
            yield (row["circuit"],
                   row["protocol"],
                   int(row["iters"]),
                   int(row["train_size"]),
                   int(row["test_size"]),
                   ImplementationDetails.from_dict(row))

already_done = Counter(chain.from_iterable(scrape_prior(p) for p in args.pre_existing))
log(already_done)


try:
    with open("./cores") as cores_file:
        CORES = int(cores_file.read())
except FileNotFoundError:
    exit(f"{__file__} was unable to find a file named \"./cores\" to tell it how many machine cores to use.")
except ValueError:
    exit(f"{__file__} was unable to parse \"./cores\" as an integer.")
assert math.log(CORES, 2).is_integer(), "We trust the division of work across reasonable-powers-of-two cores;" \
                                          + f" other values (e.g. {CORES}) are likely to cause bugs."
log(f"CORES={CORES}")


def gen_circuit_file(circuit_file, cho_file, protocol_type, config):
    circuit_filename = 'circuits/' + circuit_file

    process = ['python',
               'python/cho_builder',
               protocol_type,
               circuit_filename,
               cho_file,
               '--bias_sharing', str(config.bias_sharing),
               '--bias_and', str(config.bias_and),
               '--accidental_secret', str(config.accidental_secret),
               '--accidental_gate', str(config.accidental_gate)]
    log("Generating CHO:")
    log(process)
    p1 = subprocess.Popen(process)
    returncode = p1.wait()
    if 0 == returncode:
        log("Finished generating CHO.")
        return True
    else:
        log(f"Failed to generate CHO. Exited with {1}.")
        return False

def run_experiment(cho_filename, iters, train, test, results_filename,
                   protocol_type, config, circuit_name):
    # generate data
    iters_per_core = int(iters) // CORES
    iters_per_core = str(iters_per_core)
    iters = str(iters)
    train = str(train)
    test = str(test)

    pids = []
    for i in range(CORES):
        data_csv = open(os.path.join(temp_dir, f'data{i}.csv'), "w")
        stderr_log = open(os.path.join(temp_dir, f'stderr{i}.log'), "w")

        p1 = subprocess.Popen(['/usr/bin/time', '-f', '\'%e\'', 'cabal', 'exec', 'd-tree-data', '--',
                            iters_per_core, train, test, '-f', cho_filename],
                            stdout=data_csv, stderr=stderr_log)
        pids.append((p1, data_csv, stderr_log))

    for pid, data_csv, stderr_log in pids:
        pid.wait()
        stderr_log.close()
        data_csv.close()

    def retrieve_time(i):
        with open(os.path.join(temp_dir, f'stderr{i}.log'), "r") as f:
            return float(f.read().replace('\'', ''))

    data_time = max(retrieve_time(i) for i in range(CORES))

    # run d-trees

    with (open(os.path.join(temp_dir, 'stdout.log'), "w") as stdout_log,
          open(os.path.join(temp_dir, 'stderr.log'), "w") as stderr_log):
        data_files = [os.path.join(temp_dir, f'data{i}.csv') for i in range(CORES)]
        proc_spec = ['/usr/bin/time', '-f', '\'%e\'', 'python', '--',
                     'python/d-tree-csv.py'] + data_files + \
                    [iters, train, test] + \
                    (["--pad"] if pad_views else [])
        p2 = subprocess.Popen(proc_spec,
                              stdout=stdout_log, stderr=stderr_log)
        p2.wait()

    with open(os.path.join(temp_dir, 'stdout.log'), "r") as f:
        pval = float(f.read().replace('\'', ''))

    with open(os.path.join(temp_dir, 'stderr.log'), "r") as f:
        d_tree_time = float(f.read().replace('\'', ''))

    with open(results_filename, 'a') as f:
        line = f'{circuit_name},{protocol_type},{iters},{train},{test},{pval},{data_time},{d_tree_time},{config.as_csv_data()}\n'
        log(line)
        f.write(line)


with open(filename, 'w') as f:
    f.write('circuit,protocol,iters,train_size,test_size,pval,data_time,d_tree_time,bias_sharing,bias_and,accidental_secret,accidental_gate,outputs\n')

for (circuit_name, protocol_type, c) in product(circuit_names, protocol_types, configs):
    log(f"Running {protocol_type} on {circuit_name} with {c.short()}.")
    tasks = [(circuit_name, protocol_type, i, train, int(train/testing_ratio), c)
             for (i, train) in product(iters, trains)]
    remaining_work = [(k, rep)
                      for k in tasks
                      for rep in range(already_done[k], TRIALS)]
    if 0 < len(remaining_work):
        wrote_temp_cho = gen_circuit_file(circuit_name, cho_file, protocol_type, c)
        if wrote_temp_cho:
            for ((_, _, i, train, testing, _), rep) in remaining_work:
                log((i, train, testing, rep))
                run_experiment(cho_file, i, train, testing, filename, protocol_type, c, circuit_name)
        else:
            print("Problem generating a CHO file!", file=sys.stderr)
    else:
        log("Skipping this CHO; no work to do.")

