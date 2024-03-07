from argparse import (ArgumentParser, FileType)
from joblib import Parallel, delayed
import numpy as np
import pandas as pd
from scipy.stats import (
        #ttest_ind,
        #ttest_rel,
        wilcoxon
        )
from sklearn import tree
from sklearn.multioutput import ClassifierChain
from sys import (exit, stderr)
from warnings import catch_warnings, filterwarnings

argp = ArgumentParser(description="Run the decision tree test on a csv of data.")
#argp.add_argument("file", type=FileType('r', 1, encoding='utf_8', errors='strict'))
argp.add_argument('file', type=FileType('r', 1, encoding='utf_8', errors='strict'), nargs='+')
argp.add_argument("iterations", type=int)
argp.add_argument("trainingN", type=int)
argp.add_argument("testingN", type=int)
argp.add_argument("--pad", "-p", action="store_true", help="Pad the ideal-world views with random noise to match the dimensions of the real-world views.")
args = argp.parse_args()

NUM_ITERS = args.iterations
NUM_TRAIN = args.trainingN
PER_ITER = NUM_TRAIN + args.testingN
PAD_VIEWS = args.pad

try:
    with open("./cores") as cores_file:
        CORES = int(cores_file.read())
except FileNotFoundError:
    exit(f"{__file__} was unable to find a file named \"./cores\" to tell it how many machine cores to use.")
except ValueError:
    exit(f"{__file__} was unable to parse \"./cores\" as an integer.")

df = pd.concat([pd.read_csv(f) for f in args.file])

if len(df) != NUM_ITERS * PER_ITER:
    print(f"Using {PER_ITER} rows for {NUM_ITERS} iterations, but there are {len(df)} rows!",
          file=stderr)

ideal_cols = []
view_cols = []
honest_cols = []

for c in df.columns:
    if c.startswith('i_'): ideal_cols.append(c)
    elif c.startswith('v_'): view_cols.append(c)
    elif c.startswith('h_'): honest_cols.append(c)
    else: raise Exception('unknown column type', c)

features1_raw = df[ideal_cols].to_numpy()
features2 = df[ideal_cols + view_cols].to_numpy()

features1 = np.hstack([ features1_raw,
                        np.random.randint(0, 2, (features1_raw.shape[0], features2.shape[1] - features1_raw.shape[1]))
                     ]) if PAD_VIEWS else features1_raw

labels = df[honest_cols].to_numpy()

def run_iter(features1, features2, labels):
    features1_train = features1[:NUM_TRAIN]
    features1_test = features1[NUM_TRAIN:PER_ITER]
    features2_train = features2[:NUM_TRAIN]
    features2_test = features2[NUM_TRAIN:PER_ITER]
    labels_train = labels[:NUM_TRAIN]
    labels_test = labels[NUM_TRAIN:PER_ITER]

    model1 = ClassifierChain(tree.DecisionTreeClassifier(), order='random')
    model1.fit(features1_train, labels_train)

    model2 = ClassifierChain(tree.DecisionTreeClassifier(), order='random')
    model2.fit(features2_train, labels_train)

    p1 = model1.predict(features1_test)
    p2 = model2.predict(features2_test)
    score1 = np.linalg.norm(p1 - labels_test, ord=1, axis=1).sum() / len(p1)
    score2 = np.linalg.norm(p2 - labels_test, ord=1, axis=1).sum() / len(p1)

    epsilon = 1e-10

    return (score1+epsilon, score2+epsilon)

try:
    results = Parallel(n_jobs=CORES)(delayed(run_iter)(f1, f2, l)
                                 for f1, f2, l
                                 in zip(np.array_split(features1, NUM_ITERS),
                                        np.array_split(features2, NUM_ITERS),
                                        np.array_split(labels, NUM_ITERS)))
except Exception as e:
    print(df[:10])
    raise e

r = np.array(results)

def statistical_test(a, b) -> float:
    with catch_warnings():
        # If your testingN is less than 10, you're doing it wrong anyway...
        filterwarnings("ignore", message="Sample size too small for normal approximation", category=UserWarning)
        try:
            return wilcoxon(a, b,
                            alternative='greater',
                            zero_method='wilcox',
                            method='approx',
                            correction=True
                            ).pvalue
        except ValueError as ve:
            message, *_ = ve.args
            if message == "zero_method 'wilcox' and 'pratt' do not work if x - y is zero for all elements.":
                return 0.5  # If all the differences are zero, then we have zero evidence of insecurity.
            else:
                raise ve

print(statistical_test(r[:,0], r[:,1]))

