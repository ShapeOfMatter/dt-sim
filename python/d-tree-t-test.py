import numpy as np
from sklearn import tree
from sklearn.multioutput import ClassifierChain

def xor(a, b):
    return (a + b) % 2

def prot(c_in, h1_in, h2_in):
    n = len(c_in)

    c_s1 = np.random.randint(0, 2, size=n)
    c_s2 = np.random.randint(0, 2, size=n)
    c_s3 = xor(xor(c_s1, c_s2), c_in)

    h1_s1 = np.random.randint(0, 2, size=n)
    h1_s2 = np.random.randint(0, 2, size=n)
    h1_s3 = xor(xor(h1_s1, h1_s2), h1_in)

    h2_s1 = np.random.randint(0, 2, size=n)
    h2_s2 = np.random.randint(0, 2, size=n)

    # BUG #1: no randomness for party 2
    # h2_s1 = np.zeros(n)
    # h2_s2 = np.zeros(n)

    # BUG #2: biased randomness for party 2
    h2_s1 = np.random.choice([0,1], p=[0.45, 0.55], size=n)
    h2_s2 = np.random.choice([0,1], p=[0.45, 0.55], size=n)

    h2_s3 = xor(xor(h2_s1, h2_s2), h2_in)

    h1_sum = xor(xor(c_s1, h1_s1), h2_s1)
    h2_sum = xor(xor(c_s2, h1_s2), h2_s2)
    c_sum = xor(xor(c_s3, h1_s3), h2_s3)

    total_sum = xor(xor(c_sum, h1_sum), h2_sum)
    assert (total_sum == xor(xor(c_in, h1_in), h2_in)).all()
    corrupt_view = [c_in,           # corrupt input
                    c_s1, c_s2,     # corrupt random tape
                    c_s3,
                    h1_s3, h2_s3,   # messages received
                    h1_sum, h2_sum,
                    c_sum,
                    total_sum]  # calculated results
    return corrupt_view


def run_iter():
    ##################################################
    # training
    ##################################################

    n = 100000
    corrupt_inputs = np.random.randint(0, 2, size=n)
    honest1_inputs = np.random.randint(0, 2, size=n)
    honest2_inputs = np.random.randint(0, 2, size=n)
    corrupt_views = prot(corrupt_inputs, honest1_inputs, honest2_inputs)

    ideal_outputs = xor(xor(corrupt_inputs, honest1_inputs), honest2_inputs)
    X1 = np.array([corrupt_inputs, ideal_outputs]).T
    X2 = np.array([corrupt_inputs, ideal_outputs] + corrupt_views).T
    labels = np.array([honest1_inputs, honest2_inputs]).T

    clf1 = tree.DecisionTreeClassifier()
    chain1 = ClassifierChain(clf1, order='random')
    chain1 = chain1.fit(X1, labels)

    clf2 = tree.DecisionTreeClassifier()
    chain2 = ClassifierChain(clf2, order='random')
    chain2 = chain2.fit(X2, labels)

    ##################################################
    # testing
    ##################################################

    n = 100000
    corrupt_inputs = np.random.randint(0, 2, size=n)
    honest1_inputs = np.random.randint(0, 2, size=n)
    honest2_inputs = np.random.randint(0, 2, size=n)
    corrupt_views = prot(corrupt_inputs, honest1_inputs, honest2_inputs)

    ideal_outputs = xor(xor(corrupt_inputs, honest1_inputs), honest2_inputs)
    X1 = np.array([corrupt_inputs, ideal_outputs]).T
    X2 = np.array([corrupt_inputs, ideal_outputs] + corrupt_views).T
    labels = np.array([honest1_inputs, honest2_inputs]).T

    a1 = chain1.score(X1, labels)
    a2 = chain2.score(X2, labels)
    # print('accuracy X1:', a1)
    # print('accuracy X2:', a2)
    return (a1, a2)


from joblib import Parallel, delayed
results = Parallel(n_jobs=4)(delayed(run_iter)() for i in range(30))
r = np.array(results)
print(r)

from scipy.stats import ttest_ind
print('T-test result:', ttest_ind(r[:,0], r[:,1], alternative='less'))
