import numpy as np
from sklearn import tree

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

##################################################
# training
##################################################

n = 10000
corrupt_inputs = np.random.randint(0, 2, size=n)
honest1_inputs = np.random.randint(0, 2, size=n)
honest2_inputs = np.random.randint(0, 2, size=n)
corrupt_views = prot(corrupt_inputs, honest1_inputs, honest2_inputs)

ideal_outputs = xor(xor(corrupt_inputs, honest1_inputs), honest2_inputs)
X1 = np.array([corrupt_inputs, ideal_outputs]).T
X2 = np.array([corrupt_inputs, ideal_outputs] + corrupt_views).T
labels = (2*honest1_inputs + honest2_inputs)

clf1 = tree.DecisionTreeClassifier()
clf1 = clf1.fit(X1, labels)

clf2 = tree.DecisionTreeClassifier()
clf2 = clf2.fit(X2, labels)

##################################################
# testing
##################################################

n = 10000
corrupt_inputs = np.random.randint(0, 2, size=n)
honest1_inputs = np.random.randint(0, 2, size=n)
honest2_inputs = np.random.randint(0, 2, size=n)
corrupt_views = prot(corrupt_inputs, honest1_inputs, honest2_inputs)

ideal_outputs = xor(xor(corrupt_inputs, honest1_inputs), honest2_inputs)
X1 = np.array([corrupt_inputs, ideal_outputs]).T
X2 = np.array([corrupt_inputs, ideal_outputs] + corrupt_views).T
labels = (2*honest1_inputs + honest2_inputs)

print(f'({clf1.score(X1, labels)}, {clf2.score(X2, labels)})')
