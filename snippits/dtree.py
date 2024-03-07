
def vec_procedure(input_width, tape_width, n):
    inputs = np.random.randint(0,2, size=(input_width, n))
    tapes = np.random.randint(0,2, size=(tape_width, n))
    return procedure(inputs, tapes, n)


def run_iter(input_mask, tape_width, views_mask, outputs_mask, training_samples, testing_samples):
    ##################################################
    # training
    ##################################################

    training_data = vec_procedure(len(input_mask), tape_width, training_samples)
    x = np.concatenate((np.delete(training_data.inputs, input_mask, 0),
                        np.delete(training_data.views, views_mask, 0),
                        np.delete(training_data.outputs, outputs_mask, 0)
                      )).T
    y = np.delete(training_data.inputs, np.invert(input_mask), 0).T

    chain = ClassifierChain(tree.DecisionTreeClassifier(), order='random')
    chain = chain.fit(x, y)

    ##################################################
    # testing
    ##################################################

    testing_data = vec_procedure(len(input_mask), tape_width, testing_samples)
    x_test = np.concatenate((np.delete(testing_data.inputs, input_mask, 0),
                             np.delete(testing_data.views, views_mask, 0),
                             np.delete(testing_data.outputs, outputs_mask, 0)
                           )).T
    y_test = np.delete(testing_data.inputs, np.invert(input_mask), 0).T

    return chain.score(x_test, y_test)

def perform_test(input_mask, tape_width, views_mask, outputs_mask, training_samples, testing_samples, iterations):
    real_results = Parallel(n_jobs=-2)(
            delayed(run_iter)(input_mask, tape_width, views_mask, outputs_mask, training_samples, testing_samples)
            for i in range(iterations)
        )
    ideal_results = Parallel(n_jobs=-2)(
            delayed(run_iter)(input_mask, tape_width, np.ones_like(views_mask), outputs_mask, training_samples, testing_samples)
            for i in range(iterations)
        )
    #print(ideal_results, real_results)
    p_value = ttest_ind(ideal_results, real_results, alternative='less').pvalue
    print('{:6.5f}'.format(p_value))


