def regression():
    # ### Import required libraries
    def install_and_import(package):
        import importlib
        try:
            importlib.import_module(package)
        except ImportError:
            import pip
            pip.main(['install', package])
        finally:
            globals()[package] = importlib.import_module(package)


    results = map(install_and_import, ('pandas', 'numpy', 'sklearn'))
    set(results)

    import numpy as np
    from sklearn import datasets, linear_model
    from sklearn.metrics import mean_squared_error, r2_score

    # Load the diabetes dataset
    diabetes = datasets.load_diabetes()


    # Use only one feature
    diabetes_X = diabetes.data[:, np.newaxis, 2]

    # Split the data into training/testing sets
    diabetes_X_train = diabetes_X[:-20]
    diabetes_X_test = diabetes_X[-20:]

    # Split the targets into training/testing sets
    diabetes_y_train = diabetes.target[:-20]
    diabetes_y_test = diabetes.target[-20:]

    # Create linear regression object
    regr = linear_model.LinearRegression()

    # Train the model using the training sets
    regr.fit(diabetes_X_train, diabetes_y_train)

    # Make predictions using the testing set
    diabetes_y_pred = regr.predict(diabetes_X_test)

    text_file = open("Regression_Output.txt", "w")
    # The coefficients
    text_file.write('Coefficients: %.2f \n' % regr.coef_)
    # The mean squared error
    text_file.write("Mean squared error: %.2f \n"
          % mean_squared_error(diabetes_y_test, diabetes_y_pred))
    # Explained variance score: 1 is perfect prediction
    text_file.write('Variance score: %.2f \n' % r2_score(diabetes_y_test, diabetes_y_pred))
    text_file.close()
