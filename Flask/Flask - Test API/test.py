def df():
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

    results = map(install_and_import, ('pandas', 'sklearn'))
    set(results)

    import pandas as pd

    df = pd.read_csv("large_dataset.csv")

    df1 =  df[1:100]

    df1.to_csv("test.csv")



