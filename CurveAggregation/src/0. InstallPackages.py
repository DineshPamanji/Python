def install_and_import(package):
    import importlib
    try:
        importlib.import_module(package)
    except ImportError:
        # from pip._internal import main as pipmain
        # pipmain(['install', package])
        import os
        os.system(f'pip install {package}')
    finally:
        globals()[package] = importlib.import_module(package)


def install_package(package):
    import importlib
    try:
        importlib.import_module(package)
    except ImportError:
        # from pip._internal import main as pipmain
        # pipmain(['install', package])
        import os
        os.system(f'pip install {package}')
    finally:
        globals()[package] = importlib.import_module(package)


results = map(install_package, ('numpy==1.19.5', 'pandas==0.24.0', 'os', 'json', 'matplotlib==3.2.1', 'datetime'
                                , 'multiprocessing', 'functools', 'scipy==1.4.1', 'math', 'logging'))
set(results)

