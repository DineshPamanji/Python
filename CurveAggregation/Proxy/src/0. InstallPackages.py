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


results = map(install_package, ('numpy', 'pandas==1.2.4', 'os', 'json', 'matplotlib', 'datetime'
                                , 'multiprocessing', 'functools', 'scipy==1.6.2', 'math', 'logging'))
set(results)

