def install_and_import(package):
    import importlib
    try:
        importlib.import_module(package)
    except ImportError:
        import pip
        pip.main(['install', package])
    finally:
        globals()[package] = importlib.import_module(package)

install_and_import('flask')

from flask import Flask

import test
import Test_Regression

app = Flask(__name__)

@app.route("/")
def index():
    return "Index!"

@app.route("/hello")
def hello():
    return "Hello World!"

@app.route("/members")
def members():
    return "Members"

@app.route("/members/<string:name>/")
def getMember(name):
    return name

@app.route("/addition")
def addition():
    result = 1+1
    return "1+1 = " + str(result)

@app.route("/Subset")
def testf():
    test.df()
    return "Test: Successful"

@app.route("/Regression")
def reg():
    Test_Regression.regression()
    return "Regression: Successful"


if __name__ == "__main__":
    app.run()