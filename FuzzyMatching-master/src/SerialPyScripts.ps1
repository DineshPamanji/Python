$N=5
for($i=0; $i-le$N; $i++) {
    $callingPyExpression = "C:\Python36\Python.exe " + "C:\Excercise\FuzzyMatching\src\FuzzyMatching.py " + $i
    invoke-expression $callingPyExpression
    $process = Get-Process powershell
    $process.Kill()
}
#Get-Job