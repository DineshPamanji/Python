$N=20
for($i=0; $i-le$N; $i++) {
    Start-Job {
            & 'C:\Python36\Python.exe' C:\Excercise\FuzzyMatching\src\FuzzyMatching.py $args[0]
        } -ArgumentList $i
    }
#Get-Job