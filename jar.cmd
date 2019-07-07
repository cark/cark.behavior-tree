del /S *.*~
rd /S /Q target
mkdir target
clojure -A:jar