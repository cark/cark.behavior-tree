del pom.xml
copy start-pom.xml pom.xml
clojure -Spom
clojure -A:release %1
clojure -A:garamond
REM clojure -A:jar
REM clojure -A:deploy
REM git push --tags