del pom.xml
copy start-pom.xml pom.xml
clojure -A:release %1
clojure -A:garamond
mvn deploy
git push --tags
