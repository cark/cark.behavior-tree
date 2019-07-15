del pom.xml
clojure -Spom
clojure -A:release %1
clojure -A:garamond
clojure -A:jar
clojure -A:deploy
git push --tags