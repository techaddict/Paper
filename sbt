SBT_OPTS="-Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M"
java -Dfile.encoding=UTF8 $SBT_OPTS -jar bin/sbt-launch.jar "$@"