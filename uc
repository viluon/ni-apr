#!/bin/sh

JAR_PATH="target/scala-2.13/microc.jar"

if [ ! -f "$JAR_PATH" ]; then
  echo "$JAR_PATH: does not exist, build the project first using 'sbt assembly'"
  exit 1
fi

java -cp "$JAR_PATH" "microc.cli.Main" "$@"
