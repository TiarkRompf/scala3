#!/bin/bash

echo "Checking example: File.scala"
touch examples/sample.txt
sbt "scalac examples/File.scala" "scala Main"

echo "Checking example: TableLock.scala"
sbt "scalac examples/TableLock.scala" "scala Main"

echo "Checking example: DOM.scala"
sbt "scalac examples/DOM.scala"

echo "Checking example: SessionImp.scala"
sbt "scalac examples/SessionImp.scala"

echo "Checking example: ControlFlow.scala"
sbt "scalac examples/ControlFlow.scala"

echo "Checking example: SessionExp.scala"
sbt "scalac examples/SessionExp.scala"

echo "Checking example: Drone.scala"
sbt "scalac examples/Drone.scala"
