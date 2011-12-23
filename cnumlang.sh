#!/bin/bash

if [ $# -eq 0 ]
then
echo "Error, you need a file to compile"
else
./numlangc < $1 > Runner.java
javac -cp ./java Runner.java
fi
