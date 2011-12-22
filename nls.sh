#!/bin/bash
./numlangc < samplecode > Runner.java
javac -cp ./java Runner.java
#rm Runner.java
