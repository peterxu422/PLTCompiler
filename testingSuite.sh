#!/bin/bash
echo "Compiling and running scannerTest"
ocamllex scannerTest.mll
ocamlc -o st scannerTest.ml
./st < tokensTest.llb

echo "<The next testing goes here>"
