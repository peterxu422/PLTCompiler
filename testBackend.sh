#!/bin/bash

rm interpret
./make2.sh;
./interpret < ./tests/backendTest.llb
