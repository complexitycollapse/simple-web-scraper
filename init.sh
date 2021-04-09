#!/bin/bash

sed -i "s/%ApplicationName%/$1/g" application.lisp
sed -i "s/%ApplicationName%/$1/g" packages.lisp
sed -i "s/%ApplicationName%/$1/g" README-template.md
sed -i "s/%ApplicationName%/$1/g" start.lisp
sed -i "s/%ApplicationName%/$1/g" system.asd
rm README.md
mv README-template.md README.md
mv application.lisp $1.lisp
mv system.asd $1.asd
