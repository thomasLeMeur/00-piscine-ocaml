#!/bin/bash

./clean.sh
ocamlfind ocamlc -g -package lablgtk2 -linkpkg str.cma main.ml -o rush01
