#!/usr/bin/env bash
base=$(dirname $0)
file="$base/src/main"
cd $base
#rm -rf "$base/dist/linux"
#echo "Running Tests...\n"
#racket "$base/src/tests.rkt"
#teststat=$?
#if [ $? -ne 0 ]; then
#	echo "Tests Failed."
#	exit 255
#fi
echo "Generating files..."
cd src
python parse.py
cd ..
echo "Compiling P1..."
raco exe "src/jeopardyp1.rkt"
echo "Compiling P2..."
raco exe "src/jeopardyp2.rkt"
echo "Compiling Server..."
raco exe "src/server.rkt"
echo "Creating distro..."
mkdir -p "$base/dist/linux"
raco distribute "$base/dist/linux" src/jeopardyp1 src/jeopardyp2 src/server
echo "Compressing package..."
racket "$base/compress.rkt"
rm -rf "src/jeopardyp1"
rm -rf "src/jeopardyp2"
rm -rf "src/server"
cd "$base/dist/linux/bin"
#echo "Done. Running program.\n"
#./server
echo "Done. Executables are named server, jeopardyp1, and jeopardyp2."
