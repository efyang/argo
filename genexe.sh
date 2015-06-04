#!/usr/bin/env bash
base=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cd $base
if [ -d "$base/dist/linux" ]; then
	rm -rf "$base/dist/linux"
fi
echo "Compiling Client..."
raco exe "src/argo.rkt"
echo "Compiling Server..."
raco exe "src/argoserver.rkt"
echo "Creating distro..."
mkdir -p "$base/dist/linux"
raco distribute "$base/dist/linux" "src/argo" "src/argoserver"
echo "Compressing package..."
racket "$base/compress.rkt"
rm -rf "src/argo"
rm -rf "src/argoserver"
cd "$base/dist/linux/bin"
echo "Finished."
