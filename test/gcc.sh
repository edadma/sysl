llc -O=1 --filetype=obj $1.ll
gcc -no-pie $1.o -o $1
