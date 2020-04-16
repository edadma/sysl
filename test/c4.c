#include <stdio.h>

struct s {
  int a;
  int b;
  int c;
};

struct s mid( struct s x ) {
  return x;
}

int main(int argc, char** args) {
    struct s v = {3, 4, 5};

    printf( "%d\n", mid(v).b );
}
