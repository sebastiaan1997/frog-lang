#include <cstdio>

extern "C" bool odd(unsigned n);
extern "C" bool even(unsigned n);


void main(){
    for(unsigned int i = 0; i < 10; i++){
        printf("%d", odd(1));
        printf("%d", even(1));
    }
}