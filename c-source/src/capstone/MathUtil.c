
#include "MathUtil.h"

int min(int a, int b){
    return a < b ? a : b;
}

int max(int a, int b){
    return a > b ? a : b;
}



double abs(double a){
    return a > 0 ? a : a * -1;
}