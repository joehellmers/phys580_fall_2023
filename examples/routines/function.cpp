#include<iostream>


double f(double x) {
    return x*x;
}

int main() {
    std::cout << "C++ Example of function" << std::endl;
    double x = 1.5;
    std::cout << x << " squared is " << f(x) << std::endl;
}
