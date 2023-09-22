#include<iostream>


void f(double x, double& xsquared) {
    xsquared = x*x;
}

int main() {
    
    std::cout << "C++ Example of procedure" << std::endl;
    double x = 1.5;
    double xsquared = 0;
    f(x, xsquared);
    std::cout << x << " squared is " << xsquared << std::endl;
}
