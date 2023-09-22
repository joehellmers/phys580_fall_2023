#include<iostream>

int main() {

    std::cout << "C++ Static Array Example" << std::endl;

    const int n = 10;

    double myarray[n];

    for (int i = 0; i < n; ++i) {
        std::cout << i << ", " << myarray[i] << std::endl;
    } 
    
    return 0;
}

