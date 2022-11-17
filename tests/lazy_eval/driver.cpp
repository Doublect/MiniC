#include <iostream>
#include <cstdio>
#include <math.h> 

// clang++ driver.cpp cosine.ll -o cosine

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

extern "C" DLLEXPORT int print_int(int X) {
  fprintf(stderr, "%d\n", X);
  return 0;
}

extern "C" DLLEXPORT float print_float(float X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}

extern "C" {
    int lazy_and();
    int lazy_or();
}

int main() {

  if(lazy_and() == 0) 
    std::cout << "PASSED Lazy AND" << std::endl;
  else 
    std::cout << "FAILED Lazy AND" << std::endl;

  if(lazy_or() == 0) 
    std::cout << "PASSED Lazy OR" << std::endl;
  else 
    std::cout << "FAILED Lazy OR" << std::endl;
}