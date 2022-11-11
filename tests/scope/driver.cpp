#include <iostream>
#include <cstdio>

// clang++ driver.cpp scope.ll -o scope


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
    int scope();
}

int main() {
    int res = scope();

    switch(res) {
      case 2000:
        std::cout << "PASSED: Scopes work" << std::endl;
        break;
      case 1002:
        std::cout << "FAILED: Local scope does not work Result: " << res << std::endl;
        break;
      case 1001:
        std::cout << "FAILED: Global scope does not work Result: " << res << std::endl;
        break;
      case 3:
        std::cout << "FAILED: No scoping Result: " << res << std::endl;
        break;
    }
}