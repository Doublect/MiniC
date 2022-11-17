// MiniC program to compute the cosine of x to within tolerance eps
// use an alternating series

extern float print_float(float X);

int lazy_and() {
  int x;
  int y;
  x = 0;
  y = 1;
  
  1 || (x = 1);
  0 || (y = 0);
  0 || 1 || (x = 1);
  1 || 0 || (x = 1); 

  return x + y;
}

int lazy_or() {
  int x;
  int y;
  x = 0;
  y = 1;

  0 && (x = 1);
  1 && (y = 0);

  0 && 1 && (x = 1);
  1 && 0 && (x = 1); 
  return x + y;
}
