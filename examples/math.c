extern int printf(char *fmt, ...);

int mod(int a, int b) {
  int div = a / b;
  int mult = div * b;
  return a - mult;
}

int gcd(int a, int b) {
  if (b == 0) {
    return a;
  }
  return gcd(b, mod(a, b));
}

int is_prime(int n) {
  if (n < 2) {
    return 0;
  }

  int i = 2;
  while (i < n) {
    if (mod(n, i) == 0) {
      return 0; // Not prime
    }
    i = i + 1;
  }
  return 1; // Is prime
}

int collatz_steps(int n) {
  int steps = 0;
  while (n > 1) {
    if (mod(n, 2) == 0) {
      n = n / 2;
    } else {
      n = (3 * n) + 1;
    }
    steps = steps + 1;
  }
  return steps;
}

int main() {
  int a = 105;
  int b = 252;
  int result_gcd = gcd(a, b);
  printf("GCD of 105 and 252 is: %d\n", result_gcd);

  int check_num = 17;
  if (is_prime(check_num)) {
    printf("Number %d is PRIME\n", check_num);
  } else {
    printf("Number %d is NOT prime\n", check_num);
  }

  check_num = 20;
  if (is_prime(check_num)) {
    printf("Number %d is PRIME\n", check_num);
  } else {
    printf("Number %d is NOT prime\n", check_num);
  }

  int start = 27;
  int steps = collatz_steps(start);
  printf("Collatz steps for 27: %d\n", steps);

  return 0;
}
