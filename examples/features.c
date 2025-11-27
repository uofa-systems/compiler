extern int printf(char *fmt, ...);
extern void *malloc(int size);
extern void free(void *p);

typedef int i64; // typedef/alias

struct Data {
  int a;
  int b;
  char c;
  char d;
};

enum Color {
  RED = 1,
  GREEN, // 2
  BLUE = 5,
  CYAN // 6
};

int g = 7;
char gc = 0;
int zero = 0;

void greet() { printf("  [void] Hello from a void function!\n"); }

int sum(int *p, int n) {
  int s;
  int i;
  s = 0;
  for (i = 0; i < n; i = i + 1) {
    s = s + p[i];
  }
  return s;
}

int test_logic(int x, int y) {
  if ((x && y) || !x) {
    return 1;
  } else {
    return 0;
  }
}

void bump_char(char *pc) { (*pc)++; }

int main() {
  printf("\nTest 1: Sizeof & Typedef\n");
  int sz_int;
  int sz_char;
  int sz_struct;
  int sz_alias;
  struct Data tmp;
  int sz_expr;

  sz_int = sizeof(int);
  sz_char = sizeof(char);
  sz_struct = sizeof(struct Data);
  sz_alias = sizeof(i64);
  sz_expr = sizeof(tmp);

  printf("sizeof(int): %d (Expect 8)\n", sz_int);
  printf("sizeof(char): %d (Expect 1)\n", sz_char);
  printf("sizeof(struct Data): %d (Expect 18)\n", sz_struct);
  printf("sizeof(i64 typedef): %d (Expect 8)\n", sz_alias);
  printf("sizeof(tmp expr): %d (Expect 18)\n", sz_expr);

  printf("\nTest 2: Heap & Struct Arrow\n");
  struct Data *d;
  d = (struct Data *)malloc(sizeof(struct Data));
  d->a = 123;
  d->b = 456;
  d->c = 10;
  d->d = 11;
  printf("malloc(struct Data): a=%d b=%d c=%d d=%d\n", d->a, d->b, d->c, d->d);
  printf("sizeof(*d) via expr: %d (Expect 18)\n", sizeof(*d));
  free(d);

  printf("\nTest 3: Char Truncation\n");
  char c;
  int large_val;
  large_val = 300;
  c = large_val;
  printf("Assigned 300 to char. Result: %d (Expect 44)\n", c);
  gc = 300;
  printf("Global char after 300 assign: %d (Expect 44)\n", gc);

  printf("\nTest 4: Switch & Enum Constants\n");
  int col;
  col = GREEN; // 2
  switch (col) {
  case 1:
    printf("Case RED\n"); // RED
  case 2:
    printf("Case GREEN (Correct)\n"); // GREEN
  case 5:
    printf("Case BLUE (fallthrough OK?)\n"); // BLUE
  default:
    printf("Case DEFAULT\n");
  }

  col = CYAN; // 6
  printf("Switching on CYAN...\n");
  switch (col) {
  case 5:
    printf("BLUE\n"); // BLUE
  case 6:
    printf("CYAN (Correct)\n"); // CYAN
  default:
    printf("Default after CYAN\n");
  }

  printf("\nTest 5: Do-While\n");
  int i;
  i = 0;
  do {
    printf("i = %d\n", i);
    i++;
  } while (i < 3);

  printf("\nTest 6: Arrays & Init-List\n");
  int arr[5] = {1, 2, 3, 4, 5};
  int s;
  int *p;
  s = sum(arr, 5);
  printf("sum(arr, %d) = %d (Expect 15)\n", 5, s);
  p = arr;
  printf("p[2] = %d (Expect 3)\n", p[2]);

  printf("\n--- Test 7: Address-of & Members ---\n");
  struct Data st;
  struct Data *sp;
  st.a = 7;
  st.b = 9;
  st.c = 1;
  st.d = 2;
  printf("st.a (dot) = %d\n", st.a);
  sp = &st;
  sp->b = 42;
  printf("sp->b (arrow) = %d (Expect 42)\n", sp->b);

  printf("\nTest 8: Logic\n");
  printf("test_logic(1,1) = %d (Expect 1)\n", test_logic(1, 1));
  printf("test_logic(1,0) = %d (Expect 0)\n", test_logic(1, 0));
  printf("test_logic(0,0) = %d (Expect 1)\n", test_logic(0, 0));

  printf("\nTest 9: For/continue/break\n");
  for (i = 0; i < 5; i++) {
    if (i == 2)
      continue;
    if (i == 4)
      break;
    printf("loop i=%d\n", i);
  }

  printf("\nTest 10: Void & Cast\n");
  greet();
  i64 x;
  x = 65;
  printf("Cast int 65 to char: %c\n", (char)x);

  printf("\nGlobals\n");
  printf("g(before)=%d\n", g);
  g = g + 5;
  printf("g(after)=%d (Expect 12)\n", g);
  return 0;
}
