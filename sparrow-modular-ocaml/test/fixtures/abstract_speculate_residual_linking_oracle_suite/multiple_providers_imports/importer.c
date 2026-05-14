extern int first_provider(void);
extern int second_provider(void);

int main(void) {
  int a = first_provider();
  int b = second_provider();
  return a + b;
}
