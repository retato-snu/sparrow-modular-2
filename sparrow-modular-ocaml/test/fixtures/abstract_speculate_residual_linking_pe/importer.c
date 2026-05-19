extern int shared_g;
extern int linked_provider(void);
extern void *memcpy(void *dst, const void *src, unsigned long n);
extern char *strcpy(char *dst, const char *src);
extern unsigned long strlen(const char *src);

int main(void) {
  int src = linked_provider();
  int dst = 0;
  int out = 0;
  void *memcpy_ret = memcpy(&dst, &src, 4);
  strcpy((char *)&out, (char *)&dst);
  int n = (int)strlen((char *)&out);
  return src + 1;
}
