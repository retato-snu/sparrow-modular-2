int branchy(int x) {
  goto done;
  x = x + 100;
done:
  return x;
}

int main(void) {
  return branchy(0);
}
