int shared_taint = 0;

/* TAINT_WITNESS:user_input */
/* TAINT_WITNESS:tainted_return */
int shared_taint;

int taint_source(void) {
  shared_taint = 42;
  return 42;
}
