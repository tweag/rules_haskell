extern int real_get_thing(void);
extern void real_set_thing(int value);

int get_thing(void) {
  return real_get_thing();
}

void set_thing(int value) {
  real_set_thing(value);
}
