#include <iostream>
#include <libsci.h>

int main(int argc, char* argv[]) {
  graal_isolate_t *isolate = NULL;
  graal_isolatethread_t *thread = NULL;

  if (graal_create_isolate(NULL, &isolate, &thread) != 0) {
    fprintf(stderr, "initialization error\n");
    return 1;
  }

  //char expression[] = "(defn foo [] :foo) (foo)";
  char *result = eval_string((long)thread, &argv[1][0]);
  std::cout << result << std::endl;
  return 0;
}
