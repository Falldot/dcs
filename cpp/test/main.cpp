// Copyright (c) 2022 Falldot. All rights reserved.

#include "test.h"

#include <iostream>

int main(int argc, const char **argv) {

  Transform transform;

  transform.add(12, 1, 22, 11, 66);
  transform.add(45, 3, 22, 11, 66);
  transform.add(7, 6, 22, 11, 66);

  transform.remove(0);

  int *fs = (int *)transform.f;
  int *f2s = (int *)transform.f + transform.c;

  for (size_t i = 0; i < transform.s; ++i) {
    std::cout << fs[i] << std::endl;
  }

  std::cout << "-----------------" << std::endl;

  for (size_t i = 0; i < transform.s; ++i) {
    std::cout << f2s[i] << std::endl;
  }

  return 0;
}