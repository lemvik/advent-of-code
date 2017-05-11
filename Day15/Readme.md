This task could be solved without any programming at all:

Initial conditions could be represented as:

n + 2 = 0 mod 17
n + 2 = 0 mod 7
n + 5 = 0 mod 19
n + 4 = 0 mod 5
n + 5 = 0 mod 3
n + 11 = 0 mod 13
n + 7 = 0 mod 11 (this is for second part)

Introducing additional variables this system could be rewritten as:
n = 13f - 11
3e = 13f - 6
5d = 13f - 7
19c = 13f - 6
7b = 13f - 9
17a = 13f - 9

Or, in modulo arithmetic:

13f - 6 = 0 mod 3
13f - 7 = 0 mod 5
13f - 6 = 0 mod 19
13f - 9 = 0 mod 7
13f - 9 = 0 mod 17

This could be solved (with some trics - for example, 13f - 6 mod 3 is equivalent to f = 0 mod 3) to give
f = 4845 * xi + 189
that should match 6*xi + 5 = 0 mod 7 -> xi = 5