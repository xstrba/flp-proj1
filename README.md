# flp-proj1

Autor: Boris Štrbák (xstrba05)

### Preklad programu

```bash
$ make 
```

### Spustenie testov

```bash
$ make -s test_b # testovanie prepinaca b
$ make -s test_o # testovanie prepinaca o
```

### Spustenie programu

```bash
$ ./flp22-fun # zobrazi help
$ ./flp22-fun -i ./tests/test1.in # vypise knapsack
$ ./flp22-fun -b ./tests/test1.in # pokusi sa najst riesenie brute force metodou
$ ./flp22-fun -o ./tests/test1.in # pokusi sa najst riesenie metodou simulovaneho zihani
```