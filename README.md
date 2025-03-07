# Owl

## Usage

```sh
owl example.owl -O 3 --emit-llvm
```

```
Usage: owl [--help] [--version] [--optimize VAR] [--emit-llvm] file

Positional arguments:
  file            source file to compile [required]

Optional arguments:
  -h, --help      shows help message and exits
  -v, --version   prints version information and exits
  -O, --optimize  optimization level [nargs=0..1] [default: 3]
  --emit-llvm     emit llvm ir
```

## Examples

```rust
extern fn printf(fmt: *char, ...);

fn main(): i32 {
  printf("Hello, world!\n");

  return 0;
}
```

```rust
extern fn printf(fmt: *char, ...);

struct Point {
  x: i32,
  y: i32
}

fn add(p1: Point, p2: Point): Point {
  return Point { x: p1.x + p2.x, y: p1.y + p2.y };
}

fn main(): i32 {
  let p1 = Point { x: 1, y: 2 };
  let p2 = Point { x: 3, y: 4 };
  let p3 = add(p1, p2);

  printf("p1: (%d, %d)\n", p1.x, p1.y);
  printf("p2: (%d, %d)\n", p2.x, p2.y);
  printf("p1 + p2: (%d, %d)\n", p3.x, p3.y);

  return 0;
}
```

## Errors

```
error: field `z` not found in struct `Point`

 8 | fn add(p1: Point, p2: Point): Point {
 9 |   return Point { x: p1.x + p2.x, y: p1.y + p2.y };
10 | }
11 |
12 | fn main(): i32 {
13 |   let p1 = Point { x: 1, y: 2 };
14 |   let p2 = Point { x: 3, z: 4 };
                              ^
     note: field not found

 1 | extern fn printf(fmt: *char, ...)
 2 |
 3 | struct Point {
            ^^^^^
     note: struct `Point` defined here
```

```
error: type mismatch for field `y`. expected `i32`, found `f64`

 8 | fn add(p1: Point, p2: Point): Point {
 9 |   return Point { x: p1.x + p2.x, y: p1.y + p2.y };
10 | }
11 |
12 | fn main(): i32 {
13 |   let p1 = Point { x: 1, y: 2 };
14 |   let p2 = Point { x: 3, y: 4.0 };
                                 ^^^
     note: incorrect value here
```

```
error: missing field `y` in instantiation of struct `Point`

 8 | fn add(p1: Point, p2: Point): Point {
 9 |   return Point { x: p1.x + p2.x, y: p1.y + p2.y };
10 | }
11 |
12 | fn main(): i32 {
13 |   let p1 = Point { x: 1, y: 2 };
14 |   let p2 = Point { x: 3 };
                ^^^^^
     note: missing field `y` in this instantiation
```

```
error: mismatched types for argument `fmt` in call to function `printf`

1 | extern fn printf(fmt: *char, ...)
2 |
3 | fn main(): i32 {
4 |   printf(1);
             ^
    note: expected `*char`, found `i32`

1 | extern fn printf(fmt: *char, ...)
2 |
3 | fn main(): i32 {
4 |   printf(1);
5 | }
6 |
7 | extern fn printf(fmt: *char, ...);
                     ^^^
    note: argument defined here
```
