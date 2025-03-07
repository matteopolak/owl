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

## Hello world

```rust
extern fn printf(fmt: *char, ...);

fn main(): i32 {
  printf("Hello, world!\n");

  return 0;
}
```

## More

```rust
extern fn printf(fmt: *char, ...);

struct Point {
  x: i32,
  y: i32,
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
