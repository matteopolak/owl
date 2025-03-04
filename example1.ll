; ModuleID = 'example'
source_filename = "example"

@formatStr = private unnamed_addr constant [16 x i8] c"hello world %d\0A\00", align 1

declare i32 @printf(ptr, ...)

define i32 @main() {
entry:
  %0 = call i32 (ptr, ...) @printf(ptr @formatStr, i32 15)
  ret i32 0
}
