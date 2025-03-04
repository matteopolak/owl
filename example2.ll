; ModuleID = 'example'
source_filename = "example"

@0 = private unnamed_addr constant [16 x i8] c"hello world %d\0A\00", align 1

declare i32 @printf(ptr, ...)

define i32 @main() {
entry:
  %0 = call i32 (ptr, ...) @printf(ptr @0, i32 2)
  ret i32 1
}

declare void @exit(i32)

define void @_start() {
entry:
  %main_ret = call i32 @main()
  call void @exit(i32 %main_ret)
  unreachable
}
