declare i32 @putchar(i32)

define void @main() {
  call i32 @putchar(i32 33)
  call i32 @putchar(i32 10)
  ret void
}
