; ModuleID = 'main'
source_filename = "<string>"

declare i32 @printf(i8*, ...)

@fmt = private constant [4 x i8] c"%d\0A\00"

define void @main() {
entry:
  %0 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 add (i32 1, i32 2))
  ret void
}

