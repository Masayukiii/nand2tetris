(Sys)
@init
D=A
(G11518)
@G11519
D;JEQ
@R0
A=M
M=0
@R0
M=M+1
D=D-1
@G11518
0;JMP
(G11519)
@4000
D=A
@R0
A=M
M=D
@R0
M=M+1
@0
D=A
@G11520
D;JEQ
@R0
A=M-1
D=M
@R4
M=D
@G11521
0;JMP
(G11520)
@R0
A=M-1
D=M
@R3
M=D
(G11521)
@R0
M=M-1
@5000
D=A
@R0
A=M
M=D
@R0
M=M+1
@1
D=A
@G11522
D;JEQ
@R0
A=M-1
D=M
@R4
M=D
@G11523
0;JMP
(G11522)
@R0
A=M-1
D=M
@R3
M=D
(G11523)
@R0
M=M-1
@G11524
D=A
@R0
A=M
M=D
@R0
M=M+1
@R1
D=M
@R0
A=M
M=D
@R0
M=M+1
@R2
D=M
@R0
A=M
M=D
@R0
M=M+1
@R3
D=M
@R0
A=M
M=D
@R0
M=M+1
@R4
D=M
@R0
A=M
M=D
@R0
M=M+1
@R0
D=M
@main
D=D-A
@5
D=D-A
@R2
M=D
@R0
D=M
@R1
M=D
@Sys
0;JMP
(G11524)
@R0
M=M-1
A=M
D=M
@6
M=D
(LOOP)
@LOOP
A=M
(Sys)
@main
D=A
(G11528)
@G11529
D;JEQ
@R0
A=M
M=0
@R0
M=M+1
D=D-1
@G11528
0;JMP
(G11529)
@4001
D=A
@R0
A=M
M=D
@R0
M=M+1
@0
D=A
@G11530
D;JEQ
@R0
A=M-1
D=M
@R4
M=D
@G11531
0;JMP
(G11530)
@R0
A=M-1
D=M
@R3
M=D
(G11531)
@R0
M=M-1
@5001
D=A
@R0
A=M
M=D
@R0
M=M+1
@1
D=A
@G11532
D;JEQ
@R0
A=M-1
D=M
@R4
M=D
@G11533
0;JMP
(G11532)
@R0
A=M-1
D=M
@R3
M=D
(G11533)
@R0
M=M-1
@200
D=A
@R0
A=M
M=D
@R0
M=M+1
@R1
D=M
@1
D=D+A
@R1
M=D
@R0
A=M-1
D=M
@R1
A=M
M=D
@1
D=A
@R1
M=M-D
@R0
M=M-1
@40
D=A
@R0
A=M
M=D
@R0
M=M+1
@R1
D=M
@2
D=D+A
@R1
M=D
@R0
A=M-1
D=M
@R1
A=M
M=D
@2
D=A
@R1
M=M-D
@R0
M=M-1
@6
D=A
@R0
A=M
M=D
@R0
M=M+1
@R1
D=M
@3
D=D+A
@R1
M=D
@R0
A=M-1
D=M
@R1
A=M
M=D
@3
D=A
@R1
M=M-D
@R0
M=M-1
@123
D=A
@R0
A=M
M=D
@R0
M=M+1
@G11540
D=A
@R0
A=M
M=D
@R0
M=M+1
@R1
D=M
@R0
A=M
M=D
@R0
M=M+1
@R2
D=M
@R0
A=M
M=D
@R0
M=M+1
@R3
D=M
@R0
A=M
M=D
@R0
M=M+1
@R4
D=M
@R0
A=M
M=D
@R0
M=M+1
@R0
D=M
@add12
D=D-A
@5
D=D-A
@R2
M=D
@R0
D=M
@R1
M=D
@Sys
0;JMP
(G11540)
@R0
M=M-1
A=M
D=M
@5
M=D
@R1
D=M
@0
D=D+A
A=D
D=M
@R0
A=M
M=D
@R0
M=M+1
@R1
D=M
@1
D=D+A
A=D
D=M
@R0
A=M
M=D
@R0
M=M+1
@R1
D=M
@2
D=D+A
A=D
D=M
@R0
A=M
M=D
@R0
M=M+1
@R1
D=M
@3
D=D+A
A=D
D=M
@R0
A=M
M=D
@R0
M=M+1
@R1
D=M
@4
D=D+A
A=D
D=M
@R0
A=M
M=D
@R0
M=M+1
@R0
M=M-1
A=M
D=M
@R0
M=M-1
A=M
M=M+D
@R0
M=M+1
@R0
M=M-1
A=M
D=M
@R0
M=M-1
A=M
M=M+D
@R0
M=M+1
@R0
M=M-1
A=M
D=M
@R0
M=M-1
A=M
M=M+D
@R0
M=M+1
@R0
M=M-1
A=M
D=M
@R0
M=M-1
A=M
M=M+D
@R0
M=M+1
@R0
M=M-1
D=M
@R13
M=D
@R0
A=M
D=M
@R2
A=M
M=D
@R2
D=M+1
@R0
M=D
@1
D=A
@R13
D=M-D
A=D
D=M
@R4
M=D
@2
D=A
@R13
D=M-D
A=D
D=M
@R3
M=D
@3
D=A
@R13
D=M-D
A=D
D=M
@R2
M=D
@4
D=A
@R13
D=M-D
A=D
D=M
@R1
M=D
@5
D=A
@R13
D=M-D
A=D
A=M
(Sys)
@add12
D=A
(G11554)
@G11555
D;JEQ
@R0
A=M
M=0
@R0
M=M+1
D=D-1
@G11554
0;JMP
(G11555)
@4002
D=A
@R0
A=M
M=D
@R0
M=M+1
@0
D=A
@G11556
D;JEQ
@R0
A=M-1
D=M
@R4
M=D
@G11557
0;JMP
(G11556)
@R0
A=M-1
D=M
@R3
M=D
(G11557)
@R0
M=M-1
@5002
D=A
@R0
A=M
M=D
@R0
M=M+1
@1
D=A
@G11558
D;JEQ
@R0
A=M-1
D=M
@R4
M=D
@G11559
0;JMP
(G11558)
@R0
A=M-1
D=M
@R3
M=D
(G11559)
@R0
M=M-1
@R2
D=M
@0
D=D+A
A=D
D=M
@R0
A=M
M=D
@R0
M=M+1
@12
D=A
@R0
A=M
M=D
@R0
M=M+1
@R0
M=M-1
A=M
D=M
@R0
M=M-1
A=M
M=M+D
@R0
M=M+1
@R0
M=M-1
D=M
@R13
M=D
@R0
A=M
D=M
@R2
A=M
M=D
@R2
D=M+1
@R0
M=D
@1
D=A
@R13
D=M-D
A=D
D=M
@R4
M=D
@2
D=A
@R13
D=M-D
A=D
D=M
@R3
M=D
@3
D=A
@R13
D=M-D
A=D
D=M
@R2
M=D
@4
D=A
@R13
D=M-D
A=D
D=M
@R1
M=D
@5
D=A
@R13
D=M-D
A=D
A=M
