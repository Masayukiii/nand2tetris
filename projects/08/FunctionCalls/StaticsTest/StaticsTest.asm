(Sys.init)
@0
D=A
(G11757)
@G11758
D;JEQ
@R0
A=M
M=0
@R0
M=M+1
D=D-1
@G11757
0;JMP
(G11758)
@6
D=A
@R0
A=M
M=D
@R0
M=M+1
@8
D=A
@R0
A=M
M=D
@R0
M=M+1
@G11759
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
@2
D=D-A
@5
D=D-A
@R2
M=D
@R0
D=M
@R1
M=D
@Class1.set
0;JMP
(G11759)
@R0
M=M-1
D=M
A=D
D=M
@5
M=D
@23
D=A
@R0
A=M
M=D
@R0
M=M+1
@15
D=A
@R0
A=M
M=D
@R0
M=M+1
@G11763
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
@2
D=D-A
@5
D=D-A
@R2
M=D
@R0
D=M
@R1
M=D
@Class2.set
0;JMP
(G11763)
@R0
M=M-1
D=M
A=D
D=M
@5
M=D
@G11767
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
@0
D=D-A
@5
D=D-A
@R2
M=D
@R0
D=M
@R1
M=D
@Class1.get
0;JMP
(G11767)
@G11769
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
@0
D=D-A
@5
D=D-A
@R2
M=D
@R0
D=M
@R1
M=D
@Class2.get
0;JMP
(G11769)
(WHILE)
@WHILE
0;JMP
(Class2.set)
@0
D=A
(G11771)
@G11772
D;JEQ
@R0
A=M
M=0
@R0
M=M+1
D=D-1
@G11771
0;JMP
(G11772)
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
@R0
M=M-1
D=M
A=D
D=M
@Class2.0
M=D
@R2
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
@R0
M=M-1
D=M
A=D
D=M
@Class2.1
M=D
@0
D=A
@R0
A=M
M=D
@R0
M=M+1
@R1
D=M
@R13
M=D
@5
D=A
@R13
D=M-D
A=D
D=M
@R14
M=D
@R0
D=M-1
A=D
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
@R14
A=M
0;JMP
(Class2.get)
@0
D=A
(G11779)
@G11780
D;JEQ
@R0
A=M
M=0
@R0
M=M+1
D=D-1
@G11779
0;JMP
(G11780)
@Class2.0
D=M
@R0
A=M
M=D
@R0
M=M+1
@Class2.1
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
M=M-D
@R0
M=M+1
@R1
D=M
@R13
M=D
@5
D=A
@R13
D=M-D
A=D
D=M
@R14
M=D
@R0
D=M-1
A=D
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
@R14
A=M
0;JMP
(Class1.set)
@0
D=A
(G11785)
@G11786
D;JEQ
@R0
A=M
M=0
@R0
M=M+1
D=D-1
@G11785
0;JMP
(G11786)
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
@R0
M=M-1
D=M
A=D
D=M
@Class1.0
M=D
@R2
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
@R0
M=M-1
D=M
A=D
D=M
@Class1.1
M=D
@0
D=A
@R0
A=M
M=D
@R0
M=M+1
@R1
D=M
@R13
M=D
@5
D=A
@R13
D=M-D
A=D
D=M
@R14
M=D
@R0
D=M-1
A=D
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
@R14
A=M
0;JMP
(Class1.get)
@0
D=A
(G11793)
@G11794
D;JEQ
@R0
A=M
M=0
@R0
M=M+1
D=D-1
@G11793
0;JMP
(G11794)
@Class1.0
D=M
@R0
A=M
M=D
@R0
M=M+1
@Class1.1
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
M=M-D
@R0
M=M+1
@R1
D=M
@R13
M=D
@5
D=A
@R13
D=M-D
A=D
D=M
@R14
M=D
@R0
D=M-1
A=D
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
@R14
A=M
0;JMP
