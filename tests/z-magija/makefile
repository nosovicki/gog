all: z-magija.c
	gcc -o z-magija z-magija.c
	@#echo -e '200000 2\n1 3 200000\n2 200000\n'|./z-magija
	@echo =======================================
	@./ref <z-magija.in >z-magija.ref
	@time for((i=0;i<10;i++));do ./z-magija<  z-magija.in >z-magija.out;done && diff -c  z-magija.ref z-magija.out
