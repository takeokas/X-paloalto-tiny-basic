/* convert xbas/PaloAlto TB source to binary file */
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>

#define BUF_SIZE 512

#define iSnum(c) ('0'<=(c) && (c)<='9')

char buf[BUF_SIZE];
char *pc;

int
getNum(int *f)
{
 int c;
 int n=0;
 *f=0;
 for(;c= *pc;){
	if(!iSnum(c)) break;
	n= n*10 + (c-'0');
	pc++;
	*f=1;
 }
 return n;
}

int
skipBlank()
{int x;
	for(;;){
		if((x= *pc) != ' ') return x;
		pc++;
	}
}

void
main()
{int f,lno;
  char *x;

  for(;;){
    x=fgets(buf, BUF_SIZE,stdin);
    if(x ==NULL) break;
    buf[strlen(buf)-1]= '\0';
    pc=buf;

    skipBlank();
    lno=getNum(&f);
    putc( lno&0xff, stdout); /*LineNo. is little endian */
    putc( (lno>>8)&0xff, stdout);
    fwrite(pc,1,strlen(pc)+1, stdout);
  }  
  putc(0xFF, stdout);
  putc(0xFF, stdout);
}
