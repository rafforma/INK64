#include <stdio.h>
#include <stdlib.h>


int main (int argc,char *argv[]) {
    if (argc<3) {
        printf ("use %s charset new_charset\n",argv[0]);
        return 0x80;
    }

    FILE *fin = fopen (argv[1],"rb");
    if (!fin) {
        printf ("missing file '%s'\n",argv[1]);
        return 0x80;
    }

    FILE *fout = fopen (argv[2],"wb");
    if (!fout) {
        printf ("cannot create file '%s'\n",argv[2]);
        return 0x80;
    }

    while (!feof (fin)) {
        int i=0;
        unsigned char  b[32];
        unsigned char  d[32];
        fread (b,sizeof(b),1,fin);
        for (i=0;i<8;i++) {
            d[i] = b[i];
            d[i+8] = b[i+16];
            d[i+16] = b[i+8];
            d[i+24] = b[i+24];
        }

        fwrite (d,sizeof (d),1,fout);
    }

    fclose (fout);
    fclose(fin);

    return 0;
}