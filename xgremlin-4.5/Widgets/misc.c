/*  
 * misc.c -- Miscellaneous utility functions
 *
 * (c) Copyright 1988, 1989 Sundar Narasimhan,
 *     MIT Artificial Intelligence Laboratory,
 *     Cambridge, MA 02139 USA.
 *     See ../README for details.
 */

/*
 * modified for XGremlin, Ulf Griesmann 5/95
 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <ctype.h>
#include <stdarg.h>
#include <time.h>

/*
 * local prototypes
 */
char *
string_gets( FILE *, int * );

/*
  file_execute(filename, funct, eatwspace, cchar, argdata)
  char *filename;
  int (*funct)();
  int eatwspace;
  char cchar;
  char *argdata;

  This function provides a simple-minded way of parsing data files.
  The first argument must be a filename. The second argument is a 
  pointer to a function that will be invoked for each line in the 
  file. If the third argument is non-NULL, then the line passed
  to the second argument will not contain any leading whitespace 
  characters (spaces or tabs). The next argument is a single
  character that will enable lines to be treated as comments.
  Any line containing this character as the first character will
  be ignored by this function. The last argument will be passed
  into the calling function which must be written to take three
  arguments.
          funct(line, data, linenumber)
	  char *line, *data;
	  int linenumber;
  where the first argument points to the line itself, the second
  is the argument passed in by the user and the third argument
  is the line number associated with the current line.
*/

int
file_execute_fp(fp, funct, eatwspace, cchar, argdata)
FILE *fp;
int (*funct)();
int eatwspace;
char cchar;
char *argdata;
{
    int lineno = 0, count = 0;

    while (!feof(fp)) {
	char *string = string_gets(fp, &count);
	char *tem;
	lineno++;

	tem = string;

	if (string == NULL) 
	  goto abort;

	if (eatwspace) 
	  while(*tem == ' ' || *tem == '\t') tem++;

	if (((cchar != '\0') && (*tem == cchar)) ||
	    ((*tem == '\0') && eatwspace))
	  continue;

	/* we have reached a non empty line */
	if (funct != NULL) {
	    if ((*funct)(tem, argdata, lineno) < 0) {
		free ((char *)string);
		lineno = (int) ftell(fp);
		goto abort;
	    }
	}

	free((char *)string);
    }

  abort:
    return(lineno);
}


int
file_execute(filename, funct, eatwspace, cchar, argdata)
char *filename;
int (*funct)();
int eatwspace;
char cchar;
char *argdata;
{
    FILE *fp;
    int val;

    if (strcmp(filename, "-") == 0 ||
	strcmp(filename, "stdin") == 0)
      fp = stdin;
    else {
	if ((fp = fopen(filename, "r")) == NULL) {
	    fprintf(stderr,"file_exec: couldn't open file '%s'\n", filename);
	    return -1;
	}
    }
    val = file_execute_fp(fp, funct, eatwspace, cchar, argdata);
    if (fp != stdin)
      fclose(fp);
    return (val);
}


/*
  string_gets(fp, count_chars)
  FILE *fp;
  int  *count_chars;

  This is a version of gets() that allocates memory as it goes.
  So it wont barf on very long strings. It breaks the input
  at new lines and returns C strings or NULL on EOF.
*/
#define BUFFER_SIZE           80
char *
string_gets(fp, count_chars)
FILE *fp;
int  *count_chars;
{
    char *str = NULL;
    int  maxsize = 0;
    int  count = 0;
    int  c;

    while(!feof(fp) && ((c = getc(fp)) != EOF)) {
        if((count + 1) >= maxsize) {
            if(str == NULL) {
                str = (char *) calloc(BUFFER_SIZE, sizeof(char));
                maxsize = BUFFER_SIZE;
            } else {
                str = (char *) realloc(str, (maxsize * 2) * sizeof(char));
                maxsize = 2 * maxsize;
            }
            if(str == NULL) {
                fprintf(stderr, "calloc failure\n");
                abort();
            }
        }
        if(c == '\n')
          break;
        str[count] = (char) c;
        count++;
    }
    if(str != NULL)
      str[count] = '\0';
    *count_chars = count;
    return((char *) str);
}

 
/* 
 * miscellaneous time functions 
 */
static char *long_month_names[] = 
{ "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December" };

char *
time_to_string(int format)
{
    time_t t;
    struct tm *tmp;
    static char buf[128];

    t = time(0);
    tmp = (struct tm *)localtime(&t);

    switch (format) {
      case 0:
	/* January 1, 1991 [10:20] */
	sprintf(buf, "%s %d, %d [%02d:%02d]",
		long_month_names[tmp->tm_mon],
		tmp->tm_mday, tmp->tm_year + 1900, tmp->tm_hour, tmp->tm_min);
	break;
      case 1:
	/* 02/20/91 */
	sprintf(buf, "%02d/%02d/%d", tmp->tm_mon+1, tmp->tm_mday, tmp->tm_year);
	break;
      case 2:
	/* 02/20/91 [10:20] */
	sprintf(buf, "%02d/%02d/%d [%02d:%02d]", tmp->tm_mon+1, 
		tmp->tm_mday, tmp->tm_year, tmp->tm_hour, tmp->tm_min);
	break;
      default:
	/* January 3, 1990 */
	sprintf(buf, "%s %d, %d",
		long_month_names[tmp->tm_mon],
		tmp->tm_mday, tmp->tm_year + 1900);
	break;
    }
    return ((char *) buf);
}

