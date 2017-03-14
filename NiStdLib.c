#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* standard library for the Ni compiler */

struct string
{
    long len;
    char *str;
};

struct array
{
  long len;
  long *arr;
};


long stringCompare(struct string *str1, struct string *str2)
{
    return (long)strcmp(str1->str, str2->str);
}
struct array *makeArray(long numElements)
{
  // the total size of any array is the number of elements *
  // the number of bytes in a long, which is 8, this is because
  // in Ni, a string is a pointer, so it's 8 bytes, an int is a 
  // long, so it's 8 bytes, and a record is a pointer so it's 
  // 8 bytes. Even with "nested" records and such, they're always
  // pointers, so 8 bytes
  long total_size = sizeof(struct array) + numElements * sizeof(long);

  // create an array
  //char *bytes = malloc(total_size);
  char *bytes = calloc(total_size, 1);

  struct array *arr = (struct array *)bytes;

  // zero out the memory
  //memset(bytes, 0, total_size);

  // set the len
  arr->len = total_size - sizeof(struct array);
  // set the array pointer to point after the struct
  arr->arr = (long *)(&bytes[sizeof(struct array)]);

  return arr;
}

long arraySize(struct array *arr)
{
  return arr->len;
}

long *getElementAddressAt(struct array *arr, long index)
{
  if (index < arr->len && index >= 0)
    return &arr->arr[index];


  exit(-1);
  return 0;
}

void flush()
{
  fflush(stdout);
}


struct string *makeString(const char *str)
{
    long strsize = strlen(str); // include null terminator to make C interaction easier
    long structsize = sizeof(struct string);

    // note we add +1 so we copy the null terminator
    char *newstr = (char *)calloc(1, structsize + strsize + 1);

    strncpy(&newstr[structsize], str, strsize + 1);

    // now copy the long into the first bytes
    struct string *newstring = (struct string *)newstr;
    newstring->len = strsize;
    newstring->str = &newstr[structsize];
    return newstring;
}

#undef getchar

struct string *getChar()
{
  char str[2];
  str[1] = 0;
  str[0] = getc(stdin);

  return makeString(&str[0]);
}

long ord(struct string *str)
{
  return str->str[0];
}

struct string *chr(long i)
{
  char str[2];
  str[1] = 0;
  str[0] = (char)i;
  return makeString(&str[0]);
}

long size(struct string *str)
{
  return str->len;
}

struct string *substring(struct string *str, int first, int n)
{
  if (first >= 0 && 
      first < str->len && 
      (first + n) < str->len)
  {
    char *strd = strndup(&(str->str[first]), n);
    struct string *sstr = makeString(strd);
    free(strd);
    return sstr;
  }

  exit(-1);
  return 0;
}

struct string *concat(struct string *s1, struct string *s2)
{
  char *newstr = malloc(s1->len + s2->len + 1);
  strncpy(&newstr[0], s1->str, s1->len);
  strncpy(&newstr[s1->len], s2->str, s2->len + 1);

  struct string *sstr = makeString(newstr);
  free(newstr);
  return sstr;
}

long not(long i)
{
  return (i == 0);
}

void Exit(long i)
{
  exit((int)i);
}

void print(struct string *str)
{
    printf("%s", str->str);
}

void printi(long v)
{
    printf("%ld", v);
}

struct string *intToString(long val)
{
    char *str = (char *)malloc(32);
    snprintf(str, 32, "%ld", val);
    struct string *newstr = makeString(str);
    free(str);
    return newstr;
}

