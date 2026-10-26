// Jishan Kaliparamban didesh [251ADB016]
// https://github.com/<your-username>/calc-lab-home
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used: ChatGPT (conversation history is provided in README.md),
// standard C library documentation.

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

typedef enum {
  T_NUM_INT,
  T_NUM_FLOAT,
  T_PLUS,
  T_MINUS,
  T_STAR,
  T_SLASH,
  T_POW,
  T_LPAREN,
  T_RPAREN,
  T_END,
  T_ERROR
} TokType;

typedef struct {
  TokType type;
  long long ival;
  double dval;
  long pos;
} Token;

typedef struct {
  const char* buf;
  size_t len;
  size_t idx;
  Token cur;
  int has_error;
  long err_pos;
} Parser;

typedef struct {
  char* data;
  size_t len;
} Buf;

static void die(const char* msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

static Buf read_file(const char* path) {
  FILE* f = fopen(path, "rb");
  if (!f) die("error: cannot open input file");
  size_t cap = 1 << 16, len = 0;
  char* data = malloc(cap);
  if (!data) die("error: out of memory");
  for (;;) {
    if (len == cap) {
      cap *= 2;
      char* tmp = realloc(data, cap);
      if (!tmp) {
        free(data);
        die("error: out of memory");
        data = tmp;
      }
    }
    size_t got = fread(data + len, 1, cap - len, f);
    len += got;
    if (got == 0) {
      if (ferror(f)) {
        fclose(f);
        free(data);
        die("error: read failed");
      }
      break;
    }
  }
  fclose(f);
  Buf b = {data, len};
  return b;
}
static int mkdir_p(const char* path) {
  if (mkdir(path, 0777) == 0) return 0;
  if (errno == EEXIST) return 0;
  return -1;
}
static void skip_ws(Parser* p) {
  while (p->idx < p->len) {
    char c = p->buf[p->idx];
    if (c == ' ' || c == '\t' || c == '\r')
      p->idx++;
    else
      break;
  }
}
static void next_token(Parser* p) {
  skip_ws(p);
  if (p->idx >= p->len) {
    p->cur.type = T_END;
    p->cur.pos = (long)p->len + 1;
    return;
  }
  if (p->buf[p->idx] == '\n') {
    p->cur.type = T_END;
    p->cur.pos = (long)p->idx + 1;
    p->idx++;
    return;
  }
  char c = p->buf[p->idx];
  long pos = (long)p->idx + 1;

  if (isdigit((unsigned char)c)) {
    size_t start = p->idx;
    while (p->idx < p->len && isdigit((unsigned char)p->buf[p->idx])) p->idx++;
    int is_float = 0;
    if (p->idx < p->len && p->buf[p->idx] == '.') {
      is_float = 1;
      p->idx++;
      while (p->idx < p->len && isdigit((unsigned char)p->buf[p->idx]))
        p->idx++;
    } else {
      p->cur.type = T_ERROR;
      p->cur.pos = (long)p->idx + 1;
      p->idx++;
      return;
    }
    if (is_float) {
      p->cur.type = T_NUM_FLOAT;
      p->cur.dval = strtod(p->buf + start, NULL);
      p->cur.pos = pos;
      if (p->idx < p->len) {
        char t = p->buf[p->idx];
        if (t == 'e' || t == 'E') {
          p->cur.type = T_ERROR;
          p->cur.pos = (long)p->idx + 1;
          p->idx++;
        }
      }

    } else {
      p->cur.type = T_NUM_INT;
      p->cur.ival = strtoll(p->buf + start, NULL, 10);
      p->cur.pos = pos;
    }
    return;
  }
  if (c == '*') {
    if (p->idx + 1 < p->len && p->buf[p->idx + 1] == '*') {
      p->cur.type = T_POW;
      p->idx += 2;

    } else {
      p->cur.type = T_STAR;
      p->idx += 1;
    }
    p->cur.pos = pos;
    return;
  }
  if (c == '+') {
    p->cur.type = T_PLUS;
    p->idx++;
    p->cur.pos = pos;
    return;
  }
  if (c == '-') {
    p->cur.type = T_MINUS;
    p->idx++;
    p->cur.pos = pos;
    return;
  }
  if (c == '/') {
    p->cur.type = T_SLASH;
    p->idx++;
    p->cur.pos = pos;
    return;
  }
  if (c == '(') {
    p->cur.type = T_LPAREN;
    p->idx++;
    p->cur.pos = pos;
    return;
  }
  if (c == ')') {
    p->cur.type = T_RPAREN;
    p->idx++;
    p->cur.pos = pos;
    return;
  }
  p->cur.type = T_ERROR;
  p->cur.pos = pos;
  p->idx++;
  
}
static void set_error(Parser *p, long pos, const char *msg){
  (void)msg;
  if(!p->has_error) {p->has_error = 1; p->err_pos = pos; }
}
typedef struct {int is_float; long long ival; double dval; } value;

static Value 
