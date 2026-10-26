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
typedef struct {int is_float; long long ival; double dval; } Value;

static Value mk_int(long long v) { Value r; r.is_float = 0; r.ival = v; r.dval = 0; return r; }
static Value mk_flt(double v) { Value r; r.is_float = 1; r.ival = 0; r.dval = v; return r; }
static double as_double(Value v) { return v.is_float ? v.dval : (double)v.ival;}

static Value parse_expr(Parser* p);

static Value parse_primary(Parser* p) {
  if (p->has_error) return mk_int(0);
  Token t = p->cur;
  if (t.type == T_NUM_INT) { next_token(p); return mk_int(t.ival);}
  if (t.type == T_NUM_FLOAT) { next_token(p); return mk_flt(t.dval); }
  if (t.type == T_LPAREN) {
    next_token(p);
    Value v = parse_expr(p);
    if (p->has_error) return v;
    if (p->cur.type != T_RPAREN) { set_error(p, p->cur.pos, "missing )"); return v;
    } 
    return v;     
  }

  if (t.type == T_END) { set_error(p, t.pos, "missing operand"); return mk_int(0); }
  if (t.type == T_ERROR) { set_error(p, t.pos, "invalid character"); return mk_int(0); }

set_error(p, t.pos, "unexpected operator");
return mk_int(0);
}
static Value parse_power(Parser* p) {
  Value base = parse_primary(p);
  if (p->has_error) return base;
  if (p->cur.type == T_POW) {
    long op_pos = p->cur.pos;
    next_token(p);
    long exp_pos = p->cur.pos;
    Value exp = parse_power(p);
    if (p->has_error) return base;

    if(!base.is_float && !exp.is_float) {
      if (exp.ival < 0) { set_error(p, exp_pos, "negative exponent "); return base; }
      long long r = 1, b = base.ival, e = exp.ival;
      while (e > 0) { if (e & 1) r *= b; b *= b; e >>=1; }
      return mk_int(r);
 
    } else {
      double r = pow(as_double(base), as_double(exp));
      if (!isfinite(r)) {set_error(p, op_pos, "float result not finite"); return base; }
      return mk_flt(r);
    }
  }
  return base;

}
static Value parse_term(Parser* p) {
  Value v = parse_power(p);
  while (!p->has_error && (p->cur.type == T_STAR || p->cur.type == T_SLASH)){
    TokType op = p->cur.type;
    next_token(p);
    long rhs_pos = p->cur.pos;
    Value rhs = parse_power(p);
    if (p->has_error) return v;
    if (op == T_STAR) {
      if (!v.is_float && !rhs.is_float) v = mk_int(v.ival * rhs.ival);
      else {
        double r = as_double(v) * as_double(rhs);
        if (!isfinite(r)) { set_error(p, rhs_pos, "overflow"); return v; }
        v = mk_flt(r);
      }
    } else {
      if (!v.is_float && !rhs.is_float) {
        if (rhs.ival == 0 ) { set_error(p, rhs_pos, "division by zero"); return v; }
        v = mk_int(v.ival / rhs.ival);
      }
      else {
        if (as_double(rhs) == 0.0) { set_error(p, rhs_pos, "division by zero"); return v; }
        v = mk_flt(as_double(v) / as_double(rhs));
      }
    }
  }
  return v;
}
static Value parse_expr(Parser* p) {
  Value v = parse_term(p);
  while (!p->has_error && (p->cur.type == T_PLUS || p->cur.type == T_MINUS)) {
    TokType op = p->cur.type;
    next_token(p);
    Value rhs = parse_term(p);
    if (p->has_error) return v;
    if (op == T_PLUS) {
      if (!v.is_float && !rhs.is_float) v = mk_int(v.ival + rhs.ival) ;
      else v = mk_flt(as_double(v) + as_double(rhs));   
    } else {
      if (!v.is_float && !rhs.is_float) v = mk_int(v.ival - rhs.ival);
      else v = mk_flt(as_double(v) - as_double(rhs));
    }
  }
  return v;
}
static int process_file(const char* inpath, const char *outpath) {
  Buf b = read_file(inpath);
  FILE *out = fopen(outpath, "wb");
  if (!out) { fprintf(stderr, "error: cannot open output file\n"); free(b.data); return 1; }

  size_t i =0; 
  while (i < b.len) {
    size_t j = i;
    while (j < b.len && b.data[j] != '\n') j++;

    size_t k = i;
    while (k < j && (b.data[k] == ' ' || b.data[k] == '\t' || b.data[k] == '\r')) k++;
    
    if (k != j && b.data[k] != '#') {
      Parser p = {0};
      p.buf = b.data;

      p.len = (j < b.len) ? j + 1 : j;
      p.idx = i;

      next_token(&p);
      Value v = parse_expr(&p);

      if (!p.has_error && p.cur.type != T_END) {
        
      }
    }
  }
}