// Efficient interning of strings to Names.

// For the SVM project, this is probably overkill.  If you like data structures,
// read the paper cited below.

/* 
Adapted from code described in "Ternary Search Trees" by Jon
Bentley and Robert Sedgewick in the April, 1998, Dr. Dobb's Journal.
*/

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "name.h"

typedef struct Name *Tptr;
typedef struct Name {
  char splitchar;
  Tptr lokid, eqkid, hikid;
} Tnode;
Tptr root;

static inline const char *copy(const char *name, int len) {
  char *s = malloc(len + 1);
  assert(s);
  strncpy(s, name, len);
  s[len] = 0;
  return s;
}

static inline Tptr init(Tptr p, char c) {
  if (p == NULL) {
    p = malloc(sizeof(*p));
    assert(p);
    p->splitchar = c;
    p->lokid = p->eqkid = p->hikid = NULL;
  }
  return p;
}


static Tptr insertl(const char *name, int len) {
    const char *s = name;
    Tptr *np = &root;
    int origlen = len;
    Tptr thisnode;
    while (len >= 0) {
      char c = len == 0 ? '\0' : *s;
      assert (len == 0 || c != 0);  // needed to overload eqkid
      *np = init(*np, c);
      Tptr p = *np;
      // now: p points to a node that is ready to consume a character
      if (c == p->splitchar) {
        np = &p->eqkid;
        thisnode = p;
        s++, len--;
      } else if (c < p->splitchar) {
        np = &p->lokid;
      } else {
        np = &p->hikid;
      }
    }
    assert(np == &thisnode->eqkid);
    if (*np == NULL)
      *np = (Tptr) copy(name, origlen);
    return thisnode;
}

static void cleanup(Tptr p) {
  if (p) {
    cleanup(p->lokid);
    cleanup(p->hikid);
    if (p->splitchar != '\0')
      cleanup(p->eqkid);
    else
      free(p->eqkid);
    free(p);
  }
}

void name_cleanup(void) {
  cleanup(root);
  root = NULL;
}

const char* nametostr(Name np) {
  return (const char *)np->eqkid;
}

Name strtoname(const char *s) {
    return strtonamel(s, strlen(s));
}

Name strtonamel(const char *s, int length) {
  return insertl(s, length);
}
