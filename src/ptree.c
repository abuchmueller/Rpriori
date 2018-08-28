#include <R.h>
#include <Rdefines.h>
#include <time.h>

/*
 support counting and rule generation for user-supplied
 sets of itemsets. counts are stored in memory-efficient
 prefix-trees. 
 
 warning: the code is not thread-safe, but I guess so is
 most of the R source code.
 
 todo: 1) optimize insertion of left nodes. 2) check for
 special cases, such as all itemsets or transactions
 are empty, or the set of items is empty. 3) register
 pointer array for finalizing.
 
 note that sgCMatrix support is for package arulesSequence.
 
 Version 0.2-7
 
 (C) ceeboo 2007, 2016
 */

typedef struct pnode {
  int index;
  int count;
  struct pnode *pl;
  struct pnode *pr;
} PN;

static PN *nq, **nb = NULL;		    /* node pointers */ 
static int npn, cpn, apn;		    /* node counters */

static void pnfree(PN *p) {
  if (p == NULL)
    return;
  pnfree(p->pl);
  pnfree(p->pr);
  free(p);
  apn--;
}

static void nbfree() {
  pnfree(*nb);
  free( nb);
  nb = NULL;
}

static PN *pnadd(PN *p, int *x, int n) {
  if (n == 0)
    return p;
  cpn++;
  if (p == NULL) {			    /* append node */
p = nq = (PN *) malloc(sizeof(PN));
    if (p) {
      apn++;
      p->index = *x;
      p->count = 0;
      p->pr = NULL;
      p->pl = pnadd(NULL, x+1, n-1);
    } else
      npn = 1;
  } else
    if (p->index == *x) {		    /* existing node */
nq = p;
      p->pl = pnadd(p->pl, x+1, n-1);
    } else
      if (p->index < *x) {		    /* search right subtree */
nq = p;
        p->pr = pnadd(p->pr, x, n);
      } else {				    /* prepend node */
PN *q = nq = (PN *) malloc(sizeof(PN));
        if (q) {
          apn++;
          q->index = *x;
          q->count = 0;
          q->pr = p;
          q->pl = pnadd(NULL, x+1, n-1);
          p = q;
        } else
          npn = 1;
      }
      return p;
}

/* retrieve count */

static int pnget(PN *p, int *x, int n) {
  if (p == NULL || n == 0)
    return 0;
  cpn++;
  if (p->index == *x) {
    npn++;
    if (n == 1)
      return p->count;
    return pnget(p->pl, x+1, n-1);
  }
  if (p->index < *x) 
    return pnget(p->pr, x, n);
  return 0;				    /* set not found */
}


/*
 note that we do not drop rules with zero support
 as filtering is done later anyway. in this case, 
 confidence and lift can either be zero or NaN, 
 depending on the support of the left-hand side. 
 */

/* index itemsets */

static void pnindex(PN *p) {
  if (p == NULL)
    return;
  cpn++;
  if (p->count)
    p->count = npn++;
  pnindex(p->pl);
  pnindex(p->pr);
}

SEXP R_pnindex(SEXP R_x, SEXP R_y, SEXP R_v) {
  int i, k, f, l, n, nr, e;
  int *x;
  SEXP r, px, ix, py, iy;
#ifdef _TIME_H
  clock_t t2, t1 = clock();
#endif
  
  if (!inherits(R_x, "ngCMatrix") && 
      !inherits(R_x, "sgCMatrix"))
      error("'x' not of class ngCMatrix");
  if (!isNull(R_y) && !inherits(R_y, "ngCMatrix") 
        && !inherits(R_x, "sgCMatrix"))
    error("'y' not of class ngCMatrix");
  if (TYPEOF(R_v) != LGLSXP)
    error("'v' not of type logical");
  
#ifdef _TIME_H
  if (LOGICAL(R_v)[0] == TRUE)
    Rprintf("indexing ... ");
#endif
  
  nr = INTEGER(GET_SLOT(R_x, install("Dim")))[0];
  
  px = py = GET_SLOT(R_x, install("p"));
  ix = iy = GET_SLOT(R_x, install("i"));
  
  if (!isNull(R_y)) {
    if (nr != INTEGER(GET_SLOT(R_y, install("Dim")))[0])
      error("'x' and 'y' not the same number of rows");
    
    py = GET_SLOT(R_y, install("p"));
    iy = GET_SLOT(R_y, install("i"));
  }
  
  if (nb != NULL)
    nbfree();
  nb = (PN **) malloc(sizeof(PN *) * (nr+1));
  if (nb == NULL)
    error("pointer array allocation failed");
  
  cpn = apn = npn = 0;
  
  k = nr;
  nb[k] = NULL;
  while (k-- > 0)
    nb[k] = pnadd(nb[k+1], &k, 1);
  
  if (npn) {
    nbfree();
    error("node allocation failed");
  }
  
  f = e = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    n = l-f;
    if (n == 0) {
      if (e == 0)
        e = i;
      continue;
    }
    x = INTEGER(ix)+f;
    pnadd(nb[*x], x, n);
    if (npn) {
      nbfree();
      error("node allocation failed");
    }
    if (nq->count == 0)
      nq->count = i;
    f = l;
    R_CheckUserInterrupt();
  }
  
  PROTECT(r = allocVector(INTSXP, LENGTH(py)-1));
  
  if (isNull(R_y)) {
    e = 0;
    cpn = 0;
    npn = 1;
    
    pnindex(*nb);
  }
  
  cpn = npn = 0;
  
  f = 0;
  for (i = 1; i < LENGTH(py); i++) {
    l = INTEGER(py)[i];
    n = l-f;
    if (n == 0) {
      INTEGER(r)[i-1] = e;
      continue;
    }
    x = INTEGER(iy)+f;
    k = pnget(nb[*x], x, n);
    INTEGER(r)[i-1] = (k > 0) ? k : 0;
    f = l;
    R_CheckUserInterrupt();
  }
  
  nbfree();
  
  if (apn)
    error("node deallocation imbalance %i", apn);
#ifdef _TIME_H
  t2 = clock();
  if (LOGICAL(R_v)[0] == TRUE)
    Rprintf(" %i itemsets [%.2fs]\n", LENGTH(px) - 1, 
            ((double) t2-t1) / CLOCKS_PER_SEC);
#endif
  
  UNPROTECT(1);
  
  return r;
}


