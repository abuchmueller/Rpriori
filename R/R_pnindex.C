#include <R.h>
#include <Rdefines.h>
#include <time.h>

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