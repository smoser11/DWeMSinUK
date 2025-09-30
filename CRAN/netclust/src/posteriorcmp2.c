/*******************************************************************/
/* Computation of the log-likelihood and marginal posterior of size*/
/*******************************************************************/

#include "posteriorcmp2.h"
#include "cmp2.h"
#include <R.h>
#include <Rmath.h>
#include <math.h>

void gcmp3 (int *pop,            /* Sample degrees and a vector of 0s up to max N*/
int *nk,        /* VECTOR of counts for each degree k in sample */
int *m,              /* Number of clusters */
double *propsprior,      /* Parameters for dirichlet */
double *concent,      /* Concentration parameter for dirichlet */
int *K,              /* Max degree size */
int *n,              /* Sample size */
int *nc,              /* Sample size per cluster */
double *ppos,         /* m*K  vector of zeros */
int *samplesize,     /* Number of MC samples */
int *burnin,         /* Length of burn-in */
int *interval,       /* Number of proposals between statistics */
double *mu,          /* mean.prior.degree: mean of unit size prior */
double *dfmu,        /* df.mean.prior: 1*/
double *sigma,       /* sd.prior.degree: sd of unit size prior*/
double *dfsigma,     /* df.sd.prior: 3*/
double *lnlamproposal,   /* sd of proposal distribution for mean degree*/
double *nuproposal,      /* sd of proposal distribution for sd of degree*/
double *concentproposal,  /* concentration of proposal distribution for p*/
int *N,              /* prior$N */
int *maxN,           /* Max N */
double *sample,      /* samplesize * dimsample (parallel?)*/
double *lpriorm,     /* log prior values over values of N */
int *burnintheta,    /* burnin for eta (theta) values in MCMC */
int *verbose         /* print out */
) {
  int dimsample, Np;
  int step, staken, getone=1, intervalone=1, verboseMHcmp = 0;
  int i, j, k, ni, Ni, Ki, mi, isamp, iinterval, isamplesize, iburnin, csum;
  double dsamp, muplace, sigmaplace;
  int sizei, imaxN, imaxm, give_log0=0, give_log1=1;
  int maxpop;
  double pis, Nd;
  double temp;
  double errval=0.0000000001, lzcmp;

  GetRNGstate();  /* R function enabling uniform RNG */

ni=(*n);
Ni=(*N);
Ki=(*K);
mi=(*m);
imaxN=(*maxN);
imaxm=imaxN-ni;
isamplesize=(*samplesize);
iinterval=(*interval);
iburnin=(*burnin);
Np=0;

dimsample=(5*mi)+1+Np;

double *pi = (double *) malloc(sizeof(double) * (Ki*mi));
int *d = (int *) malloc(sizeof(int) * (ni));
int *b = (int *) malloc(sizeof(int) * ni);
int *tU = (int *) malloc(sizeof(int) * mi);
int *Nc = (int *) malloc(sizeof(int) * mi);
int *Nk = (int *) malloc(sizeof(int) * (Ki*mi));
int *Nkplace = (int *) malloc(sizeof(int) * Ki);
int *Nkpos = (int *) malloc(sizeof(int) * (Ki*mi));
double *props = (double *) malloc(sizeof(double) * mi);
double *dmu = (double *) malloc(sizeof(double) * mi);
double *dsigma = (double *) malloc(sizeof(double) * mi);
double *r = (double *) malloc(sizeof(double) * mi);
double *lnlami = (double *) malloc(sizeof(double) * mi);
double *nui = (double *) malloc(sizeof(double) * mi);
double *gammart = (double *) malloc(sizeof(double) * mi);
double *lpm = (double *) malloc(sizeof(double) * imaxm);
double *lpp = (double *) malloc(sizeof(double) * 1000);
double *pdegi = (double *) malloc(sizeof(double) * (Np+1));
double *psample = (double *) malloc(sizeof(double) * (Np+1));
double *lnlamsample = (double *) malloc(sizeof(double));
double *nusample = (double *) malloc(sizeof(double));
double *lnlamsamplefull = (double *) malloc(sizeof(double));
double *nusamplefull = (double *) malloc(sizeof(double));

maxpop=0;
/* Set d to the sample degree sizes */
/* Set 0s to 1s and max at Ki*/

k=0;
for(i=0; i<mi; i++){
  for(j=(i*imaxN); j<(i*imaxN + nc[i]); j++){
    if((pop[j]>0) && (pop[j] <= Ki)){ d[k]=pop[j];}
    if(pop[j]==0){ d[k]=1;}
    if(pop[j]>Ki){ d[k]=Ki;}
    if(pop[j]>maxpop){maxpop=pop[j];}
    k = k + 1;
  }
  dsigma[i]=sigma[i];
  dmu[i]=mu[i];
}

/* B is the backwards cumulative sum of the sample */
/* Remaining degrees in the sample at step k */

csum = 0;
for(i=0; i<mi; i++){
  csum = csum + nc[i];
  b[csum - 1] = d[csum - 1];
  if (i==0){
    for(j=(csum - 2); j>=0; j--){
      b[j] = b[j + 1] + d[j];
    }
  }
  if (i!=0){
    for(j=(csum - 2); j>=(csum - nc[i]); j--){
      b[j] = b[j + 1] + d[j];
    }
  }
}

/* Vector of counts for each degree k in population by cluster */
/* Nkpos and ppos are 0 vectors of length K*m*/

for (i=0; i<(Ki*mi); i++){
  Nk[i]=nk[i];
  Nkpos[i]=0;
  ppos[i]=0.0;
}

/* tU is the sum of degrees from units n+1 to N */
csum = 0;
for(i=0; i<mi; i++){
  csum = csum + nc[i];
  tU[i]=0;
  props[i]=propsprior[i];
  Nc[i]=(int)(round(props[i]*(double)(Ni)));
  for (j=(i*imaxN + nc[i]); j<((i + 1)*imaxN); j++){
    pop[j]=d[(int)trunc(10*unif_rand()+csum-10)];
  }
  for (j=(i*imaxN + nc[i]); j<(i*imaxN + Nc[i]); j++){
    //pop[j]=d[(int)trunc(10*unif_rand()+csum-10)];
    tU[i]+=pop[j];
  }
}

/* Draw initial phis */
/* tU[i] + b[j] is r_ij from paper*/
/* So each step is phi_ij (random exp(1)/rate parameter)*/
/* r is the sum of all phi_i*/

csum=0;
for (i=0; i<mi; i++){
  r[i]=0.;
  for(j=csum; j<(csum + nc[i]); j++){
    r[i]+=(exp_rand()/(tU[i]+b[j]));
  }
  csum = csum + nc[i];
}


/* initialize ln(mean) and sd of unit size prior*/
/* Changed this so lnlami and nui hold the initial values for each cluster */
for(i=0; i<mi; i++){
  lnlami[i] = dmu[i];
  nui[i] = dsigma[i];
}

isamp = 0;
step = -iburnin;

/* Begin MCMC */
while (isamp < isamplesize) {

  /* Draw new theta */
  /* but less often than the other full conditionals */
  if (step == -iburnin || step==(10*(step/10))) {
    for (i=0; i<mi; i++){

      /* lnlam and nu are initialized by cluster now */
      lnlamsample[0] = lnlami[i];
      nusample[0] = nui[i];
      muplace = dmu[i];
      sigmaplace = dsigma[i];

      /* Defining the Nk by cluster*/
      for(j=0; j<Ki; j++){
        Nkplace[j] = Nk[i*Ki + j];
      }

      MHcmptheta2(Nkplace,K,&muplace,dfmu,&sigmaplace,dfsigma,lnlamproposal,nuproposal,
                  &Nc[i],&Np, psample,
                  lnlamsample, nusample, &getone, &staken, burnintheta, &intervalone,
                  &verboseMHcmp);
      /* Gives samples lnlamsample and nusample from the posterior */
      /* for lnlambda and nu (theta)*/

      lnlami[i]=lnlamsample[0];
      nui[i]=nusample[0];
    }
  }
  /* Compute the unit distribution (given the new theta = (lnlam, nu))
   for each cluster */


  for(i=0; i<mi; i++){
    pis=0.;
    lzcmp = zcmp2(exp(lnlami[i]), nui[i], errval, 2*Ki, give_log1);
    pi[i*Ki]=cmp2(1,lnlami[i],nui[i],lzcmp,give_log0);
    pis+=pi[i*Ki];

    k=1;
    for (j=(i*Ki + 1); j<((i+1)*Ki - 1); j++){
      pi[j]=pi[j-1]*exp(lnlami[i]-nui[i]*log((double)(k+1)));
      pis+=pi[j];
      k+=1;
    }

    for (j=(i*Ki); j<((i+1)*Ki - 1); j++){
      pi[j]/=pis;
    }

    pis=1.;
    for (j=(i*Ki); j<((i+1)*Ki - 2); j++){
      pis-=pi[j];
    }
    pi[(i+1)*Ki - 1]=pis;
  }

  /* Draw p */
  for(i=0; i<mi; i++){
    gammart[i] = 0.;
    for (j=0; j<Ki; j++){
      gammart[i]+=(exp(-r[i]*(j+1))*pi[i*Ki + j]);
    }
    gammart[i]=log(gammart[i]);
  }

  MHcmpp2(propsprior, concent, concentproposal, props,
          K, m, &Ni, nc, gammart, &Np, psample, &getone,
          &staken, burnintheta, &intervalone, &verboseMHcmp);

  /* Draw new N */

  temp = -100000000.0;

  for (i=0; i<imaxm; i++){
    lpm[i] = 0.;
    for(j=0; j<mi; j++){
      lpm[i] += round(props[j]*(i + ni) - nc[j])*gammart[j];
      lpm[i] += lgamma(round(props[j]*(i + ni) + 1.));
      lpm[i] -= lgamma(round(props[j]*(i + ni) - nc[j] + 1.));
    }
    //    Add in the (log) prior on m: P(m)
    lpm[i]+=lpriorm[i];

    for(j=0; j<mi; j++){
      if(round(props[j]*(i + ni) - nc[j]) <= 0){
        lpm[i] = -100000000.0;
      }
    }
    if(lpm[i] > temp) temp = lpm[i];
  }

  // N = m + n

  // Standardize and un-log
  for (i=0; i<imaxm; i++){
    lpm[i]=exp(lpm[i]-temp);
  }

  // Cumulative
  for (i=1; i<imaxm; i++){
    lpm[i]=lpm[i-1]+lpm[i];
  }
  temp = lpm[imaxm-1] * unif_rand();
  for (Ni=0; Ni<imaxm; Ni++){
    if(temp <= lpm[Ni]) break;
  }
  // Add back the sample size
  Ni += ni;
  if(Ni > imaxN) Ni = imaxN;



  /* Draw phis */

  for(i=0; i<mi; i++){
    tU[i]=0;
    Nc[i]=(int)(round(props[i]*(double)(Ni)));
    for (j=(i*imaxN + nc[i]); j<(i*imaxN + Nc[i]); j++){
      tU[i]+=pop[j];
    }

  }

  csum=0;
  for (i=0; i<mi; i++){
    r[i]=0.;
    for(j=csum; j<(csum + nc[i]); j++){
      r[i]+=(exp_rand()/(tU[i]+b[j]));
    }
    csum = csum + nc[i];
  }

  /* Draw unseen sizes */
  for (i=0; i<(Ki*mi); i++){
    Nk[i]=nk[i];
  }

  for (i=0; i<mi; i++){
    // Set up pi to be cumulative for random draws
    for (j=(i*Ki + 1); j<((i + 1)*Ki); j++){
      pi[j]=pi[j-1]+pi[j];
    }

    for (j=nc[i]; j<Nc[i]; j++){
      /* Propose unseen size for unit j in cluster i */
      /* Use rejection sampling */
      sizei=1000000;
      while(sizei >= Ki){
        sizei=1000000;
        while(log(1.0-unif_rand()) > -r[i]*sizei){
          /* Now propose unseen size for unit i */
          /* In the next two lines a sizei is chosen */
          /* with parameters lnlami and nui */
          temp = unif_rand();
          for (sizei=1; sizei<=Ki; sizei++){
            if(temp <= pi[i*Ki + sizei - 1]) break;
          }
        }
      }
      pop[i*imaxN + j]=sizei;
      Nk[i*Ki + sizei-1]=Nk[i*Ki + sizei-1]+1;
    }
  }

  if (step > 0 && step==(iinterval*(step/iinterval))) {
    /* record statistics for posterity */
    Nd=(double)Ni;
    sample[isamp*dimsample  ]=Nd;
    for(i=0; i<mi; i++){
      sample[isamp*dimsample+1+i]=lnlami[i];
      sample[isamp*dimsample+1+mi+i]=nui[i];
      sample[isamp*dimsample+1+(2*mi)+i]=props[i];
      sample[isamp*dimsample+1+(3*mi)+i]=(double)(Nk[i*Ki]);
      temp=0.0;
      for (j=0; j<Ki; j++){
        temp+=(j+1.0)*Nk[i*Ki+j];
      }
      sample[isamp*dimsample+1+(4*mi)+i]=temp;
      //sample[isamp*dimsample+1+(5*mi)+i]=tU[i];
    }
    /*for (i=0; i<Np; i++){
     sample[isamp*dimsample+5+i]=pdegi[i];
    }*/
    for (i=0; i<Ki; i++){
      Nkpos[i]=Nkpos[i]+Nk[i];
      ppos[i]+=((Nk[i]*1.)/Nd);
    }
    isamp++;
    if (*verbose && isamplesize==(isamp*(isamplesize/isamp))) Rprintf("Taken %d samples...\n", isamp);
  }
  step++;
}

dsamp=((double)isamp);
for (i=0; i<Ki; i++){
  nk[i]=Nkpos[i];
  ppos[i]/=dsamp;
}

PutRNGstate();  /* Disable RNG before returning */
    free(pi);
    free(d);
    free(psample);
    free(pdegi);
    free(b);
    free(Nk);
    free(Nkpos);
    free(lpm);
    free(lnlamsample);
    free(nusample);
}

void MHcmptheta2 (int *Nk, int *K,
                  double *mu, double *dfmu,
                  double *sigma,  double *dfsigma,
                  double *lnlamproposal,
                  double *nuproposal,
                  int *N, int *Npi, double *psample,
                  double *lnlamsample, double *nusample,
                  int *samplesize, int *staken, int *burnin, int *interval,
                  int *verbose
) {
  int Np;
  int step, taken, give_log1=1, give_log0=0;
  int i, Ki, Ni, isamp, iinterval, isamplesize, iburnin;
  double ip, cutoff;
  double mui, mustar, lnlamstar, lnlami, lp;
  double pis, pstars;
  double sigmastar, sigmai, sigma2star, sigma2i, qnustar, qnui;
  double nustar, nui;
  double pithetastar, pithetai;
  double ddfmu, rdfmu, ddfsigma, dmu;
  double dsigma, dsigma2, dlnlamproposal, dnuproposal;
  double errval=0.0000000001, lzcmp;

  //GetRNGstate();  /* R function enabling uniform RNG */

  Ki=(*K);
  Np=(*Npi);
  double *pstar = (double *) malloc(sizeof(double) * Ki);
  double *pi = (double *) malloc(sizeof(double) * Ki);
  double *odegstar = (double *) malloc(sizeof(double) * Np);
  double *odegi = (double *) malloc(sizeof(double) * Np);
  double *pdegstar = (double *) malloc(sizeof(double) * Np);
  double *pdegi = (double *) malloc(sizeof(double) * Np);

  Ni=(*N);
  isamplesize=(*samplesize);
  iinterval=(*interval);
  iburnin=(*burnin);
  ddfmu=(*dfmu);
  rdfmu=sqrt(ddfmu);
  ddfsigma=(*dfsigma);
  dsigma=(*sigma);
  dsigma2=(dsigma*dsigma);
  dmu=(*mu);
  dnuproposal=(*nuproposal);
  dlnlamproposal=(*lnlamproposal);

  // First set starting values
  isamp = taken = 0;
  step = -iburnin;
  lnlami = lnlamsample[0];
  nui = nusample[0];

  pis=0.;
  lzcmp = zcmp2(exp(lnlami), nui, errval, 2*Ki, give_log1);

  pi[Np]=cmp2(Np+1,lnlami,nui,lzcmp,give_log0);
  pis+=pi[Np];
  for (i=Np+1; i<Ki; i++){
    pi[i]=pi[i-1]*exp(lnlami-nui*log((double)(i+1)));
    pis+=pi[i];
  }
  for (i=0; i<Ki; i++){
    pi[i]/=pis;
  }
  pis=1.;
  for (i=0; i<(Ki-1); i++){
    pis-=pi[i];
  }
  pi[Ki-1]=pis;

  // Now computes mean and s.d. from log-lambda and nu
  mui=0.0;
  sigma2i=0.0;
  for (i=0; i<Ki; i++){
    mui+=pi[i]*(i+1);
    sigma2i+=pi[i]*(i+1)*(i+1);
  }
  sigma2i=sigma2i-mui*mui;

  sigmai  = sqrt(sigma2i);
  pithetai = dnorm(mui, dmu, sigmai/rdfmu, give_log1);
  pithetai = pithetai+dsclinvchisq(sigma2i, ddfsigma, dsigma2);

  // Now do the MCMC updates (starting with the burnin updates)
  while (isamp < isamplesize) {
    /* Propose new theta */
    /* Now the degree distribution model parameters */
    for (i=0; i<Np; i++){
      odegstar[i] = rnorm(odegi[i], dlnlamproposal);
    }
    /* Convert from odds to probabilities */

    /* Now the degree distribution (log) mean and s.d. parameters */
    lnlamstar = rnorm(lnlami, dlnlamproposal);
    nustar = nui*exp(rnorm(0., dnuproposal));
    /* Check for magnitude */

    pstars=0.;
    lzcmp = zcmp2(exp(lnlamstar), nustar, errval, 2*Ki, give_log1);

    pstar[Np]=cmp2(Np+1,lnlamstar,nustar,lzcmp,give_log0);
    pstars+=pstar[Np];

    for (i=Np+1; i<Ki; i++){
      pstar[i]=pstar[i-1]*exp(lnlamstar-nustar*log((double)(i+1)));
      pstars+=pstar[i];
    }

    for (i=0; i<Ki; i++){
      pstar[i]/=pstars;
    }
    pstars=1.;
    for (i=0; i<(Ki-1); i++){
      pstars-=pstar[i];
    }
    pstar[Ki-1]=pstars;

    // Now compute mean and s.d. from log-lambda and nu
    mustar=0.0;
    sigma2star=0.0;
    for (i=0; i<Ki; i++){
      mustar+=pstar[i]*(i+1);
      sigma2star+=pstar[i]*(i+1)*(i+1);
    }
    sigma2star=sigma2star-mustar*mustar;

    sigmastar  = sqrt(sigma2star);

    qnustar = dnorm(log(nustar/nui)/dnuproposal,0.,1.,give_log1)
      -log(dnuproposal*nustar);

    pithetastar = dnorm(mustar, dmu, sigmastar/rdfmu, give_log1);

    pithetastar = pithetastar+dsclinvchisq(sigma2star, ddfsigma, dsigma2);
    qnui = dnorm(log(nui/nustar)/dnuproposal,0.,1.,give_log1)
      -log(dnuproposal*nui);

    /* Calculate ratio */
    ip = pithetastar-pithetai;

    for (i=0; i<Ki; i++){
      if(Nk[i]>0){
        lp = log(pstar[i]/pi[i]);
        if(fabs(lp) < 100.){ip += (Nk[i]*lp);}
      }
    }

    /* The logic is to set exp(cutoff) = exp(ip) * qratio ,
     then let the MH probability equal min{exp(cutoff), 1.0}.
     But we'll do it in log space instead.  */
    cutoff = ip + qnui-qnustar;

    /* if we accept the proposed network */
    if (cutoff >= 0.0 || log(unif_rand()) < cutoff) {
      /* Make proposed changes */
      for (i=0; i<Np; i++){
        odegi[i] = odegstar[i];
        pdegi[i] = pdegstar[i];
      }

      lnlami    = lnlamstar;
      nui = nustar;
      qnui = qnustar;
      pithetai = pithetastar;
      for (i=0; i<Ki; i++){
        pi[i] = pstar[i];
      }

      taken++;
      if (step > 0 && step==(iinterval*(step/iinterval))) {
        /* record statistics for posterity */
        lnlamsample[isamp]=lnlami;
        nusample[isamp]=nui;
        for (i=0; i<Np; i++){
          psample[i]=pdegi[i];
        }
        isamp++;
        if (*verbose && isamplesize==(isamp*(isamplesize/isamp))) Rprintf("Taken %d MH samples...\n", isamp);
      }
    }
    step++;
  }

  free(pi);
  free(pstar);
  free(odegi);
  free(odegstar);
  free(pdegi);
  free(pdegstar);
  //PutRNGstate();  /* Disable RNG before returning */
  /*Check for interrupts (if recursion is taking way too long...)*/
  R_CheckUserInterrupt();
  *staken = taken;
}


void MHcmpp2 (double *propsprior, double *concent, double *concentproposal,
              double *props, int *K, int *m, int *N, int *nc,
              double *gammart, int *Npi, double *psample,
              int *samplesize, int *staken, int *burnin, int *interval,
              int *verbose) {
  int Np;
  int step, taken, give_log1=1, give_log0=0;
  int i, Ki, Ni, mi, isamp, iinterval, isamplesize, iburnin;
  int chcki, chckstar;
  double ip, cutoff;
  double psum;
  double conc, concproposal;
  double lppi, lppstar;
  double errval=0.0000000001;

  //GetRNGstate();  /* R function enabling uniform RNG */

  Ki=(*K);
  Np=(*Npi);
  mi=(*m);
  conc=(*concent);
  concproposal=(*concentproposal);
  double *propsstar = (double *) malloc(sizeof(double) * mi);

  Ni=(*N);
  isamplesize=(*samplesize);
  iinterval=(*interval);
  iburnin=(*burnin);

  // First set starting values
  isamp = taken = 0;
  step = -iburnin;

  // Now do the MCMC updates (starting with the burnin updates)
  while (isamp < isamplesize) {

    /* Propose new props */
    chckstar = 1;

    while(chckstar > 0){
      chckstar = 0;
      psum = 0;
      for (i=0; i<mi; i++){
        //propsstar[i] = unif_rand();
        propsstar[i] = rgamma((concproposal*props[i]), 1);
        psum += propsstar[i];
      }
      for (i=0; i<mi; i++){
        propsstar[i] /= psum;
        if(propsstar[i]*Ni - nc[i] <= 0){
          chckstar ++;
        }
      }
    }

    /* Calculate ratio */
    cutoff=0;
    chcki=0;
    lppi=0;
    lppstar=0;

    for (i=0; i<mi; i++){
      /* log probability of proposed p */
      lppstar += (conc*propsprior[i] - 1.)*log(propsstar[i]);
      lppstar += lgamma(round(propsstar[i]*Ni + 1.)) - lgamma(round(propsstar[i]*Ni - nc[i] + 1.));
      lppstar += round(propsstar[i]*Ni - nc[i])*gammart[i];
      /* Add in proposal distribution */
      lppstar += (concproposal*propsstar[i] - 1.)*log(props[i]) - lgamma(concproposal*propsstar[i]);
      /* log probability of previous p */
      lppi += (conc*propsprior[i] - 1.)*log(props[i]);
      lppi += lgamma(round(props[i]*Ni + 1.)) - lgamma(round(props[i]*Ni - nc[i] + 1.));
      lppi += round(props[i]*Ni - nc[i])*gammart[i];
      /* Add in proposal distribution */
      lppi += (concproposal*props[i] - 1.)*log(propsstar[i]) - lgamma(concproposal*props[i]);
      /* check that piN - ni > 0*/
      if(props[i]*Ni - nc[i] <= 0){
        chcki ++;
      }
    }
    /* if piN is too small for any i, set the whole probability to 0 */
    if(chcki > 0){
      lppi = -100000000.0;
    }
    cutoff = lppstar - lppi;

    /* if we accept the proposed network */
    if (cutoff >= 0.0 || log(unif_rand()) < cutoff) {
      /* Make proposed changes */

      for(i=0; i<mi; i++){
        props[i] = propsstar[i];
      }

      taken++;
      if (step > 0 && step==(iinterval*(step/iinterval))) {
        isamp++;
      }
    }
    step++;
  }

  R_CheckUserInterrupt();
  *staken = taken;
}
