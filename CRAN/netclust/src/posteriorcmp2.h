#ifndef POSTERIORCMP_H
#define POSTERIORCMP_H

/* I define Scale-Inverse-Chi-Square distribution Y with a scale parameter "scale"
   and degrees of freedom "df" as

   Y=scale*df/X,
   where X~Chi-sq(df).
   
   Here, E[Y]=scale*df/(df-2)

   These functions evaluate the density and generate deviates from this distribution.
*/

#define dsclinvchisqboth(x,df,scale,give_log) (give_log ? (dchisq((df)*((double)(scale))/((double)(x)),df,1)+log(((double)(scale))*(df)/(double)((x)*(x)))) : (dchisq((df)*((double)(scale))/((double)(x)),df,0)*(df)*((double)(scale))/(double)((x)*(x))))

#define dsclinvchisq(x,df,scale) (dchisq((df)*((double)(scale))/((double)(x)),df,1)+log(((double)(scale))*(df)/(double)((x)*(x))))

#define rsclinvchisq(df,scale) ((scale)*(df)/(rchisq(df)))

void gcmp3 (int *pop,            
            int *nk,        
            int *m, 
	    double *propsprior, 
            double *concent,
            int *K, 
            int *n, 
            int *nc, 
            double *ppos,
            int *samplesize, 
            int *burnin, 
            int *interval,    
            double *mu,      
            double *dfmu,      
            double *sigma,  
            double *dfsigma,          
            double *lnlamproposal, 
            double *nuproposal,  
            double *concentproposal,
            int *N,    
            int *maxN,     
            double *sample,   
            double *lpriorm,   
            int *burnintheta,
            int *verbose  
         );

void MHcmptheta2 (int *Nk, int *K,
            double *mu, double *dfmu, 
            double *sigma,  double *dfsigma,
            double *lnlamproposal, 
            double *nuproposal, 
            int *N, int *Npi, double *psample,
            double *lnlamsample, double *nusample,
            int *samplesize, int *staken, int *burnin, int *interval,
            int *verbose
         );

void MHcmpp2 (double *propsprior, double *concent, double *concentproposal, 
              double *props, int *K, int *m, int *N, int *nc, 
              double *gammart, int *Npi, double *psample,
                 int *samplesize, int *staken, int *burnin, int *interval,
                 int *verbose
         );

#endif /* POSTERIORCMP_H */
