#include <Rmath.h>
#include "R.h"

void lamix();
void newlam();
void pcsort();

void getlam(nn,pp,x,sx,latent,lambda,order,dist,nns,s,strech,unilam,vecx,tempsx)

int *nn, *pp, *nns, *latent, *order;
double *x, *sx, *s, *lambda, *dist, *tempsx, *vecx, *strech, *unilam;

/*
#this subroutine assumes on input a set of p co-ordinate functions s
#defined on the integers 1 ... ns; ie a parametrized  curve.
#It produces a vector of lambda and fitted values sx for each value of x
# stretch left and right end boundries by factor 1+strech
*/

{

int i, j, k, k1, n = *nn, ns = *nns, p=*pp, late=*latent;
double stretch=*strech, adj;

if(stretch < 0.0) stretch=0.0;
if(stretch > 2.0) stretch=2.0;

for(j=0;j<p;j++)
{
	s[j]+=stretch*(s[j]-s[p+j]);
	s[(ns-1)*p+j]+=stretch*(s[(ns-1)*p+j]-s[(ns-2)*p+j]);

}

/* 
# if (late==1) {
# unilam[0]+=stretch*(unilam[0]-unilam[1]);
# unilam[ns-1]+=stretch*(unilam[ns-1]-unilam[ns-2]);
# }
*/

for(i=0;i<n;i++)
{
	for(j=0;j<p;j++) vecx[j]=x[i*p+j];
	lamix(ns,p,vecx,s,lambda,dist,tempsx,i);
	for(j=0;j<p;j++) sx[i*p+j]=tempsx[j];
	
	if (late==1) {
	k=(int)(lambda[i]);
	adj=lambda[i]-k;
	if (k==ns) k1=ns;
	else k1=k+1;
	lambda[i]=(1.0-adj)*unilam[k]+adj*unilam[k1];
	}	
}
if (late==0) {
for(i=0;i<n;i++) order[i]=i;
pcsort(lambda,order,n);
newlam(n,p,sx,lambda,order);

/* add 1 to orders so it starts at 1 not 0 !! - GD */

for (i=0;i<n;i++) order[i]=order[i]+1;
}
}

void newlam(n,p,sx,lambda,tag)

int n, p, *tag;
double *sx, *lambda;

{
int i,j;
double lami,tp;

lambda[tag[0]]=0.0;

for(i=0;i<n-1;i++){
	lami=0.0;
	for(j=0;j<p;j++)
        {
	tp=sx[tag[i+1]*p+j]-sx[tag[i]*p+j];
	lami=lami+tp*tp;
	}
	lambda[tag[i+1]]=lambda[tag[i]]+sqrt(lami);
}
}

void lamix(ns,p,x,s,lambda,distmin,temps,pos)

int ns, p, pos;
double *lambda, *x, *s, *distmin, *temps;

{
int i,j,ik,left,right;
double d1sqr,d2sqr,dismin,d12,dsqr,d1,w;
double lam,lammin,tp;

dismin=1000000.0;
lammin=1.0;

for(ik=0;ik<ns-1;ik++)
{
	d1sqr=0.0;
	d2sqr=0.0;
	for(j=0;j<p;j++)
		{
		tp=s[(ik+1)*p+j]-s[ik*p+j];
		d1sqr+=tp*tp;
		tp=x[j]-s[ik*p+j];			
		d2sqr+=tp*tp;
	}
	if(d1sqr<0.0000001*d2sqr){
		lam=ik;
		dsqr=d2sqr;
	}
	else{
		d12=0.0;
		for(j=0;j<p;j++) d12+=(x[j]-s[ik*p+j])*(s[(ik+1)*p+j]-s[ik*p+j]);
		d1=d12/d1sqr;
		if(d1>1.0){
			lam=ik+1;
			dsqr=d1sqr+d2sqr-2.0*d12;
		}
		else if(d1<0.0){
			lam=ik;
			dsqr=d2sqr;
		}
		else { 
			lam=ik+d1;
			dsqr=d2sqr-d12*d12/d1sqr;
		}
	}
	if(dsqr<dismin){
		dismin=dsqr;
		lammin=lam;	
                }
}
left=(int)lammin;
if(lammin>=ns) left=ns-1;
right=left+1;
w=lammin-left;
for(j=0;j<p;j++)
{
	temps[j]=(1.0-w)*s[left*p+j]+w*s[right*p+j];
}
lambda[pos]=lammin;
distmin[pos]=dismin;
}

void pcsort(a, p, n)
double *a;
int *p;
int n;
{
int i,j,v;
for(i=0; i<n; i++) p[i]=i;
for(i=1; i<n; i++)
{
v=p[i];j=i;
while ( (j>0) &&( a[p[j-1]] > a[v]))
{p[j] = p[j-1]; j--; }
p[j]=v;
}
}
