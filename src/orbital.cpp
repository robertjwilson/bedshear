#include <Rcpp.h>
#include <stdlib.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]

std::vector< std::vector<double> >  velocity(double h,  std::vector<double> Hs_V, std::vector<double> Px_V, std::vector<int> switch1_V) {

	//double mu;
	double Hs;
	double Px;
	int switch1;
	//double ar, T1, T2, T3, A1, A2, CDm, CDmax;

	int vect_size;
	vect_size = Hs_V.size();

	std::vector< std::vector<double> > output(vect_size, std::vector<double>(1));

	int rr = 0;
	// dynamic viscosity of seawater at 10C 35ppt

	for(rr = 0; rr < vect_size; rr++){

		Hs = Hs_V[rr];
		Px = Px_V[rr];
		switch1 = switch1_V[rr];

		double g, Tz, Tp, Tn;
		double tpw, Apw, Uw;
		g = 9.81;                // acceleration due to gravity (ms-2)
		if(switch1 == 1){
			Tz = Px/1.28;                // zero crossing period (sec)
			Tp = Px;
		}                      // peak wave period

		// if the input term Px is the mean wave period Tm, then:
		if(switch1==0){
			Tz = Px ;                      // zero crossing period (sec)
			Tp = (double)1.28*Px;
		}                 // peak wave period

		Tn = pow(h/g,0.5);              // natural scaling period

		tpw = Tn/Tz;
		Apw = pow( 6500 + pow( 0.56+(15.54*tpw),6) ,(double)1/6);
		Uw = (0.25*Hs) / ( Tn * pow((1 + (Apw*tpw*tpw)) ,3   ) )  ;

		output[rr][0] = Uw;
	}
	return output;
}



