

Rcpp::cppFunction('std::vector< std::vector<double> >  orbital_velocity(std::vector<double> swd_V, std::vector<double> dynv_V, double h, std::vector<double> D_V,std::vector<double> sed_V, std::vector<double> mu_V, std::vector<double> phic_V, std::vector<double> Hs_V, std::vector<double> Px_V, std::vector<int> switch1_V, std::vector<double>  phiw_V) {

double swd;
double dynv;
double D;
double sed;
double mu;
double phic;
double Hs;
double Px;
int switch1;
double phiw;
double tm_r, tmax_r;
double phi;
double ar, T1, T2, T3, A1, A2, CDm, CDmax;
std::vector< std::vector<double> > output(35040, std::vector<double>(1));

double ustar;

int rr = 0;

for(rr = 0; rr < 35040; rr++){

					swd = swd_V[rr];
					dynv = dynv_V[rr];
					D = D_V[rr];
					sed = sed_V[rr];
					mu = mu_V[rr];
					phic = phic_V[rr];
					Hs = Hs_V[rr];
					Px = Px_V[rr];
					switch1 = switch1_V[rr];
					phiw = phiw_V[rr];

					double g, Tz, Tp, Tn;
					double tpw, Apw, Uw;
					double kinv;
					double z0, Rec, CDs, CDr, A, Rew, fws, fwr;
					double Rec_cr, Rew_cr;
					double tc, tw, tm, tmax, trms, FT;
					double tc_r, tc_s;
					double tw_r, tw_s;
					double as;
					double tm_s, tmax_s;
					double  Ren, TcS, TcS_cr;
          g = 9.81;                // acceleration due to gravity (ms-2)


            // if the input term Px is peak wave period Tp, then:
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
}')



