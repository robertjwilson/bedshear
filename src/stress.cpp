#include <Rcpp.h>
#include <stdlib.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]

std::vector< std::vector<double> >  stress(double h,
                                           double D50,
                                           std::vector<double> mu_V,
                                           std::vector<double> phic_V,
                                           std::vector<double> Hs_V,
                                           std::vector<double> Px_V,
                                           int switch1,
                                           std::vector<double>  phiw_V) {

									double swd;
									double dynv;
									double sed;
									double mu;
									double phic;
									double Hs;
									double Px;
									double phiw;
									double tm_r, tmax_r;
									double phi;
									double ar, T1, T2, T3, A1, A2, CDm, CDmax;
									int vect_size;
									vect_size = mu_V.size();

									std::vector< std::vector<double> > output(vect_size, std::vector<double>(8));

									int rr = 0;

									// set up the physical constants
									// seawater density at 10C 35ppt (kg/m3)
									swd = 1026.96;
									// dynamic viscosity of seawater at 10C 35ppt

									dynv = 1.48e-3;

									// sediment density (= typically 2650 kg/m3)
									sed = 2650;


									for(rr = 0; rr < vect_size; rr++){

									//sed = sed_V[rr];
									mu = mu_V[rr];
									phic = phic_V[rr];
									Hs = Hs_V[rr];
									Px = Px_V[rr];
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
									double ustar, Ren, TcS, TcS_cr;
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

									//Derivation of intermediate terms for shear stress calculation

									kinv = dynv/swd;                         //// kinematic viscosity m2 s-1



									if(phic>phiw)
									phi = (phic-phiw)*3.1416/180;
									if(phiw>phic)
									phi = (phiw-phic)*3.1416/180;
									if(phiw==phic)
									phi = 0;


									z0 = D50/12;                               // bed roughness length
									Rec = mu*h/kinv;                         // Reynolds number for currents

									CDs = 0.0001615*exp(6*(  pow(Rec,-0.08)   ));    // current drag coefficient for smooth flow
									CDr = pow(0.40/(log(h/z0)-1),2);           // current drag coefficient for rough flow

									A = Uw*Tp/(2*3.1416) ;                  // wave semi orbital excursion
									Rew = Uw*A/kinv      ;                  // Reynolds number for waves
									fws = 0.0521*pow(Rew, - 0.187)      ;        // wave friction factor for smooth flow
									fwr = 1.39*  pow(A/z0,-0.52)  ;           // wave friction factor for rough flow

									Rec_cr = 2000 + pow(592000 * Rew,0.35)   ; // critical current Reynolds for transition from laminar to turbulent flow
									Rew_cr = 150000     ;                    // critical wave Reynolds for transition from laminar to turbulent flow

									tc = 0;
									tw = 0;
									tm = 0;
									tmax = 0;
									trms = 0;
									FT = 0;

									if(mu>0){
									if(Rec<=Rec_cr){
									tc = 3*swd*kinv*mu/h;             //current bed shear stress under laminar flow
									FT = 1;
									}
									if(Rec>Rec_cr){
									tc_r = swd*CDr*pow(mu,2);
									tc_s = swd*CDs*pow(mu,2);
									if(tc_r >= tc_s)
									tc = tc_r;
									if(tc_r < tc_s)
									tc = tc_s;

									//       tc = max(tc_r,tc_s);              //current bed shear stress under turbulent flow
									if(tc_r<=tc_s)
									FT = 2;
									if(tc_r>tc_s)
									FT = 3;
									}
									}

									// double tw_r, tw_s;
									// Calculate wave only shear stress
									if(Uw>0){
									if(Rew<=Rew_cr){
									tw = swd*pow(Rew,-0.5)*pow(Uw,2);    //wave bed shear stress under laminar flow
									FT = 1;
									}
									if(Rew>Rew_cr){
									tw_r = 0.5*swd*fwr*pow(Uw,2);
									tw_s = 0.5*swd*fws*pow(Uw,2);
									if(tw_r >= tw_s)
									tw = tw_r;
									if(tw_r < tw_s)
									tw = tw_s;

									//  tw = max(tw_r,tw_s);              //current bed shear stress under turbulent flow
									if(tw_r<=tw_s)
									FT = 2;
									if(tw_r>tw_s)
									FT =3;
									}
									}

									if( (mu>0) && (Uw==0) ){                      //current only no waves
									tm = tc;
									tmax = tc;
									trms = tc;
									}

									if( (mu==0) && (Uw>0) ){                      //waves only no currents
									tm = tw;
									tmax = tw;
									trms = tw;
									}

									if( (mu>0) && (Uw>0) ){                       // combined wave and current flow

									if(Rec<=Rec_cr && Rew<=Rew_cr){         //laminar flow

									// tc and tw will have been correctly estimated by their independent calculations
									tm = tc;
									tmax = pow( pow(tm + tw*(fabs(cos(phi))),2) + pow(tw*(fabs(sin(phi))),2) ,0.5)  ;
									trms = pow(pow(tm,2) +0.5*pow(tw,2),0.5)  ;
									FT  = 1;
									}

									if( (Rec>Rec_cr) || (Rew>Rew_cr) ){           //turbulent flow

									// recalculate tc and tw - since this is an OR statement its possible that
									// one or other of these could appear as laminar flow when estimated independently

									// current only component of stress under combined wave and current turbulent conditions
									tc_r = swd*CDr*pow(mu,2);
									tc_s = swd*CDs*pow(mu,2);

									tc = fmax(tc_r,tc_s);              //current bed shear stress under turbulent flow

									// wave only component of stress under combined wave and current turbulent conditions
									tw_r = 0.5*swd*fwr*pow(Uw,2);
									tw_s = 0.5*swd*fws*pow(Uw,2);

									// double ar, T1, T2, T3, A1, A2, CDm, CDmax;
									// calculate tm_r and tmax_r
									ar = 0.24;
									T1  =  fmax( (ar*(  pow(fwr/2, (double)0.5) )   *(A/z0)) , (double)12);

									T2 = h/(T1*z0);

									T3 = pow(  pow(CDr,2)  + (  pow(fwr/2,2)  ) * (  pow(Uw/mu,4)) , 0.25);
									//        T3=( (CDr^2) + ((fwr/2)^2) * ((Uw/mu)^4) )^0.25
									A1 = (T3*(log(T2)-1))/(2*log(T1));
									A2 = (double)0.40 * T3/log(T1);
									CDm = pow(   pow( pow(A1,2) + A2, (double)0.5) - A1 ,2) ;

									CDmax =  pow( ( pow( CDm + T3*Uw/mu*pow(fwr/2, (double)0.5)*fabs(cos(phi))    ,  (double)2)   ) +
									(   pow(  T3*Uw/mu*pow(fwr/2, (double)0.5)* fabs(sin(phi))   , (double)2)   )  , (double)0.5);

									tm_r = swd*CDm*mu*mu;
									tmax_r = swd*CDmax*mu*mu;

									if(std::isnan(tm_r)==TRUE)
									tm_r = 0;

									if(std::isnan(tmax_r)==TRUE)
									tmax_r = 0;

									//// calculate tm_s and tmax_s
									// double as, CD;
									as = 0.24;
									T1 = 9*as*Rew*(  pow(fws/2, (double)0.5)  ) *  pow  ( ( pow(CDs, (double)2)   )* pow(mu/Uw, (double)4)   + (  pow(fws/2, (double)2)  ) ,(double)0.25);

									T2 = (Rec/Rew)*(Uw/mu)*(1/as)*(  pow(2/fws, (double)0.5)   );
									T3 =  pow( (  pow(CDs,2)  ) + (  pow(fws/2,2)  )*(  pow(Uw/mu,4)  )  , (double)0.25);

									A1 = (T3*(log(T2)-1))/(2*log(T1));
									A2 = 0.40*T3/log(T1);
									CDm = pow( (  pow(  pow(A1,2) +A2,0.5) ) - A1 ,2) ;
									CDmax=  pow( ( pow( CDm + T3*Uw/mu*pow(fws/2, (double)0.5)*fabs(cos(phi))    ,  (double)2)   ) +
									(   pow(  T3*Uw/mu*pow(fws/2, (double)0.5)* fabs(sin(phi))   , (double)2)   )  , (double)0.5);

									tm_s = swd*CDm*mu*mu;
									tmax_s = swd*CDmax*mu*mu;
									if(std::isnan(tm_s)==TRUE)
									tm_s = 0;

									if(std::isnan(tmax_s)==TRUE)
									tmax_s = 0;
									tm = tm_r;
									tmax = tmax_r;

									trms = pow( pow(tm,2) + (double)0.5*  pow(tw,2)  , (double)0.5);
									FT = 3;

									}
									}

									//Finally, the Shields number calculation...
									ustar = sqrt(tmax/swd);              // bed shear velocity
									Ren = (ustar)*D50/kinv;              // Reynolds particle number
									TcS = tmax/((sed-swd)*g*D50);           // Shields stress

									TcS_cr = ( (double) 0.105*  pow(Ren,-0.3)  ) + 0.045*exp(-35*  pow(Ren, (double)-0.59)   ); // critical shields stress



									// This is potentially not as efficient as it could be. Possibly another (more efficient) way to get a 2d matrix in C++
									output[rr][0] = tc;
									output[rr][1] = tw;
									output[rr][2] = tm;
									output[rr][3] = tmax;
									output[rr][4] = trms;
									output[rr][5] = FT;
									output[rr][6] = TcS;
									output[rr][7] = TcS_cr;
									}
									return output;
									}

