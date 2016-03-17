
#include "bayesopt/bayesopt.h"

double inline_c_Optimisation_Stochastic_BayesOpt_0_c5fd47da4e55a10d4b997d4962b73a460a31de25(double val_inline_c_0) {
return ( val_inline_c_0 );
}


int inline_c_Optimisation_Stochastic_BayesOpt_1_e919766f7e72fc7f01b52086539aeac9e27e9078(double * vrx_inline_c_0, int dim_c_inline_c_1, double (* funIO_inline_c_2)(unsigned n, const double * x, double * gradien, void * fd), double * clv_inline_c_3, double * cuv_inline_c_4) {

            bopt_params bp;	// structure definition ??
            bp = initialize_parameters_to_default();
            set_learning(&bp, "L_MCMC");
            double *r = vrx_inline_c_0;
            bayes_optimization(
                dim_c_inline_c_1,
                funIO_inline_c_2,
                (void *) 0,	// we don't really need this argument, as we pass our a
                clv_inline_c_3,
                cuv_inline_c_4,
                r+1, r, bp);
        
}

