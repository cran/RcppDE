#!/usr/bin/r -t

Wild <- function(x) { 		## 'Wild' function, global minimum at about -15.81515
    sum(10 * sin(0.3 * x) * sin(1.3 * x^2) + 0.00001 * x^4 + 0.2 * x + 80)/length(x)
}

Rastrigin <- function(x) {
    sum(x+2 - 10 * cos(2*pi*x)) + 20
}

Genrose <- function(x) { 	## One generalization of the Rosenbrock banana valley function (n parameters)
    n <- length(x)
    1.0 + sum (100 * (x[-n]^2 - x[-1])^2 + (x[-1] - 1)^2)
}



library(inline)

inc <- 'double genrose(SEXP xs) {
           Rcpp::NumericVector x(xs);
           int n = x.size();
           double sum = 1.0;
           for (int i=1; i<n; i++) {
              sum += 100*( pow(x[i-1]*x[i-1] - x[i], 2)) + (x[i] - 1)*(x[i] - 1);
           }
           return(sum);
        }

        double wild(SEXP xs) {
           Rcpp::NumericVector x(xs);
           int n = x.size();
           double sum = 0.0;
           for (int i=0; i<n; i++) {
              sum += 10 * sin(0.3 * x[i]) * sin(1.3 * x[i]*x[i]) + 0.00001 * x[i]*x[i]*x[i]*x[i] + 0.2 * x[i] + 80;
           }
           sum /= n;
           return(sum);
        }

        double rastrigin(SEXP xs) {
           Rcpp::NumericVector x(xs);
           int n = x.size();
           double sum = 20.0;
           for (int i=0; i<n; i++) {
              sum += x[i]+2 - 10*cos(2*M_PI*x[i]);
           }
           return(sum);
        }

        class Fun {
        public:
	   typedef double (*FunctionPointer)(SEXP);
	   Fun( FunctionPointer ptr_ ) : ptr(ptr_) {};
	   inline FunctionPointer get() { return ptr ; }
        private:
	   FunctionPointer ptr ;
        };

        '

## now via a class returning external pointer
src.xptr <- 'std::string fstr = Rcpp::as<std::string>(funname);
             if (fstr == "genrose")
                return(XPtr<Fun>(new Fun(&genrose)));
             else if (fstr == "wild")
                return(XPtr<Fun>(new Fun(&wild)));
             else
                return(XPtr<Fun>(new Fun(&rastrigin)));
             '
create_xptr <- cxxfunction(signature(funname="character"), body=src.xptr, inc=inc, plugin="Rcpp")


#maxIt <- 100                            # not excessive but so that we get some run-time on simple problems
n <- 20
maxIt <- 50
useBS <- TRUE
storeFrom <- maxIt+1
strat <- 6                              # TODO fix segfault when strat==6

suppressMessages(library(DEoptim)) 	# the original, currently 2.0.7
suppressMessages(library(RcppDE))    	# the contender

ctrl <- DEoptim::DEoptim.control(NP=10*n,
                                 itermax=maxIt,
                                 trace=FALSE,
                                 bs=useBS,
                                 storepopfrom=storeFrom,
                                 strategy=strat)

basicDE <- function(n, maxIt, fun) DEoptim::DEoptim(fn=fun, lower=rep(-25, n), upper=rep(25, n), control=ctrl)
cppDE <- function(n, maxIt, fun) RcppDE::DEoptim(fn=fun, lower=rep(-25, n), upper=rep(25, n), control=ctrl)

set.seed(42)
print(system.time(valBasic <- basicDE(n, maxIt, function(...) Rastrigin(...))))
print(str(valBasic[[1]]))
set.seed(42)
#valCpp <- cppDE(n, maxIt, function(...) Rastrigin(...))
xptr <- create_xptr("rastrigin")
print(system.time(valCpp <- RcppDE::DEoptim(fn=xptr, lower=rep(-25, n), upper=rep(25, n), control=ctrl)))
print(str(valCpp[[1]]))
stopifnot( all.equal(valBasic[[1]], valCpp[[1]]) )

cat("# Done ", format(Sys.time()), "\n")







