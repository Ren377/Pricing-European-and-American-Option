
//  Created by Renren on 11/28/17.
//  Copyright © 2017 Renren Deng. All rights reserved.
//  Reference from Class Code IE523

#include <iostream>
#include <iomanip>
#include <cmath>
#include <fstream>
#include <cstdlib>
#include <vector>
#include <algorithm>

using namespace std;

double up_factor, uptick_prob, downtick_prob, notick_prob, risk_free_rate, strike_price;
double initial_stock_price, expiration_time, volatility, R;
int no_of_Stages;
vector< vector <double> > store_American_call;
vector< vector <double> > store_American_put;
vector< vector <double> > store_European_call;
vector< vector <double> > store_European_put;

void initialize_memo (int n)
{
    for (int i = 0; i < n + 1; i++)
    {
        vector<double> temp;
        store_American_call.push_back(temp);
        store_American_put.push_back(temp);
        store_European_call.push_back(temp);
        store_European_put.push_back(temp);
    }

    for (int i = 0; i < n + 1; i++)
    {
        for (int j = 0; j < 2 * n + 1; j++)
        {
            store_American_call[i].push_back(-1);
            store_American_put[i].push_back(-1);
            store_European_call[i].push_back(-1);
            store_European_put[i].push_back(-1);
        }
    }
}

double max(double a, double b) {
    return (b < a )? a:b;
}

double N(const double& z) {
    if (z > 6.0) { return 1.0; }; // this guards against overflow
    if (z < -6.0) { return 0.0; };
    double b1 = 0.31938153;
    double b2 = -0.356563782;
    double b3 = 1.781477937;
    double b4 = -1.821255978;
    double b5 = 1.330274429;
    double p = 0.2316419;
    double c2 = 0.3989423;
    double a=fabs(z);
    double t = 1.0/(1.0+a*p);
    double b = c2*exp((-z)*(z/2.0));
    double n = ((((b5*t+b4)*t+b3)*t+b2)*t+b1)*t;
    n = 1.0-b*n;
    if ( z < 0.0 ) n = 1.0 - n;
    return n;
};

double option_price_put_black_scholes(const double& S,      // spot price
                                      const double& K,      // Strike (exercise) price,
                                      const double& r,      // interest rate
                                      const double& sigma,  // volatility
                                      const double& time){
    double time_sqrt = sqrt(time);
    double d1 = (log(S/K)+r*time)/(sigma*time_sqrt) + 0.5*sigma*time_sqrt;
    double d2 = d1-(sigma*time_sqrt);
    return K*exp(-r*time)*N(-d2) - S*N(-d1);
};

double option_price_call_black_scholes(const double& S,       // spot (underlying) price
                                       const double& K,       // strike (exercise) price,
                                       const double& r,       // interest rate
                                       const double& sigma,   // volatility
                                       const double& time) {  // time to maturity
    double time_sqrt = sqrt(time);
    double d1 = (log(S/K)+r*time)/(sigma*time_sqrt)+0.5*sigma*time_sqrt;
    double d2 = d1-(sigma*time_sqrt);
    return S*N(d1) - K*exp(-r*time)*N(d2);
};


double european_call_option(int k, int i) {
    if (store_European_call[k][i + no_of_Stages] != -1)
        return store_European_call[k][i + no_of_Stages];
    else
    {
        if (k == no_of_Stages)
        {
            store_European_call[k][i + no_of_Stages] = max(0.0, (initial_stock_price*pow(up_factor, ((double) i))) - strike_price);
            return store_European_call[k][i + no_of_Stages];
        }
        else
        {
            store_European_call[k][i + no_of_Stages] = ((uptick_prob*european_call_option(k+1,i+1) + notick_prob * european_call_option(k + 1, i) + downtick_prob*european_call_option(k+1,i-1))/R);
            return store_European_call[k][i + no_of_Stages];
        }
    }
}

double european_put_option(int k, int i) {
    if (store_European_put[k][i + no_of_Stages] != -1)
        return store_European_put[k][i + no_of_Stages];
    else
    {
        if (k == no_of_Stages)
        {
            store_European_put[k][i + no_of_Stages] = max(0.0, strike_price - (initial_stock_price*pow(up_factor, ((double) i))));
            return store_European_put[k][i + no_of_Stages];
        }
        else
        {
            store_European_put[k][i + no_of_Stages] = ((uptick_prob*european_put_option(k+1,i+1) + notick_prob * european_put_option(k + 1, i) + downtick_prob*european_put_option(k+1,i-1))/R);
            return store_European_put[k][i + no_of_Stages];
        }
    }
}


double american_call_option(int k, int i, double current_stock_price) {
    if (store_American_call[k][i + no_of_Stages] != -1)
    {
            return store_American_call[k][i + no_of_Stages];

    }
    else
    {
        if (k == no_of_Stages)
        {
            store_American_call[k][i + no_of_Stages] = max(0.0, (current_stock_price - strike_price));
            return store_American_call[k][i + no_of_Stages];
        }
        else
        {
            store_American_call[k][i + no_of_Stages] = max((current_stock_price - strike_price),
                                                           (uptick_prob * american_call_option(k + 1, i + 1, current_stock_price * up_factor) +
                                                            (notick_prob) * american_call_option(k + 1, i, current_stock_price) +
                                                            downtick_prob * american_call_option(k + 1, i - 1, current_stock_price/up_factor))/R);
            return store_American_call[k][i + no_of_Stages];
        }
    }
    return 0;
}

double american_put_option(int k, int i, double current_stock_price) {
    if (store_American_put[k][i + no_of_Stages] != -1)
    {
        return store_American_put[k][i + no_of_Stages];
        
    }
    else
    {
        if (k == no_of_Stages)
        {
            store_American_put[k][i + no_of_Stages] = max(0.0, (strike_price - current_stock_price));
            return store_American_put[k][i + no_of_Stages];
        }
        else
        {
            store_American_put[k][i + no_of_Stages] = max((strike_price - current_stock_price),
                                                           (uptick_prob * american_put_option(k + 1, i + 1, current_stock_price * up_factor) +
                                                            (notick_prob) * american_put_option(k + 1, i, current_stock_price) +
                                                            downtick_prob * american_put_option(k + 1, i - 1, current_stock_price/up_factor))/R);
            return store_American_put[k][i + no_of_Stages];
        }
    }
    return 0;
}

int main (int argc, char* argv[])
{
    
    sscanf (argv[1], "%lf", &expiration_time);
    sscanf (argv[2], "%d", &no_of_Stages);
    sscanf (argv[3], "%lf", &risk_free_rate);
    sscanf (argv[4], "%lf", &volatility);
    sscanf (argv[5], "%lf", &initial_stock_price);
    sscanf (argv[6], "%lf", &strike_price);
    
    up_factor = exp(volatility*sqrt(2*(expiration_time/((double) no_of_Stages))));
    R = exp(risk_free_rate*expiration_time/((double) no_of_Stages));
    uptick_prob = pow((sqrt(R) - (1/sqrt(up_factor)))/(sqrt(up_factor) - (1/sqrt(up_factor))),2);
    downtick_prob = pow((sqrt(up_factor) - (sqrt(R)))/(sqrt(up_factor) - (1/sqrt(up_factor))),2);
    notick_prob = 1 - uptick_prob - downtick_prob;
    
    cout << "Recursive Binomial American-Asian Option Pricing" << endl;
    cout << "Expiration Time (Years) = " << expiration_time << endl;
    cout << "Number of Divisions = " << no_of_Stages << endl;
    cout << "Risk Free Interest Rate = " << risk_free_rate << endl;
    cout << "Volatility (%age of stock value) = " << volatility*100 << endl;
    cout << "Initial Stock Price = " << initial_stock_price << endl;
    cout << "Strike Price = " << strike_price << endl;
    cout << "--------------------------------------" << endl;
    cout << "R = " << R << endl;
    cout << "Up Factor = " << up_factor << endl;
    cout << "Uptick Probability = " << uptick_prob << endl;
    cout << "Downtick Probability = " << downtick_prob << endl;
    cout << "Notick Probability = " << 1 - uptick_prob - downtick_prob << endl;
    cout << "--------------------------------------" << endl;
    
    initialize_memo(no_of_Stages);
    
    double call_price = european_call_option(0, 0);
    cout << "Trinomial Price of an European Call Option = " << call_price << endl;
    cout << "Call Price according to Black-Scholes = " <<
    option_price_call_black_scholes(initial_stock_price, strike_price, risk_free_rate,
                                    volatility, expiration_time) << endl;
    cout << "--------------------------------------" << endl;
    double put_price = european_put_option(0, 0);
    cout << "Trinomial Price of an European Put Option = " << put_price << endl;
    cout << "Put Price according to Black-Scholes = " <<
    option_price_put_black_scholes(initial_stock_price, strike_price, risk_free_rate,
                                   volatility, expiration_time) << endl;
    
    cout << "--------------------------------------" << endl;
    double call_price_A = american_call_option(0, 0,initial_stock_price);
    cout << "Trinomial Price of an American Call Option = " << call_price_A << endl;
    double put_price_A = american_put_option(0, 0, initial_stock_price);
    cout << "Trinomial Price of an American Put Option = " << put_price_A << endl;
}
