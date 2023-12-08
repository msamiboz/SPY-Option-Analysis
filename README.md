# SPY Option Analysis
 We did an return analysis on options on SPY 
 
# Intro 

The main goal of this project is observe any potential systemic misprice in options. a few example could be these, from Kahneman's perpective[^1],system 1 which quickly operates with little effort can be produce could be meain system when taking trade actions. So from this point we can say that insurance can be expensive because especially after crisis times. In option world insurance means buying put option, so we need to anaylse that options. Or as Taleb suggests[^2] market can misprice tails by assuming they are thinner than reality.

Apart form this statements we looked general strategies. Like straddle, strangles, call/put spreads, butterflies etc. Detail of the analysis can be found under the analysis chapter.


[^1]:Daniel, Kahneman. Thinking, fast and slow. 2017.
[^2]:Taleb, Nassim Nicholas. "Black swans and the domains of statistics." The american statistician 61.3 (2007): 198-200.

# Data

Data is commercialized so we can't share it.
It contains 

# Analysis

## Manipulation

The data is filtered to have only options to end at end of 3rd weeek each month. Because that is become industry standart for SPY contracts. So there will be no liquidity problem on these contracts. When looked one can see amount of contract that jumps that day of the month. So at the end we have single maturity date for all months. Another filtering is for robustness. We excluded contracts with less than 16 days of maturity. Also in the market we can see different pricing behaviors. So we also excluded contracts with negative option price. 

After these filtering operations, we first calculate Implied volatility of the contracts to understand how market prices the contracts. Because higher implied volatility. mean higher price. To do that we need several parameters. Option price, volatility, strike price, risk free rate and time to maturity to obtain it from Black Scholes formula[^3]. 

[^3]:Black, Fischer, and Myron Scholes. "The pricing of options and corporate liabilities." Journal of political economy 81.3 (1973): 637-654.

As risk free rate we used LIBOR and SOFR. The time determined by  which rate is closest to time to maturity. For example, 42 days to maturity contract we used 1 month risk free rate and for 75 days to maturirty contract we used 3 month LIBOR/SOFR. 1,3,6 and 12 month LIBOR/SOFR used for risk free rate.

To calculate implied volatility. We used historical observed volatility of od S&P 500 index 18% is used. Another assumption we used is that dividend yield is 0.

By using again these parameters we also calculated greeks delta and gamma. 

we also calculated moneyness of a contract where how much in the money or out of money(oom) the contract with respect at the money (atm) contracts' standard deviation. So we can say that how much sigma noeynes away that contract is from the atm contract.

Then we calculate hold the maturity (htm) returns. Because relatively high commissions on trade could diminish our potential gains. So htm returns are more representable.


Hold the maturity return calculated by following formula:

$$\text{Return} = \frac{\text{Payoff} - \text{Option Price}}{\text{Underlying Price}}$$

## Analyze

### Implied Volatility Smiles

To see the structure of options through time, we used plots called implied volatility smiles. Where in this plot in each day of contracts due to our filtering this means each month. we can look implied volatilites of contracts with different deltas. X axis for put options is absolute delta and for call options is absolute 1-delta. So by this way horizontal axis represents increasing strike price. To observe out of money contracts one should examine put line before 50 delta and call line after 50 delta point.





