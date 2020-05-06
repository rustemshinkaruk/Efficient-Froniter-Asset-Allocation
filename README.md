# Efficient-Froniter-Asset-Allocation

I computed the mean-variance frontier, found the minimum-variance portfolio and the eficient frontier. I provided two examples of frontiers: one frontier with 7 assets and another frontier with only 2 assets to emphasisize the difference when you add additional (less than perfectly correalted with each other) assets.

![alt text](https://github.com/rustemshinkaruk/Efficient-Froniter-Asset-Allocation/blob/master/pic1.png)

The inner frontier is the one with 2 assets. The benefits of adding additional assets is observable on the graph.

In this case, the minimum variance portfolio of all assets is (Microsoft, Intel, Southwest, McDonalds, Johnson-Johnson)=(1.8060402, 0.2827651, 0.5455620, -1.3040313, -0.3303360) has standard deviation of 0.1635835 and the minimum variance portfolio of 2 assets is (Microsoft, Intel)=(6.503268, -5.503268) has standard deviation of 0.2777973. 




Then I added the riskless asset and constructed the tangent portfolio for the Intel-Microsoft case.

![alt text](https://github.com/rustemshinkaruk/Efficient-Froniter-Asset-Allocation/blob/master/pic2.png)


The weekly Sharpe ratio for the full set is 0.121 compared to 0.0976 for the Intel-Microsoft case. The Sharpe ratio is the slope of the line passing through the risk-free asset and the tangency portfolio.



Assume your risk aversion is A = 5: What is your optimal mix of assets?

The expected return of the tangency portfolio is 0.003483677 and the variance is
0.0005876033. The risk-free rate is 0.0005447815, Therefore, the optimal portfolio
weight for the tangency portfolio is given by

![alt text](https://github.com/rustemshinkaruk/Efficient-Froniter-Asset-Allocation/blob/master/pic4.png)



![alt text](https://github.com/rustemshinkaruk/Efficient-Froniter-Asset-Allocation/blob/master/pic2.png)
