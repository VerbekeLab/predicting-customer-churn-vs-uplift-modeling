# Why you should stop predicting customer churn and start using uplift modeling. </br><sub><sub>Floris Devriendt - Jeroen Berrevoets - Wouter Verbeke [[2021]](https://doi.org/10.1016/j.ins.2019.12.075)</sub></sub>
Uplift modeling has received increasing interest in both the business analytics research community and the industry as an improved paradigm for predictive analytics for data-driven operational decision-making. The literature, however, does not provide conclusive empirical evidence that uplift modeling outperforms predictive modeling. Case studies that directly compare both approaches are lacking, and the performance of predictive models and uplift models as reported in various experimental studies cannot be compared indirectly since different evaluation measures are used to assess their performance.

Therefore, in this paper, we introduce a novel evaluation metric called the maximum profit uplift (MPU) measure that allows assessing the performance in terms of the maximum potential profit that can be achieved by adopting an uplift model. This measure, developed for evaluating customer churn uplift models, extends the maximum profit measure for evaluating customer churn prediction models. While introducing the MPU measure, we describe the generally applicable liftup curve and liftup measure for evaluating uplift models as counterparts of the lift curve and lift measure that are broadly used to evaluate predictive models. These measures are subsequently applied to assess and compare the performance of customer churn prediction and uplift models in a case study that applies uplift modeling to customer retention in the financial industry. We observe that uplift models outperform predictive models and lead to improved profitability of retention campaigns.

## Data
The repository contains two different sample taking files:

"1 - Sample Taking - ING" to be used with the ING.RDS dataset and used to get the findings from the Devriendt paper.

"1 - Sample Taking - Criteo regular", a sampling approach for the criteo.RDS dataset.

## Usage
The code is split in four parts. To run the code, follow the following steps:

1) Make sure the dataset you want to use is in the RDS folder in either .csv or .RDS format.

2) Open "0 - runSettings" and fill out the runsettings. We use this file to easily switch between different datasets/savelocations/variables. Make sure to save this file before moving to the next step.

3) Prepare your sample with "1 - Sample Taking - XXX", this file is different for each dataset/sampling technique.

4) Make the models with "2 - Model making", this script is interchangable for most samples, if trained correctly.

5) Evaluate your models with "3 - Model Evaluation", this script is interchangable for most models, if trained correctly. Plots appear in the "RDS/datasetname/PLOTS" folder.

## Citing
Original code made by Floris Devriendt. Code cleaned and slightly changed by Jordi Vandenhouwe & Maarten De Winter.

Please cite our paper and/or code as follows:

```tex

@article{devriendt2021,
  title={Why you should stop predicting customer churn and start using uplift models},
  author={Devriendt, Floris and Berrevoets, Jeroen and Verbeke, Wouter},
  journal={Information Sciences},
  volume={548},
  pages={497--515},
  year={2021},
  publisher={Elsevier}
}


```