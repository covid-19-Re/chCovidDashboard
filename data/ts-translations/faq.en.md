#### What does the dashboard show?

The dashboard presents three types of information:

* In the default case - when neither "Clinical event probability given" nor the normalisation is selected - **absolute numbers** are displayed. Depending on the selected clinical event that is selected in the top-left panel of the website, the plot shows the number of positive tests taken on a particular day, the number of confirmed COVID-19 patients hospatalised or died on a day,, or the total number of tested persons on a day.
* When "Clinical event probability given" is checked, **estimated probabilities or relative numbers** are shown. For example, if "Hospitalisation given Positive test" is chosen, the dashboard plots the hospitalisation probability of patients who were positively tested on a particular day. In other words, the number of hospitalised persons who had taken their test on a day is divided by the number of all positive tests on that day.
* When "Activate normalisation" is selected, the plot does not show the true number of cases but the **theoretical number of cases** under the (strong) assumption that the hospitalisation probability per age group did not change throughout the pandemic. More information can be found in the above box.


#### How are the confidence intervals calculated?

Let's take the hospitalisation probability ("Hospitalisation given Positive test") as an example.

If the "Clinical event probability given" option is selected, the shown numbers can be interpreted as probabilities. This means that we get the estimated probability of a person being hospitalised when the person was tested positively on a specific day. The certainty of the estimation depends on the sample size: when many people had a positive test on a day, the certainty is higher.

The <a href="https://en.wikipedia.org/wiki/Binomial_distribution#Confidence_intervals" target="_blank">binomial distribution</a> is used as the underlying model. We set the "number of trials" n = number of positive cases, "the number of successes" k = number of hospitalisations. The maximum likelihood estimate of the probability is p = k/n . The confidence interval is calculated with the <a href="https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Clopper%E2%80%93Pearson_interval" target="_blank">Clopper-Pearson method</a>.

When a sliding window is selected, all hospitalisations and positive tests in the chosen time window are taken into account (the values are summed up in the chosen time window).
