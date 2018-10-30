1) get sample (update get_abs_and_weighted_sample <- function(sample_from_HYDRA) {
 so it only deals with nback)

2) Get the schaefer 400 parcellations for the people in the group
  -Make sure to change the actual functions script to have paths to Nback schaefer 400 as opposed to rest

3) Get the communities for those signal changes (you want a mean for each of the communities, taken by averaging the % signal change across all schaefer nodes in the community)

4) Repeat this but use the Schaefer node mapping onto the parcellations

5) DON"T DO WITHIN/BETWEEN for nback, just stop at mean signal change from nback"