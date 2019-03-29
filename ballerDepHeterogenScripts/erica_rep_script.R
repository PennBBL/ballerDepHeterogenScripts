library("mgcv")
cnb <- readRDS("/data/jux/BBL/projects/ballerDepHeterogen/results/rds/cnb_results.rds")
clinical <- readRDS("/data/jux/BBL/projects/ballerDepHeterogen/results/rds/clinical_bifactor_results.rds")
anxious_misery <- readRDS("/data/jux/BBL/projects/ballerDepHeterogen/results/rds/anxious_misery_results.rds")
nback <- readRDS("/data/jux/BBL/projects/ballerDepHeterogen/results/rds/nback_results.rds")
networks <- readRDS("/data/jux/BBL/projects/ballerDepHeterogen/results/rds/network_results.rds")

cnb_measure_names <- names(cnb[grep("_z", names(cnb))])
clinical_measure_names <- names(clinical[grep("Bifactor", names(clinical))])
clinical_measure_names <- clinical_measure_names[-grep("_ar", clinical_measure_names)] 
anxious_misery_names <- c("staiPreState", "staiPreTrait")
parcellations <- names(nback[grep("nback_func_sc", names(nback))])


output_cnb <- lapply(cnb_measure_names, function(x) {
  lm(substitute(i ~ Hydra_k3 + sex + age_in_years + ageSq, list(i=as.name(x))), data = cnb)
})
names(output_cnb) <- cnb_measure_names
output_cnb_anova <- lapply(output_cnb, anova)
output_cnb_pval <- sapply(output_cnb_anova, function(x) x$"Pr(>F)"[1])

output_clinical <- lapply(clinical_measure_names, function(x) {
  lm(substitute(i ~ Hydra_k3 + sex + age_in_years + ageSq, list(i=as.name(x))), data = clinical)
})
names(output_clinical) <- clinical_measure_names
output_clinical_anova <- lapply(output_clinical, anova)
output_clinical_pval <- sapply(output_clinical_anova, function(x) x$"Pr(>F)"[1])

output_anxious_mis <- lapply(anxious_misery_names, function(x) {
  lm(substitute(i ~ Hydra_k3 + sex + age_in_years + ageSq, list(i=as.name(x))), data = anxious_misery)
})
names(output_anxious_mis) <- anxious_misery_names
output_am_anova <- lapply(output_anxious_mis, anova)
output_am_pval <- sapply(output_am_anova, function(x) x$"Pr(>F)"[1])

output_nback <- lapply(parcellations, function(x) {
  lm(substitute(i ~ Hydra_k3 + sex + age_in_years + ageSq + nbackRelMeanRMSMotion, list(i=as.name(x))), data = nback)
})
names(output_nback) <- parcellations
output_nback_anova <- lapply(output_nback, anova)
output_nback_pval <- sapply(output_nback_anova, function(x) x$"Pr(>F)"[1])

output_network <- anova(lm(network_strength~Hydra_k3 + sex + age_in_years + ageSq + restRelMeanRMSMotion + nbackRelMeanRMSMotion + idemoRelMeanRMSMotion, data = networks))
