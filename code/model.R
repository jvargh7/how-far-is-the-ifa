
#### Final Model -----
library(survey)
svy_preg_s3 <- svydesign(id=~VILLAGEID_2,strata=~BLOCKID,data=sirohi_pregnant,weights=~village_weight)
glm_S_1 <- svyglm(AnemiaYN~no_women + Q4_8_2_copy +IFAFA_AvgConsumedPD_PreviousMonthTrimester + 
                    Any_Substances + mobile_usage + data_expected_preg,family=quasibinomial,design=svy_preg_s3)
summary(glm_S_1)
rm(glm_S_1)