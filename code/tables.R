


###Table 4
xtabs(~IFAFA_Any_Received_PresentTri + anc_registered + Trimester_23,data=sirohi_pregnant[sirohi_pregnant$hw_visit==1,])
xtabs(~IFAFA_Any_Received_PresentTri + anc_registered + Trimester_23,data=sirohi_pregnant[sirohi_pregnant$hw_visit==0,])




###Correlation between percentage of IFA received and distance to nearest health facility
dist_compliance <- sirohi_pregnant_2[sirohi_pregnant_2$Prev_Month_Tri>0,] %>% 
  group_by(VILLAGEID_2,Closest,phc_geodist,reported_dist) %>% 
  summarize(consumed_prev_month_tri = mean(IFAFA_Any_Consumed_Prev_Month_Tri,na.rm=TRUE),
            received_present_tri = mean(IFAFA_Any_Received_PresentTri,na.rm=TRUE),
            consumed_present_tri = mean(IFAFA_Any_Consumed_PresentTri,na.rm=TRUE)
  )

View(cor(dist_compliance[-c(1:2)]))
cor.test(dist_compliance$phc_geodist,dist_compliance$received_present_tri)
cor.test(dist_compliance$reported_dist,dist_compliance$received_present_tri)

summary(lm(received_present_tri ~ Closest,data=dist_compliance))