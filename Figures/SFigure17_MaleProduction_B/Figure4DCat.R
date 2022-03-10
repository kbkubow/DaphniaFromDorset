### libraries
  library(ggplot2)
  library(data.table)
  library(cowplot)
  library(patchwork)

############
### Data ###
############

### load F1 cross data (made by `makeData.F1_pheno.R`)
  load("F1_pheno.Rdata")


############################
### male proportion plot ###
############################

  male$gr <- ifelse(male$SC=="selfedA", "A", ifelse(male$SC=="B", "B", male$gr))
  male.ag <- male[!is.na(gr),list(propmale=sum(Males)/sum(NewTotal),
                                  N=sum(NewTotal)),
                    list(clone, gr)]
  male.ag[,se:=sqrt((propmale*(1-propmale))/N)]

  male.ag[,lci:=propmale-1.96*se]
  male.ag[,uci:=propmale+1.96*se]

  male.ag$gr <- factor(male.ag$gr, levels=c("A", "AxC", "C", "CxC", "B"))


#########################
### Male Production Figure ###
#########################

  male.plot <- ggplot(data=male.ag[!is.na(gr)], aes(x=gr, y=propmale, color=gr)) +
  geom_linerange(aes(ymin=lci, ymax=uci), position = position_jitter(seed = 123, width =0.2), size=.25, alpha=.75, color="black") +
  geom_point(position = position_jitter(seed = 123, width =0.2), size=2) +
  theme(legend.position="none") +
  xlab("Clone or Cross Type") + ylab("Male proportion") +theme_bw() + theme(legend.position="none") +
  scale_color_manual(values=c("#000000", "#000000", "#000000", "#000000", "#FF00FF"))

  ggsave(male.plot, file="male.plot_withDcat.pdf")
