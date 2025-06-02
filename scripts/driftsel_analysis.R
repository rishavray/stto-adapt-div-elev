library(tidyverse)
# Source the nessessary scripts from driftsel source code
source("../driftsel/rafm/RAFM.r")
source("../driftsel/driftsel.r")
source("scripts/viz_trait_tidy.R")

geno1 = read.table('data/matrix_split.txt',head=FALSE)
geno = as.matrix(geno1[,2:ncol(geno1)])
pos = read.table('data/data.012.pos',head=FALSE)
ID = read.table('data/data.012.indv',head=FALSE)
pops = scan("data/pop.txt", what = character())
climate_data = read.table('climate_pc.csv', head=TRUE, sep = ",", row.names = 1)

pop_num = as.numeric(as.factor(pops))

pop_labels = data.frame(population = pops, code = pop_num) |> 
distinct()

n = dim(geno)[1] #get the number of individuals
p = dim(geno)[2]/2

geno2 = geno
colnames(geno2) = paste(paste('loc',sep='',rep(1:p, each=2)),sep='.',rep(1:2,p))

geno2 = cbind(pop_num, geno2)

colnames(geno2)[1] = 'subpop'

ptm = proc.time()
afm = do.all(geno2, 15000, 5000, 10)   # 15000 iterations, first 5000 as burn-in, the rest saved in every 10th iteractions
proc.time() - ptm


saveRDS(afm, "data/afm_15000_5000_10.RDS")
afm = readRDS("data/afm_15000_5000_10.RDS")


Y = read.table('merged_traits.csv', head=TRUE, sep = "\t")
ped_sto = as.matrix(data.frame(ID = 1:length(pop_num),
    sire = pop_num+1000,
    dam = pop_num+2000,
    sire.pop = pop_num,
    dam.pop = pop_num))

Y = left_join(Y, climate_data, by = c("pop.x" = "pop"))

covars_scaled = cbind(1:length(pop_num), 1)

traits = as.matrix(Y[c("sla",
"postvern_stem_diam",
 "postvern_height",
 "postvern_lngst_lf",
 "postvern_no_lvs"
 )])
traits_scaled = scale(traits)
traits_scaled = cbind(1:length(pop_num), traits_scaled)

samp = MH(afm$theta, ped_sto, 
    covars_scaled,
    traits_scaled,
    15000, 5000, 10, alt=T)

saveRDS(samp, "samp_new_scaled_no_covar.RDS")
samp = readRDS("samp_new_scaled_no_covar.RDS")

# traits = 1: "sla",
# 2: "postvern_stem_diam",
# 3: "postvern_height",
# 4: "postvern_lngst_lf",
# 5: "postvern_no_lvs"

fixedpost = samp$fixed.ef
popefpost = samp$pop.ef
Gpost = samp$G
THpost = samp$theta
traits = c(1,5)
trait_names = c("SLA", "Stem Diameter", "Height", "Longest Leaf",  "No. of Leaves")
population_labels = c("BH", "CP2", "DPR", "IH", "KC2", "LV1", "LV2", "LV3", "LVTR", 
"SHA", "SQ1", "SQ3", "TM2", "WL1", "WL2", "WL3", "WV", "YO1", 
"YO10", "YO11")

# Visualize the trait relationship using the custom ggplot function
# Modify the function in the viz_trait_tidy.R script as needed to make the colors consistent
p = viz_traits_tidy(fixedpost, popefpost, Gpost, THpost, 
                      traits = c(1,3), 
                      size_param = 0.5, 
                      plot_title = "Trait Relationships",
                      population_labels = population_labels,
                      trait_names = trait_names)
p