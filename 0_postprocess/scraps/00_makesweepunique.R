###Compile the sweep uniques...

sweep <- readRDS("1_main/0_sweep_sero.RDS")
sweep_unique <- sweep%>%
                select(sweep_unique,r02:r05)%>%
                unique()

saveRDS(sweep_unique, "0_postprocess/00_sweep_unique.RDS")