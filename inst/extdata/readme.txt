The dataset includes:

1. ivivc_ref_data.RData  - the dataset for estimating PK parameters; it will
                           generate ivivc_pk_values.Rdata after fitting;
                          
2. ivivc_pk_values.RData - store previously fitted PK parameters (kel, ka, and Vd);
                           only 'kel' will be used later for ivivc modeling.

3. ivivc_test_data.Rdata - the dataset includes in-vivo (conc.obs) and in-vitro 
                           (FRD, %) dataset all together; the dataset for in-vivo
                           is only for "dosed once" for each dosage form. Thus,
                           the dataset will repeat for different pH dissolution
                           profile (such as 1.2 & 6.8).
