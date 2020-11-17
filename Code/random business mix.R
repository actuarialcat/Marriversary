library(data.table)



# random metrics -------------------------------------------------

tables_file = "tables/"

age_mix = fread(paste(tables_file, "business_mix_age.csv", sep = ""), header = TRUE)
age_mix = melt(age_mix, "F_age", variable.name = "M_age", value.name = "prob", variable.factor = FALSE)
age_mix[,M_age := as.numeric(M_age)]
setkey(age_mix, F_age)


m_smk_prob = 0.19
f_smk_prob = 0.06

m_pay_prob = 0.80

prem_mix = fread(paste(tables_file, "business_mix_prem_mode.csv", sep = ""), header = TRUE)
prem_mix = melt(prem_mix, "mat_year", variable.name = "prem_mode", value.name = "prob", variable.factor = FALSE)
setkey(prem_mix, mat_year)


# random generator -------------------------------------------------

random_policy = function(){
    valid = FALSE
    
    while(!valid){
        rnd = runif(5)
        
        # age
        found = FALSE
        i = 1
        while(!found){
            if (rnd[1] <= age_mix[i, prob]){
                found = TRUE
            } else {
                i = i + 1
            }
        }
        M_age = age_mix[i, M_age]
        F_age = age_mix[i, F_age]
        
        # smk
        if (rnd[2] < m_smk_prob){
            M_smk = "S"
        } else {
            M_smk = "N"
        }
        
        if (rnd[3] < f_smk_prob){
            F_smk = "S"
        } else {
            F_smk = "N"
        }
        
        # sex
        if (rnd[4] < m_pay_prob){
            P_sex = "M"
            P_age = M_age
            P_smk = M_smk
            B_sex = "F"
            B_age = F_age
            B_smk = F_smk
            
        } else {
            P_sex = "F"
            P_age = F_age
            P_smk = F_smk
            B_sex = "M"
            B_age = M_age
            B_smk = M_smk
        }
        
        # prem_mode
        found = FALSE
        i = 1
        while(!found){
            if (rnd[5] <= prem_mix[i, prob]){
                found = TRUE
            } else {
                i = i + 1
            }
        }
        mat_year = prem_mix[i, mat_year]
        prem_mode = prem_mix[i, prem_mode]
        
        # valaidate
        if (M_age + mat_year <= 65 && F_age + mat_year <= 65){
            valid = TRUE
        } else {
            valid = FALSE
        }
        
    }
    
    # output
    out_df = data.table(
        P_sex = P_sex,
        P_age = P_age,
        P_smk = P_smk,
        B_sex = B_sex,
        B_age = B_age,
        B_smk = B_smk,
        prem_mode = prem_mode,
        mat_year = mat_year
        
    )   
    
    return (out_df)
}



# ran all  -------------------------------------------------

outputs = list()

for (i in 1:2500){
    outputs = append(outputs, list(random_policy()))
    
    if (i %% 100 == 0){
        print (i)
    }
}

df = rbindlist(outputs)

fwrite(df, paste(tables_file, "business_mix_results.csv", sep = ""))












