#!/bin/bash

#SBATCH -J cad
#SBATCH -o e_o_files/cad.%j.o
#SBATCH -e e_o_files/cad.%j.e
#SBATCH -p normal
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -t 48:00:00
#SBATCH -A OTH24018
#SBATCH --mail-user=xliaoyi@my.utexas.edu
#SBATCH --mail-type=all

module load hdf5
module unload impi

source /work/09059/xliaoyi/ls6/software/miniconda/etc/profile.d/conda.sh
conda activate igc

pheno="cad"
sst_file=$1
outname=$2

out_sst_file="/scratch/09059/xliaoyi/dev_gwas/$(basename ${sst_file%.txt})_edit.txt"

prscs="/scratch/09059/xliaoyi/dev_gwas/PRScs/PRScs.py"
ref_dir="/scratch/09059/xliaoyi/dev_gwas/ldblk_ukbb_eur"
bim_prefix="/scratch/07880/devansh/CAD/New_PRS/PRS_INPUT_FILES/for_liaoyi/subset_data"
n_gwas=1254477

out_dir="/scratch/09059/xliaoyi/dev_gwas/PRSCS_OUTPUT/${pheno}"
concat_out=${out_dir}/prs_${pheno}_${outname}.txt

bfile_dir="/scratch/07880/devansh/CAD/New_PRS/PRS_INPUT_FILES/for_liaoyi/subset_data"
plink2="/scratch/09059/xliaoyi/harpak_lab/ukb/EXECUTABLES/plink2"

mkdir -p ${out_dir}


###
### format the sumstats so can feed to PRScs
###

# echo "Processing  $sst_file ..."

# awk 'BEGIN {FS=OFS="\t"}
# NR==1 {
#   for (i=1;i<=NF;i++) {
#     if ($i=="SNP") snp=i
#     else if ($i=="A1") a1=i
#     else if ($i=="A2") a2=i
#     else if ($i=="BETA") beta=i
#     else if ($i=="P") p=i
#   }
#   print "SNP", "A1", "A2", "BETA", "P"
# }
# NR>1 {print $snp, $a1, $a2, $beta, $p}' ${sst_file} > ${out_sst_file}


###
### Run PRScs
###

python ${prscs} \
    --ref_dir=${ref_dir} \
    --bim_prefix=${bim_prefix} \
    --sst_file=${sst_file} \
    --n_gwas=${n_gwas} \
    --out_dir=${out_dir}/${pheno}_${outname}


###
### concatenate
###

rm ${concat_out}

for i in {1..22}; do
    cat ${out_dir}/${pheno}_${outname}_pst_eff_a1_b0.5_phiauto_chr${i}.txt >> ${concat_out}  &&  \
    rm ${out_dir}/${pheno}_${outname}_pst_eff_a1_b0.5_phiauto_chr${i}.txt
done

###
### score
###
echo "Processing ${concat_out} ..."

time ${plink2} \
    --bfile ${bfile_dir} \
    --score ${concat_out} 2 4 6 cols=+scoresums \
    --out ${out_dir}/prs_output_${pheno}_${outname}
