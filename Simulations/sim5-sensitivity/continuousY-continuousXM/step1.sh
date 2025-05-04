module load python/3.8
module load R/4.2.2
dsq_path="/home/aguo28/dSQ-master"

##########################
# This is step1_dsq.sh
##########################

# Prompt user for input
echo "Enter the name for dsq job:"
read sim_input

echo "Enter jobfile prefix:"
read prefix

echo "Enter the number of jobs to run:"
read n

echo "Submit jobs after preparation? (T/F):"
read submit

# Step 1: Generate .sh and .txt files
for i in $(seq 1 ${n}); do
    python "$dsq_path/dSQ.py" \
        --job-file ${prefix}_n$i.txt \
        --batch-file ${prefix}_n$i.sh \
        --job-name "${sim_input}-${prefix}-$i" \
        --mail-type ALL \
        -o dsq/dsq-jobfile-%A_%a-%N.out
done

##########################
# This is step2_modify.sh
##########################

for i in $(seq 1 ${n}); do
  # Ensure batch script runs with python
  sed -i 's@'"$dsq_path"'/dSQBatch.py@python '"$dsq_path"'/dSQBatch.py@' "${prefix}_n$i.sh"

  # Add email, memory, and modules
  sed -i '/#SBATCH --mail-type "ALL"/a #SBATCH --mail-user=anna.guo@emory.edu\n#SBATCH --mem 1G\nmodule load python/3.8\nmodule load R/4.2.2' "${prefix}_n$i.sh"

  # Specify partition
  sed -i '1a #SBATCH --partition=day-long-cpu,short-cpu,empire' "${prefix}_n$i.sh"
done

##########################
# This is conditional submit step
##########################

if [[ "$submit" == "T" || "$submit" == "true" ]]; then
  echo "Submitting jobs..."
  for i in $(seq 1 ${n}); do
    sbatch ${prefix}_n$i.sh
  done
else
  echo "Skipping job submission."
fi