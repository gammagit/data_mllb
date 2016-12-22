Raw data
========

» Files are named as follows:
    -- Raw data files (CSV): data_sub_<subject_id>_block_<block_id>.csv
       (<block_id> 3 refers to combined data for blocks 1&2)

» The first row of each data file is the header.
    -- Data recorded in each column is as follows:
        -- Block type (easy, difficult or mixed)
        -- Coherence of stimulus pointing down
        -- Decision (0 = Up, 1 = Down)
        -- Evidence: Sum of up and down stims (Down = +1, Up = -1)
        -- High_evidence: Highest value of evidence before decision
        -- time: Number of stimuli displayed before decision

Data Analysis
=============
To analyse results (David, you can skip steps 1 & 2):

1) Create CSV files by running matlab script combine_results.m

2) Postprocess files by substituting conditions 1,2,3 with easy,diff,mix in all the csv files:
    -- In Vim, :bufdo! 1,$s/^1/easy/
               :bufdo! 1,$s/^2/diff/
               :bufdo! 1,$s/^3/mix/
               :bufdo! w

3) Analyse CSV files in R by running the script analysis_script.R
    -- Change the variable 'blockno' to the block being analysed.

4) View results in file plots_allsubs_block<block_id>.pdf

5) All R variables for each subject stored in results_sub_<subject_id>_block_<block_id>.RData


Note: For seeing analysis of block 2,
 -- run combine_results(2),
 -- change blockno in analysis_script.R,
 -- view results in plots_allsubs_block2.pdf and
 -- analysis stored in results_sub_<ix>_block_2.RData.
