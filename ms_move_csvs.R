# Master script: move csvs

# This script will create a new output folder for all graphs produced by Engagement_Metrics_IV
# It will also move all of the stale looker csvs into the old_csvs directory
# It will also move all of the freshly downloaded csvs into the input_csvs directory

# This script is designed to be run from the terminal. You have to pass it the following parameteres:

# 1. The date associated with the new looker csvs
# 2. The location of the new looker csvs
# 3. The location of the old_csvs directory
# 4. The location of the input_csvs directory

dir.create()