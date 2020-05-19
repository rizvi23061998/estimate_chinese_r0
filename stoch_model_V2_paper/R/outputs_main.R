# Run analysis--------------------------------------------------------------



# - - -
# Run bootstrap SMC 
run_fits(rep_plot=100, # number of repeats
         nn=2e3, #number of particles
         dt=0.25,
         filename="1"
)

print("Bootstrap fit done .. .. .. ")

# Output plots
plot_outputs(filename="1") # For 
print("Plotting done .. .. .. ")
plot_dispersion(filename="1") # For R0 
#

# Run models --------------------------------------------------------------



