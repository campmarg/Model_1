#Read.ME

#Code to run model and get graphs as in Modeling fishery consequences of 
#spatial closures for offshore energy: loss of fishing area and missing data
#by Campbell et al

#First decide what species running - dover sole, sablefish, lingcod, widow rockfish, yellowtail rockfish

#set up parameters for that species by copying the file for that species
#from the Parameters folder and putting the correct parameters into tparam.R
#in in mypackage -> R -> tparam.R

#next open the same species you are using in the HomeRanges folder and copy that
#and put it into the home_range.R file in mypackage -> R -> home_range.R

#make sure the package is updated to correctly correspond to this

#Now that you have the correct species paramaters and homerange loaded make
#sure you go to the correct file to run the model
# dover sole -> go to M1CF file
# sablefish -> go to M1CFRF file
# lingcod -> go to M1CF file
# widow rockfish -> go to M1CFRF file
# yellowtail rockfish -> go to M1CFRF file

#Now you can run the model

#To create the graphs, make sure the species age is inputted in the code starting
#line 117
