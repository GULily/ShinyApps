## IMPORTANT
## use the following code to install the dev version of plotly
# devtools::install_github('ropensci/plotly')
## The dev version makes sure the hover info appears on the map
## When you update this application, you need to reinstall the dev version of poorly


## Update logs:
	Old files:
	- colormatrix.RData
	- WorldPoints.RData
	- UN_vote.RData (contains two datasets)
		- vote (1-68)
		- session (1-68)

	New files:
	- UNVotes1-72full.RData (contains one dataset)
		-Votesnew ==> replace "vote"
	- UNDescriptions1-72.RData ==> replace "session"


## the color of Plotly is automatically scalable, 
## thus I wrote if else functions to test the following cases. 

## Plotly Output Test Cases:
	(Yes-1, Absent-1.5, Abstain-2, Nay-3.)
	Case: Yes, Absent, Abstain, Nay
	Case: Yes, Abstain, Nay
	Case: Yes, Nay
	Case: Yes, Absent, Abstain
	Case: Yes, Absent
	Case: Yes, Abstain
	Case: Yes


