# Erlang Project 
### **Brief:** 
This project createa a set of distributed communicating processes that communicate using the RIP protocol and compute primes.

- **Project Licence:** *This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/ or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.*\
<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.

- **Authors of Project:**
	- **Student:** Mohsin Tahir
	- **Lecturer:** Joseph Kehoe
- **Student Number:** C00250220
- **Institution:** SETU Carlow
- **Date:** 31/03/23<br>

- **How to run project:**
	- **Compile** ->  ```c(erl_proj).```
	- **Launch Nodes** -> 
	```
	PID1 = erl_proj:launchNode(jane),
	PID2 = erl_proj:launchNode(phil),
	PID3 = erl_proj:launchNode(maxine),
	PID4 = erl_proj:launchNode(jessica),
	PID5 = erl_proj:launchNode(fillipe),
	PID6 = erl_proj:launchNode(jo),
	PID7 = erl_proj:launchNode(max),
	PID8 = erl_proj:launchNode(jack),
	PID9 = erl_proj:launchNode(bruno),
	PID10 = erl_proj:launchNode(jeff).
	```
	- **Connect Nodes** ->
	```
	erl_proj:connectNode(jane, PID1 , phil, PID2 ),
	erl_proj:connectNode(phil, PID2 , maxine, PID3 ),
	erl_proj:connectNode(maxine, PID3 , jessica, PID4 ),
	erl_proj:connectNode(jessica, PID4 , fillipe, PID5 ),
	erl_proj:connectNode(fillipe, PID5 , jo, PID6 ),
	erl_proj:connectNode(jo, PID6 , max, PID7 ),
	erl_proj:connectNode(max, PID7 , jack, PID8 ),
	erl_proj:connectNode(jack, PID8 , bruno, PID9 ),
	erl_proj:connectNode(bruno, PID9 , jeff, PID10 ),
	erl_proj:connectNode(jeff, PID10 , jane, PID1 ).
	```
	- **Retrieve Answer** ->
	```erlProject:rpc(joe,{computeNthPrime,5,joe,bob,1}).```
	- **Print Routing Table** ->
	```erlProject:printTable(PID1).```
	
- **List of any required Libraries, platform issues, etc.:**
	- **Linux** - Environment
	- **Erlang** - Language
	- **emacs/VSC** - Editor
	- **doxygen** - Code documentation (optional)
	
- **List of files and what they contain:**
TBC.....
