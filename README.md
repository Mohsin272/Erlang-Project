# Erlang Project 
### **Brief:** 
This project creates a set of distributed communicating processes that communicate using the RIP protocol and compute primes.

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
	PID1 = erl_proj:launchNode(joe),
	PID2 = erl_proj:launchNode(bob),
	PID3 = erl_proj:launchNode(fred),
	PID4 = erl_proj:launchNode(amy),
	PID5 = erl_proj:launchNode(dan),
	PID6 = erl_proj:launchNode(mike),
	PID7 = erl_proj:launchNode(ann),
	PID8 = erl_proj:launchNode(jack),
	PID9 = erl_proj:launchNode(mohsin),
	PID10 = erl_proj:launchNode(jeff).
	```
	- **Connect Nodes** ->
	```
	erl_proj:connectNode(joe, PID1 , bob, PID2 ),
	erl_proj:connectNode(bob, PID2 , fred, PID3 ),
	erl_proj:connectNode(fred, PID3 , amy, PID4 ),
	erl_proj:connectNode(amy, PID4 , dan, PID5 ),
	erl_proj:connectNode(dan, PID5 , mike, PID6 ),
	erl_proj:connectNode(mike, PID6 , ann, PID7 ),
	erl_proj:connectNode(ann, PID7 , jack, PID8 ),
	erl_proj:connectNode(jack, PID8 , mohsin, PID9 ),
	erl_proj:connectNode(mohsin, PID9 , jeff, PID10 ),
	erl_proj:connectNode(jeff, PID10 , joe, PID1 ).
	```
	- **Retrieve Answer** ->
	```erlProject:rpc(joe,{computeNthPrime,5,joe,bob,1}).```
	- **Print Routing Table** ->
	```erlProject:printTable(PID1).```
	
- **List of any required Libraries, platform issues, etc.:**
	- **Linux/Windows** - Environment
	- **Erlang** - Language
	- **emacs/VSC** - Editor
	- **doxygen** - Code documentation (optional)
	
- **List of files and what they contain:**
TBC.....
