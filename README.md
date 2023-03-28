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
	- **Compile** ->  ```c(erlProject).```
	- **Launch Nodes** -> 
	```
	PID1 = erlProject:launchNode(joe),
	PID2 = erlProject:launchNode(bob),
	PID3 = erlProject:launchNode(fred),
	PID4 = erlProject:launchNode(amy),
	PID5 = erlProject:launchNode(dan).
	```
	- **Connect Nodes** ->
	```
	erlProject:connectNode(joe, PID1 , bob, PID2 ),
	erlProject:connectNode(bob, PID2 , fred, PID3 ),
	erlProject:connectNode(fred, PID3 , amy, PID4 ),
	erlProject:connectNode(amy, PID4 , dan, PID5 ),
	erlProject:connectNode(dan, PID5 , joe, PID1 ).
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
