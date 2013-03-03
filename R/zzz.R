# change the following line since R v.2.15.3 as .onAttach or .onLoad (either works fine) [2013/3/4 AM06:27:15]
# .First.lib <- function(...) {

.onAttach <- function(lib, pkg)  {

# echo output to screen
packageStartupMessage("
****************************************************************************
*                                                                          
*                                  ivivc                                   
*                                 v 0.1.6                                   
*                                                                           
* In vitro-in vivo correlation (IVIVC) is defined as the correlation       
* between in-vitro drug dissolution and in-vivo drug absorption.           
* This package is used to develop and validate an IVIVC model.                           
* The following steps will be conducted:                                   
* -> 1: Input/Edit In-vivo absorption data: IV, oral solution or IR drug   
* -> 2: Develop an IVIVC model: Fitting IV, Oral solution or IR drug       
* -> 3: Input/Edit In-vitro dissolution data and in-Vivo absorption        
*       Data: ER drug with different release rates                           
* -> 4: Develop an IVIVC model: Model Dependent Method                     
* -> 5: Evaluate an IVIVC model: Prediction error                          
*                                                                          
*           Please type 'run()' to get started.                            
*                                                                          
****************************************************************************")
}
