# change the following line since R v.2.15.3 as .onAttach or .onLoad (either works fine) [2013/3/4 AM06:27:15]

.onAttach <- function(lib, pkg)  {

# echo output to screen
packageStartupMessage("
.....................................................................

  ivivc for R 
  v0.1.9                                              
                                                                     
  In vitro-in vivo correlation (IVIVC) is defined as the correlation 
  between in-vitro drug dissolution and in-vivo drug absorption.     
  This package is used to develop and validate an IVIVC model.       
  The following steps will be conducted:                             
  -> 1: Input/Edit In-vivo data: such as IV, oral solution or IR     
  -> 2: Calculate required PK parameters: fitting IV, oral solution  
        or IR                                                        
  -> 3: Input/Edit in-vitro dissolution data and in-vivo absorption  
        data: ER formulations with different release rates            
  -> 4: Develop an IVIVC model: model dependent method               
  -> 5: Evaluate an IVIVC model: prediction error                    
                                                                     
   Please type 'run()' to get started.                               
                                                                     
.....................................................................")
}
