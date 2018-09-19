(False,25  ,("(F,L,F,F,F,L,wF)","(F,L,F,F,F,L,wE)"),"(F,L,F,F,F,wF)")                
-> The experts suggest to correct the erroneous control flow statement. We correct the erroneous movement.
& [(True,25,("(F,L,F,F,F,L,waFF)","(F,L,F,F,F,waFF)"),"(F,L,F,F,F,waFF)"), (True,25,("(F,L,F,F,L,wF)","(F,L,F,F,wF)"),"(F,L,F,F,wF)")]
(False,224 ,("(F,L,F,L,F)","(F,L,F,F)"),"(F,L,F,L)")                                 
-> {\color{red} The experts delete the erroneous rotation and stay on path. We make an error by deleting the wrong movement.}
& [(True,224,("(F,L,F,F,F,L)","(F,L,F,F,F)"),"(F,L,F,F,F)")]
(False,115 ,("(F,L,w(F,aLF))","(F,L,waLF)"),"(F,L,w(F,waLF))")                       
-> {\color{red} The experts favor a deletion over an insertion, which both lead to a correct solution.}
& [(False,221 ,("(F,L,w(F,lLF))","(F,w(F,lLF))"),"(F,L,wlLF)") {-ZIE VOLGENDE-}]
(False,221 ,("(F,L,w(F,lLF))","(F,w(F,lLF))"),"(F,L,wlLF)")                          
-> The experts delete the erroneous movement in the first statement list, whereas we delete the error in the deepest statement list.
& [(True,221,("(F,L,w(F,aFL))","(F,L,waFL)"),"(F,L,waFL)")]
(False,115 ,("(F,aLF)","aLF"),"(F,waLF)")                                            
-> The experts delete the erroneous movement whereas we insert the correct start.
& [(True,115,("(F,aLL)","(F,waLL)"),"(F,waLL)") ]

(False,0   ,("(F,aLF,wF)","(F,aLE,wF)"),"(F,aLF,wE)")                                
-> The experts suggest to correct the conditional statement starting at the else block. We correct the last error.
& [(True,0,("(aLR,wF)","(aLR,wE)"),"(aLR,wE)"), (True,0,("(F,L,F,F,F,L,aFF,wF)","(F,L,F,F,F,L,aFF,wE)"),"(F,L,F,F,F,L,aFF,wE)")]
((False,115,("(F,aLR)","(F,aLL)"),"(F,waLR)"),(9,9,9))                               
-> The experts substitute an error in the conditional statement, whereas we insert the correct start.
& [(True,115,("(F,aRF)","(F,waRF)"),"(F,waRF)")]
((False,103,("(F,lLF,F,F,F,L,wF)","(lLF,F,F,F,L,wF)"),"(F,aLF,F,F,F,L,wF)"),(0,9,9)) 
-> The experts remove the individual erroneous movement first, we correct the first erroneous control flow statement.
& [(True,103,("(F,lLF,wF)","(F,aLF,wF)"),"(F,aLF,wF)")]
((False,20,("(F,w(F,L))","w(F,L)"),"(F,wF)"),(9,8,9))                                
-> The experts suggest to remove the first forward movement so that the student makes the corner. We remove the erroneous rotation.
& [(True,20,("(F,w(L,F))","(F,wL)"),"(F,wL)")]
((False,31,("(F,waLF)","(F,waLE)"),"(F,waEF)"),(9,9,9))                              
-> The experts suggest to remove the correct the errors in the conditional statement in last to first order. Whereas we suggest a correction in first to last order.
& [(True,32,("(F,waLR)","(F,waER)"),"(F,waER)")]

((False,33,("(F,waLL)","waLL"),"(F,waEL)"),(9,10,9))                                 
-> The experts suggest to delete the first erroneous movement to make the student start correct. We suggest a deletion that makes the student turn the corner.
& [(True,33,("(F,waRL)","(F,waEL)"),"(F,waEL)")]
((False,224,("(aFL,F)","w(aFL,F)"),"aFL"),(9,0,9))                                   
-> {\color{red} Our error. The experts suggest a single edit that leads to the goal. We make a detour by deleting the erroneous movement}
& [(True,224,("(aFL,L)","aFL"),"aFL")]
((False,20,("(aFL,w(L,F))","(aFL,wF)"),"(aFL,wL)"),(8,9,9))                          
-> {\color{red} Our error. We should always direct to nearest partial solution if single edit is one.}
& [(True,20,("(aFL,w(F,L))","(aFL,wF)"),"(aFL,wF)")]
((False,11,("aLR","aER"),"waLR"),(10,10,10))                                         
-> The experts suggest to delete the error in the conditional statement whereas we choose to insert the correct solution
& [(True,11,("aRL","waRL"),"waRL")]
((False,11,("aRF","aEF"),"waRF"),(10,10,10))                                         
-> The experts suggest to delete the error in the conditional statement whereas we choose to insert the correct solution
& [(True,11,("aLF","waLF"),"waLF")]

((False,20,("aw(F,L)F","a(F,L)F"),"awFF"),(8,9,9))                                   
-> {\color{red}Experts remove the repeat-until block while we correct the erroneous rotation inside the repeat-until block (wF also removes WHILE) }
& [(True,20,("(aFL,w(F,L))","(aFL,wF)"),"(aFL,wF)")]
(False,20  ,("w(F,L,aFF,F)","w(F,aFF,F)"),"w(F,L,aFF)")                              
-> The experts suggest to delete an inner erroneous rotation, whereas we delete the last erroneous rotation that keeps the student longer on path.
& [(True,20,("w(F,L,aFL,F)","w(F,L,aFL)"),"w(F,L,aFL)")]
(False,25  ,("w(F,L,aRF)","w(F,L,aEF)"),"w(F,aRF)")                                  
-> The experts suggest to handle the errors in the conditional statement first, whereas we remove the erroneous rotation inside the statement list.
& [(True,25,("w(F,L,aLR)","w(F,aLR)"),"w(F,aLR)")]
(False,25  ,("w(F,L,lFF)","w(F,L,aFF)"),"w(F,lFF)")                                  
-> The experts suggest to handle the errors in the conditional statement first, whereas we remove the erroneous rotation inside the statement list.
& [(True,25,("w(F,L,aLF)","w(F,aLF)"),"w(F,aLF)")]
(False,104 ,("w(F,a(L,F)(R,F))","w(F,a(L,F)(L,F))"),"w(F,aF(R,F))")                  
-> The expers' suggest a substitution in the else-block of the conditional statement, which leads the student around the corner. We suggest to handle the error in the true-block
& [(True,104,("w(F,a(L,F)(L,F))","w(F,aF(L,F))"),"w(F,aF(L,F))")]

(False,14  ,("w(F,aLR)","waLR"),"w(F,aER)")                                          
-> The experts suggest to remove the erroneous forward movement, we correct the error in the true-block of the conditional statement.
& [(True,14,("w(F,aRL)","w(F,aEL)"),"w(F,aEL)")]
(False,14  ,("w(F,aRR)","w(F,aRL)"),"w(F,aER)")                                      
-> The experts suggest to substitute the error in the else-block, whereas we correct the error in the true-block.
& [(True,14,("w(F,aLF)","w(F,aEF)"),"w(F,aEF)")]
(False,13  ,("w(F,lFF)","wlFF"),"w(F,aFF)")                                          
-> The experts suggest to remove the erroneous forward movement, we correct the error in the true-block of the conditional statement.
& [(True,13,("w(F,lFL)","w(F,aFL)"),"w(F,aFL)")]
(False,107 ,("w(F,lL(F,F))","w(F,aL(F,F))"),"w(F,lLF)")                              
-> The experts substitute the erroneous condition, whereas we suggest a deletion that leads to a partial 
& [(True,107,("w(F,lL(F,R))","w(F,lLF)"),"w(F,lLF)"), (True,107,("w(F,l(L,R)F)","w(F,lLF)"),"w(F,lLF)")]
(False,31  ,("w(F,lLF,lLF)","w(F,lLF,lEF)"),"w()")                                   
-> {\color{red}ERROR in our model}
& [(True,31,("w(F,lLF,aLF)","w(F,lLF,aEF)"),"w(F,lLF,aEF)")]

(False,13  ,("w(F,lRR)","w(F,lLR)"),"w(F,aRR)")                                      
-> The experts suggest to correct the true-statement and lead the student around the corner. We suggest to correct the condition in the if-then-else statement.
& [(True,13,("w(F,lRL)","w(F,aRL)"),"w(F,aRL)")]
(False,20  ,("w(aRL,F)","w(aEL,F)"),"waRL")                                          
-> {\color{red}The experts suggest a correction that leads the student directly to the goal}
& [{-Still working on it but-} (True,20,("w(lLF,F)","wlLF"),"wlLF")]
(False,24  ,("waF(R,F)","waF(L,F)"),"waFR")                                          
-> {\color{red}The experts suggest a substitution that directly leads the student to the goal}
& [(True,107,("waF(R,L)","waFL"),"waFL")]