(seq
;;;;;;;;;; ./makeg2dlib loaded ../GUI/iniDialog.lsp ;;;;;;;;;;
;(seq
;This text makes a JTextArea component with the given settings.
; *Inputs
;-text: a string with the text that will be showed.
;-width: width of the JTextArea component.
;-heigth: height  of the JTextArea component.
;-color: font color.
;-font: name of the font.
;-style: font style (Values: 0-> PLAIN , 1 -> BOLD , 2 -> ITALIC)
;-fontSize: character's size.
;-backColor: JTextArea component's background color.
;-marginv: value for the top, botton, left and right margins.
; *Output
;-The JTextArea component with the given settings.

(define makeTextArea2 (text width height color font style fontSize backColor marginv)
  (let ( (text (object ("javax.swing.JTextArea" text)))
         (font (object ("java.awt.Font" font style fontSize)))
         (margin (object ("java.awt.Insets" marginv marginv marginv marginv )))
        )
    (seq
      (invoke text "setMaximumSize" (object ("java.awt.Dimension" width height))) 
      (invoke text "setAlignmentX" java.awt.Component.LEFT_ALIGNMENT)
      (invoke text "setForeground" color) ;font color
      (invoke text "setFont" font)
      (invoke text "setMargin" margin )
      (invoke text "setBackground" backColor)
      (invoke text "setEditable" (boolean false))
      text
    )
   )
 ) ;end makeTextArea


; text for the initial dialog.
(define iniDialogText
  "Please, wait while Maude-NPA is initializating the GUI.
This may take a few minutes." 
)

;iniDialog is a dialog which will be showed after loading a protocol,
;while the manager frame is being created.
 (define iniDialog (let ( (temp (object ("g2d.graphviewer.ProgressDialog" )))
                          (regularColor  (object ("java.awt.Color" (int 0) (int 0) (int 0))))
                          (greySilver  (object ("java.awt.Color" (int 240) (int 240) (int 240))))
                        (progressArea (apply makeTextArea2 iniDialogText (int 350) (int 60) regularColor "Arial" (int 0) (int 13) greySilver (int 15) ))
                       )
                   (seq
                     (invoke temp "add" progressArea java.awt.BorderLayout.NORTH)
                     (invoke temp "setSize" (int 380) (int 130) )
                     (invoke temp "setLocation" (int 100) (int 100) )
                     temp
                   )
                 )
 )


    (invoke iniDialog "setVisible" (boolean true))
    (invoke iniDialog "toFront")
;)

;;;;;;;;;; ./makeg2dlib loaded ../GUI/auxiliar-functions.lsp ;;;;;;;;;;

(seq
 
; main function that transforms a string into an arrayList which contains idSystem States in array format  
; each idSystem State array is composed of three elements: id, label and state (state= current strands, intruder knowledge, sequence of messages and additional data
; input:
; - str: string with all the information that Maude-NPA outputs after a run command relative to nodes.
; output:
; - arrayList of three element arrays

(define idSysS2Arr (str)
 ( let( (strShort (apply quitInitialBlank str)) ; delete the initial blank spaces.
        (array (object ("java.util.ArrayList")))
       )            
    (if (= (invoke str "replace" " " "")
           "(empty).IdSystemSet") ;this is to deal with "empty" values. Check if this would be the input in this case!
        array
        (apply idSysS2ArrRec (int 0) (int 0)  (int 0) strShort "" array ) ;get each node's information and add it to array. Return the array.
    ) ;end if
  )
) ;end idSysS2Arr

; recursive method that gets each node's information and add it to the array list 
; Matches the first parent with the last parent (with variables openPCounter and closePCounter).
; Every node's information is enclosed by parents, except the last element.
; *inputs:
; - openPCounter: counter for openned parents processed until this moment
; - closePCounter: counter for openned parents processed until this moment
; - pos: index (in the source string) of the current character that the function is processing.
; - str: string with the information that has not been processed yet.
; - currentNode: current node's processed information (the function appends the current character to this string)
; - array: arrayList which contains the three elements arrays that represent each node.
; *output:
; - array

(define idSysS2ArrRec (openPCounter closePCounter pos str currentNode array)
 ( if (>= pos (invoke str "length") ) ; end
        array 
      (let (  (strShort (apply quitInitialBlank str)) ;delete possible blank characters which are not necessary
            )
         (if (= (invoke str "startsWith" "(") (boolean false)) ; the information that is being processed corresponds to the last element.
             (seq
               (invoke array "add" (apply str2IdArr str)) ;process the string in order to obtain the three elements array
               array ; return the array (end)
             )
             (let ( (currentChar (invoke str "charAt" pos)) ;character that is being processed
                  )
                (if (and (= openPCounter closePCounter) (!= openPCounter (int 0))) ; we have finished to process an idSys string. 
                   (let ( (newStr (invoke (apply quitInitialBlank (invoke str "substring" pos)) "replace" "\n" "")) ;delete the information already processed
                          (newIdSys (apply str2IdArr currentNode)) ;translate the string into a three elements array
                        )
                     (seq                    
                      (invoke array "add" newIdSys) ;Add a new idSys 
                      (apply idSysS2ArrRec  (int 0) (int 0) (int 0) newStr "" array) ;call again the method for the next  idSys strings
                     )
                   )
                   ;else continue processing the string until it finds the last paren that matches the first paren that wraps the idSys
                   (if (= currentChar (char '('))
                      (apply idSysS2ArrRec (+ openPCounter (int 1)) ;update openned parentesis counter  and append the current char to currentNode string 
                          closePCounter (+ pos (int 1))
                          str
                          (concat currentNode currentChar)
                          array)         
                    (if (= currentChar (char ')')) ; update closed parentesis counter and append the current char to currentNode string      
                       (apply idSysS2ArrRec openPCounter
                          (+ closePCounter (int 1))
                          (+ pos (int 1))
                          str
                          (concat currentNode currentChar)
                          array) 
                       (apply idSysS2ArrRec ; if currentChar is not a parent, append the currentChar to currentNode string
                           openPCounter
                           closePCounter
                           (+ pos (int 1))
                           str
                           (concat currentNode currentChar)
                           array) 
                    ) ;end if (= currentChar (char ')')) (note: your editor may not match this parent correctly because of the ')' )
                  ) ;end if (= currentChar (char '('))
                ) ;end if
              )
         ) ;end  if
       ) 
  ) ;end if
) ;end idSysS2ArrRec




; recursive function to delete initial blank spaces
; *input:
; - str: string possible with blank spaces in its initial positions.
; *output:
; - string without initial blank spaces

(define quitInitialBlank (str)
 (try
 (seq
   (if (> (invoke str "length") (int 0) )
     (if (= (invoke str "charAt" (int 0)) (char ' ') ) 
       (apply quitInitialBlank (invoke str "substring" (int 1)) ) ; if the first character is a blank space, delete if and call again the function
       str ; there are no more 
     )
   ) ;end main if
 )
 (catch Exception
   (seq
     (invoke java.lang.System.err "println" (concat "Error in quitInitialBlank: " (invoke Exception "getMessage")))
     str
   )
 )
 ) ;end try
) ;end quitInitialBlank




; funtion that transforms only one idSystem string into a three elements array
; *input:
; - str: string which contains the information relative to only one node.
; *output
; - a three elements array: node id, node label, node state
(define str2IdArr (str)
  (let( ;initialize the array which will contain the node's id, the label (in array format) and the state
      (arrRes (mkarray java.lang.Object (int 3)))
      ; position in which the state information starts
      (iniIndexState (invoke str  "indexOf" "::"))
      ;to obtain the position in which the states ends
      (endIndexState (if  (= (invoke str "startsWith" "(<") (boolean false)) 
                              (invoke str   "length") ;there is no closing parent at the end, so we get the whole string
                              (- (invoke str   "length") (int 1)))) ;delete the last closing parent
      ; get the substring which corresponds to the state
      (state (invoke str  "substring"  iniIndexState endIndexState ))
      ; to obtain the label in array form.
      (label (apply label2Arr (invoke str "substring" (int 0) iniIndexState)))
     )
   (seq
   
    (aset arrRes (int 0) (int -1))  ; set id -1 is an initial id value. Later the id will be changed once the node is going to be added to the graph
    (aset arrRes (int 1) label) ; set label
    (aset arrRes (int 2) state) ; set state
    arrRes
   )
  )
) ;end str2IdArr



;function which translates the label (in string format) into an array.
;*input
; - str: label in string format
;*outputs
; - an array. Each of its elements corresponds to a level in the tree.

(define label2Arr (str)
   ( let ( ;delete blank spaces and break of lines characters.
           (trimmedStr (invoke (invoke str "replace" " " "") "replace" "\n" ""))
           
           ;if the node has more than one strand, the state information starts with a parent and str contains it so it has to be removed
           ; (in strId2Arr the substring "::" was used as a marker to know when the state information starts) 
           (endIndex (if (= (invoke trimmedStr "endsWith" "(") (boolean false))
                         (invoke trimmedStr "length")
                         (- (invoke trimmedStr "length") (int 1))))
           
           ; label in string format without some symbols we don't need
           (initialLab (if (= (invoke trimmedStr "startsWith" "(<") (boolean false) ) 
                            ;  last ore unique node. For example: <1>
                           (invoke trimmedStr "substring" (int 1) (- endIndex (int 1)))  ; just delete the symbols < and >
                           ; else case: there are more nodes after this. For example (<1.2>...)
                           (invoke trimmedStr "substring" (int 2) (- endIndex (int 1)))   ;  delete the symols (< and >
                           ))
           
           ; list to store the elements of a label, i.e, the id number of the node in each level of the tree.
           (emptyList (object ("java.util.ArrayList")))     
         )
      (seq
         (apply getListRec emptyList initialLab ) ; translate the label string into an arrayList
         (invoke emptyList "toArray") ; convert the arrayList into an array
      )     
   )
) ;end label2Arr



; function to obtain an arrayList with the different numbers of a label, using "." as a separator.
;* inputs
; -list: arraylist which will contain the id number of the node in each level of the tree
; -str: part of the label (in string format) which still has not being processed
;* outputs:
;- list: with all the elements.
(define getListRec (list str)
 (let ( (index (invoke str "indexOf" "."))
        )
 (if (= index (int -1)) ;the last level of the label is going to be processed. Add it and return the arrayList
     (seq
       (invoke  list "add" (invoke str "trim") )
       list
     )
   ;else, there are more levels to process. 
   (let ((number (invoke (invoke str "substring" (int 0) index) "trim")))
      (seq
        (invoke list "add" number) ;Add the current level of the label 
        (apply getListRec list  (invoke str "substring" (+ index (int 1)) )) ;and call again the function with the updated input string
      )
    )
   )
 )
) ;end getListRec



;This text makes a JTextArea component with the given settings.
; *Inputs
;-text: a string with the text that will be showed.
;-width: width of the JTextArea component.
;-heigth: height  of the JTextArea component.
;-color: font color.
;-font: name of the font.
;-style: font style (Values: 0-> PLAIN , 1 -> BOLD , 2 -> ITALIC)
;-fontSize: character's size.
;-backColor: JTextArea component's background color.
;-marginv: value for the top, botton, left and right margins.
; *Output
;-The JTextArea component with the given settings.

(define makeTextArea (text width height color font style fontSize backColor marginv)
  (let ( (text (object ("javax.swing.JTextArea" text)))
         (font (object ("java.awt.Font" font style fontSize)))
         (margin (object ("java.awt.Insets" marginv marginv marginv marginv )))
        )
    (seq
      (invoke text "setMaximumSize" (object ("java.awt.Dimension" width height))) 
      (invoke text "setAlignmentX" java.awt.Component.LEFT_ALIGNMENT)
      (invoke text "setForeground" color) ;font color
      (invoke text "setFont" font)
      (invoke text "setMargin" margin )
      (invoke text "setBackground" backColor)
      (invoke text "setEditable" (boolean false))
      text
    )
   )
 ) ;end makeTextArea


;This text makes a JTextArea component with the given settings,
; wrapping he text lines.
; *Inputs
;-text: a string with the text that will be showed.
;-width: width of the JTextArea component.
;-heigth: height  of the JTextArea component.
;-color: font color.
;-font: name of the font.
;-style: font style (Values: 0-> PLAIN , 1 -> BOLD , 2 -> ITALIC)
;-fontSize: character's size.
;-backColor: JTextArea component's background color.
;-marginv: value for the top, botton, left and right margins.
; *Output
;-The JTextArea component with the given settings.
(define makeTextAreaLabel (text width height color font style fontSize backColor marginv)
 (try
  (let ( (text (object ("javax.swing.JTextArea" text )))
         (font (object ("java.awt.Font" font style fontSize)))
         (margin (object ("java.awt.Insets" marginv marginv marginv marginv )))
        )
    (seq
     ;
      (invoke text "setLineWrap" (boolean true))
      (invoke text "setWrapStyleWord" (boolean true))
      (invoke text "setMaximumSize" (object ("java.awt.Dimension" width height)))
      (invoke text "setAlignmentX" java.awt.Component.RIGHT_ALIGNMENT)
      (invoke text "setAlignmentY"  java.awt.Component.BOTTOM_ALIGNMENT)
      (invoke text "setForeground" color) ;font color
      (invoke text "setFont" font)
      (invoke text "setMargin" margin )
      (invoke text "setBackground" backColor)
      (invoke text "setEditable" (boolean false))
      (invoke text "setVisible" (boolean true))
     
      text
    )
   )
    (catch Exception
        (invoke java.lang.System.err "println" (concat "Error in makeTextAreaLabel: " (invoke Exception "getMessage")))
   )) ;end catch, try
 ) ;end makeTextAreaLabel




;This functions returns the value of an environment variable
;* Input
;- vname: environment variable's name.
;* Output
;- value of the environment variable.

(define getEnvVariable (vname)
(try
  (  let (   (runtime (sinvoke "java.lang.Runtime" "getRuntime")) ;runtime instance
             (cmd (object ("java.lang.String" (concat "printenv " vname) ))) ;command to execute to obtain the value of
                                                                             ;an environment variable. Ex:
                                                                             ; printenv MAUDE_NPA
             (process (invoke runtime "exec" cmd)) ;process which executes the command
             ;to read the output of the command execution
             (input (invoke process "getInputStream"))
             (inputR (object ("java.io.InputStreamReader" input)))
             (bufr (object ("java.io.BufferedReader" inputR)))
         )
     (seq
       (invoke bufr "readLine") ;variable's value
       
     )
 )
  (catch Exception
      (apply displayWMessage "Runtime error.\n"
             (invoke Exception "getMessage"))
   )) ;end catch, try

) ;end getEnvVariable


;**************Methods to manage the textual information and strands frames.****************

;Closure that will be executed when a "Strands visualization" or a "Show textual information" window
;is closed. The framesList object will be updated.
;* Input
;- node: node whose textual information or whose strands have been displayed.
;- frameType: frame that has been closed: either the textual information frame (infoFrame) or
;             the strands visualization frame (strandsFrame).
;* Output
; The corresponding FLItem of the framesList will be updated, decreasing its counter.
(define closeWindowEvent (node frameType)
  (lambda (self event)
    (if (= frameType "infoFrame")
        (apply removeInfoFrameFromFLItem node)
	(apply removeStrandsFrameFromFLItem node)
    )
  )
) ;end closeWindowEvent

;Closure to create and to add an element, with its attributes initialized, to the framesList
;* Input
;- node: the node whose corresponding FLItem is being added to the frameslist.
;- graphFrame: main frame, the one that shows the search space tree.
;* Output
; A new FLItem (FrameList Item) corresponding to the node, is added to the framesList

(define addInitializedFramesListItem ( node graphFrame)
 (let ( (flItem (object ("g2d.glyph.Attributable" ))) ;the flItem object
        ; invisible frame that corresponds to the node. It is created only to add
        ; an item to the MWAControl tree with the title: "node <node's label>".
        ; the parent frame of this invisible frame is graphFrame.
        (pFrame (object ("g2d.mwa.MWAFrame" (concat "node " (getAttr node "label")) graphFrame)))
      )
  (seq
    (invoke  pFrame "setVisible" (boolean false)) ; because it's an invisible frame
    (setAttr flItem "pFrame" pFrame ) ; keep the invisible frame to dispose it when both, 
                                      ; the information frame and the strands frame are not active
    (setAttr flItem "frameCounter" (int 0)) ;to keep track if there is any frame of the node active
    (setAttr flItem "infoFrame" (object null))
    (setAttr flItem "strandsFrame" (object null))
    (invoke framesList "put" (getAttr node "label") flItem) ;add the FLItem to de framesList
  )
 )
) ;end addInitializedFramesListItem



;* Input
;- node: the selected node of the tree.
;* Output
;- invisible frame of the FLItem associated to the node.

(define getPFrameFromFLItem (node)
  (let ( (flItem (invoke framesList "get" (getAttr node "label")))
        )
    (getAttr flItem "pFrame")
  )
) ;endPFrameFromFLItem



;* Input
;- node: the selected node of the tree whose information frame is being showed.
;- infoFrame:textual information frame that is being added to the FLItem associated to the node.
;* Output
;- the upated flItem
(define addInfoFrameToFLItem (node infoFrame)
  (let ((flItem (invoke framesList "get" (getAttr node "label")))
        (currentFCounter (getAttr flItem "frameCounter"))
        )
    (seq
      (setAttr flItem "infoFrame" infoFrame)
      (setAttr flItem "frameCounter" (+ currentFCounter (int 1))) ;increment the frameCounter since a new frame
                                                                  ;has been added.
      flItem
    )
  )
) ;end addInfoFrameToFLItem



;* Input
;- node: the selected node of the tree whose strandsFrame is being showed.
;- strandsFrame: strands visualization frame that is being added to the FLItem associated to the node.
;* Output
;- the upated flItem
(define addStrandsFrameToFLItem (node strandsFrame)
  (let ((flItem (invoke framesList "get" (getAttr node "label")))
        (currentFCounter (getAttr flItem "frameCounter"))
        )
    (seq 
      (setAttr flItem "strandsFrame" strandsFrame)
      (setAttr flItem "frameCounter" (+ currentFCounter (int 1))) ;increment the frameCounter since a new frame
                                                                  ;has been added.
      flItem
    )
  )
) ;end addStrandsFrameToFLItem


;Remove the infoFrame from the FLItem associated to the node, when the information frame of this node is closed.
;* Input
;- node: node whose information frame has been closed.
;* Output
;- the FLItem is updated.

(define removeInfoFrameFromFLItem (node)
  (let ( (flItem (invoke framesList "get" (getAttr node "label")))
         (currentFCounter (getAttr flItem "frameCounter"))
         (infoFrame (getAttr flItem "infoFrame"))
        )
    (seq
       (invoke infoFrame "dispose") ;dispose the information frame
       (if (<= currentFCounter (int 1)) ; if there is not any other frame active, destroy the invisible frame
           (seq
             (invoke (getAttr flItem "pFrame") "close")
             (invoke (getAttr flItem "pFrame") "dispose")
             (invoke framesList "remove" (getAttr node "label"))
           )
           (seq ;otherwise, just update the infoFrame attribute and decrease the frame counter.
             (setAttr flItem "infoFrame" (object null))
             (setAttr flItem "frameCounter" (- currentFCounter (int 1)))
           )
       )
    )
 )
) ;end removeInfoFrameFromFLItem

;Remove the strandsFrame from the FLItem associated to the node, when the strands visualization frame of this node is closed.
;* Input
;- node: node whose information frame has been closed.
;* Output
;- the FLItem is updated.
(define removeStrandsFrameFromFLItem (node)
  (let ( (flItem (invoke framesList "get" (getAttr node "label")))
         (currentFCounter (getAttr flItem "frameCounter"))
         (strandsFrame (getAttr flItem "strandsFrame"))
        )
    (seq
       (invoke strandsFrame "dispose") ;dispose the strandsFrame
       (if (<= currentFCounter (int 1)) ; if there is not any other frame active, destroy the invisible frame
           (seq
             (invoke (getAttr flItem "pFrame") "close")
             (invoke (getAttr flItem "pFrame") "dispose")
             (invoke framesList "remove" (getAttr node "label"))
           )
           (seq ;otherwise, just update the infoFrame attribute and decrease the frame counter.
              (setAttr flItem "strandsFrame" (object null))
              (setAttr flItem "frameCounter" (- currentFCounter (int 1)))  
           )
       )
    )
 )
) ;end removeStrandsFrameFromFLItem


; Closure to know if a node has its information frame is openned or not.
;* Input
;- node: the node whose infoFrame state (openned or closed) we want to know.
;* Output
;- false it the infoFrame is not openned and true otherwise.

(define FLItemHasInfoFrame (node)
  (let ( (flItem (invoke framesList "get" (getAttr node "label")))
       )
    (if (= (getAttr flItem "infoFrame") (object null))
        (boolean false)
        (boolean true)
    )
  )
) ;end FLItemHasInfoFrame


; Closure to know if a node has its strands frame is openned or not.
;* Input
;- node: the node whose strandsFrame state (openned or close) we want to know.
;* Output
;- false it the strandsFrame is not openned and true otherwise.
(define FLItemHasStrandsFrame (node)
  (let ( (flItem (invoke framesList "get" (getAttr node "label")))
       )
    (if (= (getAttr flItem "strandsFrame") (object null))
        (boolean false)
        (boolean true)
    )
  )
) ;end FLItemHasStrandsFrame


; Closure to bring to front and show the information frame of a node when it is openned.
;* Input
;- node: the node whose information frame we want to display
;* Output
;- the information frame of the node is recovered from its corresponding FLItem
; and brought to front to the user.
(define showFLItemInfoFrame (node)
   (let ( (flItem (invoke framesList "get" (getAttr node "label")))
          (infoFrame (getAttr flItem "infoFrame"))
       )
 
       (if (!= infoFrame (object null)) ; if the infoFrame is openned, bring it to front.
         (seq
           (invoke infoFrame "setVisible" (boolean true))
           (invoke infoFrame "toFront" )
         )
       )
  )
) ;end showFLItemInfoFrame


; Closure to bring to front and show the strands frame of a node when it is openned.
;* Input
;- node: the node whose strands node we want to display
;* Output
;- the strands frame of the node is recovered from its corresponding FLItem
; and brought to front to the user.
(define showFLItemStrandsFrame (node)
  (let ( (flItem (invoke framesList "get" (getAttr node "label")))
          (strandsFrame (getAttr flItem "strandsFrame"))
       )
    (if (!= strandsFrame (object null)) ; if the strands frame is openned, bring it to front.
      (seq
         (invoke strandsFrame "setVisible" (boolean true))
         (invoke strandsFrame "toFront" )
      )
    )
  )
) ;end showFLItemStrandsFrame



; end frames management**********************************************;



;Closure to show a general Message Dialog
;*Input
;- title: title of the dialog.
;- msg: message of the dialog.
;- type: type of the message: information, warning, etc.
;                             The icon of the message dialog will depend on the type of message.
(define displayWInfoMessage (title msg type)
   ; show message in warning dialog:
   (sinvoke "javax.swing.JOptionPane" "showMessageDialog" 
	    (object null)
	    msg
	    title
            type)
    )
    
; same function than the previous one, but "type" is a string. This function is called
; from npa-assist.maude
(define displayWInfoMessageS (title msg type)
   ; show message in warning dialog:
   (sinvoke "javax.swing.JOptionPane" "showMessageDialog" 
	    (object null)
	    msg
	    title
            (sinvoke "java.lang.Integer" "parseInt" type ))
)

;Closure to guess in which color will be used to paint a node's dot or its strand.
;If it is an intruder strand or node, it will be painted  in green.
;If it is a honest strand or node, it will be painted in black.
;If the strand or the node belong to the past they will be painted in light color:
;light green for the intruder and grey for honest principals.
;If the strand or the node belong to the present of future they will be painted in dark color:
;dark green for the intruder and black for honest principals.
;*Input
;- intruder? :it an honest or an intruder node or strand?
;- time: it belongs to the past or to the present/future?
(define getStrandColor (intruder? time)
  (if (= intruder? (boolean true))
         darkGreen
         (if (= time "past")
              greyStrand
              regularColor
         )
  )
 ) ;end getStrandColor



;************* Methods to update mssage are

; Closure to change the text of the messageArea of the window that shows the graphical
; representation of the search tree.
;* Input
;- gname: id of the graph
;- text: text of the message that will be displayed in the messageArea
(define setTextMessageArea (gname text wait?)
  (let( (graph (if (instanceof gname "java.lang.String")
                             (fetch gname)
                             (object null)))     
        (frame (getAttr graph "frame" (object null)))
        (messageArea (if (!= graph (object null))
                          (getAttr graph "messageArea")
                          (apply  makeTextArea  "" 
                           (int 600) (int 30) regularColor "Arial" (int 0) (int 12) white (int 2))
                        ))
       )
       (seq
          (invoke messageArea "setText" text )
          (invoke messageArea "show")
          (invoke frame "repaint")

          (if (= wait? "yes")
              (seq
                (invoke (sinvoke "java.lang.Thread" "currentThread") "sleep" (long 1000))
                (apply setDefaultMessageArea gname)              
              ) ;end seq
          ) ;end if
       )
  )
) ;end setTextMessageArea


; This method updates the message area setting a default message
;* Input
;- gname: id of the search tree
;* Output
;- the
(define setDefaultMessageArea (gname)
  (let( (graph (if (instanceof gname "java.lang.String")
                             (fetch gname)
                             (object null)))     
        (frame (getAttr graph "frame" (object null)))
        (messageArea (if (!= graph (object null))
                          (getAttr graph "messageArea")
                          (apply  makeTextArea  "" 
                           (int 600) (int 30) regularColor "Arial" (int 0) (int 12) white (int 2))
                        ))
       )
   (seq
       (if (= (getAttr graph "anaylysisHasFinished" "") (boolean true))
           (invoke messageArea "setText" "The analysis has finished")
           (invoke messageArea "setText" "Click on \"Next\" to continue the analysis")
       )

       (invoke messageArea "show")
       (invoke frame "repaint")
   ) ;end seq
 )
) ;setDefaultMessageArea

;*****************end methods to update message area

; generic closure to close a frame when a cancel button has been pressed
(define cancelAction (frame)
   (lambda (self event)
     (invoke frame "dispose") ))


; This method draws a blank dot in the bottom part of the graphics component
; to leave a margin
;* Inputs
;- x: X coordinate (in pixels, it is not a symbolic coordinate)
;- y: Y coordinate (in pixels, it is not a symbolic coordinate)
;- component: graphical component in which the node will be drawn
;* Output
;- A new white dot is painted in the component, and the component is repainted
(define draw-blank (x y component) 
   (let (  (nodeShape (object ("java.awt.geom.Ellipse2D$Double" 
                               (int -3) (int -3) (int 6) (int 6))))
               
           (nodeGlyph (object ("g2d.glyph.Glyph"  nodeShape white white )))
                
	    (trans (let ((temp (object ("java.awt.geom.AffineTransform"))))
			(seq (invoke temp "translate" x y) temp)))
          )

    (seq
       (invoke component "add" nodeGlyph trans) ;add the node in the desired location
       nodeGlyph
    )
  )
 ) ;end define blank


)

;;;;;;;;;; ./makeg2dlib loaded ../GUI/defines.lsp ;;;;;;;;;;
;(seq
;******************strings for the load-npa-assist.maude file
(define strLoadFile
"mod IMAUDE is
  inc REWRITE .
  inc FILEMANAGER .
  inc G2D .
  inc SEQUENCER .
  inc LISTENER .
endm")


;;******************strings for the startup.txt file
(define str1Startup
 "start maude iop_maude_wrapper basura ")

(define str2Startup
"start graphics2d iop_graphics2d_wrapper $IOPBINDIR -cp  ")

(define str2StartupB
" -cp /Users/sosanpi/Documents/PFC/prototypeNew/Libraries/test.jar ")

(define str3Startup
 "  -Xms256m -Xmx1024m
start filemanager iop_filemanager
select maude")

;*****************strings for the instructions area (loadFile.lsp)
(define abstractText
"Maude-NPA is an analysis tool for cryptographic protocols that takes into account many of
the algebraic properties of cryptosystems that are not included in other tools. These include
cancellation of encryption and decryption, Abelian groups (including exclusive-or), and exponentiation.
Maude-NPA uses an approach similar to the original NRL Protocol Analyzer; it is based on unification,
and performs backwards search from a final state to determine whether or not it is reachable. Unlike
the original NPA, it has a theoretical basis in rewriting logic and narrowing, and offers support for
a wider basis of equational theories that includes associative-commutative (AC) theories. "
)

(define usageText
 "To start using Maude-NPA you have to load the file with the protocol specification.
You can do that by selecting in the  \"File \" menu the option menu \"Load protocol file...\".
However, this tool is provided with some examples. You can load one of them by choosing at
the  \"File \" menu the option menu \"Load example...\". Inmediately, a new window will be
shown with more information"
)


;*******************strings for the load protocol and load grammar frame
(define grammarExplanation
"When you load a protocol specification, a new grammar is generated and saved
as a text file at the protocol directory in the NPAAssist tool directory.
You can find a file called <protocol's name>-grammar.txt in the directory
\"Maude-NPA GUI/NPAAssist/<protocol's name>/\".
\nIf you have already loaded this protocol, you can restore an existing grammar
file by selecting the following checkbox and  browsing it. As a result, the tool
will load the protocol faster."
)

(define loadExplanation
"Load the protocol you want to analyze by browsing it. Then, press the Accept
button and, please, wait until the Maude-NPA Manager window is shown.
This process may take several minutes, depending on the protocol."
)

(define protocolFolderExplanation
"Choose the directory where you want to save all the files that will be
generated during the analysis session of this protocol"
)

(define loadExampleExplanation
 "The list below contains some protocol examples which
you can execute. Choose one of them and press the Accept
button. Then, please, wait until the Maude-NPA Manager
window is shown.This process may take several minutes,
depending on the protocol." 
 )

;******* Strings to restore a session
(define loadRestoreFileStr
"Select the file in which you saved a previous Maude-NPA session"
)

(define restoreProtocolFolderStr
"Choose the folder where you want to restore a previously saved
analysis session"
)

(define commandFile1
"#!/bin/bash
cd NPAAssist/")

(define commandFile2
"iop -i startup.txt"
)

;********* titles to show node's information. These variables will be used at npa-showInfo2.lsp file
(define titlesArray
  (array java.lang.String 
     "\n Current Strands \n ------------------------- \n"
     "\n Intruder knowledge \n ------------------------- \n"
     "\n Sequence of messages \n ------------------------- \n"
     "\n Additional information \n ------------------------- \n"
  )
 )
  
(define currentStrandsTitle
  "Current Strands \n ------------------------- \n")

(define intruderKnowledgeTitle
  "Intruder knowledge \n ------------------------- \n")

(define messagesTitle
  "Sequence of messages \n ------------------------- \n")

(define additionalInfoTitle
  "Additional information \n ------------------------- \n")


;*************************string for the window to load the specification of a Maude-NPA state *****************
(define loadExternalStateStr
"Please, select the text file where you saved the specification
of the Maude-NPA state you want to load." )

;*******String to show (either in a textual or graphical way) a Maude-NPA state
(define showStateStr
  "The specification of the Maude-NPA state you selected has
already been loaded. Select one of the buttons below to view
either the textual or the graphical representation of this
state"
)

 ;****************Colors*******************
 (define intruderColor  (object ("java.awt.Color" (int 150) (int 150) (int 150))))

 (define greyStrand  (object ("java.awt.Color" (int 150) (int 150) (int 150))))
 (define darkGreen (object ("java.awt.Color" (int 0) (int 205) (int 0))))
 (define lightGreen (object ("java.awt.Color" (int 255) (int 0) (int 0))))
 
 (define indianRed (object ("java.awt.Color" (int 255) (int 106) (int 106))))

 (define regularColor  (object ("java.awt.Color" (int 0) (int 0) (int 0)))) 
 (define intruderKnowledgeColor (object ("java.awt.Color" (int 255) (int 0) (int 0))))
 (define lightCyan (object ("java.awt.Color" (int 209) (int 238) (int 238))) )
 (define titleColor  (object ("java.awt.Color" (int 0) (int 34) (int 102))))
 (define greySilver  (object ("java.awt.Color" (int 240) (int 240) (int 240))))
 (define white  (object ("java.awt.Color" (int 255) (int 255) (int 255))))


(define lavendar (object ("java.awt.Color" (int 210) (int 202) (int 255))))
(define ltgreen (object ("java.awt.Color" (int 100) (int 255) (int 90))))
(define bluegreen (object ("java.awt.Color" (int 0) (int 255) (int 255))))

(define nodeBorderColor java.awt.Color.black)
(define regularFillColor lavendar)
(define initialFillColor ltgreen)
(define unreachableFillColor white)

(define dirEdgeColor java.awt.Color.black)
(define indirEdgeColor java.awt.Color.black)

(define black java.awt.Color.black)



;data structure to store the active frames associated to each node of the search tree.
;the key of each element of the hashtable is the node's label.
;the value of each element is an object called FLItem, defined in auxiliar-functions.lsp
(define framesList (object ("java.util.Hashtable")))


;list with the example protocols names.
(define protocolsNamesList
  (let ( (temp (object ("java.util.ArrayList")))
        )
      (seq
        (invoke temp "add" "NSPK")
        (invoke temp "add" "nspk")
        (invoke temp "add" "Diffie-Hellman")
        (invoke temp "add" "diffie-hellman")
        temp
      )
   )
) 


;;;;;;;;;; ./makeg2dlib loaded ../GUI/prettySave.lsp ;;;;;;;;;;
; (seq

  ;;
(define prettySaveAction (pname gname)
  (lambda (self event)
    (let ( (parent (if (instanceof pname "java.lang.String")
                 (fetch pname)
                 (object null)))
           (pframe (if (instanceof parent "g2d.graph.IOPGraph")
                (getAttr parent "frame")
                (object null)))
           (frame (object ("g2d.graphviewer.BaseFrame" "" 
                 (if (instanceof pframe "g2d.graphviewer.BaseFrame")  
                     pframe 
                     (apply getNPAFrame))
                    (boolean true))) )
           (fileDialog (let ( (temp (object("g2d.swing.IOPFileChooser")))
                            )
                            (seq
                              (invoke temp "setDialogTitle" "Save graph information as a file")
                              temp
                            )))
                         
           (returnVal (invoke fileDialog "showSaveDialog" frame))
           (guiVar (apply getEnvVariable "MNPA_GUI"))
           (exampleDirectory (concat guiVar "/examples"))
          )
      (seq
         (if (= returnVal javax.swing.JFileChooser.APPROVE_OPTION)
            (if (= (invoke (invoke   (invoke fileDialog "getSelectedFile") "getAbsolutePath") "startsWith" exampleDirectory)
                   (boolean true))
               ;then
              (apply displayWInfoMessage "Forbidden"
                                         "You are not allowed to save files into this directory.\nPlease choose a different directory."
                                         javax.swing.JOptionPane.ERROR_MESSAGE)
              ; else
              (let( (filePath (invoke (invoke fileDialog "getSelectedFile") "getAbsolutePath"))
                  )
                (if (= (invoke filePath "endsWith" ".txt") (boolean true))
                  (apply saveGraphInfo gname filePath) 
                  (apply saveGraphInfo gname (concat filePath ".txt"))
                )
              ) ;end environment
           ) ;end if
         ) ;endif
      ) ;end seq
    )
  )
) ;end of prettySaveAction

  
  (define saveGraphInfo (gname filePath)
      (let (  (graph (fetch gname))
              (graphLevelsList (object ("java.util.ArrayList" (getAttr graph "list")))) ; list with the different levels of the graph.
              (protocol (getAttr graph "protocol"))
              (aname (getAttr graph "aname"))
              
              (infoFile (object ("java.io.File" filePath)))
              (fwriter (object ("java.io.FileWriter" infoFile (boolean false)))) ;append text
              (pwriter (object ("java.io.PrintWriter" fwriter)))
              (levelCounter ( object ("g2d.glyph.Attributable")))
            )
          (seq
             (setAttr levelCounter "level" (int 0) )
             (invoke pwriter "println" "*****************************************************************************************")
             (invoke pwriter "println" (concat "Textual information for the analysis of the attack "  aname " of the protocol " protocol))
             (invoke pwriter "println" "*****************************************************************************************")
            
             (for graphLevel graphLevelsList
                  (let ( (currentLevel (getAttr levelCounter "level"))
                         ;************
                         (currentLevelArray  (object ("java.util.ArrayList" (invoke graphLevel "values"))))
                        )
                    (seq
                      (if (> currentLevel (int 0))
                        (seq
                          
                          (invoke pwriter "println" (concat "\nLEVEL " currentLevel))
                          (invoke pwriter "println" "+++++++++++++++++++++++++")
                          (apply levelPrettyPrint graphLevel pwriter )
                        )
                      ) ;end if
                     (setAttr levelCounter "level" (+ currentLevel (int 1)))
                   )
                  )
	       )  ;end for
	       (invoke pwriter "close")
          ) ;
       )
 ) ;end saveGraphInfo

  
; graphLevel: hash table that contains the nodes of a concrete level of the graph
  (define levelPrettyPrint(graphLevel pwriter)
    (let ( (currentLevelArray  (object ("java.util.ArrayList" (invoke graphLevel "values"))))
          )
       (seq
          (for node currentLevelArray
             (let( (label (getAttr node "label"))
                  )
               (seq                 
                  (if (= (apply isInitialState node) (boolean true))
                         (invoke pwriter "println" (concat "\nState " label " ***(this is an initial state)  \n --------------"))
                      (if (= (getAttr node "unreachable") (boolean true))
                          (invoke pwriter "println" (concat "\nState " label " ***(this is an unreachable state)  \n --------------"))
                          (invoke pwriter "println" (concat "\nState " label "\n --------------"))
                      ) ;end if
                  ) ;end if
                  ;;; crear método nodePrettyPrint???? o ponerlo directamente aquí?
                  (apply nodePrettyPrint node pwriter)
               ) ;end seq
             )
          ) ;end for
          (invoke pwriter "println")
       )   
    )
  ) ;end levelPrettyPrint


  (define nodePrettyPrint(node pwriter)
     (let( (strandsInfo (apply getPrettyInfo node (int 0)))
           (intruderInfo (apply getPrettyInfo node (int 1)))
           (messagesInfo (apply getPrettyInfo node (int 2)))
           (info (apply getPrettyInfo node (int 3)))
         )
       (seq
          (invoke pwriter "println" "* Strands:")
          (invoke pwriter "println" (concat strandsInfo " \n" ) )
          (invoke pwriter "println" "* Intruder Knowledge:")
          (invoke pwriter "println" (concat intruderInfo " \n" ))
          (invoke pwriter "println" "* Sequence of messages:")
          (invoke pwriter "println" (concat messagesInfo " \n" ))
          (invoke pwriter "println" "* Additional information:")
          (invoke pwriter "println" (concat info " \n" ))
       )
     ) 
  ) ;end nodePrettyPrint

;  ) ;end global seq

;;;;;;;;;; ./makeg2dlib loaded ../GUI/saveRestoreSession.lsp ;;;;;;;;;;
;(seq

 (define saveSessionAction (pname gname protocol) ;add argument for file name
  (lambda (self event)
    (let ( (parent (if (instanceof pname "java.lang.String")
                 (fetch pname)
                 (object null)))
           (pframe (if (instanceof parent "g2d.graph.IOPGraph")
                (getAttr parent "frame")
                (object null)))
           (frame (object ("g2d.graphviewer.BaseFrame" "" 
                 (if (instanceof pframe "g2d.graphviewer.BaseFrame")  
                     pframe 
                     (apply getNPAFrame))
                    (boolean true))) )
           (fileDialog (let ( (temp (object("javax.swing.JFileChooser")))
                            )
                            (seq
                              (invoke temp "setDialogTitle" "Save Maude-NPA analysis session")
                              temp
                            )))
                         
           (returnVal (invoke fileDialog "showSaveDialog" frame))
           (graph (fetch gname))

           (guiVar (apply getEnvVariable "MNPA_GUI"))
           (exampleDirectory (concat guiVar "/examples"))
           (messageArea (getAttr graph "messageArea"))
          )
         (if (= returnVal javax.swing.JFileChooser.APPROVE_OPTION)
           (if (= (invoke (invoke   (invoke fileDialog "getSelectedFile") "getAbsolutePath") "startsWith" exampleDirectory)
                   (boolean true))
              (apply displayWInfoMessage "Forbidden"
                                         "You are not allowed to save files into this directory.\nPlease choose a different directory."
                                         javax.swing.JOptionPane.ERROR_MESSAGE)
             ;else continue       
             (let( (protocolFolder (getAttr graph "protocolFolder")) 
                  (protocolPath (apply readProtocolFilePath (concat  protocolFolder "/load-npa-assist-restoreGrammar.maude") (int 0)))
                  (filePath (let ( (temp (invoke (invoke fileDialog "getSelectedFile") "getAbsolutePath"))
                                    )
                                 (if (= (invoke temp "endsWith" ".txt") (boolean true))
                                     temp
                                     (concat temp ".txt")
                                 ) ;end if
                            ))

                  (file (object ("java.io.File" filePath)))
                  (fwriter (object ("java.io.FileWriter" file (boolean false)))) ;append text
                  (pwriter (object ("java.io.PrintWriter" fwriter)))

                  (levels (getAttr graph "nlevels"))
                 
                )
              (seq
               ;the first line of the file must contain the location of the file that contains 
               ; the specification of the protocol.
               (invoke pwriter "println" protocolPath)
               (invoke pwriter "println" gname)
               (invoke fwriter "close")
               (invoke pwriter "close")
               
               (sinvoke "g2d.util.ActorMsg" "send" "maude" gname
		  (concat "saveNPASession " protocol " " filePath " " levels ))
             )     
           )
          
         ) ;endif
       ) ;end if    
    )
 )
) ;end saveSessionAction

;;;;******************************
;;;;*********Restore session
;;;;******************************

 (define getRestoreDataArray (restoreFilePath)
   (let( (dataArray (mkarray java.lang.String (int 3)))

         (file (object ("java.io.File" restoreFilePath)))
         (freader (object ("java.io.FileReader" file)))
         (breader (object ("java.io.BufferedReader" freader)))
       )
     (seq
       (aset dataArray (int 0) (invoke (invoke breader "readLine") "replace" " " "")) ; this reads the protocol file path
       (aset dataArray (int 1) (invoke (invoke breader "readLine") "replace" " " "")) ; this reads the tree name

       (invoke breader "close")
       (invoke freader "close")

       dataArray
     )
   )
 ) ;end getRestoreDataArray

;**************************

  
 (define restoreSessionAction (frame)
   (lambda (self event)
     (let ( (kbm (fetch "NPAManager"))
            (frame (let( (temp (object ("g2d.swing.IOPFrame" "Restore a previously saved analysis session")))
                    )
                  (seq
                    (invoke temp "setResizable" (boolean false))
                    temp
                  )))
             (panel (object ("javax.swing.JPanel" )))
             (auxPanel1 (object ("javax.swing.JPanel" )))
             (auxPanel2 (object ("javax.swing.JPanel" )))
             (auxPanel3 (object ("javax.swing.JPanel" )))

             (layout (object ("javax.swing.BoxLayout" panel javax.swing.BoxLayout.PAGE_AXIS )))

             ;generalButtons
             (acceptButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                       "Accept"
                                                       "Execute tool"
                                                       (apply  restoreSessionAcceptAction kbm frame)))))  
                                (seq
                                  (invoke temp "setSize" (int 60) (int 15))
                                  (invoke temp "setEnabled" (boolean false))
                                  temp
                                )  ))

             (cancelButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                       "Cancel"
                                                       "Cancel execution"
                                                       (apply restoreCancelAction frame))))) 
                                (seq
                                  (invoke temp "setSize" (int 60) (int 15)) temp
                                ) ))

         ;elements to load the "restore File"
         (restoreFileText (apply  makeTextArea loadRestoreFileStr (int 520) (int 45) regularColor
                               "Arial" (int 0) (int 14) greySilver (int 15)))
         (restoreFilePath (let ( (temp (object ("javax.swing.JTextField" (int 35))))
                            )
                            (seq
                              (invoke temp "setEditable" (boolean false))
                              temp
                            )))

          ;elements to choose the directory for the protocol folder
         (restoreProtocolFolderText (apply  makeTextArea restoreProtocolFolderStr (int 520) (int 50) regularColor
                                     "Arial" (int 0) (int 14) greySilver (int 15)))
         (restoreProtocolFolderPath (let ( (temp (object ("javax.swing.JTextField" (int 35))))
                                   )
                               (seq
                                 (invoke temp "setEditable" (boolean false))
                                 temp
                             )))
         
         ;a button to open a file dialog an select the file where a session was saved
         (loadRestoreFileButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                       "Browse..."
                                                       "Load a file where the session was saved"
                                                       (apply loadRestoreFileButtonAction kbm frame restoreFilePath acceptButton restoreProtocolFolderPath))))) ;******** 
                                (seq
                                  (invoke temp "setSize" (int 60) (int 15))  temp
                                ) ))


         
         ;a button to open a file dialog an select the grammar file the user wants to restore
         (selectRestoreProtocolFolderButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                            "Browse..."
                                                            "Select a folder for this analysis session"
                                                            (apply selectRestoreProtocolFolderAction kbm frame restoreProtocolFolderPath acceptButton)))))
                                (seq
                                  (invoke temp "setSize" (int 60) (int 15))
                                  temp
                                )))
       ) ;end let bindings
       (seq
          (invoke panel "setLayout" layout)
          (invoke restoreFilePath "setMaximumSize" (object ("java.awt.Dimension"  (int 250)   (int 25))))
          (invoke restoreProtocolFolderPath "setMaximumSize" (object ("java.awt.Dimension"  (int 250)   (int 25))))

          ;In this part of the window, the user will be able to select the file where he or she previously saved a Maude-NPA session
          (invoke auxPanel1 "setBorder" (sinvoke "javax.swing.BorderFactory" "createEmptyBorder" (int 0) (int 15) (int 0) (int 15)))
          (invoke auxPanel1 "setLayout" (object ("javax.swing.BoxLayout" auxPanel1 javax.swing.BoxLayout.X_AXIS )))
          (invoke auxPanel1 "add" restoreFilePath)
          (invoke auxPanel1 "add" (sinvoke "javax.swing.Box"  "createRigidArea" (object ("java.awt.Dimension" (int 20)  (int 25)))))
          (invoke auxPanel1 "add" loadRestoreFileButton)
          (invoke auxPanel1 "setMaximumSize" (object ("java.awt.Dimension" (int 500)  (int 35)) ))
          (invoke auxPanel1 "setAlignmentX" java.awt.Component.LEFT_ALIGNMENT)

          ;In this part of the window, the user will be able to select the directory for the protocol folder
          (invoke auxPanel2 "setBorder" (sinvoke "javax.swing.BorderFactory" "createEmptyBorder" (int 0) (int 15) (int 0) (int 15)))
          (invoke auxPanel2 "setLayout" (object ("javax.swing.BoxLayout" auxPanel2 javax.swing.BoxLayout.X_AXIS )))
          (invoke auxPanel2 "add" restoreProtocolFolderPath)
          (invoke auxPanel2 "add" (sinvoke "javax.swing.Box"  "createRigidArea" (object ("java.awt.Dimension" (int 20)  (int 25)))))
          (invoke auxPanel2 "add" selectRestoreProtocolFolderButton)
          (invoke auxPanel2 "setMaximumSize" (object ("java.awt.Dimension" (int 500)  (int 35))))
          (invoke auxPanel2 "setAlignmentX" java.awt.Component.LEFT_ALIGNMENT)
      
          ;;;;;
          ;Accept and cancel buttons.
          (invoke auxPanel3 "setBorder" (sinvoke "javax.swing.BorderFactory" "createEmptyBorder" (int 10) (int 0) (int 0) (int 10)))
          (invoke auxPanel3 "setLayout" (object ("javax.swing.BoxLayout" auxPanel3 javax.swing.BoxLayout.X_AXIS )))
          (invoke auxPanel3 "add" (sinvoke "javax.swing.Box"  "createHorizontalGlue" ))
          (invoke auxPanel3 "add"  cancelButton)
          (invoke auxPanel3 "add" (sinvoke "javax.swing.Box"  "createRigidArea" (object ("java.awt.Dimension" (int 10)  (int 25)))))
          (invoke auxPanel3 "add" acceptButton)
          (invoke auxPanel3"setMaximumSize" (object ("java.awt.Dimension" (int 535)  (int 55)) ))
          (invoke auxPanel3 "setAlignmentX" java.awt.Component.LEFT_ALIGNMENT)

          ;Add auxiliar panels
          (invoke panel "add" restoreFileText)
          (invoke panel "add" auxPanel1)
          (invoke panel "add" restoreProtocolFolderText)
          (invoke panel "add" auxPanel2 )
          (invoke panel "add" auxPanel3 )

          (invoke frame "add" panel)
          (invoke frame "setSize" (int 500) (int 250)) 
          (invoke frame "setVisible" (boolean true))
       )
     ) ;end let environment
   ) ;end lambda
 ) ;end restoreSessionAction

;*********************** closure for gui's events 
 ;Action for the cancel button. It will close the frame.
; *Input
;- frame: frame which will be closed.

(define restoreCancelAction (frame)
   (lambda (self event)
     (invoke frame "dispose") ))


; This method sets the necessary values for the file where a session was previously saved
; If the path to that file is valid, then the default value for the protocol folder is also set
;* Input
;- kbm: attributable manager object
;- frame: frame where the user can restore a session
;- textField: textField object that shows the path of the file where a session was saved  (selected by the user)
;- acceptButton: button "Accept" of the frame "frame"
;- tfRestoreFolder: textField object that shows the path of the protocolFolder (either selected by the user
;                   or with the default value)
;* Output
;- The frame where the user can restore a session is updated with the path to the file where the session was saved
;  (if it was correctly selected by the user), and with the default value of the protocol folder 

(define loadRestoreFileButtonAction (kbm frame textField acceptButton tfRestoreFolder)  
   (lambda (self event)
     (let ( (fileDialog (let ((temp (object("javax.swing.JFileChooser")))
                                    )
                            (seq
                              (invoke temp "setDialogTitle" "Choose file to restore session")
                              temp
                            )))
             (returnVal (invoke fileDialog "showOpenDialog" frame))
           ) ;end let bindings
       (seq
        (if (= returnVal javax.swing.JFileChooser.APPROVE_OPTION)
           (let(  (filePath (invoke (invoke fileDialog "getSelectedFile") "getAbsolutePath")) ;file with the saved data of a session
                  (protocolFolderName (apply getProtocolName filePath))  ; getProtocolName (filePath)
                  (guiVar (apply getEnvVariable "MNPA_GUI"))
               )
             (seq
                (invoke textField "setText" filePath)
                (setAttr kbm "restoreFilePath" filePath)

                ; set default value for "protocolFolder" attribute 
                (setAttr kbm "protocolFolder" (concat guiVar "/" protocolFolderName) )
                (invoke tfRestoreFolder "setText" (concat guiVar "/" protocolFolderName))

                (if (and (!= (getAttr kbm "protocolFolder") (object null))
                         (!= (getAttr kbm "protocolFolder") "" ))
                     (invoke acceptButton "setEnabled" (boolean true))
                     (invoke acceptButton "setEnabled" (boolean false))
                )
                (invoke frame "repaint")
               ; kbm
             )
           ) ;end let environment
           (seq
              (setAttr kbm "protocolFolder" "")
              (invoke acceptButton "setEnabled" (boolean false))
             ; kbm
           )
        ) ;end if
        kbm
       ) ;end seq
     ) ;end let environment
  ) ;end lambda
) ;end loadRestorefileButtonAction

; This method sets the value of the protocol folder, either the one selected by the user
; or the default value
;* Input
;- kbm: attributable manager object
;- frame: frame where the user can restore a session
;- textField:  textField object that shows the path of the protocolFolder (either selected by the user
;                   or with the default value)
;- acceptButton: button "Accept" of the frame "frame"
;* Output
;- The frame where the user can restore a session is updated with the path of the protocol folder either 
;  if it was correctly selected by the user) or with its default value   
(define selectRestoreProtocolFolderAction (kbm frame textField acceptButton)
   (lambda (self event)
   (let (  (fileDialog (let((temp (object("javax.swing.JFileChooser"))) ;file chooser element to select the protocol file
                            )
                         (seq
                           (invoke temp "setFileSelectionMode" (int 1)) ;javax.swing.JFileChooser.DIRECTORIES_ONLY)
                           (invoke temp "setAcceptAllFileFilterUsed" (boolean false))
                           temp
                         )))
           (returnVal (invoke fileDialog "showSaveDialog" frame))
           (guiVar (apply getEnvVariable "MNPA_GUI"))
           (exampleDirectory (concat guiVar "/examples"))
        )
    (seq                     
     (if (= returnVal javax.swing.JFileChooser.APPROVE_OPTION)
       (if (= (invoke (invoke   (invoke fileDialog "getSelectedFile") "getAbsolutePath") "startsWith" exampleDirectory)
              (boolean true))
           ;then
            (apply displayWInfoMessage "Forbidden"
                                         "You are not allowed to save files into this directory.\nPlease choose a different directory."
                                         javax.swing.JOptionPane.ERROR_MESSAGE)
           ; else
            (let( (filePath (invoke   (invoke fileDialog "getSelectedFile") "getAbsolutePath"))
               )
                  (seq
                     (invoke textField "setText" filePath)
                     (setAttr kbm "protocolFolder" filePath)
                     
                     (invoke frame "repaint")
                     (if (and (!= (getAttr kbm "restoreFilePath") "")
                              (!= (getAttr kbm "restoreFilePath") (object null)))
                         (invoke acceptButton "setEnabled" (boolean true))
                         (invoke acceptButton "setEnabled" (boolean false))
                     )
                     kbm
                  ) 
            )
        ) ;end if
        (if (and (!= (getAttr kbm "restoreFilePath") "")
                 (!= (getAttr kbm "restoreFilePath") (object null)))
            (seq 
                ; set default value for "protocolFolder" attribute  
                (setAttr kbm "protocolFolder" (concat guiVar "/" protocolFolderName) )
                (invoke tfRestoreFolder "setText" (concat guiVar "/" protocolFolderName))
                 
                kbm
            )
            (seq
              (setAttr kbm "protocolFolder" "")
               (invoke acceptButton "setEnabled" (boolean false))
              kbm
            )
        ) ;end if
     ) ;endif
  ) ) )
) ;end selectRestoreProtocolFolderAction

; This method is executed when the user clicks on the "Accept" button of the frame
; where the user can restore a previously saved session
;* Input
;- kbm: attributable manager object
;- frame: frame where the user can restore a session
;* Output
;- This method calls all the necessary methods to restore a previously saved session
 (define restoreSessionAcceptAction ( kbm frame)
   (lambda (self event)        
     (try   
     (seq
       
               (let( (protocolFolder (getAttr kbm "protocolFolder"))
                     (folderFile (object ("java.io.File" protocolFolder)))
                    )
                 (if (= (invoke folderFile "exists") (boolean false)) ; if the protocolFolder does not still exists, create it.
                       ;create protocol directory
                       (apply mkProtocolDir protocolFolder) ;this will create the protocol directory in the location saved in the file.
                 ) ;end if
               )
       
       (let(  (protocolFolder (getAttr kbm "protocolFolder"))
                  (restoreFilePath (getAttr kbm "restoreFilePath"))
                  ; get some necessary data of the file where the session was saved
                  (dataArray (apply getRestoreDataArray restoreFilePath))
                  (protocolFilePath (aget dataArray (int 0)))
                  (tname (aget dataArray (int 1))) 

                  ;(protocolFolder (getAttr kbm "protocolFolder"))
                  ;(restoreFilePath (getAttr kbm "restoreFilePath"))  
                  (protocolName (apply getProtocolName protocolFilePath))
                  
               ;  (file (object ("java.io.File" (concat protocolFolder "/"))))
		  (file (object ("java.io.File"  protocolFolder )))
                  (runtime (sinvoke "java.lang.Runtime" "getRuntime"))
                  (process (invoke runtime "exec" "/bin/tcsh" (object null) file ))
                  (output (object ("java.io.DataOutputStream" (invoke process "getOutputStream" ))))
             )
             (seq

               ;create the startup-restoreSession.txt file                   
               (apply createStartupFile  protocolFolder  "restoreSession")

               (apply createStartupFile  protocolFolder  "restoreGrammar")
               (apply createStartupFile  protocolFolder  "generateGrammar")
               
               ;create the load-npa-assist-restoreSession.maude file
               (apply  createRestoreSessionFile restoreFilePath protocolFolder protocolFilePath protocolName)

               ;create the load-npa-assist-restoreGrammar.maude file indicating that the grammar will be restored from
               ; a file in the protocol's directory
               (apply restoreLoadGenerateFile protocolFolder protocolName protocolFilePath "restoreGrammar")
               (apply restoreLoadGenerateFile protocolFolder protocolName protocolFilePath  "generateGrammar")
               
               (invoke frame "dispose")
               
               ; increase the stacksize 
               (invoke output "writeBytes" "unlimit stacksize\n" )
               ;launche the new analysis session
               (invoke output "writeBytes" (concat "iop -i startup-restoreSession.txt"  "\n" ))
               (invoke output "flush")
	   )    ; end seq
       ) ;end let
      ) ; end seq
       (catch Exception
              (apply displayWMessage "Execution error"  (concat "Your  session could not be restored " (invoke Exception "getMessage") ))
    
	  ) ;end catch

     ) ; end try
     
     
	 
	 
   ) ;end lambda
 ) ;end restoreSessionActionListener


; This method obtains necessary data to call then the method that creates the file "load-npa-assist-restoreGrammar.txt"
;* Input
;- protocolFolder: path to the protocol folder
;- protocolName: name of the protocol that is being analyzed
;- protocolFilePath: path to the file that contains the specification of the protocol
;- action: a string that indicates which action is taking place when launching a new analysis session
;          ("restoreGrammar","generateGrammar" or, in this case, "restoreSession")
;* Output
;- This method creates the file "load-npa-assist-restoreSession.txt" in the protocol folder
 (define restoreLoadGenerateFile (protocolFolder protocolName protocolFilePath action)
   (let ( (mnpaPath (apply getEnvVariable "MAUDE_NPA"))
          (attacksList (object ("java.util.ArrayList")))
         )
     (seq
         (apply getAttacksList protocolFilePath attacksList) ;get the list with the name of the attacks
         (apply createLoadFile  protocolFolder protocolName protocolFilePath mnpaPath
             (boolean false) attacksList action)
     ) ;end seq
   ) ;end let environment
 ) ;end define restoreLoadGenerateFile



; This method reads the path to the file that contains the specification of a protocol
; Depending on the value of the parameter "actionOption", this value will be read from
; a different file
;* Input
;- filePath: path to the file that must be read
;- actionOption: integer that denotes during which action this method is called 0: save session 1: restore session
;   if we are saving a session, the protocol file path will be read from the second line of the
;   the load-npa-assist-restoreGrammar.maude
;   if we are restoring a session, the protocol file path will be read from the first line of the
;   file selected by the user, a file in which he or she previously saved a session.
;* Output
;- string with the path to the file that contains the specification of a protocol

 (define readProtocolFilePath (filePath actionOption)
   (let (   (file (object ("java.io.File" filePath)))
            (freader (object ("java.io.FileReader" file)))
            (breader (object ("java.io.BufferedReader" freader)))
            (protocolFilePath (if (= actionOption (int 0))
                                  (seq
                                   (invoke breader "readLine")
                                   (invoke (invoke breader "readLine") "replace" "load" "")
                                  )
                                  (invoke breader "readLine")
                              ))
         )
      (seq
         (invoke breader "close")
         (invoke freader "close")
         protocolFilePath
      )
    )
 ) ;end readProtocolFilePath



; This method creates the file " load-npa-assist-restoreSession.maude" in the protocol folder
;* Input
;- restoreFilePath: path to the file where a session was saved
;- protocolFolder: path to the protocol folder
;- protocolFilePath: path of the file that contains the specification of the protocol
;- protocolName: name of the protocol
 (define  createRestoreSessionFile (restoreFilePath protocolFolder protocolFilePath protocolName)
  (try
   (let( ;the file will be allocated at the protocol directory
         (loadFile (object ("java.io.File" (concat protocolFolder "/load-npa-assist-restoreSession.maude" ))))
         (fwriter (object ("java.io.FileWriter" loadFile (boolean false)))) ;append text
         (pwriter (object ("java.io.PrintWriter" fwriter)))
         (mnpaPath (apply getEnvVariable "MAUDE_NPA"))
         (guiVar (apply getEnvVariable "MNPA_GUI"))
         (tname (apply readTnameFromRestoredFile restoreFilePath))
         (grammarPath (concat protocolFolder "/" protocolName "-grammar.txt"))
        )
     (seq
          ;write the user's settings
          (invoke pwriter "println" (concat "load " mnpaPath))
          (invoke pwriter "println" (concat "load " protocolFilePath))
          (invoke pwriter "println" (concat "load " guiVar "/clt-npa \n\nload model-checker"))

 (invoke pwriter "println" (concat "load " guiVar "/load-imaude-seq.maude"))

          (invoke pwriter "println" (concat "load " guiVar "/npa-assist \nloop init . \n\n(seq" ))
          (invoke pwriter "println" (concat "(loadg2dlib graphics2d " guiVar "/npa-g2dlib.lsp )"))
          (invoke pwriter "println" (concat "(restoreNPASession " protocolName " " tname " " restoreFilePath ")"))
          (invoke pwriter "println" (concat "(saveGrammar " protocolName " " grammarPath " )"))
          (invoke pwriter "println" (concat "(startListener2 " tname " npareq graphics2d)")) ;;check that graphreq and graphics2d are the right values

          (invoke pwriter "println" (concat "(restoreNPATree graphics2d " tname " " protocolName " " restoreFilePath ")")) ;***********complete
          (invoke pwriter "println" ")") ;this must be the last instruction for writing in the load-npa-assist-restoreSession File
                  
          (invoke fwriter "close")
          (invoke pwriter "close")
     ) ;end seq
   ) ;end let
     (catch Exception
      (apply displayWMessage "Error" (concat "The file load-npa-assist-restoreSession.maude could not be created at your protocol's directory.\n"
             (invoke Exception "getMessage") ))
   )) ;end catch ;try
 ) ; end createRestoreSessionFile


; This function returns the saved tname of a Maude-NPA session that is being restored
;* Input
;- filePath: path to the file where a session was saved
;* Output
;- a string with the id name of the graph that was saved and that is being restored
 (define readTnameFromRestoredFile (filePath)
   (let (   (file (object ("java.io.File" filePath)))
            (freader (object ("java.io.FileReader" file)))
            (breader (object ("java.io.BufferedReader" freader)))
            (tname (seq
                     (invoke breader "readLine")
                     (invoke breader "readLine")
                   ))
         )
      (seq
         (invoke breader "close")
         (invoke freader "close")
         tname
      )
    )
 ) ;end readTnameFromRestoredFile


; This function creates a new NPA tree with the data of the previously saved tree, and restores
; the levels of the tree that were saved. Finally it updates the visualization
; of the NPA tree.
; *input:
; - tname: id of the saved tree
; - protocol: protocol's name of the saved tree
; - restoreFile: path of the file where a previous session was saved
; - aname: attack's name of the saved session
; - nlevel: number of levels of the tree that were created during the saved session.
(define restoreNPATree (tname protocol restoreFile aname nlevel)
(try
    (seq    
      ; fistly, we create and empty graph
      (apply createMtTree tname aname protocol "true")

       ; restore attributes of the graph and kbm objects
       (let ( (graph (fetch tname))
             (protocolFilePath (apply readProtocolFilePath restoreFile (int 0)))
            )
         (seq
          (setAttr graph "protocolPath" protocolFilePath)
          (setAttr graph "currentAction" "restoring")
          ;;;;
          (setAttr graph "isRestoringSession?" (boolean true))
         )
        ) ;end let environment


       
      ;; and, finally, display the graph
       (apply showNPAGraph tname "NPAManager" protocol (concat "Attack " aname) "" toolBarFunNPA)  
       (apply setTextMessageArea tname "Restoring session..." "no")
      ;then, add to the tree all the level entries and redisplay it
      (sinvoke "g2d.util.ActorMsg" "send" "maude" tname 
		 (concat "restoreSavedLevels 0 " nlevel )) 
  
    ) ;end seq
(catch Exception
    (invoke java.lang.System.err "println" (concat "Error in restoreNPATree: " (invoke Exception "getMessage")))
 )
) ;end try
) ;end restoreNPAGraph


; ) ;end main seq

;;;;;;;;;; ./makeg2dlib loaded ../GUI/npa-MWAManager.lsp ;;;;;;;;;;
;(seq

 
;This closure creates a new tab in the SEPanel of the graph frame (the one in which the search space tree
; is diplayed) to show the hierarchy of opened frames. This hierarchy will help the user to manage the amount
; of existing frames, when he or she is consulting the textual information or the strands picture of a node.
(define setNPATabMWAManager (gname frame)
  (let ( (sepanel (invoke frame "getSEPanel"))
          (npaTab (apply mkNPATabMWAManager gname sepanel frame))
          (indexTab  (invoke sepanel "indexOfTab" "Window Manager")) ;change tab's title
        )
    (seq 
     ;  (if (!= (int -1) indexTab)
;	  (invoke sepanel "remove" indexTab)
;	)
       
       (invoke  sepanel "addTab" "Window Manager" (object null)  npaTab "Find and open an active window of this session")
     )
   )
 ) ;end setNPATabMWAManager

(define mkNPATabMWAManager (gname sepanel frame ) 
( let ((graph (if (instanceof gname "java.lang.String")
                             (fetch gname)
                   (object null)))
        (tree  (sinvoke "g2d.mwa.MWAControl" "getTree"))
        (width (invoke sepanel "getWidth" ))
        (height (invoke sepanel "getHeight" ))
        ;tab's base component
        (tabComponent (object ("javax.swing.JPanel")))
        (scrollPane (object ("javax.swing.JScrollPane" tree)))
        (scrollDim (object ("java.awt.Dimension" (int 130) (int 150))))
        (treeMinDim (object ("java.awt.Dimension" (int 200) (int 300))))
        (treeMaxDim (object ("java.awt.Dimension" (int 210) (int 310))))
 
      )
  (seq
     (invoke frame "pack")
     scrollPane
  )  
 )
) ;end mkNPATabFind
 

;)



;;;;;;;;;; ./makeg2dlib loaded ../GUI/npa-loadFile.lsp ;;;;;;;;;;
;(seq

    
    
; This method displays a window in which the user can load a protocol generating a new grammar 
; or even using a preexistent grammar.
;*Inputs:
;- title is the window title
;- kbm is the manager object (basically an a-list)
;*Output:
;-show the window
(define initNPALoadwdo (title kbm)
     (let (
        (frame (object ("g2d.graphviewer.ManagerFrame" title)))
        (toolbar (object ("javax.swing.JToolBar")))      

        ;;Elements to add a menu item to load a protocol
        (jmenu (invoke (invoke frame "getOrigJMenuBar") "getMenu" (int 0))) ;the frame's menu element
        (loadActionListener (object ("g2d.closure.ClosureActionListener" (apply loadMItemAction kbm)))) ;action listener for the menu item
        (loadMItem  (object ("javax.swing.JMenuItem" "Load protocol specification..."))) ;menu item

        ;load examples
        (loadExampleActionListener (object ("g2d.closure.ClosureActionListener" (apply loadExampleAction kbm)))) ;action listener for the menu item
        (loadExampleMItem  (object ("javax.swing.JMenuItem" "Load predefined protocol..."))) ;menu item

        ;load an external Maude-NPA state specification, given as a text file
        (loadExternalStateActionListener (object ("g2d.closure.ClosureActionListener" (apply loadExternalStateAction kbm frame)))) ;closure defined in showMnpaState.lsp
        (loadExternalStateMItem  (object ("javax.swing.JMenuItem" "Load a Maude-NPA state specification..."))) ;menu item

        ;restore a stored session
        (restoreSessionActionListener (object ("g2d.closure.ClosureActionListener"
                                            (apply restoreSessionAction frame)))) ;action listener for the menu item defined in saveRestoreSession.lsp
        (restoreSessionMItem  (object ("javax.swing.JMenuItem" "Restore session..."))) ;menu item
                                  
       )
    (seq
      (invoke iniDialog "setVisible" (boolean false))

      (apply setInstructionsArea frame)
     ;;add a menu item to load a protocol
      (invoke loadMItem "addActionListener" loadActionListener)
      (invoke jmenu "add" loadMItem (int 0))
      
      ;; add a menu item to load an example protocol
      (invoke loadExampleMItem "addActionListener" loadExampleActionListener)
      (invoke jmenu "add" loadExampleMItem (int 1))

      ;; add a menu item to load the specification of a Maude-NPA state
      (invoke loadExternalStateMItem "addActionListener" loadExternalStateActionListener)
      (invoke jmenu "add" loadExternalStateMItem (int 2))

      ;; add a menu item to restore a previously stored session
      (invoke restoreSessionMItem "addActionListener" restoreSessionActionListener)
      (invoke jmenu "add" restoreSessionMItem (int 3))
      
      (invoke frame "setSize" (int 650) (int 550))
      (invoke frame "setVisible" (boolean true))
) )  ; seq ; let ; initNPALoadwdo
) ; end initNPALoadwdo


; a manager object that can be accessed by its unique name
; it has attributes that store things we want to access,
; like the list of kbs (eventually protocols, ...)
(define defNPALoadProtocol ()
  (let ( 
         (name  "NPAManager")
        (kbm0 (fetch name))
        (kbm (if (= kbm0 (object null))  ; **** don't create if exists
                  (object ("g2d.glyph.Attributable"))
                  kbm0))
     )
    (seq
      (if (= kbm0 (object null)) (invoke kbm "setUID" name)) 
        (apply initNPALoadwdo "Maude-NPA GUI" kbm) 
    )
)) ;end defNPALoadProtocol



; method to make and add the instruction text which appears in the first showd window.
; *Input
;- frame: frame which will contain the instruction's text.
; *Output
;- the same input frame with the instructions added.

(define setInstructionsArea (frame)
(let (
        (panel (object ("javax.swing.JPanel" )))
        (layout (object ("javax.swing.BoxLayout" panel javax.swing.BoxLayout.PAGE_AXIS )))
        (scrollPane (object ("javax.swing.JScrollPane" panel)))
        (title (apply makeTextArea "About the Maude-NPA manager \n ______________________________ "
                                    (int 250) (int 50) titleColor "Arial" (int 1) (int 14) white (int 15) ))
        
        (abstract (apply makeTextArea  abstractText (int 620) (int 150) regularColor "Arial" (int 0) (int 13) white (int 15) ))

        (usageTitle (apply makeTextArea "Usage \n_____"
                                    (int 250) (int 50) titleColor "Arial" (int 1) (int 14) white (int 15)))
        (usage (apply makeTextArea usageText (int 620) (int 100) regularColor "Arial" (int 0) (int 13) white (int 15)))
        
    )
  (seq
   (invoke panel "setBackground" white )
   (invoke panel "setLayout" layout)
   ;add elements
   (invoke panel "add" title)
   (invoke panel "add" abstract)
   (invoke panel "add" usageTitle)
   (invoke panel "add" usage)
   
   (invoke frame "add" scrollPane)
   frame
  )
)
) ;end getInstructionsArea


;This method will show a new window to load a protocol when the user selects the
;menu option "File->Load protocol file..."
; *Inputs
;-kbm: protocol manager object that will store the protocol and the grammar the user wants to load.
; *Output
;- the window with all the options to load the protocol and the grammar file.

(define loadMItemAction (kbm)
 (lambda (self event)
  (let ( (frame (let( (temp (object ("g2d.swing.IOPFrame" "Load protocol specification")))
                    )
                  (seq
                    (invoke temp "setResizable" (boolean false))
                    temp
                  )
           ))
         (panel (object ("javax.swing.JPanel" )))
         (auxPanel1 (object ("javax.swing.JPanel" )))
         (auxPanel2 (object ("javax.swing.JPanel" )))
         (auxPanel3 (object ("javax.swing.JPanel" )))
         (auxPanel4 (object ("javax.swing.JPanel" )))
         
         (layout (object ("javax.swing.BoxLayout" panel javax.swing.BoxLayout.PAGE_AXIS )))

                  ;general buttons
         ;acceptButton will launch a new instance of IOP with the selected protocol and
         ;with the user's settings.
         (acceptButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                       "Accept"
                                                       "Execute tool"
                                                       (apply acceptButtonAction kbm frame)))))
                                (seq
                                  (invoke temp "setSize" (int 60) (int 15))
                                  (invoke temp "setEnabled" (boolean false))
                                  temp
                                )  ))
        ;cancelButton will close the current window and will do nothing more
        (cancelButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                       "Cancel"
                                                       "Cancel execution"
                                                       (apply cancelAction frame)))))
                                (seq
                                  (invoke temp "setSize" (int 60) (int 15)) temp
                                ) ))
        
        ; elements to load an existing grammar
         (grammarText (apply  makeTextArea grammarExplanation (int 520) (int 165) regularColor "Arial" (int 0) (int 14) greySilver (int 15)))

         (grammarPath (let ( (temp (object ("javax.swing.JTextField" (int 25))))
                            )
                            (seq
                              (invoke temp "setEditable" (boolean false))
                              temp
                            )))
         (checkGrammar (object ("javax.swing.JCheckBox" "Load existing grammars"))) ;a checkBox to indicate that a grammar will be restored
          
         ;a button to open a file dialog an select the grammar file the user wants to restore
         (loadGrammarButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                       "Browse..."
                                                       "Load an existing grammar"
                                                       (apply loadGrammarAction kbm frame grammarPath checkGrammar)))))
                                (seq 
                                  (invoke temp "setSize" (int 60) (int 15))
                                  (invoke temp "setEnabled" (boolean false))
                                  temp
                                )  ))
        
         (checkGrammarActionListener (object ("g2d.closure.ClosureActionListener"  ;action listener for the checkBox
                                                 (apply checkGrammarAction kbm frame checkGrammar loadGrammarButton grammarPath))))

         ;elements to load the protocol file
         (protocolText (apply  makeTextArea loadExplanation (int 520) (int 75) regularColor "Arial" (int 0) (int 14) greySilver (int 15)))
         (protocolPath (let ( (temp (object ("javax.swing.JTextField" (int 35))))
                            )
                            (seq
                              (invoke temp "setEditable" (boolean false))
                              temp
                            )))

         ;elements to choose the directory for the protocol folder
         (protocolFolderText (apply  makeTextArea protocolFolderExplanation (int 520) (int 50) regularColor
                                     "Arial" (int 0) (int 14) greySilver (int 15)))
         (protocolFolderPath (let ( (temp (object ("javax.swing.JTextField" (int 35))))
                                   )
                               (seq
                                 (invoke temp "setEditable" (boolean false))
                                 temp
                             )))

                  ;a button to open a file dialog an select the grammar file the user wants to restore
         (loadProtocolButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                       "Browse..."
                                                       "Load an existing protocol specification"
                                                       (apply loadProtocolAction kbm frame protocolPath  protocolFolderPath acceptButton)))))
                                (seq
                                  (invoke temp "setSize" (int 60) (int 15))  temp
                                ) ))
         
         ;a button to open a file dialog an select the grammar file the user wants to restore
         (selectProtocolFolderButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                            "Browse..."
                                                            "Select a folder for this analysis session"
                                                            (apply selectProtocolFolderAction kbm frame protocolFolderPath acceptButton)))))
                                (seq
                                  (invoke temp "setSize" (int 60) (int 15))
                                  temp
                                )))
       )
  (seq
   ;;check box to load an existing grammar
      (invoke checkGrammar "addActionListener" checkGrammarActionListener)
      (invoke panel "setLayout" layout)
      (invoke grammarPath "setMaximumSize" (object ("java.awt.Dimension" (int 250)  (int 25))))
      (invoke protocolPath "setMaximumSize" (object ("java.awt.Dimension"  (int 250)   (int 25))))
      (invoke protocolFolderPath "setMaximumSize" (object ("java.awt.Dimension"  (int 250)   (int 25))))

      ;In this part of the window, the user will be able to select whether if he wants to
      ;restore a new grammar (and select it) or generate a new one.
      ;                                                                                     int top, int left, int bottom, int right
      (invoke auxPanel1 "setBorder" (sinvoke "javax.swing.BorderFactory" "createEmptyBorder" (int 0) (int 15) (int 0) (int 15)))
      (invoke auxPanel1"setLayout" (object ("javax.swing.BoxLayout" auxPanel1 javax.swing.BoxLayout.X_AXIS )))
      (invoke auxPanel1 "add" checkGrammar)
      (invoke auxPanel1 "add" (sinvoke "javax.swing.Box"  "createRigidArea" (object ("java.awt.Dimension" (int 20)  (int 25)))))
      (invoke auxPanel1 "add" grammarPath)
      (invoke auxPanel1 "add" (sinvoke "javax.swing.Box"  "createRigidArea" (object ("java.awt.Dimension" (int 20)  (int 25)))))
      (invoke auxPanel1 "add" loadGrammarButton )
      (invoke auxPanel1 "setMaximumSize" (object ("java.awt.Dimension" (int 500)  (int 35)) ))
      (invoke auxPanel1 "setAlignmentX" java.awt.Component.LEFT_ALIGNMENT)

      ;In this part of the window, the user will be able to select the protocol he wants to load.
      (invoke auxPanel2 "setBorder" (sinvoke "javax.swing.BorderFactory" "createEmptyBorder" (int 0) (int 15) (int 0) (int 15)))
      (invoke auxPanel2 "setLayout" (object ("javax.swing.BoxLayout" auxPanel2 javax.swing.BoxLayout.X_AXIS )))
      (invoke auxPanel2 "add" protocolPath)
      (invoke auxPanel2 "add" (sinvoke "javax.swing.Box"  "createRigidArea" (object ("java.awt.Dimension" (int 20)  (int 25)))))
      (invoke auxPanel2 "add" loadProtocolButton)
      (invoke auxPanel2 "setMaximumSize" (object ("java.awt.Dimension" (int 500)  (int 35)) ))
      (invoke auxPanel2 "setAlignmentX" java.awt.Component.LEFT_ALIGNMENT)

      ;Accept and cancel buttons.
      (invoke auxPanel3 "setBorder" (sinvoke "javax.swing.BorderFactory" "createEmptyBorder" (int 10) (int 0) (int 0) (int 10)))
      (invoke auxPanel3 "setLayout" (object ("javax.swing.BoxLayout" auxPanel3 javax.swing.BoxLayout.X_AXIS )))
      (invoke auxPanel3 "add" (sinvoke "javax.swing.Box"  "createHorizontalGlue" ))
      (invoke auxPanel3 "add"  cancelButton)
      (invoke auxPanel3 "add" (sinvoke "javax.swing.Box"  "createRigidArea" (object ("java.awt.Dimension" (int 10)  (int 25)))))
      (invoke auxPanel3 "add" acceptButton)
      (invoke auxPanel3"setMaximumSize" (object ("java.awt.Dimension" (int 535)  (int 55)) ))
      (invoke auxPanel3 "setAlignmentX" java.awt.Component.LEFT_ALIGNMENT)

      ;In this part of the window, the user will be able to select the directory for the protocol folder
      (invoke auxPanel4 "setBorder" (sinvoke "javax.swing.BorderFactory" "createEmptyBorder" (int 0) (int 15) (int 0) (int 15)))
      (invoke auxPanel4 "setLayout" (object ("javax.swing.BoxLayout" auxPanel4 javax.swing.BoxLayout.X_AXIS )))
      (invoke auxPanel4 "add" protocolFolderPath)
      (invoke auxPanel4 "add" (sinvoke "javax.swing.Box"  "createRigidArea" (object ("java.awt.Dimension" (int 20)  (int 25)))))
      (invoke auxPanel4 "add" selectProtocolFolderButton)
      (invoke auxPanel4 "setMaximumSize" (object ("java.awt.Dimension" (int 500)  (int 35)) ))
      (invoke auxPanel4 "setAlignmentX" java.awt.Component.LEFT_ALIGNMENT)

      ;Add auxiliar panels
      (invoke panel "add" grammarText)
      (invoke panel "add" auxPanel1)
      (invoke panel "add" protocolText)
      (invoke panel "add" auxPanel2 )
      (invoke panel "add" protocolFolderText)
      (invoke panel "add" auxPanel4 )
      (invoke panel "add" auxPanel3 )

      (invoke frame "add" panel)
      (invoke frame "setSize" (int 550) (int 470)) 
      (invoke frame "setVisible" (boolean true))
  )
  )
 )
) ;end loadMItemAction


;Action for the cancel button. It will close the frame.
; *Input
;- frame: frame which will be closed.

(define cancelAction (frame)
   (lambda (self event)
     (invoke frame "dispose") ))



;This method allows the user to select the protocol file he wants to load from his or her
;file system by showing a file chooser dialog. Once the user has choosen the file,
; a text field will be updated showing the path of the selected protocol file.
; *Input
;-kbm:  protocol manager object.
;-frame: frame which contains the button that launches this action, i.e, the one to load
;        the protocol and the grammar file.
;-textField: text field that will show the selected protocol's path.
;- pfTextField: textField object that shows the protocol folder path

(define loadProtocolAction (kbm frame textField pfTextField acceptButton)
  (lambda (self event)
   (let (  (fileDialog (object("g2d.swing.IOPFileChooser"))) ;file chooser element to selct the protocol file
           (returnVal (invoke fileDialog "showOpenDialog" frame))
        )
    (seq                     
     (if (= returnVal javax.swing.JFileChooser.APPROVE_OPTION)
           (let( (filePath (invoke   (invoke fileDialog "getSelectedFile") "getAbsolutePath"))
                  (guiVar (apply getEnvVariable "MNPA_GUI"))
                  (protocolName (apply getProtocolName filePath))
                )
                  (seq
                     (invoke textField "setText" filePath)
                     (setAttr kbm "protocol" protocolName)
                     (setAttr kbm "protocolPath" filePath)

                     ;default settings for protocolFolder
                     (invoke pfTextField "setText" (concat guiVar "/"  protocolName   ))
                     (setAttr kbm "protocolFolder" (concat guiVar "/"  protocolName  ))
                     (invoke acceptButton "setEnabled" (boolean true))

                     (invoke frame "repaint")
                     kbm
                  )
           )
           (seq
              (setAttr kbm "protocolFolder" "")
              (setAttr kbm "protocol" "")
              (invoke pfTextField "setText" "")
              (invoke frame "repaint")
              (invoke acceptButton "setEnabled" (boolean false))
              kbm
           )
     ) ;endif
  ) ) )
) ;end loadProtocolAction



;This method allows the user to select the directory where the user wants to allocate the protocol folder
; i.e. the folder where the Maude-NPA will create all the necessary files during the analysis session
;  Once the user has choosen the file,
; a text field will be updated showing the path of the selected protocol folder.
; *Input
;-kbm:  protocol manager object.
;-frame: frame which contains the button that launches this action, i.e, the one to load
;        the protocol and the grammar file.
;-textField: text field that will show the selected protocol folder's path.

(define selectProtocolFolderAction (kbm frame textField acceptButton)
  (lambda (self event)
   (let (  (fileDialog (let( (temp (object("g2d.swing.IOPFileChooser"))) ;file chooser element to select the protocol file
                            )
                         (seq
                           (invoke temp "setFileSelectionMode" (int 1)) ;javax.swing.JFileChooser.DIRECTORIES_ONLY)
                           (invoke temp "setAcceptAllFileFilterUsed" (boolean false))
                           temp
                         )))
           (returnVal (invoke fileDialog "showSaveDialog" frame))
           (guiVar (apply getEnvVariable "MNPA_GUI"))
           (exampleDirectory (concat guiVar "/examples"))
        )
    (seq                     
     (if (= returnVal javax.swing.JFileChooser.APPROVE_OPTION)
        ;then
        (if (= (invoke (invoke   (invoke fileDialog "getSelectedFile") "getAbsolutePath") "startsWith" exampleDirectory)
              (boolean true))
           ;then
            (apply displayWInfoMessage "Forbidden"
                                         "You are not allowed to save files into this directory.\nPlease choose a different directory."
                                         javax.swing.JOptionPane.ERROR_MESSAGE)
           ; else
           (let( (filePath (invoke   (invoke fileDialog "getSelectedFile") "getAbsolutePath"))
               )
                  (seq
                     (invoke textField "setText" filePath)
                     (setAttr kbm "protocolFolder" filePath)
                     (invoke frame "repaint")
                     (if (and (!= (getAttr kbm "protocolPath") "")
                              (!= (getAttr kbm "protocolPath") (object null)))
                         (invoke acceptButton "setEnabled" (boolean true))
                         (invoke acceptButton "setEnabled" (boolean false))
                     )
                     kbm
                  ) 
           )
        ) ;end if
        ;else returnVal =!= APPROVE_OPTION, use default protocolFolder
        (let ( (protocolName (getAttr kbm "protocol"))
              )
          (seq
            (if (or (= protocolName (object null))
                     (= protocolName ""))
                (seq
                   (setAttr kbm "protocolFolder" "")
                   (invoke acceptButton "setEnabled" (boolean false))
                )
               ;else set default protocolFolder values
              (seq
                     ;default settings for protocolFolder
                     (invoke textField "setText" (concat guiVar "/" protocolName   ))
                     (setAttr kbm "protocolFolder" (concat guiVar "/" protocolName   ))
                     (invoke acceptButton "setEnabled" (boolean true))
              )
            ) ;end if

          kbm
          )
        ) ;end else part
     ) ;endif
  ) ) )
) ;end selectProtocolFolderAction




;This method perfoms the action which will be executed when the user wants to load the choosen protocol.
;The files load-npa-assist.maude and startup.txt are created in a new directory whose name is the protocol's file name
;without the extension and using the user's settings.
;If the user loaded an existing grammar file to restore it, this grammar file will be copied at the protocol's directory.
;Finally, an instance of IOP will be loaded and the protocol manager window will be displayed.
;* Inputs
;-kbm: load protocol manager object
;-frame: main frame

(define acceptButtonAction (kbm frame)
  (lambda (self event)
   (try
     (if (= (getAttr kbm "protocolPath") (object null))
      (throw (object ("java.lang.Exception" "No protocol file was loaded. Please, select a file \nby clicking on the last 'Browse...' button and press 'Accept'")))

      (let ( (pathMnpa (apply getEnvVariable "MAUDE_NPA")) ;this environment variable indicates where the user has located the
                                                         ;Maude-NPA tool
          (filePath (getAttr kbm "protocolPath" ""))
          (protocolName (getAttr kbm "protocol" ""))
          (grammarFile (getAttr kbm "grammarFile" "")) ;path of the grammar file that the user wants to restore (in case it exists)
          (attacksList (object ("java.util.ArrayList"))) ;initialization of the protocol's list of attacks
          (protocolFolder (getAttr kbm "protocolFolder" ""))
         )
        (seq
                 (invoke frame "dispose")

                 (apply mkProtocolDir protocolFolder)
                 
                 (apply getAttacksList filePath attacksList) ;build a list with the attacks specified at the protocol specification file
                
                 ;create the load-npa-assist-generateGrammar.maude file indicating that the grammar must be generated 
                 (apply createLoadFile protocolFolder protocolName filePath pathMnpa (boolean true) attacksList "generateGrammar")
                  
                 ;create the load-npa-assist-restoreGrammar.maude file indicating that the grammar will be restored from
                 ;a file in the protocol's directory
                 (apply createLoadFile  protocolFolder protocolName filePath pathMnpa (boolean false) attacksList "restoreGrammar")

                 ;create the startup-generateGrammar.txt file to load load-npa-assist-generateGrammar.maude
                 (apply createStartupFile  protocolFolder  "generateGrammar")

		 ;create the startup-restoreGrammar.txt file to load load-npa-assist-restoreGrammar.maude
                 (apply createStartupFile  protocolFolder  "restoreGrammar")

                 (if (!= grammarFile "")
                      ;create the load-npa-assist.maude file indicating that the grammar must be generated 
                      ;(apply createLoadFile protocolName filePath pathMnpa (boolean true) attacksList)
                      (seq
                        ;copy the grammar file selected by the user into the protocol's directory
                        (apply copyGrammarFile  protocolFolder protocolName grammarFile)

                         
                        ;start a new IOP instance with the user's configuration given by the
                        ;files startup.txt and load-npa-assit.maude.
                        (apply startIOP  protocolFolder "restoreGrammar")
		      )
                      ;start a new IOP instance with the user's configuration given by the
                      ;files startup.txt and load-npa-assit.maude. 
                      (apply startIOP  protocolFolder "generateGrammar")

                 ) ;end if      
         )
        )
       ) ;end if
         (catch Exception
              (apply displayWMessage "Execution error"  (concat "Your protocol could not be loaded. " (invoke Exception "getMessage") ))
    
         ) ;end catch
       ) ;end try
          
  )) ;end acceptButtonAction


;get the protocol name given the path of the protocol file the user wants to load
;*Inputs
;- filePath : path of the protocol's file the user wants to load.
;* Outputs
;- the absolute name of the file without the extension. In this example, the returned
; value would be nspk.
(define getProtocolName (filePath)
 (let ( (startP (+ (invoke filePath "lastIndexOf" "/") (int 1)))
        (endP (invoke filePath "lastIndexOf" "."))
       )
   ( if (and (> startP (int 0)) (> endP (int 0)))
        (invoke filePath "substring" startP endP)
        filePath
    ) ;end if
 )
) ;end getProtocolName


               

;This method creates a new folder for a protocol in the NPAAssist directory
;* Inputs
;- protocolFolder:folder where the protocol's directory will be located

(define mkProtocolDir (protocolFolder) 
 ( let( (fl (object ("java.io.File" protocolFolder )))
       )
       (seq
	   (if (= (invoke fl "exists") (boolean false))
	       (invoke fl "mkdirs") ;make dir
	   )
	    ;end if
       ; protocolFolder
        fl
       )
 )
)  ;end mkProtocolDir




;This method creates the file load-npa-assist.maude, taking into account if the grammar must be
;generated or it will be restored from an existing grammar file.
;* Inputs
;-pname : protocol name
;-ppath : protocol path
;-mpath : maude-npa path
;-firstTime? : is the first time this protocol is loaded? If true, then the grammar must be generated.
;             Otherwise, the grammar will be restored from an existing grammar file.
;-attactksList:  list of the protocol's attacks.
;NOTE: the variables str<number>LoadNPAAssist have been defined at defines.lsp

(define createLoadFile (protocolFolder pname ppath mpath firstTime? attacksList grammarAction)
 (try
   (let (  ;the file will be allocated at the protocol directory
         (loadFile (object ("java.io.File" (concat  protocolFolder "/load-npa-assist-" grammarAction  ".maude" ))))
         (fwriter (object ("java.io.FileWriter" loadFile (boolean false)))) ;append text
         (pwriter (object ("java.io.PrintWriter" fwriter)))
         (attacksString (apply list2String attacksList))
         (guiVar (apply getEnvVariable "MNPA_GUI"))
         (grammarPath (concat protocolFolder "/" pname "-grammar.txt"))
        )
   (seq
    ;write the user's settings
    (invoke pwriter "println" (concat "load " mpath) )
    (invoke pwriter "println" (concat "load " ppath))

    (invoke pwriter "println" (concat "load " guiVar "/clt-npa \n\nload model-checker"))
    
  ;  (invoke pwriter "println" (concat "load " guiVar "/lib-util"))
   ; (invoke pwriter "println" (concat "load " guiVar "/lib-meta"))
    ;(invoke pwriter "println" (concat "load " guiVar "/lib-imaude"))
      
    (invoke pwriter "println" (concat "load " guiVar "/load-imaude-seq.maude"))
    
  ;  (invoke pwriter "println" strLoadFile)
    (invoke pwriter "println" (concat "load " guiVar "/npa-assist \nloop init . \n\n(seq" ))
    (invoke pwriter "println" (concat "(loadg2dlib graphics2d " guiVar "/npa-g2dlib.lsp )"))
    
    (invoke pwriter "println" "(initNPAeset maude graphics2d graphics2d)")
    
    (if (= firstTime? (boolean true))
        (seq
           ;the grammar will be generated
           (invoke pwriter "println" (concat "(generateGrammar " pname ")"))
           (invoke pwriter "println" (concat "(saveGrammar " pname " " grammarPath ")"))
        )
        (invoke pwriter "println" (concat "(restoreGrammar " pname " " grammarPath ")")) ;the grammar will be restored
    )

    (invoke pwriter "println" (concat "(startListener2 " pname " npareq graphics2d )"))
    (invoke pwriter "println" (concat "(initNPAManager graphics2d " pname " " attacksString " ))"))

    (invoke fwriter "close")
    (invoke pwriter "close")
   
   )
  )
   (catch Exception
      (apply displayWMessage "Error" (concat "The file load-npa-assist.maude could not be created at your protocol's directory.\n"
             (invoke Exception "getMessage") )
              javax.swing.JOptionPane.ERROR_MESSAGE)
   )) ;end catch ;try
 
) ;end createLoadFile



;This method makes the file startup.txt to execute the IOP instance. This file will be located in the
; protocol directory.
;* Input
;-protocolPath: relative path to the protocol's directory. for examples protocolPath=examples+/protocol
;NOTE: the variables str<number>Startup have been defined at defines.lsp

(define createStartupFile (protocolFolder action) 
(try
  (let ( (startupFile  (object ("java.io.File" (concat protocolFolder "/startup-" action ".txt" ))))
         (fwriter (object ("java.io.FileWriter" startupFile (boolean false))))
         (pwriter (object ("java.io.PrintWriter" fwriter)))
         (bldir (apply getEnvVariable "BLDIR"))
	  ; MNPA_LIB
	 (libDir (apply getEnvVariable "MNPA_LIB")) 
        )
   (seq
    (invoke pwriter "print" str1Startup)
    (invoke pwriter "println" (concat "load-npa-assist-" action))
    (invoke pwriter "print" str2Startup)
    (invoke pwriter "print" bldir)
    
    ;;;
    (invoke pwriter "print" (concat " -cp " libDir "/mnpa-gui.jar" ))
    ;;;
    
    (invoke pwriter "println" str3Startup)
    
    (invoke fwriter "close")
    (invoke pwriter "close")
   )
 )
 (catch Exception
      (apply displayWMessage "The file startup.txt could not be created at your protocol's directory.\n"
             (invoke Exception "getMessage")
	  javax.swing.JOptionPane.ERROR_MESSAGE)
	    
   )) ;end catch ;try
)  ;end createStartupFile



;This method loads a new instance of the IOP using the file startup.txt generated for the selected protocol
; and with the user's configuration. This new IOP instance will load the protocol manager window so that the
; user will be able to select an attack and execute the Maude-NPA tool.
;* Input
;-protocolFolder: protocol's name.
;-grammarAction: string indicating if the grammar file must be generated or restored.
;* Output
; A new IOP instance is launched, sugin the user's settings.
(define startIOP (protocolFolder grammarAction)
(try
  (let ( ; (file (apply mkProtocolDir protocolFolder))    ;***********************************
         (file (object ("java.io.File" protocolFolder))) 
         (runtime (sinvoke "java.lang.Runtime" "getRuntime"))
	 (process (invoke runtime "exec" "/bin/tcsh" (object null) file ))
         (output (object ("java.io.DataOutputStream" (invoke process "getOutputStream" ))))
     )
    (seq   
     ; increase the stacksize 
     (invoke output "writeBytes" "unlimit stacksize\n" )
     (invoke output "writeBytes" (concat "iop -i startup-" grammarAction ".txt \n" ))
     (invoke output "flush")
    )
  )
  (catch Exception
      (apply displayWMessage "Error: Maude-NPA could not load your protocol.\n"
             (invoke Exception "toString")
	  javax.swing.JOptionPane.ERROR_MESSAGE)
   )) ;end catch ;try
) ;end startIOP


;Concatenate the elements of an arraylist building a string.
;* Input
;-arrayList: list whose elements must be concatenated into a string.
;* Output
;- a string containing the arrayList elements.

(define list2String (arrayList)
 (let ( (strBuf (object ("java.lang.StringBuffer")))
      )
   (seq
     (apply list2StringRec arrayList strBuf (int 0))
     (invoke strBuf "toString")
   )
  )
) ;end list2String



;Recursive auxiliar function to cancatenate the elements of an arrayList in a string.
;* Input
;-arrayList: list whose elements must be concatenated into a string.
;-strBuf: string buffer which will store temporaly the elements of the list as strings.
;-pos: position of the current element of the list that will be added to the string buffer.
;* Output
;- a string buffer containing the arrayList elements.

(define list2StringRec (arrayList strBuf pos)
   (if (= pos (invoke arrayList "size"))
       strBuf
       (seq
        (invoke strBuf "append" (concat (invoke arrayList "get" pos) " "))
        (apply list2StringRec arrayList strBuf (+ pos (int 1)))
       )
   )
) ;end list2StringRec



;This method makes a list which contains all the protocol's attacks specified at the protocol's file.
;* Input
;-filePath: path of the protocol's file.
;-attacksList: empty list to store the attacks name.
;* Output
;- a list whose elemets are the names of the attacks

(define getAttacksList (filePath attacksList)
  (let ( (protocolFile (object ("java.io.File" filePath)))
         (fileReader (object ("java.io.FileReader" protocolFile)))
         (bufreader (object ("java.io.BufferedReader" fileReader)))
        )
     (apply getAttacksListRec  attacksList bufreader)
  )
) ;end getAttacksList



;Recursive and auxiliar method to make a list with the protocol attacks's name.
;* Input
;-attaksList: list which will store the attacks's name.
;-bufreader: buffered reader to read from the protocol's file.
;* Output
;- list with all the attacks's name.

(define getAttacksListRec (attacksList bufreader)
 (let ( (line (invoke bufreader "readLine"))
       )

   
    (if (= line (object null)) ;there is nothing more to read.
        (seq
         ;close the bufreader
           (invoke bufreader "close")
           attacksList ;return the list
        )
        (let ( (lineTrimmed (invoke line "replace" " " ""))
               (pattern (object ("java.lang.String" "eqATTACK-STATE(" ))) ;string used to specify the attacks
              )
         (if (!= (invoke lineTrimmed  "startsWith" pattern) (boolean true))
             (apply getAttacksListRec attacksList bufreader)
             (let ((positionCP (invoke lineTrimmed "indexOf" ")" ))
                   (attackName (invoke lineTrimmed "substring" (invoke pattern "length")
                                             positionCP)))
               (seq
                 (invoke attacksList "add" (concat "a" attackName))
                 (apply getAttacksListRec attacksList bufreader)
               )
             )
         )
        )
    
   )
  )
) ;end getAttacksListRec



;This method performs the action for the mouse listener of the checkBox used to load an existing grammar file.
;If the checkBox is selected, then the Browse button to load the grammar file is enabled. Otherwise,
;the button is disabled.
; *Inputs
;-frame: main frame.
;-checkGrammar: checkBox whose action listener is performed by this method.
;-loadGrammarButton: button which will be enabled or disabled depending on the checkBox state.

(define checkGrammarAction (kbm frame checkGrammar loadGrammarButton textField)
  (lambda (self event)
      (seq
          (if (= (invoke checkGrammar "isSelected" ) (boolean true))
                (invoke loadGrammarButton "setEnabled" (boolean true))  
                (seq
                  (invoke loadGrammarButton "setEnabled" (boolean false))
                  (setAttr kbm "grammarFile" "")
                  (invoke textField "setText" "")
                )
                
          ) ;end if
          (invoke frame "repaint")
     )
  )
) ;end checkGrammarAction 


;This method performs the action listener for the Browse... button, i.e, allows the user to select an existing
;grammar to restore it and to not generate it again. The grammar file's path will be set as an attribute of the
; manager object.
;* Inputs
;-kbm: manager object
;-frame: main frame
;* Output
;-the manager object with the grammarFile attribute set. This attribute contains the chosen grammar file path.
 
(define loadGrammarAction (kbm frame textField checkGrammar)
  (lambda (self event)
    (let ( (fileDialog (object("g2d.swing.IOPFileChooser")))
           (returnVal (invoke fileDialog "showOpenDialog" frame))
          )
      (if (= (invoke checkGrammar "isSelected" ) (boolean true))
         (if (= returnVal javax.swing.JFileChooser.APPROVE_OPTION)
           (let( (filePath (invoke   (invoke fileDialog "getSelectedFile") "getAbsolutePath")))
             (seq
                (setAttr kbm "grammarFile" filePath)
                (invoke textField "setText" filePath )
                (invoke frame "repaint")
                kbm
              )
            )
         ) ;end if
       ) ;end if
    )
  ) 
) ;end loadGrammarAction


;This method copies the grammar file loaded by the user into the protocol's directory.
; *Inputs
;-protocolName: protocol's name.
;-grammarFile: path of the grammar file chosen by the user.

(define copyGrammarFile (protocolFolder protocolName grammarFile)
 (let (  ;File chosen by the user
       (sourceFile (object ("java.io.File" grammarFile)))
       (fileReader (object ("java.io.FileReader" sourceFile)))
       (bufreader (object ("java.io.BufferedReader" fileReader)))

       ;File copy located at the protocol directory
       (targetFile (object ("java.io.File" (concat protocolFolder "/" protocolName "-grammar.txt" ))))
       (fwriter (object ("java.io.FileWriter" targetFile (boolean false)))) ;append text
       (pwriter (object ("java.io.PrintWriter" fwriter)))
      )
   (seq
      (apply copyGrammarFileRec bufreader pwriter)
      (invoke bufreader "close")
      (invoke pwriter "close")
   )
 )
) ;end copyGrammarFile


;This recursive method copies the grammar file loaded by the user into the protocol's directory.
;It reads a line from the source file and writes it in the target file.
; *Inputs
;-bufreader: buffered reader to read a line from the source file.
;-pwriter: print writer to write the read line in the target file.
; *Outputs
;- the buffered reader with one more read line.

(define copyGrammarFileRec (bufreader pwriter)
  (let ( (line (invoke bufreader "readLine"))
        )
     (if (!= line (object null))
         (seq
           (invoke pwriter "println" line)
           (apply copyGrammarFileRec bufreader pwriter)
         )
         bufreader
     )
  )
) ;end copyGrammarFileRec



;This method will show a new window to load one of the protocol examples when the user selects the
;menu option "File->Load example..."
; *Inputs
;-kbm: protocol manager object that will store the protocol and the grammar the user wants to load.
; *Output
;- the window with a list of some of the protocol examples.

(define loadExampleAction (kbm)
  (try
  (lambda (self event)
     (let ( (frame (object ("g2d.swing.IOPFrame" "Load predefined protocol")))
            (panel (object ("javax.swing.JPanel" )))
            (emptyPanel (object ("javax.swing.JPanel" )))
            (emptySPanel (object ("javax.swing.JPanel" )))
         
         (model (let ( (temp (object ("javax.swing.DefaultListModel")))
                     )
                 (seq
             
                   (invoke temp "addElement" "Diffie-Hellman")
		   (invoke temp "addElement" "Homo-hpc")
		   (invoke temp "addElement" "Homo-NSL")
		   (invoke temp "addElement" "NSL")
		   (invoke temp "addElement" "NSPK")
		   (invoke temp "addElement" "Secret06")
		   (invoke temp "addElement" "Secret07")
		   (invoke temp "addElement" "XOR-NSL")
		   temp
                 ) ))
         ;list of example protocols. The user can select one of its elements.
         (list (let ( (temp (object ("javax.swing.JList" model) )) )
                (seq
                 ;(if (invoke temp "isSelectionEmpty")
                  ; (invoke temp "setSelectedIndex" (int 0)))
                 temp
                )
	    ))
	  (listScroll (object ("javax.swing.JScrollPane" list)))
	  
         (textArea (apply  makeTextArea loadExampleExplanation (int 520) (int 50) regularColor "Arial" (int 0) (int 14) greySilver (int 15)))
          ;general buttons
         ;acceptButton will launch a new instance of IOP with the selected example protocol and
         ;using the user's settings.
         (acceptButton (let ( (temp (object ("g2d.swing.IOPButton" "Accept" "Execute tool" (apply startIOPExample kbm)))))
                                (seq
                                  (invoke temp "setSize" (int 80) (int 15))
                                  (invoke temp "setEnabled" (boolean false))
                                  temp
                                )  ))
         ;The cancel button will close the current window.
         (cancelButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                       "Cancel"
                                                       "Cancel execution"
                                                       (apply cancelAction frame)))))
                                (seq
                                  (invoke temp "setSize" (int 80) (int 15)) temp
                                ) ))
         (layout (object ("javax.swing.BoxLayout" panel javax.swing.BoxLayout.PAGE_AXIS )))
         (mouseListener (object ("g2d.closure.ClosureMouseListener")))
       )
   (seq
    
     ;(setAttr kbm "protocol" "NSPK")
     (setAttr kbm "protocol" "")
     (invoke mouseListener "setMouseAction" (int 500)  (apply setSelectedProtocol list kbm acceptButton frame) )
    
     (invoke list "addMouseListener" mouseListener)
     (invoke list "setFixedCellHeight" (int 20))
     (invoke list "setFixedCellWidth" (int 100))
    
     (invoke emptyPanel "setPreferredSize" (object ("java.awt.Dimension" (int 20) (int 40))))
     (invoke emptySPanel "setPreferredSize" (object ("java.awt.Dimension" (int 410) (int 20))))
    
     (invoke panel "setMaximumSize" (object ("java.awt.Dimension" (int 85) (int 40))))
     (invoke panel "setPreferredSize" (object ("java.awt.Dimension" (int 85) (int 40))))
     (invoke panel "setLayout" layout)
    
     (invoke frame "add" textArea java.awt.BorderLayout.NORTH)
  ;   (invoke frame "add" list java.awt.BorderLayout.CENTER)
      (invoke frame "add" listScroll java.awt.BorderLayout.CENTER)
     (invoke frame "add" emptyPanel java.awt.BorderLayout.WEST)
     (invoke panel "add" acceptButton)
     (invoke panel "add" cancelButton)
     (invoke frame "add" panel java.awt.BorderLayout.EAST)
     (invoke frame "add" emptySPanel java.awt.BorderLayout.SOUTH)
    
     (invoke frame "setSize" (int 410) (int 320)) ;220
     (invoke frame "repaint")
     (invoke frame "setVisible" (boolean true))
   )
 )
 )
      (catch Exception
         (seq
            (apply displayWInfoMessage
                  "Error"
                      (concat "An exception was raised while loading the predefined examples: \n  "
                              (invoke Exception "getMessage"))
		 javax.swing.JOptionPane.ERROR_MESSAGE)
         )
      ) ;end catch
   ) ;end try
) ;end of loadExampleAction


;This method loads a new instance of the IOP using the file startup.txt generated for the selected example protocol
; and with the user's configuration. This new IOP instance will load the protocol manager window so that the
; user will be able to select an attack and execute the Maude-NPA tool.
;-kbm: protocol manager object.

(define startIOPExample (kbm)
  
(lambda (self event)
 (let ( (mnpaPath (apply getEnvVariable "MAUDE_NPA"))
        (guiVar (apply getEnvVariable "MNPA_GUI"))
        (attacksList (object ("java.util.ArrayList")))
        (protocol (getAttr kbm "protocol")) ;name of the protocol example
        (protocolFolder (concat guiVar "/examples/" protocol))
        (protocolPath (concat protocolFolder "/" protocol ".maude")) 
      )
  (seq
 
    
    (apply getAttacksList protocolPath attacksList) ;get the list with the name of the attacks
     
    ;create the load-npa-assist.maude file for the protocol
    (apply createLoadFile protocolFolder protocol protocolPath  mnpaPath (boolean false) attacksList "restoreGrammar")

    ;create the startup.txt file for the protocol
    (apply createStartupFile protocolFolder "restoreGrammar" )

    ; create the startup.txt and load-npa-assist.maude files to load the protocol
    ; and generate the grammar, just in case the user needs it later
    (apply createLoadFile protocolFolder protocol protocolPath  mnpaPath (boolean true) attacksList "generateGrammar")
    (apply createStartupFile protocolFolder "generateGrammar" )

    ;launch a new instance of IOP using the user's settings.
    (apply startIOP protocolFolder "restoreGrammar" )
  )
 )
)
) ;end startIOPExample


 
;this method sets the attribute "protocol" to the kbm object. This attribute stores
;the example protocol that will be loaded and its value is given by the selected value
;of the list which contains the example protocols.
; *Inputs
;-list: list with the name of the example protocols.
;-kbm: protocol manager object.
;-acceptButton: accept button to start the analysis of the predefined protocol selected
;               by the user
; *Output
;-the kbm object with its protocol attribute value updated.

(define setSelectedProtocol (list kbm acceptButton frame)
 (lambda (self event) 
       (let ( ;get the name of the selected example protocol.
          (protocol (if (= (invoke list "getSelectedValue") (object null))
                         ""
                        (object ("java.lang.String" (invoke list "getSelectedValue") ))
                        ))
      ;    (guiVar (if (!= protocol "")
       ;;               (apply getEnvVariable "MNPA_GUI")
        ;              ""))
       (guiVar (object ("java.lang.String" "/Users/sosanpi/Documents/PFC/prototype/NPAAssist")))
          (protocolPath (if (!= protocol "")
                            (concat guiVar "/examples/" protocol "/" protocol ".maude")
                            ""))
          (protocolFolder (if (!= protocol "")
                              (concat guiVar "/examples/" protocol)
                            ""))
         )
   (seq
 
     (if (!= protocolPath "")
         (invoke acceptButton "setEnabled" (boolean true)))
     (setAttr kbm "protocol"  protocol)
     (setAttr kbm "protocolPath" protocolPath)
     (setAttr kbm "protocolFolder" protocolFolder)
     (invoke frame "repaint")
     kbm
   )
   )
)
)


;end setSelectedProtocol

;) ;end 

;;;;;;;;;; ./makeg2dlib loaded ../GUI/npa-manager.lsp ;;;;;;;;;;
;(seq

 
  
; (apply displayMessage %title %message)
(define displayWMessage (title msg)
   ; show message in warning dialog:
   (sinvoke "javax.swing.JOptionPane" "showMessageDialog" 
	    (object null)
	    msg
	    title
	    javax.swing.JOptionPane.WARNING_MESSAGE) 
)
;

;; makes an action that can be used to add a button to the toolbar
;; label is the button label
;; tip is the tooltip text that shows up if you mouse over the button
;; list is the selectable list of KBs being managede
;; cmd is part of the message to be sent to maude

(define mkKBAction (label tip list cmd)
 ( try
    (let ((closure
           (lambda (self event)
                (let ((selection (invoke list "getSelectedValue")))
                  (if (= selection (object null))
                      (invoke java.lang.System.err "println" 
                           (concat "Manager no protocol selected for " cmd)) 
                      (sinvoke "g2d.util.ActorMsg" 
                               "send" "maude" selection cmd)))))
         )
    (object ("g2d.closure.ClosureAbstractAction"
             label
				 (object null) ; icon
				 tip
             (object null) ; accelerator
				 (object null) ; mnemonic
				 closure     ; action closure
            ))
    )
  (catch Exception
      (invoke java.lang.System.err "println" (concat "Error in mkKBAction " (invoke Exception "getMessage")))
  )
) ;end try
) ;mkKBAction


;; title is the window title
;; kbm is the manager object (basically an a-list)
;; attacks is an array of strings -- attack names
(define initNPAwdo (title kbm protocol attacks)
  (try
    (let(  (frame (object ("g2d.graphviewer.ManagerFrame" protocol )))
           (attackList (object ("javax.swing.JList" attacks)))
           (toolbar (object ("javax.swing.JToolBar"  
                          javax.swing.JToolBar.VERTICAL)))
           (text (apply makeTextArea "Select an attack to start the analysis" (int 50) (int 20) black
                        "Times" (int 1) (int 13) greySilver (int 3)))
           (acceptButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                       "Accept"
                                                       "Execute tool"
                                                       (apply selectedAttackAction kbm attackList protocol)))))
                            (seq
                              (invoke temp "setSize" (int 60) (int 15))
                              temp
                            )))
         )
      (seq
       (setAttr kbm "frame" frame)
       (invoke frame "setSize" (int 250) (int 200))
       (invoke attackList "setFixedCellHeight" (int 20))

       (invoke toolbar "addSeparator")
       (invoke toolbar "add" acceptButton (int -1))
       (invoke toolbar "setFloatable" (boolean false))

       (invoke frame  "add" text java.awt.BorderLayout.NORTH)
       (invoke frame  "add" toolbar java.awt.BorderLayout.EAST)
       (invoke frame  "add" attackList java.awt.BorderLayout.CENTER)

       (setAttr kbm "npaframe" frame)

       (if (and (invoke attackList "isSelectionEmpty")
                (> (lookup attacks "length") (int 0))) 
            (invoke attackList "setSelectedIndex" (int 0))
       ) ;end if

       ;hide the dialog window that that is showed while a protocol's grammar is
       ;being generated or restored and while the search space tree is initialized.
       (invoke iniDialog "setVisible" (boolean false))
       (invoke frame "setVisible" (boolean true))
     ) ;end seq
        

    ) ;end let
       (catch Exception
	      (invoke java.lang.System.out "println" (concat "Error in initNPAWdo: " (invoke Exception "getMessage")))
       )
  ) ; end try
) ; end initNPAwdo



(define selectedAttackAction (kbm attackList protocol)
 (lambda (self event)
   (try
       (let ( (aname (object ("java.lang.String" (invoke attackList "getSelectedValue"))))
             )
         (seq
            (if (and (!= aname "")
                     (!= aname (object null)))
                (seq
                  (setAttr kbm "aname" aname)
                   (sinvoke "g2d.util.ActorMsg" 
                  "send" "maude" protocol (concat "initAttack " aname))
                )
            ) ; end if
         ) ;end seq
       ) ;end let
       (catch Exception
	      (invoke java.lang.System.err "println" (concat "Error in selectedAttackAction: "
                                                      (invoke Exception "getMessage")))
       ) ;end catch
   ) ;end try
 )
) ; end selectedAttackAction





;; a manager object that can be accessed by its unique name
;; it has attributes that store things we want to access,
;; like the list of kbs (eventually protocols, ...)
(define defNPAManager ( protocol attacks)
(seq 
  (let ( (name  "NPAManager")
        (kbm0 (fetch name))
        (kbm (if (= kbm0 (object null))  ; **** don't create if exists
                  (object ("g2d.glyph.Attributable"))
                  kbm0))
     )
    (seq
      (if (= kbm0 (object null))
          (invoke kbm "setUID" name))

      (setAttr kbm "nExtStates" (int 0)) ; counter of the number of external States that are being shown
      (apply initNPAwdo "NPA Protocol Manager" kbm protocol attacks)

    )
)) )


;) ;seq





;;;;;;;;;; ./makeg2dlib loaded ../GUI/showMnpaState.lsp ;;;;;;;;;;
;(seq ;;;; beginning of showMnpaState.lsp

;* Input
;- kmb: npa manager attributable object
;- pframe: frame of the initial window of the GUI
;* Output
;- a new small window will be shown to the user, to allow him or her to load
;  a text file with the specification of a Maude-NPA state.
(define loadExternalStateAction (kbm pframe)
  (lambda (self event)
      (let (  (frame (let( (temp (object ("g2d.swing.IOPFrame" "Load the specification of a Maude-NPA state")))
                                )
                  (seq
                    (invoke temp "setResizable" (boolean false))
                    temp
                  )))
             (panel (object ("javax.swing.JPanel" )))
             (auxPanel1 (object ("javax.swing.JPanel" )))
             (auxPanel2 (object ("javax.swing.JPanel" )))

             (layout (object ("javax.swing.BoxLayout" panel javax.swing.BoxLayout.PAGE_AXIS )))

              ;elements to load the specification file of a Maude-NPA state
             (loadExternalStateText (apply  makeTextArea loadExternalStateStr (int 520) (int 45) regularColor
                               "Arial" (int 0) (int 14) greySilver (int 15)))
             (stateSpecificationFilePath (let ( (temp (object ("javax.swing.JTextField" (int 35))))
                            )
                            (seq
                              (invoke temp "setEditable" (boolean false))
                              temp
                            )))
             ;; generalButtons
             (acceptButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                       "Accept"
                                                       "Load a Maude-NPA state specification"
                                                         (apply  loadStateSpecificationAction kbm frame)))))  
                                (seq
                                  (invoke temp "setSize" (int 60) (int 15))
                                  (invoke temp "setEnabled" (boolean false))
                                  temp )  ))

             (cancelButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                       "Cancel"
                                                       "Cancel action"
                                                       (apply cancelAction  frame))))) 
                                (seq
                                  (invoke temp "setSize" (int 60) (int 15)) temp  ) ))
                         ;a button to open a file dialog an select the grammar file the user wants to restore
            (loadStateSpecificationFileButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                       "Browse..."
                                                       "Load the specification file of a Maude-NPA state"
                                                       (apply loadStateSpecificationFileButtonAction kbm frame stateSpecificationFilePath acceptButton))))) 
                                (seq
                                  (invoke temp "setSize" (int 60) (int 15))  temp ) ))
            )
         (seq
          (invoke panel "setLayout" layout)
          (invoke stateSpecificationFilePath "setMaximumSize" (object ("java.awt.Dimension"  (int 250)   (int 25))))

          (invoke auxPanel1 "setBorder" (sinvoke "javax.swing.BorderFactory" "createEmptyBorder" (int 0) (int 15) (int 0) (int 15)))
          (invoke auxPanel1 "setLayout" (object ("javax.swing.BoxLayout" auxPanel1 javax.swing.BoxLayout.X_AXIS )))
          (invoke auxPanel1 "add" stateSpecificationFilePath)
          (invoke auxPanel1 "add" (sinvoke "javax.swing.Box"  "createRigidArea" (object ("java.awt.Dimension" (int 20)  (int 25)))))
          (invoke auxPanel1 "add" loadStateSpecificationFileButton)
          (invoke auxPanel1 "setMaximumSize" (object ("java.awt.Dimension" (int 500)  (int 35)) ))
          (invoke auxPanel1 "setAlignmentX" java.awt.Component.LEFT_ALIGNMENT)

           ;Accept and cancel buttons.
          (invoke auxPanel2 "setBorder" (sinvoke "javax.swing.BorderFactory" "createEmptyBorder" (int 10) (int 0) (int 0) (int 10)))
          (invoke auxPanel2 "setLayout" (object ("javax.swing.BoxLayout" auxPanel2 javax.swing.BoxLayout.X_AXIS )))
          (invoke auxPanel2 "add" (sinvoke "javax.swing.Box"  "createHorizontalGlue" ))
          (invoke auxPanel2 "add"  cancelButton)
          (invoke auxPanel2 "add" (sinvoke "javax.swing.Box"  "createRigidArea" (object ("java.awt.Dimension" (int 10)  (int 25)))))
          (invoke auxPanel2 "add" acceptButton)  
          (invoke auxPanel2"setMaximumSize" (object ("java.awt.Dimension" (int 535)  (int 55)) ))
          (invoke auxPanel2 "setAlignmentX" java.awt.Component.LEFT_ALIGNMENT)

          ;add auxiliar panels
          (invoke panel "add" loadExternalStateText)
          (invoke panel "add" auxPanel1)
          (invoke panel "add" auxPanel2)

          (invoke frame "add" panel)
          (invoke frame "setSize" (int 420) (int 190))
          (invoke frame "setVisible" (boolean true))
          
           
         )
       )
   )
) ;end loadExternalStateaction

;(apply loadStateSpecificationFileButtonAction kbm frame stateSpecificationFilePath acceptButton)))))
 ; metodo que se ejecuta cuando se hace click sobre el botón para seleccionar el fichero que contiene la especificación del estado
; This method sets the attribute "stateFilePath" of the kbm object with the path of
; the file selected by the user. It also updates the text of the textBox element
; of the window with that value and enables the button "Accept"
;* Input
;- kbm: npa manager attributable object
;- frame: frame of the window where the user loaded the file 
;- textField: textField element that shows the path of the file selected by the user
;- acceptButton: the "Accept" button of the window where the user loaded the file
;* Output
;- The updated kbm object. The Accept button is enabled and the text of the
;  the textField object is updated
(define loadStateSpecificationFileButtonAction (kbm frame textField acceptButton)
  (lambda (self event)
    (let ( (fileDialog (object("g2d.swing.IOPFileChooser")))
           (returnVal (invoke fileDialog "showOpenDialog" frame)) 
          )
      (if (= returnVal javax.swing.JFileChooser.APPROVE_OPTION)
           (let( (filePath (invoke   (invoke fileDialog "getSelectedFile") "getAbsolutePath")))
             (seq
                
                (setAttr kbm "stateFilePath" filePath)  
                (invoke textField "setText" filePath )
                (invoke acceptButton "setEnabled" (boolean true))
                (invoke frame "repaint")
                kbm
              )
            )
         ) ;end if
    ) ;end let environment
  ) ;end lambda
) ;end loadStateSpecificationFileButtonAction


; Closure associated to the "Accept" button of the window that  allows the user to load
; the file that contains the specification of a Maude-NPA state.
;* Input
;- kbm: npa manager attributable object
;- frame: frame of the window where the user loaded the file
;* Output/Result/Effect
;- A new window will be shown to the user, to allow him or her to visualize either
;  the graphical or the pretty-textual representation of the state
(define loadStateSpecificationAction (kbm frame)
 (lambda (self event)
   (let( (stateFilePath (getAttr kbm "stateFilePath"))
       ) ;end let bindings
     (seq
       (if (and (!= stateFilePath (object null))
                (!= stateFilePath ""))
           (seq
             ;open the new window that allows the user to visualize the graphical
             ; representation of the state
             (apply openStatePanel kbm frame)
           )
           ;else show alert message: no specification file was loaded
           (apply displayWInfoMessage "Error"
                  "Please, select a file that contains the specification\n of a Maude-NPA state"
                  javax.swing.JOptionPane.ERROR_MESSAGE)
       ) ;end if
     ) ;end seq
   ) ;end let environment
 ) ;end lambda
) ;end loadStateSpecificationAction


; This method creates and shows the new window that allows the user to visualize the graphical
; representation of the state
;* Input
;- kbm: npa manager attributable object
;- previousFrame: frame of the window where the user loaded the file
;* Output/Result/Effect
;- A new window will be openned. This windo will allow the user
;  to visualize either the graphical or the pretty-textual representations
;  of the Maude-NPA state selected by the user.
(define openStatePanel(kbm previousFrame)  
  (try
     (let ( (stateFilePath (getAttr kbm "stateFilePath"))
            (frame (object ("g2d.mwa.MWAFrame" "Show Maude-NPA state")))
            (panel (object ("javax.swing.JPanel" )))
            (emptyPanel (object ("javax.swing.JPanel" )))
            (emptySPanel (object ("javax.swing.JPanel" )))

            (treePanel (object ("javax.swing.JPanel" )))

            (textArea (apply  makeTextArea showStateStr (int 520) (int 50)
                              regularColor "Arial" (int 0) (int 14) greySilver (int 15)))
            (layout (object ("javax.swing.BoxLayout" panel javax.swing.BoxLayout.PAGE_AXIS )))
            ; button to load the textual representation of the Maude-NPA state
            (textualButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                 "Show Textual Representation"
                                                 "Show Textual Representation of the Maude-NPA state"
                                                  (apply  extStateTextualRepresentation kbm frame))))) 
                                (seq
                                  (invoke temp "setSize" (int 210) (int 15))
                                  temp)  ))
            ; button to load the textual representation of the Maude-NPA state
            (graphicalButton (let ( (temp (object ("g2d.swing.IOPButton" "Show Graphical Representation" "Show Graphical Representation of the Maude-NPA state"
                                                  (apply  extStateGraphicalRepresentation kbm frame))))) 
                                (seq
                                  (invoke temp "setSize" (int 210) (int 15))
                                  temp)  ))
            ;The cancel button will close the current window.
            (cancelButton (let ( (temp (object ("g2d.swing.IOPButton"
                                                       "Cancel"
                                                       "Cancel execution"
                                                       (apply cancelAction frame)))))
                                (seq
                                  (invoke temp "setSize" (int 80) (int 15)) temp
                                ) ))
            ;; frame tree manager
            (tree  (sinvoke "g2d.mwa.MWAControl" "getTree"))
            (scrollPane (object ("javax.swing.JScrollPane" tree)))
            (scrollDim (object ("java.awt.Dimension" (int 130) (int 150))))
            (treeMinDim (object ("java.awt.Dimension" (int 200) (int 300))))
            (treeMaxDim (object ("java.awt.Dimension" (int 210) (int 310))))
           ) ;end let bindings
       (seq
         (invoke emptyPanel "setPreferredSize" (object ("java.awt.Dimension" (int 20) (int 40))))
         (invoke emptySPanel "setPreferredSize" (object ("java.awt.Dimension" (int 410) (int 20))))
         (invoke treePanel "setPreferredSize" (object ("java.awt.Dimension" (int 80) (int 40))))
         
         
         (invoke panel "setMaximumSize" (object ("java.awt.Dimension" (int 225) (int 40))))
         (invoke panel "setPreferredSize" (object ("java.awt.Dimension" (int 225) (int 40))))
         (invoke panel "setLayout" layout)

         (invoke frame "add" textArea java.awt.BorderLayout.NORTH)
         (invoke frame "add" scrollPane java.awt.BorderLayout.CENTER)
         (invoke frame "add" emptyPanel java.awt.BorderLayout.WEST)
         (invoke panel "add" textualButton)
         (invoke panel "add" graphicalButton)
         (invoke panel "add" cancelButton)
         (invoke frame "add" panel java.awt.BorderLayout.EAST)  
         (invoke frame "add" emptySPanel java.awt.BorderLayout.SOUTH)
     
         (invoke frame "setSize" (int 510) (int 250))
         (invoke frame "repaint")
         (invoke frame "setVisible" (boolean true))
     
         ;close the frame where the user loaded the file with the state specification
         (invoke previousFrame "setVisible" (boolean false))
         (invoke previousFrame "dispose")
         
       ) ;end seq
     ) ;end let environment
     (catch Exception
         (apply displayWMessage "Error"
                                (concat "The Maude-NPA state could not be displayed.\nError: "
                                         (invoke Exception "getMessage")))
     ) ;end catch
  ) ;end try catch
) ;end openStatePanel


;******** Common methods shared between graphical and textual representations ****
;********************************************************************************* 

; method to create a NPANode to store the state info of a external Maude-NPA state
; this is a simplified version of the newNPANode method that creates and IOPNode
; object with the necessary information to be then passed as an argument
; to the functions that show the graphical and the pretty-textual representations
; of a Maude-NPA state
;* Input
;- kbm: npa manager attributable object
;* Output
;- An IOPNode object that is not associated to any graph and to any
;  IOPEdge object. 
(define newNPANodeExtState (kbm)
 (try

  (let(  ; path to the file that contains the specification of the Maude-NPA state
         (filePath (getAttr kbm "stateFilePath"))
         ; content of the file
         (content (apply readExtStateFile filePath))
         ; label of the state, obtained from the file
         (label (apply getLabelFromState content))
         ; state information of the state
         (stateInfo (apply getExtStateInfo content))
         (nid (getAttr kbm "nExtStates" (int 0)))
         (node (object ("g2d.graph.IOPNode" (concat "" nid) label 
                                "ellipse" nodeBorderColor regularFillColor)))
       ) ;end let bindings
    (seq

      (setAttr node "nid" nid)
      (setAttr node "label" label)
      (setAttr node "state" stateInfo)

      ;update the number of external states that have been shown
      ; this is necessary because of the framesList object, that keeps
      ; track of the frames with the textual and graphical representations
      ; of maude-npa states that are openned at each moment
      (setAttr kbm "nExtStates" (+ nid (int 1)))

      (if (= (apply isInitialState node) (boolean true))
          (setAttr node "initialState" (boolean true))
          (setAttr node "initialState" (boolean false))
      ) ;end if

      node
    ) ;end seq
  ) ;end let environment
    (catch Exception
      (invoke java.lang.System.err "println" (concat "Error in newNPANodeExtState: " (invoke Exception "getMessage")))
    )
   
  ) ;end try
) ;end newNPANodeExtState


; This method obtains the label of a state from the specification
; of the Maude-NPA state
;* Input
;- stateSpec: string with the specification of the Maude-NPA state
;* Output
;- a string with the label of the Maude-NPA state (ex. "< 1 . 4 . 4 >")
(define getLabelFromState (stateSpec)
 (try
  (let ( (iniPos (invoke stateSpec "indexOf" "<"))
         (endPos (+ (invoke stateSpec "indexOf" ">") (int 1)))
        ) ;end let bindings
        (invoke stateSpec "substring" iniPos endPos)
  ) ;end let environment
    (catch Exception
    (seq
      (invoke java.lang.System.err "println" (concat "Error in getLabelFromState: " (invoke Exception "getMessage")))
      ""
    )
  )
 ) ;end try
) ;end getLabelFromState

 
;(apply getExtStateInfo filePath)) ;(apply readExtStateFile filePath))
; Method to obtain only the state information from the specification
; of the Maude-NPA state
;* Input
;- stateSpec: string with the specification of the Maude-NPA state
;* Output
;- string with the state information (strands || intruder knowledge || sequence of messages || additional information)
(define getExtStateInfo (stateSpec)
  (try
 
  (let ( (content1 (invoke stateSpec "replace" "> (" ">"))
         (content2 (invoke content1 "replace" "||" " || "))
         (iniPos (+ (invoke content2 "indexOf" ">") (int 1)))
         (stateInfo  (invoke  content2 "substring" iniPos) )
         (stateInfo2 (apply quitInitialBlank stateInfo ))
 
        ) ;end let bindings
    (seq
       stateInfo2
    ) ;end seq
  ) ;end let environment
    (catch Exception
    (seq
      (invoke java.lang.System.err "println" (concat "Error in getExtStateInfo: " (invoke Exception "getMessage")))
      ""
    )
  )
    
 ) ;end try
) ;end getExtStateInfo


; Method to read the specification of a Maude-NPA state from a
; text file.
;* Input:
;- filePath: path to the file taht contains the specification
;* Output
;- String with the content of the file
(define readExtStateFile (filePath)
 (try
  (let( (file (object ("java.io.File" filePath)))
         (freader (object ("java.io.FileReader" file)))
         (breader (object ("java.io.BufferedReader" freader)))

         (strBuf (object ("java.lang.StringBuffer"))) ;hace luego un toString
       ) ;end let bindings
    (seq
       (apply readExtStateFileRec breader strBuf)
       (invoke breader "close")
       (invoke freader "close")
       (invoke strBuf "toString")
    ) ;end seq
  ) ;end let environments
  (catch Exception
    (seq
      (invoke java.lang.System.err "println" (concat "Error in readExtStateFile: " (invoke Exception "getMessage")))
      ""
    )
  )
 ) ;end try
) ; end readExtStateFile

; Recursive method to read a new line of the file that contains the specification
; of a Maude-NPA state
;* Input
;- breader: bufferedReader object to read from the file selected by the user
;- strBuf: stringBuffer object that stores the content read from the file
;* Output
;- if the file has not been read completely, append the new read line to the
;  string buffer object and continue reading. Once the file has been completely read,
;  return the content of the string buffer
(define readExtStateFileRec (breader strBuf)
 (try
  (let ( (line (invoke breader "readLine"))
        )
    (seq
       (if (and (!= line (object null))
                (!= line ""))
           (seq
               (invoke strBuf "append" line)
               (apply readExtStateFileRec breader strBuf)
           )
           strBuf
       ) ;end if
    ) ;end se
  ) ;end let bindings
   (catch Exception
    (seq
      (invoke java.lang.System.err "println" (concat "Error in readExtStateFileRec: " (invoke Exception "getMessage")))
      ""
    )
  )
 ) ;end try
) ;end readExtStateFileRec



;***************************************************************
;*                 TEXTUAL REPRESENTATION                      *
;***************************************************************

; Closure that is executed when the user selects the button to show the textual
; representation of the loaded state specification
;* Input
;- kbm: maude-npa manager attributable object
;- frame: frame in which the user can choose to view either the graphical or the textual
;         representation of the state he or she has loaded
;* Output
;- A new window will be openned to show the user the pretty-textual representation
;  of the state he or she has loaded
(define extStateTextualRepresentation (kbm frame)
 (lambda (self e)
   (let(  
         (filePath (getAttr kbm "stateFilePath")) 
         (node (apply newNPANodeExtState kbm))  
        ) ;end let bindings
       (apply  openInfoFrameExtState frame node "State Information of external Maude-NPA state") ;check if it works, since it is a lambda function.
   ) ;end let environment
 ) ;end lambda
) ;end extStateTextualRepresentation


; This  method creates and shows the window that displays the pretty-textual representation
;  of the state loaded by the user
;* Input
;- frame: frame in which the user can choose to view either the graphical or the textual
;         representation of the state he or she has loaded
;- node: IOPNode object whose state information will be shown
;- title: part of the title of the window that will show the textual representation of this state.
;         This part will contain the label of the state
;* Ouput
;- The  window that displays the pretty-textual representation
;  of the state loaded by the user will be created and shown
(define openInfoFrameExtState (frame node title)
      (try
        (seq
        ; if framesList doesn't contain a FLItem that represents the node, create it.
         (if (= (invoke framesList "containsKey" (getAttr node "label")) (boolean false))
              (apply addInitializedFramesListItem  node frame) ;add the entry frame
         )
         ; if the node hasn't got any information frame associated, create the frame
         ; and store it as the infoFrame attribute of the FLItem that corresponds to this node.
         (if (= (apply FLItemHasInfoFrame node) (boolean false))
             (apply addInfoFrameToFLItem node (apply getNPANodeInfoFrame frame node title))
         )

         (apply showFLItemInfoFrame node) ; show the information frame
        ) ;end seq
        (catch Exception
          (apply displayWMessage "Error" (concat
                                            (concat  (concat  (concat "Error: the textual information of node " (getAttr node "nid" ""))
                                                          " of the graph \n")
                                                     (concat  title " could not be displayed.\n"))
                                            (invoke Exception "getMessage")))
        ) ;end catch
     ) ;end try
) ;end  openInfoFrameExtState




;***************************************************************
;*                  GRAPHICAL REPRESENTATION                   *
;***************************************************************

; Closure that is executed when the user selects the button to show the graphical
; representation of the loaded state specification
;* Input
;- kbm: maude-npa manager attributable object
;- frame: frame in which the user can choose to view either the graphical or the textual
;         representation of the state he or she has loaded
;* Output
;- A new window will be openned to show to the user the graphical representation
;  of the state he or she has loaded
(define extStateGraphicalRepresentation (kbm frame)
 (lambda (self e)
   (try
    (let( (text "En extStateGraphical")
         (filePath (getAttr kbm "stateFilePath"))
         (node (apply newNPANodeExtState  kbm)) 
        ) ;end let bindings

       (apply showStateStrands frame node "External state")
    ) ;end let environment
    (catch Exception
        (invoke java.lang.System.err "println" (concat "Error in extStateGraphicalRepresentation: " (invoke Exception "toString")))
    )
   ) ;end try
 ) ;end lambda
) ;end extStateGraphicalRepresentation


;;;;;;;;;; ./makeg2dlib loaded ../GUI/npa-showInfo.lsp ;;;;;;;;;;
;(seq        ;*********** DEBUG settings *********; define verbosity; (supdate "g2d.util.ActorMsg" "VERBOSE" (boolean true))(supdate "g2d.runtime.ExceptionHandler" "SHUTDOWN_AFTER_ERROR" (boolean false))(sinvoke "g2d.jlambda.Debugger" "setVerbosity" (boolean true));;; *********** exception handling *********; define exception handler(sinvoke "g2d.jlambda.Debugger" "setHandler" (object ("g2d.runtime.ExceptionHandler"))); add shutdown hook to send shutdown command to IOP(invoke (sinvoke "java.lang.Runtime" "getRuntime")         "addShutdownHook"         (object ("g2d.runtime.ShutdownHook" "shutdown_hook")))         (define branchClosure (tname aname protocol rootNode?)           (lambda (self event)             (apply createMtTree tname aname protocol rootNode?)            )         ) ; creates the popupmenu and add the menu options;*inputs:;-frame: parent frame in which the popup menu will be displayed;-graph: graph whose information is going to be shown;-node: node which the user clicked the mouse over;-event: mouse event itself;*outputs;- the popup menu is added to the frame and displayed(define showNPAPopup (frame graph node event)  ( let (         (nid (getAttr node "nid"))         (name  "NPAManager")         (kbm (fetch name))         ;;         (graphTitle (getAttr graph "title"))                  ;popup menu object and its options         (popup (object ("javax.swing.JPopupMenu")))         ;(mitem1 (object ("javax.swing.JMenuItem" "Edit node properties")))         ;(mitem2 (object ("javax.swing.JMenuItem" "Get only this node's branch")))         (mitem7 (object ("javax.swing.JMenuItem" "Strands visualization")))         (mitem8 (object ("javax.swing.JMenuItem" "View state information")))         ;popup menu's location         (gp (invoke frame "getGraphPanel"))         (vp (invoke gp "getParent"))         (loc (invoke event "getPoint"))         (loc1 (invoke vp "getViewPosition"))       ;;; only works if in scroll pane -- i.e. graph is big         (ourX (- (lookup loc "x")(lookup loc1 "x")))         (ourY (lookup loc "y"))       )    (seq      ;add menuItems and listener      (if (!= nid "0") ; these menu options are not necesary for the root node        (seq          (invoke mitem7 "addActionListener"                  (object ("g2d.closure.ClosureActionListener" (apply mkNPAShowStrandsClosure frame node graphTitle))))          (invoke mitem8 "addActionListener"                  (object ("g2d.closure.ClosureActionListener" (apply showNPANodeInfoClosure frame node graphTitle))))                    ;(invoke popup "add" mitem2)          (invoke popup "add" mitem7)          (invoke popup "add" mitem8)         )      ) ;end if      (invoke popup "show" frame ourX (+ ourY (int 100))) ;show the popup menu at the desired position      )    )  ); closure for the "View node contents" menu's option (node's color, shape, size...);*inputs:;-frame: parent frame in which the Node dialog will be displayed;-node: node which the user clicked the mouse over(define mkNPANodeDialogClosure (frame node)   (lambda (self e)        (apply doNPANodeDialog frame node)        )) ; end mkNPANodeDialogClosure; nodeDialog is a popup with the node's properties. the user can change these properties, ; but they only affect the visual part, i.e, if I change a node's label, the frame shows this; new label, but, internally, the nodes continues with the old label, which is necessary for ; the showBranch function;*inputs:;-frame: parent frame in which the Node dialog will be displayed;-node: node which the user clicked the mouse over(define doNPANodeDialog (frame node)    (let (            (nodeDialog (object ( "g2d.graph.NodeDialog" frame node)))     )          (seq         (invoke nodeDialog "setTitle" (concat "Edit node properties (id " (getAttr node "nid") " )" ))         (invoke nodeDialog "setContents")         (invoke nodeDialog "setSize" (int 400) (int 250))         (invoke nodeDialog "setVisible" (boolean true))     ) )) ; end doNPANodeDialog;;;;;; closure for the "See only the branch...." button;*inputs;-graph: graph whose information is going to be shown;-node: node which the user clicked the mouse over;*output;-another window is created to show the tree's branch which the node belongs to(define mkNPABranch (graph node kbm)   (lambda (self e)        (apply doNPABranch graph node kbm)    )) ; end mkNPABranch(define doNPABranch (graph node kbm)    (let (               (npaIdStr (apply arr2str (getAttr node "npaId")))            (gname (invoke graph "getUID"))     )          (seq         ; build the branch and show it in another window         (apply getBranch gname (getAttr  node "nid" ) kbm) ; this method is defined in the launcher.lsp file         (apply showNPAGraph (concat gname (getAttr  node "nid")) (object null) (concat "Branch of node " (getAttr  node "nid")) " "  toolBarFunNPA )                  (sinvoke "g2d.util.ActorMsg" "send" "maude" gname (concat "showBranchNode "  npaIdStr))     ) )) ; end mkNPABranch;;;;;; closure for the options related with the node's state information;*inputs:;-frame: parent frame in which the Node dialog will be displayed;-node: node which the user clicked the mouse over;-action: option selected by the user: view current strands, intruder's knowledge, sequences of messages;         or additional data in string format, to set the frame's title;-optionNumber: number corresponding to the action selected;*outputs:;-show a new dialog showing only the selected information of the node(define mkNPANodeInfoClosure (frame node action optionNumber)   (lambda (self e)    (let (          (text (invoke (apply getInfoState node optionNumber) "replace" "\n" "")) ; gets the required node's information, depending on the action          (fText (apply formatText text optionNumber)) ; format the text to show it in a nicer way.          (nodeId (getAttr node "nid"))          (newF (object ("g2d.swing.IOPFrame" (concat action " of node " nodeId))))          (areaT (object ("javax.swing.JEditorPane" "text/plain" fText )))          (scroll (object ("javax.swing.JScrollPane" areaT)))          (margin (object ("java.awt.Insets" (int 25) (int 25) (int 25) (int 25))))                  )      (seq         (invoke areaT "setMargin" margin)         (invoke newF "add" scroll)         (invoke newF "setLocation" (int 150) (int 150))         (invoke newF "setSize" (int 650) (int 500))         (invoke newF "setVisible" (boolean true))       )    )    )) ; end mkNPABranch; gets the required node's information, depending on the action;*inputs;-node: node which the user clicked the mouse over;-optionNumber: number corresponding to the action selected;*output;-extracts the information selected by the user from the whole node's data(define getInfoState (node optionNumber)  ( let(        (nodeState  (getAttr  node "state") )        )    (seq       (try            ( do ( ;Maude-NPA separates the different data in the output with the characters "||"                   ;so we look for this characters  as many times as the optionNumber value                   ;the easiest way to do this would be with the String.split method, but for some                   ;reason it didn't work well.                (i (int 0) (+ i (int 1)))                (newState nodeState (invoke newState "substring" (+ (invoke newState "indexOf" "||") (int 3))))              )                ((= i optionNumber)                 (try (invoke newState "substring" (int 0)                              (invoke newState "indexOf"  "||") )                 (catch Exception newState) )) ;if optionNumber=3 newState.indexOf("||")=-1 because is the last fragment and there aren't more                                               ;  "||" so an exception is thrown. Anyway, we want the newState value so the function returns it.              (invoke newState "substring" (int 0) (invoke newState "indexOf"  "||") )            ) ; end do            (catch Exception newState )       ) ; end try     ) ;end seq      )) ; end getInfoState;this method gives format to a text which corresponds to a node's information a user wants to view;*inputs:;-text: source text without any kind of format.;-action: number corresponding to the action selected by the user;*output;-the text with each element in a different line (this is the current format we give).(define  formatText (text action)  (seq      ; depending on the action,the format is given in a different way      (if (or (= action (int 0) )  (= action (int 3) ))          (apply formatTextCurrentStrands text)          (apply formatTextGeneral text)      )    ) ;end seq ) ; end formatText;method to format current strands and auxiliary data text;*-inputs;-text: source text without any kind of format.;*outputs;-the input text is splitted in different lines using the character "&" as a separtaor(define formatTextCurrentStrands (text)  (let (        (strandsArray (invoke text "split" "&")) ;this is the separator character that Maude-NPA uses        (strb (object ("java.lang.StringBuffer")))        )    (seq         (for item strandsArray              (seq (invoke strb "append" item)                   ;the split function removes the character "&" so it must be added again together with a break line character                   (invoke strb "append" (object ("java.lang.String" (concat "&" "\n"))) ))) ;end for                                          ; in the last for iteration, a "&" character has been added, but it is not needed, so it has to be deleted.        (invoke  (invoke strb "toString") "substring" (int 0)  (invoke  (invoke strb "toString") "lastIndexOf" (char '&') ))     )  ) ) ; end formatText;method to format intruder knowledge and sequence of messages text;*inputs;-text: is the whole text without any type of format;*output;-text with each element in a different line(define formatTextGeneral (text) ( apply formatTextGeneralRec  (int 0) (int 0)  (int 0) text "") ) ;end formatTextIntruderKnowledge; this  method splits the whole text into lines. A new new line will be returned when there are the same number of opened parentesis and closed parentesis;and the current character is a ',' (the first openned parenthesis has been matched by a closed parenthesis). When the "pos" value is equal or greater than the textSrc's length we return the empty string to indidate that;we have finished to process a line or that there are not more character in the textSrc string. In this last case, the do loop in;formatTextIntruderKnowledge method will finish.;*Inputs;-openPCounter: number of opened parentesis found in the current line;-closePCounter: number of opened parentesis found in the current line;-pos: current position of the textSrc that we are processing;-textSrc: string which contains the entire IntruderKnowdledge information without any kind of format.;-textShow: current formatted text that will be displayed later. In each iteration we add the current char we are processing(define formatTextGeneralRec (openPCounter closePCounter pos textSrc text2Show)(seq ( if (>= pos (invoke textSrc "length") ) ; finish       text2Show   ( let ( (currentChar (invoke textSrc "charAt" pos))          )    (if (and (= openPCounter closePCounter) (= currentChar (char ','))) ; return a new line         ( apply formatTextGeneralRec  (int 0) (int 0) (+ pos (int 1)) textSrc (concat text2Show currentChar "\n"))                (if (= currentChar (char '('))             ; update openned parentesis counter  and text2Show                       (apply formatTextGeneralRec (+ openPCounter (int 1)) closePCounter (+ pos (int 1)) textSrc (concat text2Show currentChar))             (if (= currentChar (char ')'))                 ; update closed parentesis counter  and text2Show                 (apply formatTextGeneralRec openPCounter (+ closePCounter (int 1))  (+ pos (int 1)) textSrc (concat text2Show currentChar))                 ; update only the current position and text2Show                 (apply formatTextGeneralRec openPCounter closePCounter (+ pos (int 1)) textSrc (concat text2Show currentChar))              )         )     )    )  )) ) ;end formatTextIntruderKnowledgeRec;)
;;;;;;;;;; ./makeg2dlib loaded ../GUI/npa-showInfoNew.lsp ;;;;;;;;;;
;(seq

     
; Closure that displays the textual information frame associated to a node, i.e.,
; the frame stored in the infoFrame attribute of the element of the framesList that 
; corresponds to this node. If the infoFrame is already active, show it. If not, initialize
; it and add it to the framesList element that corresponds to this node.
;* Input
;- graphFrame: frame in which the search space tree is displayed.
;- node: search space tree node whose information the user wants to see.
;* Output
; the frame in which the user can choose the information  he or she wants to view.
 
(define showNPANodeInfoClosure (graphFrame node graphTitle)
    (lambda (self e)
      (try
        (seq
        ; if framesList doesn't contain a FLItem that represents the node, create it.
         (if (= (invoke framesList "containsKey" (getAttr node "label")) (boolean false))
              (apply addInitializedFramesListItem  node graphFrame) ;add the entry frame
         )
         ; if the node hasn't got any information frame associated, create the frame
         ; and store it as the infoFrame attribute of the FLItem that corresponds to this node.
         (if (= (apply FLItemHasInfoFrame node) (boolean false))
             (apply addInfoFrameToFLItem node (apply getNPANodeInfoFrame graphFrame node graphTitle))
         )

         (apply showFLItemInfoFrame node) ; show the information frame
        ) ;end seq
        (catch Exception
          (apply displayWMessage "Error" (concat
                                            (concat  (concat  (concat "Error: the textual information of node " (getAttr node "nid" ""))
                                                          " of the graph \n")
                                                     (concat  graphTitle " could not be displayed.\n"))
                                            (invoke Exception "getMessage")))
        ) ;end catch
     ) ;end try
    )
) ;end showNPANodeInfoClosure


; This closure creates the information frame. Initially, it's empty and the user has to to choose
; which information he or she wants to see, by selecting some checkboxes. When a checkbox is selected,
; the corresponding information is showed. When it is not selected, the corresponding information is
; removed.
;* Input
;- graphFrame: frame in which the search space tree is displayed.
;- node: search space tree node whose information the user wants to see.
;* Output
; the information frame, without any selected checkbox.

 (define getNPANodeInfoFrame (graphFrame node graphTitle)
  (let(  (pframe (apply getPFrameFromFLItem node))
         (frame (object ("g2d.mwa.MWAFrame" (concat (concat graphTitle " - ")
                                                    (concat "Textual information of node " (getAttr node "label")))  pframe)))
         (toolbar (object ("g2d.toolbar.ToolBar")))
         ;There is a checkbox for each kind of state information.
         (checkBoxStrands (object ("g2d.toolbar.ToolCheckBox" "Show strands")))
         (checkBoxIntruderKnowledge (object ("g2d.toolbar.ToolCheckBox" "Show intruder knowledge")))
         (checkBoxMessages (object ("g2d.toolbar.ToolCheckBox" "Show sequence of messages")))
         (checkBoxAdditional (object ("g2d.toolbar.ToolCheckBox" "Show  additional information")))
         ;array that will contain the information associated to a state.Initially, it will be empty.
         ;Elements will be added when the user selects a checkbox. Current strands information will be
         ;stored in pos 0, intruder knowledge in pos 1, sequence of messages in pos 2 and additional
         ;information in pos 3.
         (contentArray (apply initializeContentArray)) 
         (panel (object ("javax.swing.JPanel")))
         ;element in which textual information will be displayed
         (areaText (object ("javax.swing.JEditorPane" "text/plain" "" )))
         (scroll (object ("javax.swing.JScrollPane" areaText
                                                   javax.swing.JScrollPane.VERTICAL_SCROLLBAR_ALWAYS
                                                   javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS )))
         (margin (object ("java.awt.Insets" (int 0) (int 25) (int 25) (int 25))))
         (explanation (apply makeTextArea
                   "Select the information you want to view. Then, you can also deselect the information you don't want to see."
                   (invoke frame "getWidth") (int 30) regularColor "Arial" (int 0) (int 12) greySilver (int 5)))

         (windowListener (object ("g2d.closure.ClosureWindowListener")))
         
         (menuBar  (invoke frame "getOrigJMenuBar"))
         (fileMenu (object ("javax.swing.JMenu" "File...")))

         (saveStateInfoItem (object ("javax.swing.JMenuItem" "Save...")))
         (saveStateInfoActionListener (object ("g2d.closure.ClosureActionListener" (apply saveStateInfoAction node contentArray frame))))
        )
     (seq
       ;menu
       (invoke saveStateInfoItem "addActionListener" saveStateInfoActionListener)
       (invoke fileMenu "add" saveStateInfoItem (int 0))
       (invoke menuBar "add" fileMenu)
       
       (invoke windowListener "setActionClosure" (int 201)  (apply closeWindowEvent node "infoFrame" ))
       (invoke frame "addWindowListener" windowListener)
       (invoke frame "setResizable" (boolean false))
       (invoke panel "setMaximumSize" (object ("java.awt.Dimension" (int 800) (int 420))))
       (invoke panel "setMinimumSize" (object ("java.awt.Dimension" (int 800) (int 420))))
       (invoke areaText "setPreferredSize" (object ("java.awt.Dimension" (int 780) (int 400))))
       (invoke areaText "setEditable" (boolean false))

       ;Show the node extra information by default **************
       (invoke areaText "setText" (apply writeNodeExtraInformation node))
       (invoke areaText "repaint")
       
       (invoke panel "add" explanation java.awt.BorderLayout.NORTH)
       (invoke panel "add" scroll java.awt.BorderLayout.SOUTH)
       
       (invoke checkBoxStrands "addActionListener"
               (object ("g2d.closure.ClosureActionListener"
                        (apply showInfoCheckboxClosure checkBoxStrands node areaText (int 0) currentStrandsTitle contentArray))))
       (invoke checkBoxIntruderKnowledge "addActionListener"
               (object ("g2d.closure.ClosureActionListener"
                        (apply showInfoCheckboxClosure checkBoxIntruderKnowledge node areaText (int 1) intruderKnowledgeTitle contentArray))))
       (invoke checkBoxMessages "addActionListener"
               (object ("g2d.closure.ClosureActionListener"
                        (apply showInfoCheckboxClosure checkBoxMessages node areaText (int 2) messagesTitle contentArray))))
       (invoke checkBoxAdditional "addActionListener"
               (object ("g2d.closure.ClosureActionListener"
                        (apply showInfoCheckboxClosure checkBoxAdditional node areaText (int 3) additionalInfoTitle contentArray))))
       
       (invoke toolbar "append" checkBoxStrands)
       (invoke toolbar "append" checkBoxIntruderKnowledge)
       (invoke toolbar "append" checkBoxMessages)
       (invoke toolbar "append" checkBoxAdditional)

       (invoke areaText "setMargin" margin)

       (invoke frame "setSize" (int 800) (int 520))

       (invoke frame "add" toolbar java.awt.BorderLayout.NORTH)
       (invoke frame "add" panel java.awt.BorderLayout.CENTER)
       frame
     )
    )
 ) ;end getNPANodeInfoFrame



(define saveStateInfoAction (node contentArray frame) ; node parameter is not necessary
 (lambda (self e)
  (let (
          (fileDialog (let ((temp (object("javax.swing.JFileChooser")))
                                    )
                            (seq
                              (invoke temp "setDialogTitle" "Choose the file to save the node's information")
                              temp
                            )))
          (returnVal (invoke fileDialog "showSaveDialog" frame))
          (guiVar (apply getEnvVariable "MNPA_GUI"))
          (exampleDirectory (concat guiVar "/examples"))
        )
    (seq                     
     (if (= returnVal javax.swing.JFileChooser.APPROVE_OPTION)
       (if (= (invoke (invoke   (invoke fileDialog "getSelectedFile") "getAbsolutePath") "startsWith" exampleDirectory)
              (boolean true))
           ;then
            (apply displayWInfoMessage "Forbidden"
                                         "You are not allowed to save files into this directory.\nPlease choose a different directory."
                                         javax.swing.JOptionPane.ERROR_MESSAGE)
           ; else
            (let( (filePath (invoke   (invoke fileDialog "getSelectedFile") "getAbsolutePath"))
                  (text (apply getTextFromContentArray contentArray))
               )
              (seq
                (sinvoke "g2d.util.IO" "string2File"
                     (concat "\n" (invoke frame "getTitle")
                             "\n**************************************************************************")
                     filePath)
                (sinvoke "g2d.util.IO" "string2File" (apply writeNodeExtraInformation node) filePath (boolean true))
                (sinvoke "g2d.util.IO" "string2File" text filePath (boolean true))
              ) ;end seq
            ) ;end else
       ) ;end if
     ) ;end if
   ) ;end seq
  )
 ) ;end lambda
) ;end saveStateInfoAction

; this closure updates the information of a node that must be shown when the user
; selects or deselects one of the checkboxes.
;* Input
;- checkbox: checkbox that the user selected or deselected.
;- node: node whose information is being displayed.
;- areaText: part of the frame in which the information is written.
;- optionNumber: which kind of information the user wants to view or hide
;          (0: current strands, 1: intruder knowledge, 2: sequence of messages, 3: additional information)
;- title: title associated to the information that will be showed if the user has selected a checkbox.
;- contentArray: array that contains the state information associated to a node (see initializeContentArray
;                for more information).
;* Output
; the visualization of the information is updated, adding or hidding contents.

(define showInfoCheckboxClosure (checkBox node areaText optionNumber title contentArray)
  (lambda (self e)
    (let ( ; element of contentArray that corresponds to the information that must be showed or
           ; hiden.
           (subArray (aget contentArray optionNumber)) )
     (seq
       (if (= (invoke checkBox "isSelected") (boolean false)) ;the checkbox is not selected. 
           ; visibility of this information is disabled.
           (aset subArray (int 0) (boolean false))
            
            (seq ;else enable its content 
               (aset subArray (int 0) (boolean true))  
               (if (= (aget subArray (int 1)) "")  ;and get the information if it has not been obtained before
                 (let (   (text (invoke (apply getInfoState node optionNumber) "replace" "\n" "")) ; gets the required node's information, depending on the action
                          (finalText (if (and (= optionNumber (int 0))
                                              (invoke (invoke text "replace" " " "") "endsWith" "])")
                                         )                               
                                         (invoke text "substring" (int 0) (- (invoke text "length") (int 2)))
                                         text
                                     ))
                          (fText (apply formatText finalText optionNumber)) ; format the text to show it in a nicer way.
                      )
                   (aset subArray (int 1) fText))
               ) ;end inner if 
            ) ;end seq (else part)
       ) ;end outner if
       ;update the information that must be displayed
       (apply writeContentArray contentArray areaText node))
     )
  ) ) ;end showInfoCheckboxClosure


; This closure creates and initializes an array of length 4 that will store a node's state information.
(define initializeContentArray ()
  (let ( (contentArray (mkarray java.lang.Object (int 4)))
        )
     (apply initializeContentArrayRec contentArray (int 0))
  )
) ;end initializeContentArray


; This closure initializes each element of the contentArray.
; Each element will be an array of length 2. The first position will indicate
; whether the information is visible or not, and the second will store
; the text to show when the corresponding checkbox has been selected.
;* Input
;- contentArray: array that stores a node's state information.
;- pos: current element of contentArray to initialize.
;* Output
; an initialized contentArray.

(define initializeContentArrayRec (contentArray pos)
  (if (> pos (int 3))
      contentArray
      (let ( (subArray (mkarray java.lang.Object (int 2))))
        (seq
           (aset subArray (int 0) (boolean false))
           (aset subArray (int 1) "")
           (aset contentArray pos subArray)
           (apply initializeContentArrayRec contentArray (+ pos (int 1)))
         )
       )
 )
) ;end initializeContentArrayRec


; Closure to write the information that must be showed to the user, i.e. the one
; that is enabled.
;* Input
;- contentArray: array that stores a node's state information.
;- areaText: graphical component in which the information has to be written.
;* Output
; updated visualization of the node's state information.

(define writeContentArray (contentArray areaText node)
  (let (  (infoState (apply writeNodeExtraInformation node))
          (text  (apply getTextFromContentArrayRec contentArray (int 0) infoState))
        )
    (seq
     (invoke areaText "setText" text)
     (invoke areaText "repaint")
    )
  )
) ;end writeContentArray


; This closure gets the text of the enabled information from the contentArray
;* Input
;- contentArray: array that stores a node's state information.
;* Output
; a string with the text of enabled information.

(define getTextFromContentArray (contentArray)
    (apply getTextFromContentArrayRec contentArray (int 0) (object ("java.lang.String" " ")))
) ;end getTextFromContentArray


; This closure concatenates to the text to show, the information of the current element of
; contentArray if this its corresponding information is enabled.
;*Input
;- contentArray: array that stores a node's state information.
;- pos: current element of contentArray that is being processed.
;- text: variable that stores and accumulates the information to display.
;*Output
; the entire text to display.

(define getTextFromContentArrayRec (contentArray pos text)
    (if (>= pos (lookup contentArray "length")) ;the closure has processed all the elements of contentArray
        text ;return the entire text.
        (let ( (subArray (aget contentArray pos)) ) ;get the current element
          (if (= (aget subArray (int 0)) (boolean true)) ; if the information is enabled, it should be displayed
           (let (   (title (aget titlesArray pos))
                    (subArrayText  (aget subArray (int 1)) )
                 )
             ;add to the accumulator variable, the title for the content to show, and the information.
               (apply getTextFromContentArrayRec contentArray (+ pos (int 1)) (concat text "\n" title subArrayText) )    
           )
           ;else, if the information if disabled, process next element.
            (apply getTextFromContentArrayRec contentArray (+ pos (int 1)) text)
         )
        ) 
    ) ;end if
) ;end getTextFromContentArrayRec
  




(define getPrettyInfo(node optionNumber)
  (let ( (text (invoke (apply getInfoState node (int 0)) "replace" "\n" ""))
         (finalText (if (and (= optionNumber (int 0))
                             (invoke (invoke text "replace" " " "") "endsWith" "])"))                             
                        (invoke text "substring" (int 0) (- (invoke text "length") (int 2)))
                        text
                    ))
       )
       (apply formatText finalText optionNumber)

  )
) ;end getPrettyInfo


;(apply writeNodeExtraInformation areaText node)
(define writeNodeExtraInformation (node)
 (let ( (strBuf (object ("java.lang.StringBuffer")))
       )
  (seq
      (if (= (getAttr node "initialState") (boolean true))
          (invoke strBuf "append" "\n** The state is an initial state\n")
          (invoke strBuf "append" "\n** The state is not an initial state\n")
      ) ;end if
     
      
      (if (= (getAttr node "unreachable") (boolean true))
           (invoke strBuf "append" "** The state is unreachable\n")
           (invoke strBuf "append" "** The state is not unreachable\n")
      ) ;end if
      
      (invoke strBuf "toString")
       ;text
  ) ;end seq
 ) ;en let environment
) ;end writeNodeExtraInformation

;;;;;;;;;; ./makeg2dlib loaded ../GUI/npa-graph.lsp ;;;;;;;;;;

;(seq
      
;;; *********** DEBUG settings *********

; define verbosity
; (supdate "g2d.util.ActorMsg" "VERBOSE" (boolean true))
(supdate "g2d.runtime.ExceptionHandler" "SHUTDOWN_AFTER_ERROR" (boolean false))
(sinvoke "g2d.jlambda.Debugger" "setVerbosity" (boolean true))

;;; *********** exception handling *********
; define exception handler
(sinvoke "g2d.jlambda.Debugger" "setHandler" (object ("g2d.runtime.ExceptionHandler")))

; add shutdown hook to send shutdown command to IOP
(invoke (sinvoke "java.lang.Runtime" "getRuntime") 
	"addShutdownHook" 
	(object ("g2d.runtime.ShutdownHook" "shutdown_hook")))




; build the color key 
(define npaColorKey ()
  (let (
	     (colorkey (object ("g2d.swing.IOPColorKey")))
	     (colors (array java.awt.Color 
             regularFillColor 
             initialFillColor
             unreachableFillColor
			    ))
	     (keys (array java.lang.String 
			  "Regular state" 
			  "Initial state"
                          "States without successor states"
			  ))
			)
	 (seq 
	  (invoke colorkey "add" colors keys)
     colorkey
    )
  )
) ;end npaColorKey



; this method creates a action closure
;*input
;-label: text for the action
;-tip: tip message
;-closure: closure which will be executed
;*output
;-the closureAbstractAction object

(define mkAction (label tip closure)
    (object ("g2d.closure.ClosureAbstractAction"
             label
	     (object null) ; icon
	     tip
             (object null) ; accelerator
	     (object null) ; mnemonic
	     closure     ; action closure
) ) )

  
                                          

; creates the node and adds it to the graph
;*input
;-graph: graph which the node belongs to
;-moudeClickedClosure: closure that will be executed when the user clicks on the node
;-nid: node's id
;-lab: node's label
;-state: node's state information
;-npaId: node's id in the Maude-NPA tool.
;-addToGraph?: boolean argument indicating whether the node must be added to the IOPGraph or
;    not because the current node is an initial state already added.
;*output
;- new created node, after being added to the graph

(define newNPANode (graph mouseClickedClosure nid lab state npaId addToGraph?) 
(try  
 (seq
  (let (  (node (object ("g2d.graph.IOPNode" nid lab 
                                "ellipse" nodeBorderColor regularFillColor)))
	(list  (getAttr graph "list"))
	(lastP (- (invoke list "size") (int 1))) ; last position of the graph list attribute
        (lastRow (invoke list "get" lastP)) ; list of the last level of nodes. It may be empty if this node is the first that is being added in this level
	                                     ; or it may contain more nodes
    )
    
    (seq 
     ;set node's attributes
     (setAttr node "nid" nid)
     (setAttr node "label" lab)
     (setAttr node "npaId" npaId)
     (setAttr node "state" state)
     ; initialize predessor node attribute
     ; it will be binded at the newNPAEdge
     (setAttr node "parent" (object null)) 
     (setAttr node "children" (object ("java.util.LinkedList"))) ; successor nodes

     (setAttr node "unreachable" (boolean false))

     ;; add the new node in the list of the last position of the  main list
     ;; i.e list[lastposition].add(node)
      (invoke lastRow "put" lab node)

   ;  (setAttr graph "list" list)


     (if (= addToGraph? (boolean true))
      (seq
       (invoke node "setNodeShape" "box")
       (invoke node "setBaseDimension" (double 0) (double 0))
       (invoke node "setMouseAction" java.awt.event.MouseEvent.MOUSE_CLICKED mouseClickedClosure)

       ;if the node represents an initial state, then it will be colored in a different color
       ;and it will be added to the graph's initial states list, if it still has not been added.
       ;
       (if (= (apply isInitialState node) (boolean true))
        (if (= (invoke (getAttr graph "initialStates") "contains" lab) (boolean false))
            (seq
               (invoke node "setFillColor" ltgreen)
               (setAttr node "initialState" (boolean true))
               (invoke (getAttr graph "initialStates") "add" lab)
               (setAttr graph "newInitialState" (boolean true))
               (invoke graph "addNode" node)
            )
        )
        (seq
            (setAttr node "initialState" (boolean false))
            (invoke graph "addNode" node) ; a regular node should be added to the graph too
        )
       ) ;end if
      ) ;end seq
     )  ;end if
   
   graph))   ;  node))
   )
 (catch Except
   (apply displayWInfoMessage
                  "Error"
                      (concat "An exception was raised while creating a node that corresponds to a Maude-NPA state:\n  "
                              (invoke Except "getMessage"))
		   javax.swing.JOptionPane.ERROR_MESSAGE)
 )
) ;end try  
)  ;end newNPANode


; This method adds repeated initial nodes to the internal data structure that stores all the
; states generated by the Maude-NPA. This action is necessary to include the
; initial states already found in the next levels after the first time they are generated
; when the user needs to save the information of a tree as a text file. Otherwise;
; an initial state would appeared only once, at the level where is was generated for
; the first time.
; These states are not added again to the graphical tree. 
;* Input
;- graphId: identifier name of the tree
;- nid: id of the the node that is being added
;- lab: label of the node that is being added (id of the corresponding Maude-NPA state)
;- state: state information given by the Maude-NPA
;- npaId: node's id in the Maude-NPA tool
;* Output
;- The intenal data structure that stores all the Maude-NPA states is updated with
; the repeated node that is added to the last level generated (it might be empty)
(define addRepeatedInitialNode (graphId nid lab state npaId) 
 (let ( (graph (fetch graphId))
        (list  (getAttr graph "list"))
	(lastP (- (invoke list "size") (int 1))) ; last position of the graph list attribute
        (lastRow (invoke list "get" lastP)) ; list of the last level of nodes. It may be empty if this node is the first that is being added in this level
	                                     ; or it may contain more nodes
        (node (object ("g2d.graph.IOPNode" nid lab 
                                "ellipse" nodeBorderColor regularFillColor)))
      )
  (seq
      ;set node's attributes
     (setAttr node "nid" nid)
     (setAttr node "label" lab)
     (setAttr node "npaId" npaId)
     (setAttr node "state" state)
     ; initialize predessor node attribute
     ; it will be binded at the newNPAEdge
     (setAttr node "parent" (object null)) 
     (setAttr node "children" (object ("java.util.LinkedList"))) ; successor nodes

     (setAttr node "unreachable" (boolean false))

     ;; add the new node in the list of the last position of the  main list
     ;; i.e list[lastposition].add(node)
     (invoke lastRow "put" lab node)
    ; (setAttr graph "list" list)
  ) ;end seq
 ) ;end environment
) ;end addRepeatedInitialNode



; method to build an edge between two nodes
;*input
;-graph: graph in which the edge will be created
;-srcid: source node's id
;-tgtid: target node's id
;-indirect?: is the edge indirect? The stroke of the edge changes depending on this value
;*output
;-new created edge, after being added to the graph

(define newNPAEdge (graph srcid tgtid indirect?)
  (let ((src (invoke graph "getNode" srcid))
        (tgt  (invoke graph "getNode" tgtid))
        (color (if (= indirect? "true") dirEdgeColor  indirEdgeColor))
        (e (object ("g2d.graph.IOPEdge" src tgt color))) 
	(childrenList (getAttr src "children"))	
    )
    
    (seq 
     ; can replace "dashed" by "dotted"
     (if (= indirect? "true")  (invoke e "setStyle" "dashed"))
     (invoke e "setDoubleEnded" (boolean false)) 
     (setAttr e "indirect" indirect?)
     ; updating parent and children attributes
     (setAttr tgt "parent" src)
     (invoke childrenList "add" tgt)
     (setAttr src "children" childrenList)
     (invoke graph "addEdge" e) ;add the edge
     e))
) ;newNPAEdge


; makes the closure that will be executed when the user click the mouse on a node ;;; 
(define mkNPAMouseClickedClosure (graph) 
  (lambda (self e)
    (seq 
      (if (instanceof self "g2d.graph.IOPNode")
          (apply doNPAMouseClickedActionDirect graph self e)
          (object null)
      )
  ))
)

; when we click on a node a popup menu will be shown so that the user will be able to choose an option. 
(define doNPAMouseClickedActionDirect (graph node e)
   (let ((npaIdStr (apply arr2str (getAttr node "npaId")))
         (gname (invoke graph "getUID"))
   	 ; to show the branch
	 (frame (getAttr graph "frame" (object null)))
        )
     (seq         
           (if (or
              (= java.awt.event.MouseEvent.BUTTON3 (invoke e "getButton")) ;if right button click
              (and (= java.awt.event.MouseEvent.BUTTON1 (invoke e "getButton")) (= (boolean true) (invoke e "isControlDown")) )) ; or left right button click whith control key pressed
                ;show popup menu
               (apply showNPAPopup frame graph node e)
            ) ; end if

       ;; ask maude for contents
	(sinvoke "g2d.util.ActorMsg" "send" "maude" gname (concat "showNode "  npaIdStr)) 
    )
        
  ) 
) ;end doNPAMouseClickedActionDirect



; this methods transforms an array of strings into only one string
;*input
;-arr: array whose elements are strings
;*output
;-an string in which all the array's elements have been appended, separated by a blank space

(define arr2str (arr)
  (let ((strb (object ("java.lang.StringBuffer"))) )
    (seq
      (for item arr (seq (invoke strb "append" item) (invoke strb "append" " ")))
      (invoke strb "toString")
     )
  )
) ;end arr2str



;; place holder
(define colorNPANode (node) regularFillColor)


;method to finish the implementation of the toolbar
;*input:
;-toolbar: Baseframe toolbar (initializated with some elements)
;-gname: name of the graph that is being displayed
;-graph: the graph which is being displayed
;-frame: Maude-NPA GUI main frame
;-parent: frame's parent element
;*output
;-full and complete Maude-NPA GUI toolbar


(define toolBarFunNPA (toolbar gname graph frame parent)
  (let ((tf  (object ("g2d.toolbar.ToolTextField")))
        (nextClosure 
           (lambda (self event)
             (let ((steps (invoke (invoke tf "getValue") "intValue")) ;numbers of next steps that the user wants to execute with Maude-NPA
                   (graph (if (instanceof gname "java.lang.String")
                             (fetch gname)
                             (object null)))
                   ) ;; show the next #steps steps
                 (apply nextClosureRec gname steps (int 0))
              ))) ; nextClosure
                (nextTip "Expand search tree")
        (nextButton (object ("g2d.toolbar.ToolButton" 
                 (apply mkAction "Next" nextTip nextClosure))))
       ) ; letbindings
 ; prepend buttons and things in tool bar
    (seq
     (invoke toolbar "add" (apply npaColorKey))
     (invoke toolbar "prepend"
             (sinvoke "g2d.toolbar.SeparatorFactory" "makeLargeSep"))
     ; text field for number of steps
     (invoke tf "setToolTipText" "Specify number of levels to expand search tree")
     (invoke toolbar "prepend" tf)
     (invoke toolbar "prepend" 
                     (sinvoke "g2d.toolbar.SeparatorFactory" "makeSmallSep"))
     (setAttr graph "nextButton" nextButton) ;***************
     ; next button
      (invoke toolbar "prepend" nextButton)
   ) ;seq
 ) ; let
) ; toolBarFunNPA

;****************************************************************************
; This closure will be called (from nextClosure and from showUpDatedNPAGraph) as many times as number of analysis steps the user
; wants to obtains. Each new level will be added to the tree (graph) and showed to
; the user, so that he or she will not have to wait until all the levels have been
; generated.
;* Input
;- gname: identification name of the graph that represents the search space tree.
;- steps: number of levels that the user wants to obtain.
;- current: current step that is being generated. For example, the second step out of three.

 (define nextClosureRec (gname steps current)
   (let ( (graph (fetch gname))
          (oldLevels (if (!= graph (object null))
                         ;(invoke (getAttr graph "list") "size")
                           (getAttr graph "nlevels")
                           (int 0)
                         ))

          (messageArea (getAttr graph "messageArea"))
          (nextButton (getAttr graph "nextButton"))
         )
     (if (and (< current steps)  
              (= (getAttr graph "analysisFinished") (boolean false)))
       ;the analysis has not finished
       (seq
          ;disable nextButton
          (if (!= nextButton (object null))
              (invoke nextButton "setEnabled" (boolean false))
          )

          
          ;show a dialog telling the user that Maude-NPA is obtaining the next level.
           (invoke messageArea "setText" "Please, wait while the next level is being generated...")
           (invoke messageArea "show")
           
          ; Maude-NPA still has to obtain, at least, one more level
          (setAttr graph "nextStep?" (boolean true))
          ; number of levels that the user wants to obtain
          (setAttr graph "nSteps" steps)
          ; how many steps have already been obtained
          (setAttr graph "currentStep" current )

          ;disable the menu item to save a session
          (invoke (getAttr graph "saveSessionMI") "setEnabled" (boolean false))
          
	  ; (invoke java.lang.System.err "println" (+ current (int 1)))
	  ; (invoke java.lang.System.err "println" oldLevels )
          ;call to Maude-NPA to obtain the next analysis step
          (sinvoke "g2d.util.ActorMsg" "send" "maude"  gname
			(concat "nextLevel "  oldLevels " " oldLevels))

       )
       ;the analysis has finished
       (seq
        ;all the steps have been generated.
        ;enable nextButton
          (if (!= nextButton (object null))
              (invoke nextButton "setEnabled" (boolean true))
          )
           ;enable the menu item to save a session
          (invoke (getAttr graph "saveSessionMI") "setEnabled" (boolean true))
        ;reset attributes
          (setAttr graph "nextStep?" (boolean false))
          (setAttr graph "nSteps" (int 0))
          (setAttr graph "currentStep" (int 0))
          (invoke messageArea "setText" "Done...")
          (invoke messageArea "show")
           
          (apply showUpDatedNPAGraph gname)
       )    
    )
   )
) ;end nextClosureRec

;****************************************************

;; "NS-0" (object null) (boolean true) "Attack 0" ""  toolBarFunNPA 
(define showNPAGraph (gname pname bool title subtitle toobarfun) "")  

;method to show the graph in a frame. It loads the main GUI window
;*input
;-gname: name of the graph which is going to be displayed
;-pname: parent frame's name
;- protocol: protocol name
;-title: frame's title
;-subtitle: frame's subtitle
;-toolBarFun: full toolbar that will be added to the frame
;*output
;-updated graph. It will show the main window which displays the graph.
;                      str  str or null 
(define showNPAGraph (gname pname  protocol title subtitle toolBarFun)
(try
  (let ((parent (if (instanceof pname "java.lang.String")
                 (fetch pname)
                 (object null)))
        (pframe (if (instanceof parent "g2d.graph.IOPGraph")
                (getAttr parent "frame")
                (object null)))
        (frame (object ("g2d.graphviewer.BaseFrame" "" 
                 (if (instanceof pframe "g2d.graphviewer.BaseFrame")  
                     pframe 
                     (apply getNPAFrame))
                    (boolean true))) ) 
        (sepanel (invoke frame "getSEPanel"))    
        (graph (fetch gname))
        (toolbar (invoke frame "getToolBar"))
;;;CLT this is to customize what the Find tab shows        
        (fClosure (lambda (node) (getAttr node "label" "???")))
       
        
        ;; add text Area to show messages
        (messageArea (apply  makeTextArea  "Click on \"Next\" to continue the analysis"  
                           (int 600) (int 30) regularColor "Arial" (int 0) (int 12) white (int 2)))

         (testframe (object ("g2d.swing.IOPFrame" "Generating")))
         (panel (object ("javax.swing.JPanel" )))

         ;add a menu option to save the state information of the graph
         (fileMenu (invoke frame "getFileMenu"))
         
         (prettySaveItem (object ("javax.swing.JMenuItem" "Save  search tree information...")))
         (prettySaveActionListener (object ("g2d.closure.ClosureActionListener" (apply prettySaveAction pname gname))))

         ;;save a Maude-NPA analysis session 
         (saveSessionItem (object ("javax.swing.JMenuItem" "Save Session ...")))
         (saveSessionActionListener (object ("g2d.closure.ClosureActionListener" (apply saveSessionAction pname gname protocol))))
         (kbm (fetch "NPAManager"))

         ;;;
         (nextButton (if (!= graph (object null))
                         (getAttr graph "nextButton")
                         (object null)) )
         
       )
    (seq
  ; prepend buttons and things in tool bar
     (apply toolBarFun toolbar gname graph frame pname )
     (invoke graph "setStrokeWidth" (float 1.0))

     ; set area to display execution messages
     (invoke panel "add" messageArea)
     (invoke frame  "add" panel java.awt.BorderLayout.SOUTH)
     (setAttr graph "messageArea" messageArea)
     
     ;*******set attributes
      (setAttr graph "gname" gname)
      (setAttr graph "attackTree" title)
      (setAttr graph "protocol" protocol)
      (setAttr graph "title" (concat (concat protocol " - ") title))
        ;*** to enable or disable the save session item (an analysis session can be saved only when the tool is not generating a new level
      (setAttr graph "saveSessionMI" saveSessionItem)
      
     ; add prettySaveItem to the File menu
     (invoke prettySaveItem "addActionListener" prettySaveActionListener)
     (invoke fileMenu "add" prettySaveItem (int 1))

     ;add saveSessionItem to the File menu  
     (invoke saveSessionItem "addActionListener" saveSessionActionListener)
     (invoke fileMenu "add" saveSessionItem (int 2))
     
     ; do layout with dot by default
     (invoke graph "doLayout")

     ;;add tab for MWA Manager
     (apply setNPATabMWAManager gname frame)

     (if (and (!= (getAttr graph "isRestoringSession?") (object null))
              (!= nextButton (object null)))
         (if (= (getAttr graph "isRestoringSession?") (boolean true))
             (seq
                (invoke nextButton "setEnabled" (boolean false))
                (invoke saveSessionItem "setEnabled" (boolean false))
             )
             (seq
                (invoke nextButton "setEnable" (boolean true))
                (invoke saveSessionItem "setEnabled" (boolean true))
             )  
         ) ;end if then part
     ) ;end if
     
   ; make frame visible and display at front
     (invoke frame "setVisible" (boolean true))
     (invoke frame "toFront")
     (invoke frame "setTitle" (concat (concat protocol " - ") title))
     
   ; update frame and set graph into frame
     (invoke frame "setGraph" graph)
     (invoke frame "setSubtitle" subtitle)
    
     
;;;CLT this is how to customize what the Find tab shows
      (invoke sepanel "setFindTabCellRendererClosure" fClosure)

      graph
  ))
     (catch Exception
    (invoke java.lang.System.err "println" (concat "Error in ShowNPAGraph: " (invoke Exception "getMessage")))
 )
) ;end try
) ; end showNPAGraph


(define getNPAFrame ()
  (let ((kbm (fetch "NPAManager")))
      (if (instanceof kbm "g2d.glyph.Attributable")
        (getAttr kbm "npaframe")
        (object null)
      )
  )
) ;end getNPAFrame




; redisplaying updated search tree
;* Input
;- gname: id name of the tree
;* Output
;- the updated graph is redisplayed
(define showUpDatedNPAGraph (gname)
(try
  (let ((graph (if (instanceof gname "java.lang.String")
                             (fetch gname)
                             (object null)))
         (messageArea (if (!= graph (object null))
                          (getAttr graph "messageArea")
                          (apply  makeTextArea  "" 
                           (int 600) (int 30) regularColor "Arial" (int 0) (int 12) white (int 2))
                        ))
         (nextButton (if (!= graph (object null))
                         (getAttr graph "nextButton")
                         (object null)) )
         (saveSessionMItem (getAttr graph "saveSessionMI" ))

        )
    (seq
     (invoke iniDialog "dispose") 
       
       (if (instanceof graph "g2d.graph.IOPGraph")
        (let ((frame (getAttr graph "frame" (object null))))
          (seq
	    (try
              (invoke graph "doLayout")
	      (catch excep
	        (seq
                  (apply displayWInfoMessage
                  "Error in showUpDatedNPAGraph"
                      (concat "There is an exception doingLayout in showUpDatedNPAGraph method:  "
                              (invoke excep "getMessage")))
	        ) ;end seq inside cath
               ) ;end catch
             ) ;end try

            (try
             (seq
              (invoke frame "setGraph" graph)
              
              ;call to get the next step              
              (if (and (!= graph (object null))
                      (= (getAttr graph "nextStep?") (boolean true)))
                 (let ( (steps (getAttr graph "nSteps"))
                      (current (getAttr graph "currentStep"))
                     )
                    (apply nextClosureRec gname steps (+ current (int 1)))
                 )
                ;once all the required levels have been generated, show popup messages
                 (seq
                   ;****check if an initial state has been found
                   (if  (= (getAttr graph "newInitialState") (boolean true))
                     (seq
                       (apply displayWInfoMessage "An initial state was found"
                       "An initial state for the specified attack has been found.\nTherefore, your cryptoprotocol is unsecure."
                       javax.swing.JOptionPane.INFORMATION_MESSAGE)
                       (setAttr graph "newInitialState" (boolean false))
                      )
                   ) ;end if newInitialState

                   ;*************
                    (if (and (!= (getAttr graph "isRestoringSession?") (object null))
                             (and (!= nextButton (object null))
                                  (!= saveSessionMItem (object null))))
                        (if (= (getAttr graph "isRestoringSession?") (boolean true))
                          (seq
                           (invoke nextButton "setEnabled" (boolean false))
                           (invoke saveSessionMItem "setEnabled" (boolean false))
                          )
                          (seq
                           (invoke nextButton "setEnabled" (boolean true))
                           (invoke saveSessionMItem "setEnabled" (boolean true))
                          )  
                        ) ;end if then part
                    ) ;end if
                          
                    ;************

                    
                   ;*******check if the analysis has finished
                    (if   (= (getAttr graph "analysisFinished") (boolean true))
                     (seq
                          (apply paintUnreachableStates graph)

                          (apply displayWInfoMessage "The end"
                                 "The analysis has finished."
                                 javax.swing.JOptionPane.INFORMATION_MESSAGE)

                          (invoke messageArea "setText" "The analysis has finished"  )
                          ;disable nextButton
                          (if (!= nextButton (object null))
                              (invoke nextButton "setEnabled" (boolean false)))
                          ;to stop the analysis
                          (setAttr graph "nextStep?" (boolean false))
                          (setAttr graph "currentStep"  (getAttr graph "nSteps"))
                     )
                     (invoke messageArea "setText" "Click on \"Next\" to continue the analysis"  )
                    ) ;end if analysisFinished


                   (invoke frame "repaint") 
                )  ;end seq
              
                ) ;end if

               
             ) ;end seq inside try
              (catch excep
	        (seq
                    ;; remove
                    (apply displayWInfoMessage
                        "Error in showUpDatedNPAGraph 2nd part"
                          (concat "There is an exception in the second part of the showUpDatedNPAGraph method:  "
                              (invoke excep "getMessage"))
                           javax.swing.JOptionPane.ERROR_MESSAGE)
	         )
	    
               )
           ) ;end second try 
          ) ; seq
        ) ;let
     ) ;if
    ) ;end seq
  ) ; let
   (catch Exception
    (invoke java.lang.System.err "println" (concat "Error in ShowUpDatedGraph: " (invoke Exception "getMessage")))
 )
) ;end try
) ; showUGraph


(define updateRestoringSessionAttr (gname restoring?)
(try

  (let ( (graph (if (instanceof gname "java.lang.String")
                             (fetch gname)
                             (object null)))
        ) ;end let bindings
     (if (and (!= graph (object null))
              (!= (getAttr graph "isRestoringSession?") (object null)))
         (if (= restoring? "true")
              (setAttr graph "isRestoringSession?" (boolean true))
              (setAttr graph "isRestoringSession?" (boolean false))
         )
         
     ) ;end if
  ) ;end let environment

 
 (catch Exception
    (invoke java.lang.System.err "println" (concat "Error in updateRestoringSessionAttr: " (invoke Exception "getMessage")))
 )
) ;end try
) ;end updateRestoringSessionAttr



;;;;;;;;;; ./makeg2dlib loaded ../GUI/npa-graph-functions.lsp ;;;;;;;;;;
;(seq    ; given the new states computated by Maude-NPA and converted into an array of three elements array,; build the nodes and add them to the graph;*input;- graphId: graph identifaction;- array: Maude-NPA result for a new level computation in the required format;*output;-the updated graph, with the new level of nodes added. (define addLevel (graphId array) (try  ( let( (graph (fetch graphId)))     (seq  (try (seq            ;check if the analysis has already finished. If not so, continue. Otherwise, stop.           (if (= (apply analysisHasFinished graphId array) (boolean false))             (let (               (oldLevels (getAttr graph "nlevels"))    	       (updatedLevels (+ oldLevels (int 1)))	       (mouseClickedClosure (apply mkNPAMouseClickedClosure graph))	       (list (getAttr graph "list")) ;; list of lists	       (lastLevel (invoke list "get" (- (invoke list "size" ) (int 1 )))) ;; last element of list ;;--list with the pairs (label,nid) of the last level                                                                         )	       (seq   (setAttr graph "nlevels" updatedLevels)                ;; firstly, we add a new empty element in the list attribute of the graph so that we will be able 	        ;; to keep the nodes of the new level we are adding in the last position of the list	        (invoke list "add"  (object ("java.util.Hashtable")))	        (setAttr graph "list" list) 	        ; for each new state	        (for element array                    (let ( 			 ; get the information required to create the node                         ; the id of the new node will be the number of nodes in the graph			 (nidInt (getAttr graph "numberNodes"))                         (nid (invoke (object( "java.lang.Integer" nidInt)) "toString")) ;parse the int to string			 (lab  (apply arr2label (aget element (int 1))))			 (state (aget element (int 2)))			 (parentId (apply findParentIdent lastLevel lab)) ; find the id  of the node's parent given the node's label		     )                        (if (= (invoke (getAttr graph "initialStates") "contains" lab) (boolean false))		       (seq                         ;create and add the new node to the graph			   (apply newNPANode graph mouseClickedClosure nid lab state (array java.lang.String  nid) (boolean true))			   (setAttr graph "numberNodes" (+ nidInt (int 1))) ;update the number of nodes that the graph has.                                   (if (!= parentId (object null)) ; make the edge between the node and its parent node			     (apply newNPAEdge graph parentId nid "false")			     ;; else, its parent is the root node whose id is 0			     (apply newNPAEdge graph "0" nid "false")  ) ;end if                       ) ;end seq                       ;else, it is a repeated initial state generated before, that should not be added to the IOP graph  ; but that should be added to the data structure of the tree, in order to be printed later.                                                  (apply addRepeatedInitialNode graphId  nid lab state (array java.lang.String  nid))                     ) ;end if      		  )          	      ) ;end for                            ) ;end seq then part              ) ;end then part                      (seq ;else (the analysis has finished) ;show message             (setAttr graph "analysisFinished" (boolean true))          )          ) ;end if;           (apply paintUnreachableStates graph)  ;emphasize unreachable states          ) ;end seq     (catch var        (seq	        (invoke java.lang.System.err "println" (concat "There is an exception in addLevel method:  " (invoke var "getMessage")))                 ;; remove                (apply displayWInfoMessage "Exception in addLevel method"                         (concat "There is an exception in addLevel method:  " (invoke var "getMessage"))                         javax.swing.JOptionPane.ERROR_MESSAGE)                graph     )) ; end catch   ) ;end try ) ;end outer seq)     (catch Exception    (invoke java.lang.System.err "println" (concat "Error in addLevel: " (invoke Exception "getMessage"))) )) ;end try) ; end addLevel;this method checks if a node is a leaf of the computation tree. If it is, then it will be coloured in a different color.;this method will be called from function newNPAnode.;An initial state is the one in which the past part of all the strands is empty.; *Inputs;- the node's state information; *Outputs;- true if the state is an initial state or, if not, false.(define isInitialState (node)   (if (!= (getAttr node "state") "")      ( let (  (text (invoke (apply getInfoState node (int 0)) "replace" "\n" ""))               (strandsArray (invoke text "split" "&"))          )        (if (= (apply isInitialStateRec strandsArray (int 0)) (boolean false))            (boolean false)            (let( (iKnowledge (apply getInfoState node (int 1)))                  (iKnowledge2 (invoke iKnowledge "replace" "\n" ""))                  (iKnowledge3 (invoke iKnowledge2 "replace" " " ""))                 )              (if (= (invoke iKnowledge3 "contains" ")inI") (boolean true))                  (boolean false)                  (boolean true)              ) ;end if            ) ;end environment        ) ;end if      )    (boolean false)  ) ;end if) ;end isInitialState;recursive closure to checks if a node is a leaf of the computation tree.;this closure checks whether the past part of one of the strands is empty or not.;* Input;- array: collection in which each element is one of the state strands.;-current: position of the array element that is being checked.;* Output; false if there is a strand with a non-empty past part, and boolean if, in the end, ; all the strands past parts were empty.(define isInitialStateRec (array current)  (if (>=  current (lookup array "length"))      (boolean true)      (let ( ;get the strand to process             (initialText (invoke (aget array current) "replace" " " ""))             (posIni (+ (invoke initialText "indexOf" "::[") (int 3)))             ;strand text without useless symbols.             (text1 (invoke initialText "substring" posIni))            )            (if (= (invoke text1 "startsWith" "nil,") (boolean true))                (boolean false)                (apply isInitialStateRec array (+ current (int 1)))            )                )   ) ;end if) ;isInitialStateRec;closure to guess if the analysis has finished or not(define  analysisHasFinished (graphId array) (if (or (= (invoke array "size") (int 0))         (and (= (invoke array "size") (int 1))              (= (invoke array "get" (int 0)) "(empty).IdSystemSet"))     )  ;if no states were obtained in the last level, the analysis has finished             (boolean true)  ;otherwise, check if all the states obtained in the last level are initial states.         (apply checkEnd graphId array) ) ;end if) ;end analysisHasFinished;auxiliar closure to guess if the analysis has alreay finished or not;it checks if all the states obtained in the last level are initial states.(define checkEnd (graphId array)  (let( (graph (fetch graphId))           (list (getAttr graph "list")) ;; list of lists	  (lastLevel (invoke list "get" (- (invoke list "size" ) (int 1 )))) ;; last element of list --list with the pairs (label,nid) of the last level          (initialStates (getAttr graph "initialStates"))      )        (if (= (invoke list "size") (int 0))            (boolean false)            (apply checkEndRec initialStates lastLevel array (int 0))        )  ) ;end else part) ;end checkEnd;recursive closure to guess if all the states of the last level are initial;* Input;- initialStates: list of initial states already obtained.;- lastLevel: array with the penultimate generated level of states;- array: array with the last generated states that are being added to the search tree.;- pos: position, in array, of the state that is being checked.;* Output; true if the anylisis has finished, i.e., if all the states just generated are initial states,; or false if the analysis should continue.(define checkEndRec (initialStates lastLevel array pos) (if (>= pos (invoke array "size"))      (boolean true)     (let( (currentState (invoke array "get" pos))           (lab (apply arr2label (aget currentState (int 1))))             )       ; if any state was added to the last level of the tree (because all the states that Maude-NPA       ; computed last time were initial states) and the state that is currently being checked is an       ; initial state, check the next state. If not, the analysis has to continue.       (if (= (invoke initialStates "contains" lab) (boolean true))          (apply checkEndRec initialStates lastLevel array (+ pos (int 1)))          (boolean false)       )      )  ) ;end if) ;end checkContinueRec;*********************;****************methods to find the parent node of a given node *****************************; this method finds the id  of the node's parent given the node's label, searching only in the last level of the graph;*input:;- lastLevel: hashTable which contains the nodes of the graph last obtained level.;- childLab: label of the node whose parent id we want to obtain.;*output:;- the id of the node's parent node.(define findParentIdent (lastLevel childLab)   (try       (let (  	       (nChildLab (invoke childLab "substring" (int 1) (- (invoke childLab "length" ) (int 1)))) ; to quit the "<" and ">" of the label               (lastLevelArray (object ("java.util.ArrayList" (invoke lastLevel "values"))))	   )	    (apply findParentId  lastLevelArray nChildLab (int 0) (invoke lastLevelArray "size" ))       )       (catch Exception (seq	   (invoke java.lang.System.out "println" (concat "Exception in findParentIdent: " (invoke Exception "toString")))	   (object null)))   )); tries to match the child node's label (the node we want to add to the graph) with a label of some node in the last level of the grah, which will be its parent.; The parent's label must be a prefix of the child's label, but it's necessary to deal with the states with brackets and keys.; if a parent's label matches with the child's label, return the id of the current node; if no parent's label is found, the parent node will be the one whose id is 0, the default parent.;*input;- lastLevel: arrayList which contains the nodes of the graph last obtained level.;- childLab: label of the node whose parent id we want to obtain.;- current: current position of the lastLevel which we are dealing with.;           The function tries to do matching with the element stored at this position of the array;- length: length of the childLab;*output;- the node's parent label(define findParentId (lastLevel  childLab current length)(seq    (try	     (if  (= length current )		 (object ("java.lang.String" "0")) ; the parent is the root node (default case). End		(seq		    (let (			    (currentNode (invoke lastLevel "get" current))                            ; to quit the "<" and ">" of the current node's label			    (currentLabel (invoke (getAttr currentNode "label") "substring" (int 1) (- (invoke (getAttr currentNode "label") "length" ) (int 1))))                             ; quit the parenthesis of the current node's label to operate with the labels later			    (currentLabel2 (invoke (invoke (invoke currentLabel  "replace" "(" "") "replace" ")" "") "trim"))			    (currentId (getAttr currentNode "nid"))                            ; quit the parenthesis of the label of the node we want to add (child node)  to operate with it later			    (childLabShort (invoke (invoke (invoke childLab "replace" "(" "") "replace"  ")" "") "trim")) 			 )			(seq                         ; to process the states with keys , which look like < (1[1]).(5[2]).3.(17{1}).2 >			    (if  (= (apply makeTest2 currentLabel2 childLabShort)  (boolean true))				currentId				; to process states with brackets and keys, which look like  < (1[1]).(5[1]).3.(12[1]).(2[2,{1}]).16				( if (= (apply makeTest3 currentLabel2 childLabShort) (boolean true))				    currentId				    ; to process simple states, which look like < 1 . 3 . 15 >				    ( if (= (invoke childLabShort "startsWith" currentLabel2) (boolean true))					currentId					(apply findParentId lastLevel childLabShort (+ current (int 1)) length) ; try with the next node of the last level 				    )				    ; end if				) ;end if			    )			     ; end if			) ; end seq		    ) ; end let		) ; end seq	    ) ; end if length = current						      (catch Exception (seq	     (invoke java.lang.System.out "println" (concat "Exception in findParentId " Exception)) 	     (object ("java.lang.String" "0"))	 )     ) ))) ; end findParentId; to process the states with keys , which look like < (1[1]).(5[2]).3.(17{1}).2 > ; whose parent is the node with label (1[1]).(5[2]).3.17; note that the parenthesis and blank spaces have been removed from the label(define makeTest2 (currentLab childLab)(seq    ( if (> (invoke childLab "length") (invoke currentLab "length"))	(try	    (  let (		    ; parent's and child's label are the same until this position of the string		    ; 1[1].5[2].3.17		    ; 1[1].5[2].3.17{1}.2 		    (label2Length (invoke currentLab "length"))		    (nextChar (invoke childLab "charAt" (+ label2Length (int 1)))) ; the first character in child's label that doesn't match with the parent's label		)		(seq		    ( if (and (= (invoke childLab "startsWith" currentLab) (boolean true)) (= nextChar (char 123))) ;char 123 ={			(boolean true)			(boolean false)		    )		)			    )	    (catch exception (seq		    (boolean false))	    )	) ; end try	 (boolean false)     ) ; if          )      ) ; end test2   ; to process states with brackets and keys, which look like  < (1[1]).(5[1]).3.(12[1]).(2[2,{1}]).16  ; whose parent is the node with label < (1[1]).(5[1]).3.(12[1]).(2[2])> ; note that the parenthesis and blank spaces have been removed from the label(define makeTest3 (currentLab childLab)(seq    ( if (> (invoke childLab "length") (invoke currentLab "length"))	(try	    (  let ( ; parent's and child's label are the same until this position of the string		    ; 1[1].5[1].3.12[1].2[2]		    ; 1[1].5[1].3.12[1].2[2,{1}].16		    (lastCoincidence (- (invoke currentLab "length") (int 1)))		    (currentLabel3 (invoke currentLab "substring" (int 0) lastCoincidence)) ; to quit the closing bracket ]		    (nextChar (invoke childLab "charAt" lastCoincidence))  ; the first character in child's label that doesn't match with the parent's label		) 		(seq		    (if  (and (= (invoke childLab "startsWith" currentLabel3) (boolean true)) (= nextChar (char 44))) ; char 44=','			(boolean true)			(boolean false)		    )		)			    )	    	    (catch exception (seq		    (boolean false))	    )	) ; end try     	 (boolean false)     ) ; if ));;*********************methods to show only the branch of the tree in which a given node is included****; given the graph id and the node id, obtains only the branch of the graph that contains the node; the branch will be a graph too;*input;-graphID: id of the graph which the given node belongs to;- nodeId: id of the selected node, whose branch we want to obtain;*output;-the graph's branch which the node belongs to.(define getBranch (graphId nodeId kbm)    (	let(  	    (graph (fetch graphId))	    (branchId (object ("java.lang.String" (concat graphId nodeId)))) ; right now the id of the branch will be concatenation of the graph and node ids.	    ;(branch (apply  createEmptyGraph "branch" branchId "false")) ;the branch is a graph too            (branch (apply createMtTree branchId  (getAttr kbm "aname") (getAttr kbm "protocol")  (boolean true)))	    (mouseClickedClosure (apply mkNPAMouseClickedClosure graph))	    (node (invoke graph "getNode" nodeId)) ;node whose branch we want to obtain	)	(seq            ;firstly, we add the selected node to an empty graph	    (apply newNPANode branch mouseClickedClosure nodeId (getAttr node "label") (getAttr node "state") (array java.lang.String  nodeId))            ;then build up the branch, with a bottom to top strategy            (apply mountBranch branch nodeId graph)           )    )) ;end getBranch; this method builds up the branch which the node belongs to with a bottom to top strategy; by finding the parent node of the current node until it arrives to the root node, the one; which hasn't got parent node.;*input:;-branch: the subgraph from the whole graph which will contain only the predecessors nodes of the selected node;-nodeID: id of the current node whose parent node we want to obtain in order to go one level upper in the graph;-graph: entire graph. We need it to get the parent node of the current node;*output;- updated branch graph with the parent node of the current node added.(define mountBranch (branch nodeId graph)    (	let(            ;obtain the parent Node of the current node	    (parentNode (getAttr (invoke graph "getNode" nodeId) "parent"))            ;mouse listener for the node	    (mouseClickedClosure (apply mkNPAMouseClickedClosure graph))	)	(seq	    	    (if (= parentNode (object null))		branch ; the current node is the root node, so we have finished building the branch		( let ( ;else, get the parent node			 (parentId (getAttr parentNode "nid"))		     )		     		   (seq                     ;add the parent node to the branch graph		     (apply newNPANode branch mouseClickedClosure parentId (getAttr parentNode "label") (getAttr parentNode "state") (array java.lang.String  parentId))		     (apply newNPAEdge branch  parentId nodeId "false") ;build the edge between the parent and the child node		     (apply mountBranch branch parentId graph) ; we go one level upper		 )		 	     )	 )     ) )) ;end mountBranch; the node's label will be given as an array of strings, so that this method transforms the array into a string with the labels format;*input;- arr: array of string that represents a node's label;*output;-node's label as a string(define arr2label (arr)  (let ((strb (object ("java.lang.StringBuffer" "<"))) )    (seq      (for item arr (seq (invoke strb "append" item) (invoke strb "append" ".")))        (let (	     (str (invoke strb "toString")) 	     (strShort (invoke str "substring" (int 0) (- (invoke strb "length") (int 1))))	 )	 (concat strShort ">" )        )    )  )) ;end arr2label;************Closures to paint, in white color, the unreachable states of the previous level.(define paintUnreachableStates (graph)  (if (= (getAttr graph "analysisFinished") (boolean true))      (apply paintUnreachableStatesLast graph)      (apply paintUnreachableStatesNotLast graph)  ) ;end if) ;;end paintUnreachableStates(define paintUnreachableStatesLast (graph)  (let ( (unreachableStatesLast (apply getUnreachableStatesLast graph))        )    (seq       (for element unreachableStatesLast ;for each unreachable state         (let( (node (invoke graph "getNode" (getAttr element "nid")))                  )            (if (= (apply isInitialState node) (boolean false))              (seq                 (invoke node "setFillColor" unreachableFillColor) ;change its fill color                 (setAttr node "unreachable" (boolean true))              )            ) ;end if         )       ) ;end for    ) ;end seq  ) ;end let ) ;end paintUnreachableStatesLast    (define getUnreachableStatesLast (graph) (let( (list (getAttr graph "list"))       )    (seq       (if (and (!= list (object null))            (> (invoke list "size") (int 2)))         (let( (initialStates (getAttr graph "initialStates" (object ("java.util.ArrayList"))))               (lastLevelList (invoke (invoke list "get" (- (invoke list "size") (int 1))) "values"))               (initialUnreachableStateList  (object ("java.util.ArrayList" lastLevelList))) ;*******************concurrent modification               (finalUnreachableStateList  (object ("java.util.ArrayList" lastLevelList)))             )           (seq              (if (and (!= initialUnreachableStateList (object null))                       (> (invoke initialUnreachableStateList "size") (int 0)))                (seq                   (for node initialUnreachableStateList                         ;**********************************concurrent modification                        (if (= (invoke initialStates "contains" (getAttr node "label")) (boolean true))                              (invoke finalUnreachableStateList "remove" node) ;**********************************concurrent modification                        ) ;end if                   ) ;end for                )                 (object ("java.util.ArrayList"))              ) ;end if              initialUnreachableStateList           ) ;end seq         ) ;end let          (object ("java.util.ArrayList"))       ) ;end if    ) ;end seq  )) ;end paintUnreachableLast; This closure obtains a list with the unreachable states of the previous level when the analysis; has not yet finished ; and changes its fill color to red;* Input;- graph: search space tree that represents the analysis of a protocol.;* Output; updated graph with the unreachable nodes painted in red color.(define paintUnreachableStatesNotLast (graph) (let ( (list (getAttr graph "list"))      )   (seq     (if (and (!= list (object null))            (> (invoke list "size") (int 2)))         (let (  (unreachableNodes (apply getUnreachableStates graph)) ;get unreachable states list                 )           (for nid unreachableNodes ;for each unreachable state             (let( (node (invoke graph "getNode" nid))                  )               (seq                 (invoke node "setFillColor" unreachableFillColor) ;change its fill color                 (setAttr node "unreachable" (boolean true))               )             )           ) ;end for         )     ) ;end if     graph   )  )) ;end paintUnreachableStatesNotLast; this closure obtains the unreachable states of the previous level.; first, it gets the last level, which contains the so called, children nodes.; then it gets the penultimate level (the previous level), which contains the parent nodes; of the childre nodes.; At the begining, all the parent nodes are considered unreachable. Then, for each child node; the closure obtains its parent node and removes it from the initial unreachable nodes list.; After analysing all the children nodes, the unreachable nodes list is returned.;* Input;- graph: search space tree that represents the analysis of a protocol.;* Output;- a list with all the unreachable states of the previous level.(define getUnreachableStates (graph)(try (let(  (list (getAttr graph "list"))        ; list with children nodes (the ones from the last level)        (childrenList (invoke list "get" (- (invoke list "size") (int 1))))        (childrenArray (invoke childrenList "values"))        ;list with parent nodes (the ones form the previous level)        (parentList (invoke list "get" (- (invoke list "size") (int 2))))        ;initial unreachable states list that contains all the parent nodes id's.        (unreachableIni (apply initializeUnreachableList parentList graph))     )     (seq      (for childNode childrenArray        ( let( ; get the id of child node's parent.              (parentId (apply findParentIdent parentList (getAttr childNode "label")))                )            ;remove from the unreachable states list, the parent node of this child node.            (invoke unreachableIni "remove" parentId)        ))      unreachableIni        )) (catch exception (seq  (invoke java.lang.System.err "println" (concat "Error in getUnreachableStates " (invoke exception "toString") ))  (object ("java.util.ArrayList")) )))) ;end getUnreachableStates; This closure builds an initial unreachable states list that will contain; the id of all the previous level nodes that are not initial states.;* Input;- list: hashtable that contains the previous level nodes (parent nodes).;- graph: search space tree that represents the analysis of a protocol.;* Output; an Array List whose elements are the id's of the parent nodes.(define initializeUnreachableList (list graph)  (let( (initialStates (getAttr graph "initialStates" (object ("java.util.ArrayList"))))        (parentList (invoke list "values"))        (unreachableList (object ("java.util.ArrayList")))       )   (try    (seq       (for node parentList           (if (= (invoke initialStates "contains" (getAttr node "label" "")) (boolean false))              (invoke unreachableList "add" (getAttr node "nid"))           )       )       unreachableList    )    (catch exception       (seq          (invoke java.lang.System.err "println" (concat "Error en initialize " (invoke exception "toString") ))          unreachableList       ))  ) ;end try )) ;end initializeUnreachableList ;) ;end file 
;;;;;;;;;; ./makeg2dlib loaded ../GUI/npa-showStrands.lsp ;;;;;;;;;;


;(seq

; main method which will show the strands
;*inputs:
;-frame: (I need to check this)
;-node: selected node whose strands the user wants to view
;*outputs:
;- a new window will be created to show graphically the node's strands
 
(define mkNPAShowStrandsClosure (graphFrame node graphTitle)
  (lambda (self e)
     (apply showStateStrands graphFrame node graphTitle)
  )
) ;end mkNPAgetchStrandsClosure


;
(define showStateStrands (pFrame node graphTitle)
  (try   
   (let ( 
         (nid (getAttr node "nid"))
         ;get the initial data structure which contains the strands nodes, messages, precedences and more
         (storageObject (apply mkStorageObject node))
         ;order vertically the strand's nodes following John Ramsdell Strand Layout algorithm
         (storageObjectOrdered (apply orderStrandsVertically nid))
         ;
          (storageObjectModified (apply updateWithAttackStrand nid))
         ;reorder vertically the strand's nodes to ovoid messages and nodes overlapings
         (levelsList (apply orderLevelsList nid))
       )
    
      (seq
         (setAttr storageObjectModified "levelsList" levelsList)
	  
	  
	  
         ; if framesList doesn't contain a FLItem that represents
         ; the node, create it.
         (if (= (invoke framesList "containsKey" (getAttr node "label")) (boolean false))
              (apply addInitializedFramesListItem  node pFrame) ;add the entry frame
         )
          
         ; if the node hasn't got any strands frame associated,
         ; create the frame and store it as the strandsFrame attribute 
         ; of the FLItem that corresponds to this node.
         (if (= (apply FLItemHasStrandsFrame node) (boolean false))
               (apply addStrandsFrameToFLItem node (apply drawStrands  node pFrame graphTitle))
 
         )

         (apply showFLItemStrandsFrame node)
      ) ; end seq   
     )
   (catch Exception
     (seq
      (apply displayWMessage "Error" (concat "Error: the strands of node " nid  " of the graph \n"
                                              graphTitle " could not be displayed.\n"
                                              (invoke Exception "getMessage")))
      (invoke java.lang.System.err "println" (concat "Error in showStateStrands: " (invoke Exception "getMessage")))
      )   
   )) ;end try
 ) ;end showStateStrands



;the storageObject is the data structure that contains the necessary information to draw a strand
;its attributes are:
;* strandsMatrix: is a list, each one of whose elements corresponds to an strand. Each list will contain the nodes of each strand stored
;                  in other list.
;*intruderList: is a hashtable whose keys are messages and whose object indicates if the  message has been learned by the intruder or not.
; Other data structure created:
;*messagesList: hashtable that contains all the messages of the strands. It would be useful to build up the precedences among the nodes.
;*Input:
;-node: node whose strands the user wants to view

(define mkStorageObject (node)
 (try
  (let( 
        ; list to store the intruder's knowledge
        (intruderList (apply getIntruderKnowledgeList node)) 
        ;get and array which contains the different strands as strings. (without the precedences between nodes)
        (strandsMatrixInitial (apply mkStrandsMatrix node)) 
        ; this list will contain for each message, the node which it belongs to
        (messageList (apply mkMessageMatrix strandsMatrixInitial ))
        
        (nid (getAttr node "nid"))
        (olderVersion (fetch (concat "storageObject" nid)))
        ;create an empty storageObject if it has not been created
        (storageObject (if (= olderVersion (object null))
                           (object ("g2d.glyph.Attributable"))
                            olderVersion ))
       )
    (seq
     ;;update messageMatrix with messages of the strands of the attack specification
  ;   (apply updateMessageMatrix strandsMatrixInitial messageMatrix)
     (setAttr storageObject "messageList" messageList)

     ;set identification to the storageObject
     (if (= olderVersion (object null))
            (invoke storageObject "setUID" (concat "storageObject" nid)) 
     )

      ;;returns the strandsMatrix with the precedences set up
       (setAttr storageObject "matrix" (apply setPrecedences strandsMatrixInitial messageList))
       (setAttr storageObject "intruderList" intruderList)

       storageObject    
     )
  )
  (catch Exception
    (invoke java.lang.System.err "println" (concat "Exception in mkStorageObject: " (invoke Exception "getMessage") ))
  )
  ) ;end try
) ;end mkStorageObject


;;;;;;;;;;;;;;----------------------Methods to build the intruder's knowledge data structure-----------------------

; method which will get the intuder's knowledge from the node's state and build  a list with the messages included in it.
(define getIntruderKnowledgeList (node)
(try
(let(
     (text (invoke (apply getInfoState node (int 1)) "replace" "\n"  "" ))
   )
  (seq
    (apply  buildIntruderList text)
  )
 )
(catch Exception
      (invoke java.lang.System.err "println" (concat "Exception in getIntruderKnowledge: " (invoke Exception "getMessage") ))
  )
) ;end try
) ;end getIntruderKnowledge


;This method builds a list with the intruder's knowledge
;*input:
;-text: intruder's knowledge text extracted from Maude-NPA output
;*ouptu:
;- a list which contains the intuder's knowledge information as data structure, easier to use.

(define buildIntruderList (text)
 (let(
        (list (object ("java.util.Hashtable")))
      )
    (apply  buildIntruderListRec  (int 0) (int 0)  (int 0) text "" list)
 )
) ;end  buildIntruderList
 

;this method extracts the different messages from the intruder's knowledge information. For each messages
;indicates wheter it has been learned by the intruder or not.
;to split the whole text, this algorithm matches a openning parenthesis with a close parenthesis and the word inI, !inI, irr or inst
;which indicates if this message belong to the intruder's knowledge or no. The algorithm covers the initial string and when it has processes and entire
;message adds it to a list indicating if it has been learned of not.
;*Input:
;-openPCounter: number of opened parentesis found in the current message
;-closePCounter: number of opened parentesis found in the current message
;-pos: current position of the textSrc that we are processing
;-textSrc: string which contains the entire IntruderKnowdledge information without any kind of format.
;-currentMessage: message which the algorithm is currently processing. In each iteration we add the current char we are processing to it.
;-list: hashtable whose elements have the following format:
;             key-> is a message in string format
;             object-> a boolean value which indicates if the message (key) has been learned by the intruder or not
;*Output:
;-the list with the messages and their boolean values



(define  buildIntruderListRec (openPCounter closePCounter pos textSrc currentMessage list)
  (let (
        (srcLength (invoke textSrc "length")) 
        )
 ( if (>= pos srcLength)  ; the whole string has been processed. End.
    list ;return the list which contains all the messages
        
   ( let ( (currentChar (invoke textSrc "charAt" pos)) ;character which is being processed
          )
    (if (and (= openPCounter closePCounter)
             (or (= currentChar (char ',')) (= pos (- srcLength (int 1)) ) )) ; an entire message has been read. Process it.
        (let (    
               (trimmedMessage (invoke currentMessage "trim"))
               (message (if (= (invoke trimmedMessage "endsWith" ",") (boolean true) )
                      (invoke trimmedMessage "substring" (int 0) (- (invoke trimmedMessage "length") (int 1)))
                      trimmedMessage
               ))
               ; does this messasge belongs to the intruder knowledge?
               (intruder? (if (= (invoke message "endsWith" "!inI") (boolean true))
                              (boolean false)
                              (if (or (= (invoke message "endsWith" "inI") (boolean true))
                                  (= (invoke message "startsWith" "irr")  (boolean true))
                                  (= (invoke message "startsWith" "inst")  (boolean true)))
                                 (boolean true)
                                 (boolean false)
                              )
                            ))
               (message2 (apply extractMessage message intruder? )) ;delete the characters which don't belong to the message
              )
          (seq
          
            (invoke list "put" message2 (object ("java.lang.Boolean" intruder?))) ;add the message with its boolean value
            ( apply  buildIntruderListRec  (int 0) (int 0) (+ pos (int 1)) textSrc "" list ) ;process the others messages
          )
        ) ; end of part "then"
        (if (= currentChar (char '('))
            ; update openned parentesis counter  and currentMessage          
            (apply  buildIntruderListRec (+ openPCounter (int 1)) closePCounter (+ pos (int 1)) textSrc (concat currentMessage currentChar) list ) 
            (if (= currentChar (char ')'))
                 ; update closed parentesis counter  and currentMessage
                 (apply  buildIntruderListRec openPCounter (+ closePCounter (int 1))  (+ pos (int 1)) textSrc (concat currentMessage currentChar) list) 
                 (apply buildIntruderListRec openPCounter closePCounter (+ pos (int 1)) textSrc (concat currentMessage currentChar) list)
                 ; update only the current position and currentMessage
             ) ; end if
         ) ;end if
     ) ;end if
    ) ;end else part of first if
    ) ;end if
) ;end let
) ;end  buildIntruderListRec


; this method extracts only the message without the character that indicates if it belongs to the intruder's knowledge or not
;*Input:
;-trimmedMessage: complete string which includes some character we don't want
;-intruder?: this variable tells wheter the message belongs to the intruder's knowledge or no
;*Output:
;-the message without any other character

(define  extractMessage (trimmedMessage intruder?)
  (let(
        (message (if (= (invoke trimmedMessage "endsWith" ",") (boolean true) )
                      (invoke (invoke trimmedMessage "substring" (int 0) (- (invoke trimmedMessage "length") (int 1))) "trim")
                      (invoke trimmedMessage "replace" " " "")
                     ))
       )
    (seq
      (if (= intruder? (boolean false))
          (let( ;delete character we don't want
               (posIni  (if (= (invoke message "startsWith" "(") (boolean true))
                            (int 1)
                            (int 0)))
               
               (posEnd (if (= (invoke message "startsWith" "(") (boolean true))
                          (- (invoke message "length") (int 5)) ;5 characters ")!inI"
                          (- (invoke message "length") (int 4)) )) ;4 characters "!inI"      
               )
            (invoke message "substring" posIni posEnd)
           )
       ;the message belongs to the intruder knowledge. 
          (if (= (invoke message "endsWith" "inI") (boolean true)) 
               (let(
                      (posIni  (if (= (invoke message "startsWith" "(") (boolean true))
                            (int 1)
                            (int 0)))
               
                      (posEnd (if (= (invoke message "startsWith" "(") (boolean true))
                          (- (invoke message "length") (int 4)) ;4 characters ")inI"
                          (- (invoke message "length") (int 3)) )) ;3 characters "inI"  
                    )
                  (invoke message "substring" posIni posEnd)
                )
             (if (= (invoke message "startsWith" "irr(") (boolean true))
                   (invoke message "substring" (int 4)  (- (invoke message "length") (int 1))) ; irr( message )
                   (invoke message "substring" (int 5)  (- (invoke message "length") (int 1))) ;inst( message )
              ) ;end if
           ) ; end if 
      ) ;end first if
      )
    )          
) ; end extractMessage

;;;;;;;;;;;;;;----------------------Methods to build the strands data structure-----------------------

;*Input
;-node: node whose strands are going to be displayed
;*Output:
;-an array which contains in each position only one strand as a string

(define getStrandsArray (node)
  ( let(
           (text (invoke (apply getInfoState node (int 0)) "replace" "\n" "")) ; gets the node's current strands information
        )
           (invoke text "split" "&")
  )
) ;end getStrandsArray



; Method to obtain each strand's nodes
;*Input:
;-strandsArray: array whose elements are the strings which contains the information for each strand
;*Outputs
;- a matrix in which for each strand its nodes are specified

(define mkStrandsMatrix (node)
(try
  (let(  (strandsInfo (apply getInfoState node (int 0)))
         ;delete the last parenthesis of the strands information
         (strandsInfoModified (invoke strandsInfo "substring" (int 0)
                                                              (- (invoke strandsInfo "length") (int 1))))
         (text (invoke strandsInfoModified "replace" "\n" "")) ; gets the node's current strands information
         (text2 (invoke text "replace" "\t" ""))  ;************new
         (text3 (invoke text2 "replace" "  " " ")) ;************new
	 (strandsArray (invoke text3 "split" "&"))
       ; each entry of the hash table will correspond to the list of nodes of each strand
        (strandsMatrix (object ("java.util.ArrayList"))) 
      )
        (apply mkStrandsMatrixRec strandsArray strandsMatrix (int 0))
  )
  (catch Exception
       (invoke java.lang.System.err "println" (concat "Error in mkStrandsMatrix: " (invoke Exception "getMessage") ))
  )
  ) ;end try
) ;end mkStrandsMatrix


;*Input:
;-array: array  whose elements are the strings which contains the information for each strand
;-strandsMatrix: data structure which contains the nodes for each strand that has already been processed
;-currentStrand: number of the strand which is is going to be processed
;*Output:
;-the full strandsMatrix with all the strands processed

(define mkStrandsMatrixRec (array strandsMatrix currentStrand)
 (try
  (if (>=  currentStrand (lookup array "length"))
      strandsMatrix ; the method has finished so that it  returns the list with the nodes of each strands. End
      (let (  (strandFull  (aget array currentStrand)) ;get the string that corresponds to the current strand
              (strand1 (apply formatStrandString strandFull )) ;format the strand deleting characters we don't need
              (posPast (invoke strand1 "indexOf" "|" ))  
              (strand2 (if (= (invoke strand1  "startsWith" "|")  (boolean true))
                    (invoke strand1 "replace" "|" "") ;the strand does not have past messages
                    (invoke strand1 "replace" "|" ",")))
              
              (strand (if (= (invoke (invoke strand2 "trim") "endsWith" ",)") (boolean true))
                         (invoke strand2 "substring" (int 0) (- (invoke strand2 "length") (int 1)) )
                          strand2))
               ;guess if this strand is a regular or an intruder's strand
              (intruder? (invoke (invoke strandFull "replace" " " "") "startsWith" "::nil"))
              ;*******new
              (strandFull1 (apply quitInitialBlank strandFull))
              (messagesString (apply getMessagesFromStrand strandFull1))
              (messagesList (apply getArrayStrandMessages messagesString))
              (isIntruder? (apply isIntruderStrand strandFull))

              (strandNodesList (apply buildStrandNodesList messagesList currentStrand isIntruder?))
           )
        (seq
         (invoke strandsMatrix "add"  (apply buildStrandNodesList messagesList currentStrand isIntruder?) )
          ;continue processing the others strands
          (apply mkStrandsMatrixRec array strandsMatrix (+ currentStrand (int 1)))
         )
       ) ;end else part
  ) ;end if
    (catch Exception
      (invoke java.lang.System.err "println" (concat "Error in mkStrandsMatrixRec: " (invoke Exception "getMessage")))
    )
  ) ;end try
) ;end mkStrandsMatrixRec



;*************operators to extract information of a strand string representation
; This method checks whether a strand is an intruder or honest strand
;* Input
;- strandFull: is a string with the text of a strand
;* Output
;- true if the strand is an intruder strand and false otherwise.

(define isIntruderStrand (strandFull)
  (try
  (let (  
          (lastMinusPos (invoke  strandFull "lastIndexOf" "-"))
	  (firstPlusPos (invoke strandFull "indexOf" "+"))
        )
  (seq    
      (< lastMinusPos firstPlusPos)
      
  ) ;end seq
 )
      (catch Exception
      (invoke java.lang.System.err "println" (concat "Error in isIntruderStrand: " (invoke Exception "getMessage")))
    )
  ) ;end try
) ;end isIntruderStrand



; This method obtains the substring of a strand that contains only the messages, without the
; definition of the fresh variables generated at this strand.
;* Input:
;- strandFull: string that contains the entire textual representation  of the strand
;* Output:
;- substring of "strandFull" that contains only the messages.

(define getMessagesFromStrand (strandFull)
 (try
  (let ( (endFreshSet (invoke strandFull "indexOf" "::" (int 2)))
         (iniPos (if (!= endFreshSet (int -1))
                     (+ endFreshSet (int 4)) ;4 characters= :: + blank space + open bracket
                     (int 0)))
         (endPos (if (= (invoke strandFull "endsWith" ")")
                        (boolean true))
                     (- (invoke strandFull "length") (int 2)) ; 2 character = ) + closing bracket
                     (- (invoke strandFull "length") (int 1)) ; 1 character = closing bracket
                 ))
        )
  (seq
      (invoke strandFull "substring" iniPos endPos)
  )
 ) ;end let environmnet
  (catch Exception
    (invoke java.lang.System.err "println" (concat "Error in getMessagesFromStrand: " (invoke Exception "getMessage")))
  )
 ) ;end try
) ;end getMessagesFromStrand


;obtains all the messages of a strand as an array
;* Input
;- strandMessages: is a string that contains only the text with the messages of a strand
;* Output
;- This method splits the entire string representation of a strand into
;  several substrings that correspond to each one of the messages of the strand.
;  Each one of these substring is then added to an arrayList that, at the end, will
;  contain the messages of a strand.

(define getArrayStrandMessages (strandMessages)
 (try
  (let ( (messagesList (object ("java.util.ArrayList")))
         (res (apply getArrayStrandMessagesRec (int 0) (int 0) (int 0) strandMessages "" messagesList))
       )
    messagesList
  ) ;end let environment
   (catch Exception
     (invoke java.lang.System.err "println" (concat "Error in getarrayStrandMessages: " (invoke Exception "getMessage")))
   )
 ) ;end try
) ;end getArrayStrandMessages


; this function buidls an array list with all the messages 
(define getArrayStrandMessagesRec (openPCounter closePCounter pos textSrc currentMessage list)
  (try
    (seq
      (if (>= pos (invoke textSrc "length")) ; all the messages had been processed
          list ;return the list which contains all the strand nodes
          ;else, read nextCharacter
          (let ( (currentChar (invoke textSrc "charAt" pos))
                )
             (if (and (= openPCounter closePCounter)      
                      (or (= currentChar (char 44)) ;char 44 = ,
                          (= currentChar (char 124)))) ; vertical bar character
                 ;then part: a message has been read. Add it to the list and call function
                 (let ( (copyCMessage (object ("java.lang.String" currentMessage )))
                       )
                   (if (= currentChar (char 124))
                       (seq  
                         (if (!= (invoke copyCMessage "replace" " " "") "nil")
                              (invoke list "add" currentMessage)
                         )
                                                  
                         (invoke list "add" (concat "" currentChar))
                         (apply getArrayStrandMessagesRec openPCounter closePCounter (+ pos (int 1)) textSrc "" list)
                       )
 
                        (if (!= (invoke copyCMessage "replace" " " "") "nil")
                             (seq 
                               (invoke list "add" (apply quitInitialBlank currentMessage))
                               (apply getArrayStrandMessagesRec openPCounter closePCounter (+ pos (int 1)) textSrc "" list)
                             )
                            (apply getArrayStrandMessagesRec openPCounter closePCounter (+ pos (int 1)) textSrc "" list)
                        ) ;end if
                  ) ;end if
                 )
                 ;else, check if it is a ( or ) or another character
                 (if (= currentChar  (char 40))
                    ; update openned parentesis counter  and currentMessage  
                    (apply getArrayStrandMessagesRec (+ openPCounter (int 1)) closePCounter (+ pos (int 1)) textSrc
                       (concat currentMessage currentChar) list)
                    (if (= currentChar (char 41)) 
                      ; update closed parentesis counter  and currentMessage
                      (apply getArrayStrandMessagesRec openPCounter (+ closePCounter (int 1))  (+ pos (int 1)) textSrc
                             (concat currentMessage currentChar) list)
                      ; update only the current position and currentMessage
                      (apply  getArrayStrandMessagesRec openPCounter closePCounter (+ pos (int 1)) textSrc
                              (concat currentMessage currentChar) list) 
                    ) ;end if 
                 ) ; end else part
             ) ;end if    
          ) ;end else part
      ) ;end if >= pos (invoke textSrc "length")
    ) ;end let environment 
    (catch Exception
      (invoke java.lang.System.err "println" (concat "Error in getArrayStrandMessagesRec: " (invoke Exception "getMessage")))
    )
  ) ;end try
) ;end getArrayStrandMessagesRec


; This method creates an attributable object (called strandNode) that encapsulates
; each one of the messages of a strand.
;* Input:
;- messageList: arrayList whose elements are strings that correspond to the messages of a strand
;- strandNumber: given number to the strand which the messages belongs to. This number corresponds
;                to the order of the specific strand into the "current strands" information, i.e. 0
;                for the first strand, number 1 for the second strand and so on.
;- intruderStrand?: boolean value that indicates if the strand is an intruder strand (true) or not (false)
;* Output
;- an arrayList whose elments are attributable objects called strandNodes, that encapsulate a message
;  of a strand.

(define buildStrandNodesList (messageList strandNumber intruderStrand? ) ;messageList is an arrayList
  (try
   (let ( 
          (temporalPosition (invoke messageList "indexOf" (concat "" (char 124)) ) )
          (strandNodeList (object ("java.util.ArrayList")))
         )
    (seq
     (for message messageList
       (let ( (messageCopy (invoke (object ("java.lang.String" message)) "replace" " " ""))
             )
        (if (and (!= messageCopy "nil")
                 (!= messageCopy (concat "" (char 124))))
            (let ( 
                  (messageRank  (invoke messageList "indexOf" message))
                  (firstFuture? (if (and (= messageRank (+ temporalPosition (int 1)))     
                                         (>= temporalPosition (int 1)))
                                  (boolean true)
                                  (boolean false)))
                  
                   (time (if (<=  messageRank temporalPosition)
                            "past"
                            "future"))                                   
                  
                  (strandRank  (if (and (= time "future")
                                        (> messageRank (int 0)))
                                   (- messageRank (int 1))
                                   messageRank))

                  (strandNode (apply mkStrandNode message strandRank strandNumber intruderStrand? firstFuture? time))
                 )
         (seq
           (invoke strandNodeList "add" strandNode)
         ) ;end seq
        ) ;end let environment
       ) ;end if the message is not discarded (nil or vertical bar)
       ) 
      ) ;end for
       strandNodeList
    ) ;end seq 
   ) ;end let environment
   (catch Exception
     (seq
      (invoke java.lang.System.err "println" (concat "Error in buildStrandNodesList: " (invoke Exception "getMessage")))
      (object ("java.util.ArrayList"))
     )
   )
  ) ;end try
) ;end buildStrandNodesList


;This method deletes the signe and other parenthesis from the string representation of a message
;* Input
;- message: string with the text of a message of a strand, including the signe
;* Output
;- a string with the message text itself, without the signe and the parenthesis

(define getShortMessage ( message )
 (try
  (let( (copy (object ("java.lang.String" message)))
        (iniPos (+ (invoke message "indexOf" "(") (int 1))) ;1 or 2?
        (endPos (invoke message "lastIndexOf" ")"))  
       )
    (seq
      (invoke message "substring" iniPos endPos)
    )
  )
  (catch Exception
      (invoke java.lang.System.err "println" (concat "Error in getShortMessage: " (invoke Exception "getMessage")))
   )
  ) ;end try
) ;end getShortMessage

;********************* end new functions for messageMatrix
;*******************************************

;format the strand deleting characters we don't need
;*Input:
;-text: string which contains the informatino related with only one strand without any format
;*Output
;-the same text without some characters we don't need to process the strand

(define formatStrandString (text)
  (let( 
        (posIni (+ (invoke text "indexOf" ":: [") (int 4)))
        (text1 (invoke text "substring" posIni (- (invoke text "length") (int 1) )))
        (text2 (invoke text1 "replace" " " ""))
        (text3 (invoke text2 "replace" "nil," ""))
        (text4 (invoke text3 "replace" "nil" ""))
        (text5 (invoke text4 "replace" "]" ""))
        (text6 (if (= (invoke text5  "startsWith" "|")  (boolean true))
                    (invoke text5 "replace" "|" "") ;the strand does not have past messages
                    (invoke text5 "replace" "|" ",")))
       )
    text5
  )
)  ; end formatStrandString


; This method obtains the nodes of a strand, given as a string
;*Inputs:
;-strand: string which represents a strand
;-currentStrand: number of the strand which is going to be processed
;-intruder?: is the strand a regular or an intruder strand?
;*Ouput:
;-a list whose elements are strand's nodes

(define getNodesFromStrand (strand currentStrand intruder? posPast )
 (let(
         (nodesList (object ("java.util.ArrayList"))) ;this list will contain the nodes that correspond to an strand
         (res  (apply  getNodesFromStrandRec  (int 0) (int 0)  (int 0) strand "" (int 0) nodesList currentStrand intruder? posPast (boolean false)))
      )

   res

 )
)  ; end getNodesFromStrand
 

;this method splits a strand in substring which correspond to each of its messages (each message is a strand node) by
;matching opening parentesis with closing. Once a message has been read, a new strand node is created and added to a list
;*Inputs:
;-openPCounter: number of opened parentesis found in the current line
;-closePCounter: number of opened parentesis found in the current line
;-pos: current position of the textSrc that we are processing
;-textSrc: string which contains the entire strand string
;-currentMessage: current formatted text that will be displayed later. In each iteration we add the current char we are processing
;-Rank: position of the message (and the new strand node too) in the strand
;-nodesList: list that contains the strand nodes
;-strandNumber: number of the current strand
;-intuder?: is the strand a regular or an intruder strand?
;-posPast: position of the bar that separates past and future messages at the strand text. If its value is 0, then there are
;          no messages from the past.
;-future?: to guess if we are we still parsing messages from the past. 
;          In this case, its value will be false,
;          Otherwise, its value will be true.

(define getNodesFromStrandRec (openPCounter closePCounter pos textSrc currentMessage Rank nodesList strandNumber intruder? posPast future?)
(try

 (let (
        (srcLength (invoke textSrc "length")) 
        )
(seq
 ( if (>= pos srcLength)  ; all the messages had been processed
   nodesList ;return the list which contains all the strand nodes
       
   ( let ( (currentChar (invoke textSrc "charAt" pos))
          )
    (if (and (= openPCounter closePCounter)
             (or (= currentChar (char ',')) (= pos (- srcLength (int 1)) ))) ; a message has already been read. 
        (let (    ;if this is the first node with a message in the future, the tool should paint a line-separator in the strand
                  (firstFuture? (apply isFirstFutureNode pos posPast future?))
                  (time (apply getTimeNode pos posPast))
;                 Create a new strand node 
                  (strandNode (apply mkStrandNode currentMessage Rank strandNumber intruder? firstFuture? time))
              )
          (seq
   
           (invoke nodesList "add" Rank strandNode ) ;add the strand node to the nodesList
           ; continue processing the others messages
           ( apply  getNodesFromStrandRec  (int 0) (int 0) (+ pos (int 1)) textSrc "" (+ Rank (int 1)) nodesList  strandNumber intruder? posPast firstFuture? )
          )
        ) ; end of part "then"
        (if (= currentChar (char '('))
                ; update openned parentesis counter  and currentMessage  
             (apply  getNodesFromStrandRec (+ openPCounter (int 1)) closePCounter (+ pos (int 1)) textSrc
                (concat currentMessage currentChar) Rank nodesList strandNumber intruder? posPast future? )        
            (if (= currentChar (char ')'))
                 ; update closed parentesis counter  and currentMessage
                 (apply  getNodesFromStrandRec openPCounter (+ closePCounter (int 1))  (+ pos (int 1)) textSrc
                         (concat currentMessage currentChar) Rank nodesList strandNumber intruder? posPast future?)
                 ; update only the current position and currentMessage
                 (apply  getNodesFromStrandRec openPCounter closePCounter (+ pos (int 1)) textSrc (concat currentMessage currentChar)
                         Rank nodesList strandNumber intruder? posPast future?) 
             ) ;end if
         ) ;end if
     ) ;end if
    ) ;end of part else of first if
  ) ;end if
) ;end seq
)
(catch Exception
  (invoke java.lang.System.err "println" (concat "error " Exception) ))
)
) ;end  getNodesFromStrandRec


(define isFirstFutureNode ( pos posPast future?)
   ( if (and (and  (!= posPast (int 0))
             (> pos posPast) )
             (= future? (boolean false)))
       (boolean true)
       (boolean false)
    ) ;endIf
) ;end isFirstFutureNode

(define getTimeNode (pos posPast)
  (if (and  (!= posPast (int 0))
             (<= pos posPast) )
      "past"
      "future"
  )
) ;end isPastNode

; this method builds a new strand node with the necessary information
;*Inputs:
;-message: strand's message that corresponds to this node.
;-strandRank: ; Rank in the strands space, i.e. the vertical position in the strans space.
           ; It can be seen as the order in which the node will be drawn.
           ; initially its value is the rank in its strand (see Strand Layout document form John Ramsdell)
;strandNumber: number of the strand which the node belongs to
;intruder?: is the strand which the node belongs to an intruder strand?
;*Outputs:
;-node with the necessary information as attributes

(define mkStrandNode (message strandRank strandNumber intruder? firstFuture? time)
  (let(
       (strandNode (object("g2d.glyph.Attributable"))) ; main object that encapsulates a message of a strand
       (trimmedMessage (invoke (object ("java.lang.String" message)) "replace" " " ""))
      )
  (seq
    ;set attributes
    (setAttr strandNode "strandRank" strandRank) ; rank within the strand.
    (setAttr strandNode "strandNumber" strandNumber)
    (setAttr strandNode "predecessor" (object null) ) ; predecessor node from other strand
    (setAttr strandNode "sucessor" (object null))  ; sucessor node from other strand
    (setAttr strandNode "spaceRank" strandRank)
    (setAttr strandNode "message" trimmedMessage)
    (setAttr strandNode "originalMessage" message)
    (setAttr strandNode "intruder" intruder?)
    (setAttr strandNode "firstFuture" firstFuture?) 
    (setAttr strandNode "time" time)

    strandNode
   )
  )
) ;end mkStrandNode



; this method creates a hashtable that contains the messages of this state and the node that sent or received this message.
; The node is given as a Point object, whose X coordenate is the strand number which it belongs to, and whose Y coordenate is the
; Rank of the node in its strand.
; The message is the key and the node is the value of the hash table.
; *Inputs
;- strandsMatrix: matrix with the nodes created using the current Strands information
; *Output
;- a hashtable whose keys are strand messages and whose values are the nodes which sended or received this message.

(define  mkMessageMatrix (strandsMatrix)
 (try
  (let(
       (messageMatrix (object("java.util.Hashtable")))
       )
    (seq
     (for strand strandsMatrix
         (for strandNode strand
             (let( (message (getAttr strandNode "message"))  ;get the message
                   (strandNumber (getAttr strandNode "strandNumber")) ;get the number of the strand that sended or received the message
                   (strandRank (getAttr strandNode "strandRank")) ;get the node's Rank in the strands space.
                   (position (object("java.awt.Point" strandNumber strandRank))) ; ;create a Point object to represent the strand node.
                  )
               (invoke messageMatrix "put" message  position) ;add a new element to the hashtable 
             )
          ) ;end for
       ) ;end for

     messageMatrix
     ) ;end seq
   )
  (catch Exception
       (invoke java.lang.System.err "println" (concat "Error in mkMessageMatrix: " (invoke Exception "getMessage")))
  )
  ) ;end try
) ;end mkMessageMatrix


; this function builds the precedences among the strands nodes, taking into account which node sent a message and which one received it
; by setting for each node its successor or predecessor node (only one of both can be disctinct of null) in a different strand and it's
; successor and predecessor node in its strand.
; *Inputs
;-strandsMatrix: matrix with the nodes created using the current Strands information
;-messageMatrix: hash table which contains, for each message in each strand, (i.e for all the messages in the strands space) the node which
;                sent or received this message.
;*Outputs:
;-an updated version of strandsMatrix, in which every node has information about it predecessor or successor node in other strands.

(define setPrecedences (strandsMatrix messageMatrix)
(seq
    (try
      (for strand strandsMatrix
           (for strandNode strand
                 (let ( (message (getAttr strandNode "message"))
                       (inverseMessage (apply getInverseMessage message))
                       ; guess if the node sends a message or receives it so that we know if we have to find it
                       (senderNode? (invoke message "startsWith" "+" ))
                     
                       ;; get the successor node (in a different strand) if it exists. If it exists, the node will be the one located
                       ;; at strandsMatrix [successorPosition.x][succesorPosition.y]
                       (successorNode (if (= senderNode? (boolean true))
                                          (apply getSuccessorNode strandNode senderNode? inverseMessage strandsMatrix messageMatrix)
                                          (object null)))

                       ;; get the predecessor node (in a different strand) if it exists. If it exists, the node will be the one located
                       ;; at strandsMatrix [predecessorPosition.x]predecessorPosition.y]
                       (predecessorNode (if (= senderNode? (boolean false))
                                            (apply getPredecessorNode strandNode senderNode? inverseMessage strandsMatrix messageMatrix)
                                            (object null)))
                     )
                   (seq
                      ; set precedences
                      (if (!= successorNode (object null))
                          (seq
                            (setAttr strandNode "successor" successorNode)
                          )
                      ) ; end if
                      
                      (if (!= predecessorNode (object null))
                        (seq
                          (setAttr strandNode "predecessor" predecessorNode)
                        )
                      ) ; end if
                   ) 
                 )
            ) ; end for
       ) ; end for
  (catch Exception
       (invoke java.lang.System.err "println" (concat "Error in setPrecedences: "  (invoke Exception "getMessage")))
  )
  ) ;end try
      strandsMatrix
        

     ) ; end seq
 ) ; end setPrecendences


; given a message, this function obtains the inverse message, i.e: if the given message was a message sended by a node (it starts with a "+")
; the same message is returned, but as if it was a message received by a node, starting with "-" sign.
; *inputs
;-message: original message whose inverse we want to obtain
; *output
;- the inverse message of the given message

(define getInverseMessage (message)
 (try
  ( let ( (signe (invoke message "charAt" (int 0))) ;get the signe (- or +)
          (unsignedMessage (invoke message "substring" (int 1))) ;get only the message, without any signe
         )
    (seq
     ;obtain the inverse signe, and return the message preceded by the inverse signe
     (if (= signe (char '+') )
         (concat "-" unsignedMessage)
         (concat "+" unsignedMessage)
      ) ;end if
     ) ;end seq
   )
    (catch Exception
       (invoke java.lang.System.err "println" (concat "Error in getInverseMessage: " (invoke Exception "getMessage")))
  )
  ) ;end try
) ;end getInverseMessage

;;;*************************************************************************

;This method obtains the position of a message within its corresponding strand
;* Input:
;- message: specific message whose position at its corresponding strand we want to obtain
;- messageList: arrayList with all the  messages of a strand
;* Output
;- integer number that denotes the position of this message within its corresponding strand

(define getPositionByMessage(message messageList)
  (invoke messageList "get" message)
) ;end getPositionByMessage



; This method obtains the strandNode from the strandsMatrix object allocated
; at the specific symbolic position
;* Input
;- strandsMatrix: object that contains all the messages of all the strands
;                 of a Maude-NPA state.
;- position: symbolic position of the strandNode we want to obtain
;* Output
;- strandNode allocated at the given position of the given strandsMatrix

(define getStrandNodeByPosition (strandsMatrix position)
  (let ( (xCoord (lookup position "x")) ; symbolic x-coordinate
         (yCoord (lookup position "y")) ; symbolic y-coordinate 
        )
      (invoke  (invoke strandsMatrix "get" xCoord) "get" yCoord)
  ) 
) ;end getStrandNode


; This method updates the strandsMatrix with the messages of the strands of the
; attack state
;* Input
;- nid: id of the Maude-NPA state whose strands will be displayed
;* Output
;- storage object with the updated strandsMatrix
(define updateWithAttackStrand (nid)
  (try
  (let ( ;get data structure
         (storageObject (fetch (concat "storageObject" nid)))
         (strandsMatrix (getAttr storageObject "matrix"))
         (intruderList (getAttr storageObject "intruderList"))
         (messageList (getAttr storageObject "messageList"))
         
         (attackStrand (object ("java.util.ArrayList")))
         (nodes (invoke intruderList "keys"))
        )
   (seq
      (apply updateWithAttackStrandRec strandsMatrix intruderList attackStrand nodes
             messageList  (invoke strandsMatrix "size"))  
              
      (invoke strandsMatrix "add" attackStrand)

      (apply updateMessageMatrix strandsMatrix messageList)
      storageObject
    )
  )
    (catch Exception
       (invoke java.lang.System.err "println" "Error in updateWithAttackStrand: " (invoke Exception "getMessage"))
  )
  ) ;end try
 ) ;end addAtackStrand


(define updateWithAttackStrandRec (strandsMatrix intruderList attackStrand nodes messageList strandNumber)
  (if (= (invoke nodes "hasMoreElements") (boolean false))
      attackStrand
      (seq
        (let ( (element (invoke nodes "nextElement"))
               (value  (invoke intruderList "get" element))
              )
         (seq
              (if (and (= value (object("java.lang.Boolean" (boolean true)) ))
                      (and (= (invoke messageList "containsKey" (concat "-(" element")")) (boolean true))
                          (= (invoke messageList "containsKey" (concat "+(" element ")")) (boolean false))) )
                  (let ( (attackNode (apply mkStrandNode (concat "+(" element ")")
                                            (int 0) strandNumber (boolean false) (boolean false) "future" ))
 
                         (inverseMessage (apply getInverseMessage (getAttr attackNode "message")) )
                         (partnerPosition (apply getPositionByMessage inverseMessage messageList))
                         (partnerNode (apply getStrandNodeByPosition strandsMatrix partnerPosition))    
                        )
                    (seq
                     ; update information and precedences
                      (setAttr attackNode "spaceRank" (getAttr partnerNode "spaceRank"))
                      (setAttr attackNode "successor" partnerNode)
                      (setAttr partnerNode "predecessor" attackNode)
                      
                      (invoke attackStrand "add" attackNode) 
                    )
                  ) 
            ) ;end if
            (apply updateWithAttackStrandRec strandsMatrix intruderList attackStrand nodes messageList strandNumber)
          ) ;end seq
       ) ;end environment
      ) ;end seq
   ) ;end if
) ;end addAttackStrandRec




; (apply updateMessageMatrix strandsMatrixInitial messageMatrix)
(define updateMessageMatrix (strandsMatrix messageMatrix)
 ( let ( (lastLevel (invoke strandsMatrix "get" (- (invoke strandsMatrix "size") (int 1))))
        )
   (seq

    (for node lastLevel
        (let( (message (getAttr node "message"))  ;get the message
              (strandNumber (getAttr node "strandNumber")) ;get the number of the strand that sended or received the message
              (nodeNumber (getAttr node "strandRank")) ;get the node's Rank in the strands space.
              (position (object("java.awt.Point" strandNumber nodeNumber))) ; ;create a Point object to represent the strand node.
             )
              (invoke messageMatrix "put" message  position) ;add a new element to the hashtable
        )
     ) ;end for
     
      messageMatrix
    )
  )
) ;end updateMessageMatrix


;;;;******************************************************************************


; This function obtains the successor node in other strand of a given node.
; *Inputs:
;-node: node whose successor node we want to obtain
;-senderNode?: does the node send a message? If it does then it has got a successor node.
; Otherwise, it receives a message and it will have a predecessor node, but not a successor node.
;-inverseMessage: inverse of the node's message. In case the inverseMessage is received by a node, this node will be the
;                 successor node of the given node.
;- strandsMatrix: matrix with the nodes created using the current Strands information
;- messageMatrix: hash table which contains, for each message in each strand, (i.e for all the messages in the strands space) the node which
;                sent or received this message
; *Outputs
;- if the node has a successor node, it is returned. Otherwise, a null object is returned.

(define getSuccessorNode (node senderNode? inverseMessage strandsMatrix messageMatrix)
 (try
  ( let ( ; if the node sends a message  and the node that receives the message already exists, look for the node position
          ; which will receive it. Otherwise, it's null
          (successorPosition (if (= senderNode? (boolean true))
                                   (invoke messageMatrix "get" inverseMessage)
                                   (object null)))
        )
         (if (!= successorPosition (object null))
              (invoke (invoke strandsMatrix "get" (lookup successorPosition "x")) "get"(lookup successorPosition "y"))
              (object null))
  ) ;end let environment
    (catch Exception
      (seq
       (invoke java.lang.System.err "println" (concat "Error in getSuccessorNode: " (invoke Exception "getMessage")))
      ) ;end 
    )
  ) ;end try
 )  ; end getSuccessorNode



; This function obtains the predecessor node in other strand of a given node.
; *Inputs:
;-node: node whose predecessor node we want to obtain
;-senderNode?: does the node send a message? If it doesn't then it has got a predeessor node.
; Otherwise, it sends a message and it will have a predecessor node, but not a successor node.
;-inverseMessage: inverse of the node's message. In case the inverseMessage is received by a node, this node will be the
;                 successor node of the given node.
;- strandsMatrix: matrix with the nodes created using the current Strands information
;- messageMatrix: hash table which contains, for each message in each strand, (i.e for all the messages in the strands space) the node which
;                sent or received this message
; *Outputs
;- if the node has a predecessor node, it is returned. Otherwise, a null object is returned.

(define getPredecessorNode (node senderNode? inverseMessage strandsMatrix messageMatrix)
 (try
  (let (   ; if the node receives a message, look for the node position which sendt it. Otherwise, it's null
           (predecessorPosition (if (= senderNode? (boolean false))
                                     (invoke messageMatrix "get" inverseMessage)
                                     (object null)))
        )

        (if (!= predecessorPosition (object null))
            (invoke (invoke strandsMatrix "get" (lookup predecessorPosition "x")) "get"(lookup predecessorPosition "y"))
            (object null)
        )
   )
    (catch Exception
       (invoke java.lang.System.err "println" (concat "Error in getPredecessorNode: " (invoke Exception "getMessage")))
  )
  ) ;end try
) ;end getPredecessorNode



;this function calls other functions to order the nodes in the strands spaces to know when the node will be drawn.
; *Inputs
;-nid: node id whose strands we want to show.
; *Outputs
;-the updated storage object with the strands nodes ordered

(define orderStrandsVertically (nid)
 (try 
  (let (
         ;The first phase stretches strands so as to eliminate up-wardly sloping arrows
         (storageObject (apply orderPhaseI nid))
         ;the second phase compresses the strands so as to eliminate some unnecessary stretching
         (storageObject2 (apply orderPhaseII nid)) 
       )
     storageObject2 ;return the updated storage object 
   )
   (catch Exception
     (seq
        (invoke java.lang.System.err "println" (concat "Error in orderStrandsVertically: " (invoke Exception "getMessage") ))
     )
   ) ;end catch
     
 ) ;end try
) ; end orderStrandsVertically


(define orderPhaseI (nid)
  (try
     (let (
           (storageObject (fetch (concat "storageObject" nid)))
           (strandsMatrix (getAttr storageObject "matrix"))
           (todoList (apply mkTodoList strandsMatrix))
          )
           (apply orderPhaseIRec todoList nid)
     ) ;end try body
   (catch Exception
     (seq
        (invoke java.lang.System.err "println" "Error in order PhaseI")
     )
   ) ;end catch
  ) ; end try
) ;end orderPhaseI



(define orderPhaseIRec (todoList nid)
  (try
   (let ( ;obtain the storageObject corresponding to the node whose strands we want to display
         (storageObject (fetch (concat "storageObject" nid)))
         ;get strandsMatrix, the matrix with the nodes created using the current Strands information
         (matrix (getAttr storageObject "matrix"))
        )
     (seq
       (if (= (invoke todoList "size") (int 0))
           storageObject
           ( let( (currentNode (invoke todoList "get" (int 0)))
                  (predecessorNode (getAttr currentNode "predecessor"))
                 )
             (seq
               (invoke todoList "remove" (int 0))
               
               (if (!= predecessorNode (object null))
                  (seq
                   (if (< (getAttr currentNode "spaceRank") (getAttr predecessorNode "spaceRank"))
                       (seq
                         (setAttr currentNode "spaceRank" (getAttr predecessorNode "spaceRank"))
                    
                         (let ( (nextStrandNode (apply getNextStrandNode matrix currentNode))
                            )
                            (if (!= nextStrandNode (object null))
                                (apply linearizeRule nextStrandNode todoList matrix) 
                            )
                         ) ;end environment
                    
                         
                       ) ;end then part
                       (seq
                      
                         (setAttr predecessorNode "spaceRank" (getAttr currentNode "spaceRank"))
                         
                         (let ( (nextStrandNode (apply getNextStrandNode matrix predecessorNode))
                            )
                            (if (!= nextStrandNode (object null))
                                (apply linearizeRule nextStrandNode todoList matrix) 
                            )
                         ) ;end environment
                                                  
                       ) ;end else part
                   ) ;end if < comparation

                 ) ;end seq
               ) ;end if predecessorNode != null
               (apply orderPhaseIRec todoList nid)
             ) ;end seq
           ) ;end else part
       ) ;end if
     ) ;end seq
    ) ;end environment
   (catch Exception
     (seq
    
         (invoke java.lang.System.err "println" "Error in order PhaseIRec")
         (invoke java.lang.System.err "println"  (invoke Exception "toString"))
       
     )
   ) ;end catch
  ) ;end try 
) ;end orderPhaseIRec



(define linearizeRule (strandNode todoList matrix)
(try
  (let (
        (previousStrandNode  (apply getPrevStrandNode matrix strandNode))
       )
   (seq
    (if (and (!= previousStrandNode (object null))
             (<= (getAttr strandNode "spaceRank")
                 (getAttr previousStrandNode "spaceRank")
             )
        )
        (seq 
           (setAttr strandNode "spaceRank" (+ (getAttr previousStrandNode "spaceRank") (int 1)) )
 
           (if ( != (getAttr strandNode "successor") (object null))
               (invoke todoList "add" (getAttr strandNode "successor"))
           ) ;end if exist strandNode's successor

           (let ((nextStrandNode (apply getNextStrandNode matrix strandNode))
               )
             (if (!= nextStrandNode (object null))
                (apply linearizeRule nextStrandNode todoList matrix)
                todoList ;finish
             )
           ) ;end environment
        ) ;end seq then part
        ;else part of first if
        todoList ;finish
    ) ;end if
   ) ;end seq
 ) ;end environment
     (catch Exception
     (seq
    
         (invoke java.lang.System.err "println" "Error in linearizeRule ")
         (invoke java.lang.System.err "println"  (invoke Exception "toString"))
       
     )
   ) ;end catch
  ) ;end try 
) ;end linearizeRule


(define orderPhaseII (nid)
(try
     (let (
           (storageObject (fetch (concat "storageObject" nid)))
           (strandsMatrix (getAttr storageObject "matrix"))
           (todoList (apply mkTodoList strandsMatrix))
          )
      (seq
           (apply orderPhaseIIRec todoList nid)
      ) ;end seq
     ) ;end try body
   (catch Exception
     (seq
        (invoke java.lang.System.err "println" "Error in order PhaseII")
     )
   ) ;end catch
  ) ; end try
) ;end orderPhaseII

(define orderPhaseIIRec (todoList nid)
  (try
   (let ( ;obtain the storageObject corresponding to the node whose strands we want to display
         (storageObject (fetch (concat "storageObject" nid)))
         ;get strandsMatrix, the matrix with the nodes created using the current Strands information
         (matrix (getAttr storageObject "matrix"))
        )
     (seq      
       (if (= (invoke todoList "size") (int 0))
           storageObject
           (let ( (currentNode (invoke todoList "get" (int 0)))
                  (nextStrandNode (apply getNextStrandNode matrix currentNode))
                  (successorNode (getAttr currentNode "successor"))
                  (minimumRank  (if (and (!= nextStrandNode (object null))
                                        (!= successorNode (object null)))
                                   (if (< (- (getAttr nextStrandNode "spaceRank") (int 1))
                                          (getAttr successorNode "spaceRank"))
                                       (- (getAttr nextStrandNode "spaceRank") (int 1))
                                       (getAttr successorNode "spaceRank")
                                   ) ;end then part 
                                    (if (!= nextStrandNode (object null))
                                        (- (getAttr nextStrandNode "spaceRank") (int 1))
                                        (if (!= successorNode (object null))
                                          (getAttr successorNode "spaceRank")
                                          (getAttr currentNode "spaceRank")
                                        ) ;end else part
                                    ) ;end else part

                                )) 
                               
                 ) ;end let bindings
             (seq
               (invoke todoList "remove" (int 0))
               (if (< (getAttr currentNode "spaceRank") minimumRank)
                   (seq
                     (setAttr currentNode "spaceRank" minimumRank)
                     (if (!= (getAttr currentNode "predecessor") (object null))
                         (invoke todoList "add"  (getAttr currentNode "predecessor") )
                     ) ;end if
                     (let ( (previousStrandNode (apply getPrevStrandNode matrix currentNode) ))
                       (if( != previousStrandNode (object null))
                           (invoke todoList "add" previousStrandNode)
                       )
                     ) ;end environment
                   ) ;end seq then part
               ) ;end if
               (apply orderPhaseIIRec todoList nid)
             ) ;end seq
           ) ;end then part
       ) ;end if
     ) ;end seq
    ) ;end environment
   (catch Exception
     (seq
        (invoke java.lang.System.err "println" "Error in order PhaseIIRec")
        (invoke java.lang.System.err "println" (invoke Exception "getMessage"))
     )
   ) ;end catch
  ) ;end try 
) ;end orderPhaseIIRec

 



;function to update a node spaceRank when it has a predecessor node in other strand. After the update, it calls
;orderPhaseIRec again to continue ordering. Note that the node and its predecessor must have the same spaceRank value.
; * Inputs
;- todoList: list which contains the nodes that must be ordered
;- nid: id of the node whose strands we want to show.
; * Outputs
;- the updated storage object

(define predecessorCase (todoList nid)
 (try
   (let ( 
         (strandNode (invoke todoList "get" (int 0)))
         (predecessorNode (getAttr strandNode "predecessor"))
         (predRank (getAttr predecessorNode "spaceRank"))
        )
     (seq
              (invoke todoList "remove" (int 0)) ;update todoList
              (if (< (getAttr strandNode "spaceRank") predRank) ;update node spaceRank
               (seq
                   (setAttr strandNode "spaceRank" predRank) ;stretch
                   (apply linearizeRule todoList strandNode nid)
               ) ;end then part
               (if (> (getAttr strandNode "spaceRank") predRank) ;update predecessor node spaceRank  
                   (seq
                      (setAttr predecessorNode "spaceRank" (getAttr strandNode "spaceRank")) ;stretch
                      (apply linearizeRule todoList predecessorNode nid)

                  ) ;end then part

               ) ;end if
	   ) ; end if 
	   (apply orderPhaseIRec todoList nid) 
       )
   )
   (catch Exception
     (seq
        (invoke java.lang.System.err "println" (concat "Error in predecessorCase: " (invoke Exception "getMessage")))
     )
   ) ;end catch
   
  ) ;end try
) ;end predecessorCase



;function to update a node spaceRank when it has a successor node in other strand. After the update, it calls
;orderPhaseIRec again to continue ordering. Note that the node and its successor must have the same spaceRank value.
; * Inputs
;- todoList: list which contains the nodes that must be ordered
;- nid: id of the node whose strands we want to show.
; * Outputs
;- the updated storage object

(define successorCase (todoList nid)
(try
 (let ( 
         (strandNode (invoke todoList "get" (int 0)))
         (successorNode (getAttr strandNode "successor"))
         (succRank (getAttr successorNode "spaceRank"))
     )
     
     (seq
      (invoke todoList "remove" (int 0)) ;update todoList
        (if (< (getAttr strandNode "spaceRank") succRank)
            (seq
               (setAttr strandNode "spaceRank" succRank) ;stretch
	       (apply linearizeRule todoList strandNode nid)
	   )
	   
	 ) ;end if
	 (apply orderPhaseIRec todoList nid) 
     )
 )
    (catch Exception
     (seq
        (invoke java.lang.System.err "println" (concat "Error in successorCase: " (invoke Exception "getMessage")))
     )
   ) ;end catch
 ) ;end try
)
;end successorCase


; this function builds the todo list with the strands nodes that must be ordered, i.e, the
; strands nodes contained in the strands matrix
; *Input
;- matrix: matrix which contains all the strands nodes with the necessary data to display them.
; *Output
;- list with the strand nodes.

(define mkTodoList (matrix)
(try
 (let (
         (list (object ("java.util.ArrayList")))
        )
    (seq
     (for strand matrix ;for each strand
      (for node strand ;for each strand node
            (invoke list "add" node) ;add the node to list
       ) ; end for
      ) ; end for
     list
    ) ; end seq
 )
    (catch Exception
     (seq
        (invoke java.lang.System.err "println" (concat "Error in mkTodoList: " (invoke Exception "getMessage")))
     )
   ) ;end catch
 ) ;end try
) ; end mkTodoList



; this function calculates the maximum value of two given numbers
(define calculateMax (a b)
  (if (< a b )
       b
       a
  )
) ; end calculateMax



; this function gets the previous strand node of a given node.
; *Inputs:
;- matrix: matrix which contains all the strand nodes
;- node: node whose previouse strand node we want to obtain
; *Output
;-the previous Strand Node (i.e, the next node in the strand)  or, if it does not exists a null object.

(define getPrevStrandNode (matrix node)
(try
  (if (= (getAttr node "strandRank") (int 0))
      (object null)
      ( invoke (invoke matrix "get" (getAttr node "strandNumber"))  "get" (- (getAttr node "strandRank") (int 1))) )
   (catch Exception
     (seq
        (invoke java.lang.System.err "println" (concat "Error in getPrevStrandNode: " (invoke Exception "getMessage")))
     )
   ) ;end catch
 ) ;end try
) ; end getPrevStrandNode



; this function gets the next strand node of a given node.
; *Inputs:
;- matrix: matrix which contains all the strand nodes
;- node: node whose previouse strand node we want to obtain
; *Output
;-the next Strand Node (i.e, the next node in the strand)  or, if it does not exists a null object.

(define getNextStrandNode (matrix node)
(try
 (if (>= (getAttr node "strandRank") (- (invoke ( invoke matrix "get" (getAttr node "strandNumber")) "size") (int 1)) ) ;;;excepcion get
                            (seq
            
                             (object null)
                             )
 
                             (invoke ( invoke matrix "get" (getAttr node "strandNumber")) "get"
                                     (+ (getAttr node "strandRank") (int 1 ) ))
                             
 ) ; end if
      (catch Exception
     (seq
    
         (invoke java.lang.System.err "println" "Error in getNextStrandNode ")
         (invoke java.lang.System.err "println"  (invoke Exception "toString"))
     )
   ) ;end catch
  ) ;end try 
) ; end getNextStrandNode

 

; method to build a list which will contain for each space Rank a list with the nodes placed there.
; The node's spaceRank attribute is not modified, but it will be modified at the
; ordering algorithms
; *Input
; -nid: id of the node we want to display
; *Output
; -an array list which contains, for each position i>=0, the nodes whose space Rank value is i.

(define mkLevelsList (nid)
(try
  (let ( (storageObject (fetch (concat "storageObject" nid)))
         (strandsMatrix (getAttr storageObject "matrix"))
         (levelsList (object ("java.util.ArrayList")))     
        )
    (seq
      (for strand strandsMatrix
          (for strandNode strand
           (let ( (index (getAttr strandNode "spaceRank"))
                 )
             (seq
                                                          
                 (if (>= index (invoke levelsList "size")) ; the spaceRank is greater than the list
                                                   ; size and this position does not still exists.
                     (apply mkFillList levelsList index))   ;so fill the list with empty positions

                     (let( (listAux  (invoke levelsList "get" index)))
                       (seq
                        (invoke listAux "add" strandNode)    ;add the node to this level list
                       )
                     )
             ) ;end seq
           )
         ) ;end for
      ) ;end for
      levelsList
    ) ;end seq
   )
     (catch Exception
     (seq
        (invoke java.lang.System.err "println" (concat "Error in mkLevelsList: " (invoke Exception "getMessage")))
     )
   ) ;end catch
 ) ;end try
) ;mkLevelsList



;this function orders the level's list to leave only two nodes as much, in each level.
; This is necessary to avoid overlappings between different couples of nodes (in each couple,
; a node sends a message and another node receives it). There may be only one node in a level too.
; *Input
; -nid: id of the node we want to display
; *Output
;-the updated level's list with, as much, two nodes in each level.

(define orderLevelsList (nid)
(try
  ( let ( 
          (levelsList (apply mkLevelsList nid))
         )
     (apply orderLevelsListRec levelsList  (int 0))
 
   )
     (catch Exception
     (seq
        (invoke java.lang.System.err "println" (concat "Error in orderLevelsList: " (invoke Exception "getMessage")))
     )
   ) ;end catch
 ) ;end try
) ;end orderLevelList

(define  showLevelsList (matrix)
  (seq
    ;for each displaying space level draw each node
	 (for level matrix
            (seq
             (invoke java.lang.System.err "println" "***************************")
             (for strandNode level
                (seq
                 (invoke java.lang.System.err "println" (getAttr strandNode "message"))
                 (invoke java.lang.System.err "println" (concat "Spacerank:" (getAttr strandNode "spaceRank")) )
                ; (invoke java.lang.System.err "println" )

                 )
              )
            )
           )
        )
  ) ;end showLevelsList

;This function checks if there are more than two nodes at some levelsList level.
; In this case, a node is chosen and if it has a node which receives a message from the choosen node or
;sends a message to the choosen node and both are added to an empty list.
;Then, the levelsList is splitted into two list:
     ;-listHead: contains the levels lower than the level of the processed node
     ;-listTail: contains the levels greater than the level of the processed node.
;In addition to this, there is another list
; *Input
;-levelsList: current levelsList. Some of its levels have already been processed but some other have not.
;-pos: current level of the levelsList which is going to be processed
; *Output
;-updated levelsList with all the levels processed.

(define orderLevelsListRec (levelsList pos)
(try
(seq
 (if (>= pos (invoke levelsList "size")) ; all the levels have already been processed. The function has finished. 
        levelsList ;return the last version of the levels list
         ;else
         (let ( (currentLevel (invoke levelsList "get" pos)))
             (if (= (invoke currentLevel "size") (int 0))
                (apply orderLevelsListRec levelsList (+ pos (int 1)))
                ;else
                 (let (   (strandNode (invoke currentLevel "get" (int 0))) ;get a strandNode
                          ;partnerNode is the successor or the predecessor of the node we are going to process
                           (partnerNode ( if (!= (getAttr strandNode "predecessor") (object null)) 
                                                 (getAttr strandNode "predecessor")
                                                (if (!= (getAttr strandNode "successor") (object null))
                                                        (getAttr strandNode "successor")
                                                        (object null)
                                         )))   
	               ) ;end let bindings
                     (if (>= (invoke currentLevel "size") (int 2)) ;there are more than two nodes at a level.                                 
                       (let ( (listHead (object ("java.util.ArrayList" levelsList)))
                                (listTail (object ("java.util.ArrayList" levelsList)))
                                (partnerIndex (if (!= partnerNode (object null))
                                                  (seq
                                                     (invoke currentLevel "indexOf" partnerNode)
                                                  )
                                                 (int -1))) 
                               (emptyList (object ("java.util.ArrayList")))
                               (resultList (object ("java.util.ArrayList")))
                              )
                       (seq
                         (setAttr strandNode "spaceRank" pos) ;update strandNode's spaceRank
                         (invoke emptyList "add" strandNode)
                         (invoke currentLevel "remove" (int 0))
		 
			 
                         (if (!= partnerNode (object null))
                           (seq
                            (setAttr partnerNode "spaceRank" pos) ;update partnerNode's spaceRank
                            (invoke emptyList "add" partnerNode)
			    (if (!=  partnerIndex (int -1))
				(invoke currentLevel "remove" (- partnerIndex (int 1))) 
			    )

                         )) ;end if
                         
                        ;get the levels lower than the current level from the initial levelList
                        (apply removeRange listHead pos (- (invoke listHead "size") (int 1)))
                        ;get the levels greater than the current level from the initial levelList
                        (apply removeRange listTail (int 0) pos)

  		       
		        (if (> (invoke listHead "size") (int 0))
		          (invoke resultList "addAll" listHead)) ;end if

                        (invoke resultList "add" emptyList)
 
                        (if (> (invoke currentLevel "size") (int 0))
		          (invoke resultList "add" currentLevel) )
 
                        (if (> (invoke listTail "size") (int 0))
		          (invoke resultList "addAll" listTail))

                        (apply orderLevelsListRec resultList (+ pos (int 1)))
                      )
                    ) ;end let then part
                   ;else, there is only one or two strandNodes in the current level
                   (seq
                     (setAttr strandNode "spaceRank" pos)
                     (apply orderLevelsListRec levelsList  (+ pos (int 1)))
	           )
                ) ;end if (if (>= (invoke currentLevel "size") (int 2))
        ) ;end let else part
    ) ;end  (if (= (invoke currentLevel "size") (int 0))
   ) ;end let main else part
  ) ;end main if
 ) ;end main seq
   (catch Exception
     (seq
        (invoke java.lang.System.err "println" (concat "Error in orderLevelsListRec: " (invoke Exception "getMessage")))
     )
   ) ;end catch

 ) ;end try
 ) ;end orderLevelsListRec



;this function fills a list with empty elements until the index "index".
; *Input
;-list: list which we want to add empty elements to.
;-index: list's index until we want to add empty elements
; *Output
;-updated list with the empty elements added. Its size now is index+1

(define mkFillList (list index)
(try
 (let ((ntimes (+ (- index (invoke list "size")) (int 1))) ;number of empty elements that must be added
      )
    (apply mkFillListRec list ntimes (int 0))
 )
    (catch Exception
     (seq
        (invoke java.lang.System.err "println" (concat "Error in mkFillList: " (invoke Exception "getMessage")))
     )
   ) ;end catch
 ) ;end try
) ;end mkFillList



;this function fills a list with empty elements until the index "index".
; *Input
;-list: list which we want to add empty elements to.
;-ntimes: number of empty elements that must be added
;-current: number of empty elements already added
; *Output
;-updated list with one more empty element added

(define mkFillListRec (list ntimes current)
(try
 (let ( (emptyList (object ("java.util.ArrayList")))
        )
    (if (= ntimes current)
        list
        (seq
          (invoke list "add" emptyList)
          (apply mkFillListRec list ntimes (+ current (int 1)))
        )
    )
   )
    (catch Exception
     (seq
        (invoke java.lang.System.err "println" (concat "Error in mkFillListRec: " (invoke Exception "getMessage")))
     )
   ) ;end catch
 ) ;end try
 ) ;end nkFillListRec


;function to remove  from a list all of the elements whose index is between ini and end indexes, both included
; *Input:
;-list: list some of whose elements are being removed
;-ini: index from which the list elements will be removed, itself included
;-end: index of the last list element that will be removed
; *Output
;-new list without the elements included in the rang limited by ini and end

(define removeRange (list ini end)
(try
  (let( (ntimes (+ (- end ini) (int 1))))
    (apply removeRangeRec list ini ntimes (int 0))
   )
     (catch Exception
     (seq
        (invoke java.lang.System.err "println" (concat "Error in removeRange: " (invoke Exception "getMessage")))
     )
   ) ;end catch
 ) ;end try
 ) ;end removeRange


;function to remove an element of the list
; *Input
;-list: list some of whose elements are being removed
;-ini: index of the element that we want to remove.
;-ntimes: number of elements that must be removed.
;-current: current number of elements that have already been removed
; *Output
;-the list with one less element.

(define removeRangeRec (list ini ntimes current)
(try
  (if (>= current ntimes)
      list
      (seq
        (invoke list "remove" ini) ;remove element
        (apply removeRangeRec list ini ntimes (+ current (int 1))) ;update number of elements already removed
       )
  )
     (catch Exception
     (seq
        (invoke java.lang.System.err "println" (concat "Error in removeRangeRec: " (invoke Exception "getMessage")))
     )
   ) ;end catch
 ) ;end try
) ;end removeRangeRec

;) ; end file



;;;;;;;;;; ./makeg2dlib loaded ../GUI/npa-drawStrands.lsp ;;;;;;;;;;

;;;****** global definitions
(define labelFontSize (int 13))

;defines to split a message in more than one line
(define numCharsPerSpace (int 25)) ;19
(define lengthTextSpace (int 70)) ;130

; horizontal space between strands
  (define hSpace (int 200) )   ;220

; vertical space between nodes within a strand 
 (define vSpace (int 90)) ;80 before change

; margin-left value to apply to the frame that shows the strands
 (define marginLeft (int 90))

; margin-left value to apply to the frame that shows the strands
; (define marginTop (int 210))
 (define marginTop (int 80)) ;40

;;****; start defining functions

; main function to draw the strands of a node
; *input:
; - nid: node's id
; *output:
; - display the strands in a separate frame
 (define drawStrands (node graphFrame graphTitle)
  (seq
      
   (let ( (nid (getAttr node "nid"))
          (pframe (apply getPFrameFromFLItem node))
          ; data structure which stores all the information related with strands
          (storageObject (fetch (concat "storageObject" nid)))
          ;matrix contains, for each strands, its nodes
          (matrix (getAttr storageObject "matrix"))
          ;intruderList stores information related with the intruder's knowledge
          (intruderList (getAttr storageObject "intruderList"))
          ;levelsList is a list which contains the nodes for each level in which the displaying space is divided
          (levelsList (getAttr storageObject "levelsList"))
          ;graphical components   
         
         
          (bComponent (apply getCaption)) ;caption component
         ; strands will be drawn over an IOPView, so that the user will be able to zoom in or zoom out it.
    ;      (baseComponent (object ("g2d.swing.IOPView"
	;	     (boolean true)
		;     (boolean true))))
	  (baseComponent (object ("mnpa.myIOPView"
		     (boolean true)
		     (boolean true))))

	 
          
          (frame (object ("g2d.mwa.MWAFrame" (concat (concat graphTitle " - ")
                                                     (concat "Strands Visualization node " (getAttr node "label")))
                                                     pframe  ))) 
         
          ;to leave an empty space at the bottom of the frame
          (emptyPanel  (let ( (panel (object ("javax.swing.JPanel" )))  )
                         (seq
                            (invoke panel "setBackground" white)
                            (invoke panel "setSize" (int 900) (int 60)) ;frame size
                            (invoke panel "setMinimumSize" (object ("java.awt.Dimension"  (int 900)   (int 60))))
                            (invoke panel "setMaximumSize" (object ("java.awt.Dimension"  (int 900)   (int 60))))
                            panel 
                        )))
          ;window event to execute when the frame is closed
         (windowListener (object ("g2d.closure.ClosureWindowListener")))

         (lastLevel (invoke levelsList "size"))

 ; setJMenuBar(JMenuBar menubar) 
	 (menuBar (object ("javax.swing.JMenuBar")))
	 (fileMenu (object ("javax.swing.JMenu" "File")))
         (shMenu (object ("javax.swing.JMenuItem" "Hide Caption")))
         (shActionListener (object ("g2d.closure.ClosureActionListener" (apply showHideCaption frame bComponent shMenu))))
     
	 
	 (sStrandPicMenu (object ("javax.swing.JMenuItem" "Save picture")))
         (sStrandPicActionListener (object ("g2d.closure.ClosureActionListener" (apply saveStrandPicture baseComponent frame))))
	 
     )
     (seq
      
      
      (invoke shMenu "addActionListener" shActionListener)
      (invoke sStrandPicMenu "addActionListener" sStrandPicActionListener)

      
      
      (invoke fileMenu "add" sStrandPicMenu)
      (invoke menuBar  "add" fileMenu)
      (invoke menuBar "add" shMenu)
      
      (invoke frame "setJMenuBar" menuBar)
      
      (invoke windowListener "setActionClosure" (int 201)  (apply closeWindowEvent node "strandsFrame" ))
       (invoke frame "addWindowListener" windowListener)
       
      (invoke bComponent "setMaximumSize" (object ("java.awt.Dimension"  (int 900)   (int 155))) )
       ;add caption image to the frame 
      (invoke frame "add" bComponent java.awt.BorderLayout.NORTH)
      (invoke frame "add" baseComponent java.awt.BorderLayout.CENTER)
      
      (invoke baseComponent "setMaximumSize" (object ("java.awt.Dimension"  (int 800)   (int 440))) )
          
      ;for each displaying space level draw each node
	 (for level levelsList
             (for strandNode level
                  ( let (
                           (strandNumber (getAttr strandNode "strandNumber")) ;column
                           (Rank (getAttr strandNode "spaceRank")) ;row
                           (intruder? (getAttr strandNode "intruder")) ;does the strandNode belong to an intruder strand?
                           (time (getAttr strandNode "time"))
                           (xPosition (+ marginLeft (* strandNumber hSpace))) ;X coordenate of the strandNode
                           (yPosition (+ marginTop (* Rank vSpace) )) ;Y coordenate of the strandNode
                         )
                    (seq                    
                       ;;draw strandNode as a dot
                       (apply strand-node xPosition yPosition intruder? baseComponent (getAttr strandNode "firstFuture") time)
                         
                       ;;draw strand double line
                       (if (and (!= (apply getNextStrandNode matrix strandNode) (object null))
                                (!= (getAttr strandNode "strandNumber") (- (invoke matrix "size") (int 1)) ))
                         (seq
                        
			       (let(
                                  (nextRank (getAttr (apply getNextStrandNode matrix strandNode) "spaceRank" ))
                                  (length (- (* (-  nextRank (getAttr strandNode "spaceRank")) vSpace ) (int 6)))  
                                )
                                 (seq             
                                  (apply strand-darrow length  xPosition   (+ yPosition (int 6)) intruder? baseComponent time)  
                                
                                 )
			       )
                         )
		       )
		     ;end if
                       ;; draw message
                       (apply strand-message strandNode baseComponent intruderList nid)
                     ) ;end seq
                   )
               ) ;end for
           ) ;end for

          ;draw margin at the bottom part of the image
          (apply draw-blank (* (int 5) hSpace) (* lastLevel vSpace) baseComponent) 
          
          (invoke frame "add" emptyPanel java.awt.BorderLayout.SOUTH)
          (invoke frame "setSize" (int 910) (int 600)) 
          frame
      ) ;end seq
)
)
)  ;end drawStrands


 
;method that draws a black point at the end of the picture
; to set a margin at the bottom part of the component where the strands
; are drawn
;*inputs
;- x: X coordinate
;- y: Y coordinate
;- intruder?: does the node belong to an intruder strand?
;- component: graphical component in which the node will be drawn
;-firstFuture?: is the node the fist one with a message of the future? If so, paint a line-separator
;- time: does the node belong to the past or to the future?
(define strand-node (x y intruder? component firstFuture? time) 
   (let (
               (color (apply getStrandColor intruder? time))
               
               (nodeShape (object ("java.awt.geom.Ellipse2D$Double" 
                               (int -3) (int -3) (int 6) (int 6))))
               
               (nodeGlyph (object ("g2d.glyph.Glyph"  nodeShape color color )))
                
	       (trans (let ((temp (object ("java.awt.geom.AffineTransform"))))
			(seq (invoke temp "translate" x y) temp)))
          )

    (seq
       (if (= firstFuture? (boolean true))
           (let (
               (stroke (object ("java.awt.BasicStroke" (float 1.1))))
             
               (line (object ("java.awt.geom.Line2D$Double" 
			  (int 0) (int 0) (int 25) (int 20)))) ;original length -10
               (arrowLine 
	          (let ((temp (object ("g2d.glyph.Glyph" line color color))))
	          (seq (invoke temp "setStroke" stroke) temp)))
          
               (transLine (let ((temp (object ("java.awt.geom.AffineTransform"))))
                       (seq (invoke temp "translate" (int -12)   (int -50)) temp)))

               (trans (let ((temp (object ("java.awt.geom.AffineTransform"))))
                        (seq (invoke temp "translate" x y) temp)))

               ;slash is the marker to differenciate between the past and the future
               (slash (let ((temp (object ("g2d.glyph.GlyphList")))) 
			 (seq (invoke temp "add" arrowLine transLine)
                              (invoke temp "translate" x y)
			      temp)))
                  
               )
            (seq
              (invoke component "add" nodeGlyph trans)
              (invoke component "add" slash)
            )

           )
         (seq
           (invoke component "add" nodeGlyph trans) ;add the node in the desired location
         )
        ) ;end if
     
       nodeGlyph
    )
  )
 ) ;end define strand-node



;method that draws the vertical strand double line
;*inputs
;-length: length of the line
;- x: X coordinate
;- y: Y coordinate
;- intruder?: does the node belong to an intruder strand?
;- component: graphical component in which the node will be drawn
;- time: does the node belong to the past or to the future?

 (define strand-darrow (length x y intruder? component time)
    (let (     
            (stroke (object ("java.awt.BasicStroke" (float 1.1))))

            (color (apply getStrandColor intruder? time)) 
            
	    (polyline   (let ((temp (object ("java.awt.geom.GeneralPath" 
			        	java.awt.geom.GeneralPath.WIND_EVEN_ODD 
			         	(int 4)))))
	                    (seq (invoke temp "moveTo" (int 0) (int -10))
		              (invoke temp "lineTo" (int 10) (int 0))
		              (invoke temp "lineTo" (int 0) (int 10))
                              (invoke temp "lineTo" (int 10) (int 0))
                             temp)))
            
          (arrowHead 
	        (let ((temp (object ("g2d.glyph.Glyph" polyline color color))))
	        (seq  
                     (invoke temp "setFill" color)
                     (invoke temp "setStroke" stroke)
                     temp)))
          
          (lineA (object ("java.awt.geom.Line2D$Double" 
			  (int 0) (int 0) (- length (int 20)) (int 0)))) ;original length -20
          (arrowLineA 
	   (let ((temp (object ("g2d.glyph.Glyph" lineA color color))))
	      (seq (invoke temp "setStroke" stroke) temp)))
          
          (transA (let ((temp (object ("java.awt.geom.AffineTransform"))))
		    (seq (invoke temp "translate" (int 0) (int 2)) temp)))
          
          (lineB  (object ("java.awt.geom.Line2D$Double" 
			   (int 0) (int 0) (- length (int 20)) (int 0))))  ;original length -10
          
	  (arrowLineB 
	   (let ((temp (object ("g2d.glyph.Glyph" lineB color  color))))
	     (seq (invoke temp "setStroke" stroke) temp)))
          
	  (transB (let ((temp (object ("java.awt.geom.AffineTransform"))))
		    (seq (invoke temp "translate" (int 0) (int -2))
			 temp)))
          
	  (transH (let ((temp (object ("java.awt.geom.AffineTransform")))) 
		    (seq (invoke temp "translate" (- length (int 25)) (int 0))
			 temp)))
          
	  (doubleArrow (let ((temp (object ("g2d.glyph.GlyphList")))) 
			 (seq (invoke temp "add" arrowHead transH)
			      (invoke temp "add" arrowLineA transA)
			      (invoke temp "add" arrowLineB transB)
			      (invoke temp "rotate" 
				      (/ java.lang.Math.PI (int 2))
				      (double 0) (double 0))
 
			      (invoke temp "translate" x (+ y (int 5)))
			      temp)))
	  )
      (seq (invoke component "add" doubleArrow)  doubleArrow))
    ) ;end strand-darrow


;method to draw the message associated to a node
;*input
;- node: strand's node whose message is being drawn
;- component: graphical component in which the node will be drawn
;- intruderList: information related with the intruder's knowledge
;- nid: node's id (this is a Maude-NPA tree's node)

(define strand-message (node component intruderList nid)
 ( let (  ;obtain the predecessor and successor node's from other strands (only one of them may be different from null)
          ;the predecessor node is the one which sended the message that "node" receives
          (predecessor (getAttr node "predecessor"))
          ;the successor node is the one which will received the message that "node" sends
          (successor (getAttr node "successor"))
        )
    (seq
    
      ( if (and (= predecessor (object null)) (= successor (object null))  )
           ; the node has unknown predecessor or successor at this moment
           (apply strand-messageUnknown node component nid)
           ;else we will consider only the sender nodes (considering both sender and receiver node's  would be redundant)
           ( if (and (= predecessor (object null)) (!= successor (object null))  )
                (if (>= (getAttr successor "strandNumber") (getAttr node "strandNumber")) ;guess the direction of the arrow
                    ;; draw the arrow to the right
                    (apply  strand-rarrow node component nid)  
                    ;;else draw the arrow to the left
                    (apply  strand-larrow node component nid)     
                ) ; end if
           )  ;end if
      )  ;end if
     
   ) ;end seq delete
 )
)  ;end strand-message


; method to draw a message when the node which sent it or is going to receive it is unknown
; the message will be drawn on the right side of the node
;* input
;- node: strand's node whose message is being drawn
;- component: graphical component in which the node will be drawn
;- nid: node's id (this is a Maude-NPA tree's node)

(define strand-messageUnknown (node component nid)
(let ( ;get the message text
       (message  (invoke (getAttr node "message") "trim"))
       (signe (invoke message "charAt" (int 0)))
       (messageShortAux (invoke message "substring" (int 2) (- (invoke message "length") (int 1)))) ;2 characters: the signe and a parenthesis
       ;(messageShort (apply cutString messageShortAux vSpace))

       ;
        (pMessage (apply getShortMessage (getAttr node "originalMessage")))
        (copyPMessage (object ("java.lang.String" pMessage )))
        (m2 (invoke copyPMessage "replace" "  " " "))
        (messageShort (invoke m2 "replace" "( " "("))
             
       ; data structure which stores all the information related with strands
       (storageObject (fetch (concat "storageObject" nid)))
       ;intruderList stores information related with the intruder's knowledge
       (intruderList (getAttr storageObject  "intruderList"))
 
       (nodeNumber (getAttr node "strandNumber")) ;column
       (Rank (getAttr node "spaceRank")) ;row

       ;location of the dot, taking into acount the frame margins
       (xPosition (+ (* nodeNumber  hSpace) marginLeft) ) ;X coordinate
       (yPosition (+ (* Rank vSpace) (- marginTop (int 5)))) ;Y coordinate

       ;text foreground color: black if the node belongs to a regular strand
       ;and grey if it belongs to an intruder strand
       (color (if (= (invoke intruderList "get" messageShortAux) (object ("java.lang.Boolean" (boolean true))))
                          intruderKnowledgeColor
                       regularColor))

       (labelGlyph (object ("g2d.glyph.TextGlyph" pMessage (int 12) "Arial" color ))) ; text size font color

       (transText (let ((temp (object ("java.awt.geom.AffineTransform"))))
		    (seq (invoke temp "translate"  (int 5)  (int 0)) temp)))

       (text (let ((temp (object ("g2d.glyph.GlyphList")))) 
			 (seq (invoke temp "add" labelGlyph transText)
			      (invoke temp "translate"  (+ xPosition (int 5))  yPosition)
			      temp)))
       (shortArrow (apply getShortArrow node signe)) ; to indicate if the node is receiving or sending and unknown message.
      )
  (seq
  
      (invoke component  "add" text)
      (invoke component "add" shortArrow)
  )
      
 )
)  ;end strand-message-unknown


; method which draws an arrow to the right with its message on it
;* input
;- node: strand's node whose message is being drawn
;- component: graphical component in which the node will be drawn
;- nid: node's id (this is a Maude-NPA tree's node)

(define strand-rarrow (node component nid)
(try
    (let ( ;get the message text
           (message  (invoke (getAttr node "message") "trim"))
           (messageShort (invoke message "substring" (int 2) (- (invoke message "length") (int 1)))) ;2 characters: the signe and a parenthesis
           ; data structure which stores all the information related with strands
           (storageObject (fetch (concat "storageObject" nid)))
           ;intruderList stores information related with the intruder's knowledge
           (intruderList (getAttr storageObject  "intruderList"))

          ;text foreground color: black if the node belongs to a regular strand
          ;and grey if it belongs to an intruder strand
          (color (if (= (invoke intruderList "get" messageShort) (object ("java.lang.Boolean" (boolean true))))
                          intruderKnowledgeColor
                          regularColor))
              
          (successor (getAttr node "successor")) ;the node which will receive the message

          ;location of the arrows, taking into acount the frame margins
          (xPosition (+ (* (getAttr node "strandNumber") hSpace) (+ marginLeft (int 6)))) ;X coordinate
          (yPosition (+ (* (getAttr node "spaceRank") vSpace) marginTop)) ;Y coordinate

          ;arrow's length= ( (successor.strandNumber - predecessor.strandNumber) * space between strands ) - 6 (aproximately the dot's radius)
          (length (- (* (- (getAttr successor "strandNumber") (getAttr node "strandNumber") ) hSpace ) (int 18))) 

         ; (messageShort (apply cutString messageShortAux length))
          
          ;draw the arrow head
          (aX (array int (int 0) (int 0) (int 5)))
          (aY (array int (int -3) (int 3) (int 0)))
          (stroke (object ("java.awt.BasicStroke" (float 1))))

          (trans0 (let ((temp (object ("java.awt.geom.AffineTransform"))))
                    (seq (invoke temp "scale" (float 0.75) (float 0.75))
                         temp)))

          
          (head (object ("java.awt.Polygon" aX aY (int 3))))
          
          (arrowHead (let ((temp (object ("g2d.glyph.Glyph" head regularColor regularColor ))))
                       (seq 
			(invoke temp "setStroke" stroke)
			temp)))
          
           (trans1 (let ((temp (object ("java.awt.geom.AffineTransform"))))
                    (seq (invoke temp "translate"(-  length (int 6)) (int 0)) ;length -3
                         temp)))
          
          (line  (object ("java.awt.geom.Line2D$Double" 
			  (int 6) (int 0) (- length (int 5)) (int 0))))

          (arrowLine (let ((temp (object ("g2d.glyph.Glyph" line regularColor regularColor ))))
                       (seq 
			(invoke temp "setStroke" stroke)
			temp)))

         

          (rArrowGlyph (let ((temp (object ("g2d.glyph.GlyphList"))))
                         (seq (invoke temp "add" arrowHead trans1) 
                              (invoke temp "add" arrowLine)
                              (invoke temp "translate" xPosition  yPosition)     
			      temp)))
          (distance (- (getAttr successor "strandNumber") (getAttr node "strandNumber") ))
          ;message with blank spaces
          (pMessage (apply getShortMessage (getAttr node "originalMessage")))
       
	  )
      (seq
       ;add the message over the arrow
        (apply drawLabel pMessage  (getAttr node "strandNumber") (getAttr successor "strandNumber") (getAttr node "spaceRank") component distance color)
        (invoke component "add" rArrowGlyph) rArrowGlyph
      )
     )
(catch Exception
     (invoke java.lang.System.err "println" (concat "Error in strand-rarrow: " (invoke Exception "gerMessage")))
  )
 ) ;end try
) ;end strand-rarrow



; method which draws an arrow to the left with its message on it
;* input
;- node: strand's node whose message is being drawn
;- component: graphical component in which the node will be drawn
;- nid: node's id (this is a Maude-NPA tree's node)

 (define strand-larrow (node component nid)
  (try
    (let (
          ;get the message text to check in the intruderList
           (message  (invoke (getAttr node "message") "trim"))
           (messageShort (invoke message "substring" (int 2) (- (invoke message "length") (int 1)))) ;2 characters: the signe and a parenthesis
 
           ; data structure which stores all the information related with strands
           (storageObject (fetch (concat "storageObject" nid)))
           ;intruderList stores information related with the intruder's knowledge
           (intruderList (getAttr storageObject  "intruderList"))

          ;text foreground color: black if the node belongs to a regular strand
          ;and grey if it belongs to an intruder strand
          (color (if (= (invoke intruderList "get" messageShort) (object ("java.lang.Boolean" (boolean true))))
                      intruderKnowledgeColor
                      regularColor))

          (successor (getAttr node "successor")) ;the node which will receive the message
            
          ;location of the arrows, taking into acount the frame margins
          (xPosition (+ (* (getAttr node "strandNumber") hSpace) (- marginLeft (int 15)))) ;X coordinate
          (yPosition (+ (* (getAttr node "spaceRank") vSpace) marginTop)) ;Y coordinate

          ;arrow's length= ( (successor.strandNumber - predecessor.strandNumber) * space between strands ) - 6 (aproximately the dot's radius)
          (length (+ (* (- (getAttr successor "strandNumber") (getAttr node "strandNumber") ) hSpace ) (int 5))) ;15

          ;to draw the arrow
          (aX (array int (int 0) (int 0) (int 5)))
          (aY (array int (int -3) (int 3) (int 0)))
          (stroke (object ("java.awt.BasicStroke" (float 1.0))))

          (trans0 (let ((temp (object ("java.awt.geom.AffineTransform"))))
                    (seq (invoke temp "scale" (float 0.75) (float 0.75))
                         temp)))
          (head (object ("java.awt.Polygon" aX aY (int 3))))
          
          (arrowHead (let ((temp (object ("g2d.glyph.Glyph" head regularColor regularColor ))))
                       (seq 
			(invoke temp "setStroke" stroke)
			temp)))
          
          (line  (object ("java.awt.geom.Line2D$Double" 
			  (int 0) (int 0) (+ length (int 22))  (int 0))))
          
          (arrowLine (seq
                      (let (
                            (temp (object ("g2d.glyph.Glyph" line regularColor regularColor)))  )
                          (seq
                           (invoke temp "setStroke" stroke)
                           temp
                          ))))
          
          (trans1 (object ("java.awt.geom.AffineTransform")))

          
          (lArrowGlyph (let ((temp (object ("g2d.glyph.GlyphList"))))
                         (seq
                              (invoke temp "add" arrowHead)
                              (invoke temp "add" arrowLine)
                              (invoke temp "translate" xPosition  yPosition)
                              (invoke temp "rotate" java.lang.Math.PI)
			      temp)))
          
          (distance (-  (getAttr node "strandNumber") (getAttr successor "strandNumber") ))
          ;message with blanck spaces that will be shown in the graohical representation of the strands
           (prettyMessage (apply getShortMessage (getAttr node "originalMessage")))
	  )
         (seq
          (apply drawLabel prettyMessage (getAttr successor "strandNumber")  (getAttr node "strandNumber") (getAttr node "spaceRank") component distance color )
          (invoke component "add" lArrowGlyph)
          lArrowGlyph
         )
      )
  (catch Exception
     (invoke java.lang.System.err "println" (concat "Error in strand-larrow: " (invoke Exception "gerMessage")))
  )
 ) ;end try
) ;end strand-larrow



;Closure to draw a short arrow beside a node that receives a message whose sender strand
;is unknown or that sends a message to an unknown strand.
;If the node is sending the message, the arrow looks to the right
;otherwise, it looks to the left.
;* Input
;- node: strand node that receives or sends the message.
;- signe: signe of the message, i.e., + or -.

(define getShortArrow (node signe)
 (let (  (xPosition (+ (* (getAttr node "strandNumber") hSpace)  (+ marginLeft (int 7)) )) ;X coordinate
         (yPosition (+ (* (getAttr node "spaceRank") vSpace) (+ marginTop (int 3)))) ;Y coordinate
         (stroke (object ("java.awt.BasicStroke" (float 1.0))))
         (aX (array int (int 0) (int 0) (int 5)))
         (aY (array int (int -3) (int 3) (int 0)))
         (head (object ("java.awt.Polygon" aX aY (int 3))))
          
         (arrowHead (let ((temp (object ("g2d.glyph.Glyph" head regularColor regularColor ))))
                       (seq 
			(invoke temp "setStroke" stroke)
			temp)))

         (trans1 (let ((temp (object ("java.awt.geom.AffineTransform"))))
                    (seq (invoke temp "translate" (int 20) (int 0)) ;length -3
                         temp)))
          
          (line  (object ("java.awt.geom.Line2D$Double" 
			  (int 0) (int 0) (int 25)  (int 0))))
          
          (arrowLine (seq
                      (let (
                            (temp (object ("g2d.glyph.Glyph" line regularColor regularColor)))  )
                          (seq
                           (invoke temp "setStroke" stroke)
                           temp
                          ))))
          
         (lArrowGlyph (let ((temp (object ("g2d.glyph.GlyphList"))))
            (seq
                              (invoke temp "add" arrowLine)
                              (invoke temp "add" arrowHead trans1) 
                              (if (= signe (char '-'))
                                  (invoke temp "rotate" java.lang.Math.PI))
                              (invoke temp "translate" xPosition  yPosition)
			      temp)))
       )
   lArrowGlyph
 )
) ;end getShortArrow


;This method gets the caption's image and adds it to a panel.
(define getCaptionOld ()
  (let ( (panel (object ("javax.swing.JPanel" )))
         (guiVar (apply getEnvVariable "MNPA_GUI"))
         (imageIcon (object ("javax.swing.ImageIcon" (concat guiVar "/Images/caption.png") "Caption")) )
         (jlabel (object ("javax.swing.JLabel" imageIcon )))
        )
  (seq
     
     
    (invoke panel "setBackground" white)
    (invoke panel "setSize" (int 900) (int 155))
    (invoke panel "setMaximumSize" (object ("java.awt.Dimension"  (int 900)   (int 155))))
    (invoke panel "setMinimumSize" (object ("java.awt.Dimension"  (int 900)   (int 155))))
    (invoke panel "add" jlabel)
    panel 
  )
 )
) ;end getCaption



;This method gets the caption's image and adds it to a panel.
(define getCaption ()
   (try 
  (let ( 
	 ( layout (object("java.awt.GridLayout" (int 3) (int 4))))
	  
	  
	 (panel (object ("javax.swing.JPanel" )))
         (guiVar (apply getEnvVariable "MNPA_GUI"))
	  (iHonestPast (object ("javax.swing.ImageIcon" (concat guiVar "/Images/pastHonest.png") "Past Honest Strand")) )
	  (lHonestPast (object ("javax.swing.JLabel" iHonestPast )))
	  (tHonestPast (object ("javax.swing.JLabel" "Past and present honest strand")))
	  
	  (iHonestFuture (object ("javax.swing.ImageIcon" (concat guiVar "/Images/futureHonest.png") "Future Honest Strand")) )
	  (lHonestFuture (object ("javax.swing.JLabel" iHonestFuture )))
	  (tHonestFuture (object ("javax.swing.JLabel" "Future honest strand")))
	  
	  (iIntruderStrand (object ("javax.swing.ImageIcon" (concat guiVar "/Images/intruderStrand.png") "Intruder Strand")) )
	  (lIntruderStrand (object ("javax.swing.JLabel" iIntruderStrand )))
	  (tIntruderStrand (object ("javax.swing.JLabel" "Intruder Strand")))
	  
	  (iIK (object ("javax.swing.ImageIcon" (concat guiVar "/Images/intruderKnowledge.png") "Intruder Knowledge")) )
	  (lIK (object ("javax.swing.JLabel" iIK )))
	  (tIK (object ("javax.swing.JLabel" "IntruderKnowledge")))
		      
	  
	  (iTemporalMark (object ("javax.swing.ImageIcon" (concat guiVar "/Images/temporalMark.png") "Temporal Mark")) )
	  (lTemporalMark (object ("javax.swing.JLabel" iTemporalMark )))
	 
	  (tTemporalMark (object ("javax.swing.JTextArea" "Temporal Mark: messages above it belong \nto the past and messages below it belong \nto the future")))
	 
	 (font (object ("java.awt.Font" "Times" (int 0) (int 13))))
     )
  (seq
     (invoke panel "setLayout" layout)
   (invoke tTemporalMark "setEditable" (boolean false))
   (invoke tTemporalMark "setWrapStyleWord" (boolean false))
   (invoke tHonestPast "setFont" font)
   (invoke tIntruderStrand "setFont" font)
   (invoke tIK "setFont" font)
   (invoke tHonestFuture "setFont" font)
   (invoke tTemporalMark "setFont" font)
    
   (invoke panel "setBackground" white)
   
   (invoke panel "add" lHonestPast)
   (invoke panel "add" tHonestPast)
   
   (invoke panel "add" lIntruderStrand)
   (invoke panel "add" tIntruderStrand)
 
   (invoke panel "add" lHonestFuture)
   (invoke panel "add" tHonestFuture)
   
 
   (invoke panel "add" lIK)
   (invoke panel "add" tIK)
   
   (invoke panel "add" lTemporalMark)
   (invoke panel "add" tTemporalMark)

    panel 
  )
)
 (catch Exception
     (invoke java.lang.System.err "println" (invoke Exception "getMessage"))
 )
)
 ;end try
) ;end getCaption




;This method allows the user to show or hide the caption
;* Input
;- frame: frame where the strands are displayed
;- captionPanel: JPanel object that contains the caption
;- shMenu: menuItem object that allows the user to show or hide the caption
;* Output/Effect
;- If the caption is visible, then it will be hidden and the menuItem
;  will allow the user to make the caption visible again.
; If the caption is hidden, it will be showed again and the menuItem
; will allow the user to hide the caption

(define showHideCaption (frame captionPanel shMenu)
  (lambda (self e)
    (seq
      (if (= (invoke shMenu "getText" ) "Hide Caption")
          ; hide caption
          (seq
           (invoke shMenu "setText" "Show Caption")
           (invoke captionPanel "setVisible" (boolean false))
          )
        ; else, show caption
          (seq
           (invoke captionPanel "setVisible" (boolean true))
           (invoke shMenu "setText" "Hide Caption")
          )
      ) ;end if
     (invoke frame "repaint")
    )
  )
) ;end define


(define saveStrandPicture (componentView  frame)
  (lambda (self e)
    (let (  (fileDialog (object("g2d.swing.IOPFileChooser"))) ;file chooser element to selct the protocol file
            (returnVal (invoke fileDialog "showSaveDialog" frame))
	    (component (invoke  componentView "getIOPComponent"))
	    
	)
	
    (seq
      (if (= returnVal javax.swing.JFileChooser.APPROVE_OPTION)
	  (let( (filePath (invoke (invoke fileDialog "getSelectedFile") "getAbsolutePath"))
	      )
	    (if (= (invoke filePath "endsWith" ".png") (boolean true))
	      (sinvoke "mnpa.ScreenImage" "writeImage" (sinvoke "mnpa.ScreenImage" "createImage" component) filePath)
	      (if (= (invoke filePath "lastIndexOf" ".") (int -1))
		     (sinvoke "mnpa.ScreenImage" "writeImage" 
			 (sinvoke "mnpa.ScreenImage" "createImage" component) (concat filePath ".png"))
		     (sinvoke "mnpa.ScreenImage" "writeImage" 
			 (sinvoke "mnpa.ScreenImage" "createImage" component) 
			 (concat (invoke filePath "substring" (int 0) (invoke filePath "lastIndexOf" "."))
				 ".png"))
		     ) ;end if
		 ) ;end if
   
	  ) ;end let
	   
      ) ;end if
    )    
)
)
) ;end saveStrandPicture



; This method estimates the number of lines that will have
; the JTextArea that will show a message.
;* Input:
;- message: message that will be shown in the strands graphical representation
;* Output/Effect
;- number of lines in which the message will be splitted and, thus, the number
;  of lines of the JTextArea that will show that message as a message exchanged
;  between two strands. This number of lines depends on the horizontal space
;  defined between two strands.

(define estimateNumberLines (message)
 (try
   (let( (totalLength (invoke message "length"))
         (numberLines (/ totalLength numCharsPerSpace))
         (mod (% totalLength numCharsPerSpace))
         (finalLines (if (> mod (int 3))
                         (+ numberLines (int 1))
                         numberLines))
        )
     (seq 
        (if (< finalLines (int 1))
            (int 1)
            finalLines
        )
     )
   )
   (catch Exception
     (invoke java.lang.System.err "println" (concat "Error in estimateNumberLines: " (invoke Exception "getMessage")))
   )
 ) ;end try
) ;end estimateNumberLines

;Closure to split the text of a message is various lines
;*Input
;- message:  message text  (without any break line) to split.
;- distance: distance between the two strands that are exchanging a message.
;* Output
; message text splitted in various lines.

(define cutString (message distance)
  (let ( (maxCharsPerLine (* distance numCharsPerSpace)) ;max number of chars per line
         (shortMessage (if (> (invoke message "length") maxCharsPerLine)
                           (apply cutStringRec "" message maxCharsPerLine)
                           message))
        )
     shortMessage
  )
) ;end cutString


;recursive closure to  split the text of a message is various lines
;* Input
;- message: lines of the message already obtained from the original message.
;- text2cut: part of the original text that has not been processed.
;- maxCharsPerLine: maximum amount of chars that can be displayed at a line.
;                   Its value depends on the distance that exists between the two strands
;                   that are exchanging the message.

(define cutStringRec (message  text2cut maxCharsPerLine)
  (let ( (length2c (* (invoke text2cut "length") (int 6)))
        )
    (seq
      (if (<= (invoke text2cut "length") maxCharsPerLine )
          (concat message text2cut)
        
          (let ( (newLine (invoke text2cut "substring" (int 0) maxCharsPerLine))
                 ;this closure will take into account symbols "<", ")" and ";" to not
                 ;separate in different lines the same subterm.
                 (ltPosition (- (invoke newLine "lastIndexOf" "<") (int 1))) ;the < will be at the next line
                 (cPPosition (invoke newLine "lastIndexOf" ")"))
                 (dCPosition (invoke newLine "lastIndexOf" ";"))
                 (endIndex1  (sinvoke "java.lang.Math" "max" ltPosition  cPPosition))
                 (endIndex2 (sinvoke "java.lang.Math" "max" dCPosition endIndex1))
                 ; the last character of the string to cut will be the minimum value chosen between the
                 ;maximum number of character per line and the last index of the three special symbols.
                 (endIndex3 (if (<= endIndex2 (int 0))
                                maxCharsPerLine
                                (sinvoke "java.lang.Math" "min" maxCharsPerLine endIndex2)))
                 
                 ;the new line we have obtained form the unprocessed text.
                 (finalNewLine (invoke text2cut "substring" (int 0) (+ endIndex3 (int 1))))
                 ;remove from text2cut the new line.
                 (newText2cut  (invoke text2cut "substring" (+ endIndex3 (int 1))))
                )
               (apply cutStringRec (concat message  finalNewLine "\n")  newText2cut maxCharsPerLine)
           )
      ) ;end if
    )
  )
) ;end cutStringRec
 
;Closure to draw the label with the text of a message exchanged between two strands.
;* Input:
;- message: text of the message to show
;- x1Coord: symbolic x-coordinate of the node situated in the left part of the arrow
;- x2Coord: symbolic x-coordinate of the node situated in the right part of the arrow
;- yCoord: symbolic y-coordinate of the node whose label is going to be drawn
;- component: IOPView component where the strands are being shown
;- distance: symbolic distance between the two nodes that are ex-changing the message
;- color: color of the text of the message: red if the message belongs to the
;          intruder knowledge or black, otherwise.
;* Output:
;- a label with the message will be painted 
(define drawLabel (message x1Coord x2Coord yCoord component distance color)
 (try
  (let (    (numberLines  (apply estimateNumberLines message))
            (base (invoke component "getIOPComponent"))
            ;;;,
            (copyPMessage (object ("java.lang.String" message )))
            (m2 (invoke copyPMessage "replace" "  " " "))
            (m3 (invoke m2 "replace" "( " "("))
        )
    (seq
      (apply obtainPaintLabel x1Coord x2Coord yCoord base m3 numberLines color)
    )
  )
  (catch Exception
     (invoke java.lang.System.err "println" (concat "Error in drawlabel: " (invoke Exception "gerMessage")))
  )
 ) ;end try
) ;end drawLabel



;;Closure to draw the label with the text of a message exchanged between two strands.
;* Input
;- x1Coord: symbolic x-coordinate of the node situated in the left part of the arrow
;- x2Coord: symbolic x-coordinate of the node situated in the right part of the arrow
;- yCoord: symbolic y-coordinate of the node whose label is going to be drawn
;- base: IOPView component where the strands are being shown
;- message: text of the message to show
;- numberLines: number of lines of the text of the message
;- color: color of the text of the message: red if the message belongs to the
;          intruder knowledge or black, otherwise.
;* Output:
;- a label with the message will be painted 
(define obtainPaintLabel (x1Coord x2Coord yCoord base message numberLines color )
  (try
    (let ( ;get the left-most position in the x-axis of the label
          (x1PosLabel (apply getx1PosLabel x1Coord x2Coord numberLines))
          (length     (- hSpace (int 35))) ;length of the label 45
          (textHeight (* numberLines (int 15))) ; height of the label
          ;top-most position in the y-axis of the label
          (yPosLabel (-  (+ marginTop            
                            (* yCoord vSpace))
                         (+ textHeight (int 6))
                     ))
          ;label component with the corresponding text, shape and color
          (label  (apply makeTextAreaLabel message  length textHeight color "Arial" (int 0) labelFontSize white (int 0)))
        )
    (seq
        (invoke label "setBounds" x1PosLabel yPosLabel length textHeight )
        (invoke base "add" label)
        (invoke base "repaint")
        base
        
    )
  ) ; end let environment
      (catch Exception
     (invoke java.lang.System.err "println" (concat "Error in obtainPaintLabel: " (invoke Exception "gerMessage")))
  )
 ) ;end try
) ;end obtainPaintLabel

;* Input:
;- x1Coord: symbolic x-coordinate of the node situated in the left part of the arrow
;- x2Coord: symbolic x-coordinate of the node situated in the right part of the arrow
;- numberLines: number of lines of the text of the message
;* Output:
;- left-most position in the x-axis of the label
(define getx1PosLabel (x1Coord x2Coord numberLines)
  (try
    (let ( ; real position of the x1Coord coordinate in the component where the strands are being painted
           (x1Pos (+ (* x1Coord hSpace) marginLeft))
           ; real position of the x2Coord coordinate in the component where the strands are being painted
           (x2Pos (+ (* x2Coord hSpace) marginLeft))
           ; central real position between the two nodes in the component where the strands are being painted
           (centralPos (+  (/ (- x2Pos x1Pos)
                            (int 2))
                           (+ (* x1Coord hSpace)
                              marginLeft)
                           ))
           ; symbolic distance between the two nodes
           (diference  (sinvoke "java.lang.Math" "abs" (- x1Coord x2Coord)))
          )
     (seq
       ; if the two nodes are contiguous, then the left-most position of the label
       ; is located 20px to the right of the left-most node
       (if (<= diference  (int 1))
           (+ (+ marginLeft
                 (* x1Coord hSpace))
              (int 20))
           ;else if the two nodes are separated by more than 1 symbolic position, the
           ; left-most position of the label is situated hSpace/2 px to the left of the
           ; central position
           (+  (- centralPos
                    (/ hSpace (int 2))
                 )
              (int 20))
        ) ;end if
     )
    )
 (catch Exception
     (invoke java.lang.System.err "println" (concat "Error in getx1PosLabel: " (invoke Exception "gerMessage")))
  )
 ) ;end try
  ) ;end getx1PosLabel





;;;;;;;;;; ./makeg2dlib loaded ../GUI/clt-funs.lsp ;;;;;;;;;;
(seq

;;; calls from NPA Assistant
; (apply defNPAManager %protocol %attacks)  
; code in npa-manager.lsp
; (apply createMtTree %tname %aname %protocol %root?)
; (apply extendNPATree %tname %fringe)
; (apply showNPAGraph %tname \"NPAManager\" %title %subtitle toolBarFunNPA)
; (apply showUpDatedNPAGraph %tname )


; creates an empty graph and initialize it (if needed) 
; with the root node (the one with id=0 and label=<>)  
; clt mod of createEmptyGraph -- record protocol name as well
;;  sonia's varnames  id    gname
;*input
;- tname: tree's name
;- aname: attack's name
;- gname:graph name
;- id: graph identification
;- rootNode?: indicates if an initialization (adding the root node) is necessary
;             (i.e, if this method has been called by thecreateBranch method
;*output
; an empty graph with the required attributes
 
(define createMtTree (tname aname protocol rootNode?)
  (let ((graph (object ("g2d.graph.IOPGraph")))
        (kbm (fetch "NPAManager"))
        (pwd (apply getEnvVariable "PWD"))
       )
     (seq
      (invoke graph "setUID" tname)
	    (setAttr graph "aname" aname)
	    (setAttr graph "protocol" protocol)
    ;; list of lists that will contain the nodes of each level computated	    
	    (setAttr graph "list" (object ("java.util.ArrayList")))
            (setAttr graph "initialStates" (object ("java.util.ArrayList")))
            
	    (setAttr graph "colorFun" colorNPANode )
            (setAttr graph "numberNodes" (int 1))
            (setAttr graph "analysisFinished" (boolean false))
            (setAttr graph "nextStep?" (boolean false))
            (setAttr graph "currentState" (int 0))
            (setAttr graph "newInitialState" (boolean false))
            (setAttr graph "nlevels" (int 0)) 
            (setAttr graph "protocolFolder" pwd)  
            (setAttr graph "currentAction" "")

             ;1 because an empty graph contains the root node
	    (let ((list (getAttr graph "list")))
		    (seq
		     ;; add the root node
		     ;; firstly, we add a new empty element in the list 
		     ;; attribute of the graph so that we will be able 
		     ;; to keep the nodes of the new level we are adding in 
		     ;; the last position of the list
		      (invoke list "add"  (object ("java.util.Hashtable")))
		     ;; this if is necessary because the method createBranch uses
		     ;; createEmptyGraph, but in this case, is not necessary to
		     ;; initialize the graph with the root node.
		     (if (= rootNode? "true")
		       (apply newNPANode graph 
		              (apply mkNPAMouseClickedClosure graph) "0" "<>" ""
		                 (array java.lang.String ) (boolean true))
		     )
		     graph
		 )) ;; seq let
	 )) ;; seq let 
 ) ;end createMtTree
 
;; for testing
(define idSysArr1
 (array java.lang.Object 
   (array java.lang.Object "s1" 
     (array java.lang.String "1")
     "(:: nil :: [ nil, -(pk(b,a ; N)), +(pk(a, N ; n(b,r))), -(pk(b,n(b,r))) | nil ]) || (n(b,r) inI, empty) || nil || nil"
     ))
 )


(define extendNPATree (tname idSysSetStr)
  (let (
      (arr (apply idSysS2Arr idSysSetStr))
   )
   
(seq
  ;   (sinvoke "g2d.util.IO" "string2File" idSysSetStr "fooSoniaMAY.txt" (boolean true))
    ; (invoke java.lang.System.err "println" idSysSetStr)
     (apply addLevel tname arr)
  )
  )
) ;end extendNPATree 


) ;; top seq


;; !!NB  idSysS2Arr needs to deal with "empty"


 
)

