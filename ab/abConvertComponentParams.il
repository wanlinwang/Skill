/* abConvertComponentParams.il

Author     A.D.Beckett
Group      Custom IC (UK), Cadence Design Systems Ltd.
Language   SKILL
Date       Jan 14, 2001 
Modified   Sep 03, 2019 
By         A.D.Beckett

Can either convert a single cellView - in the current window:

abConvertComponentParams("conv.config")

Or one in a variable:

abConvertComponentParams("conv.config" cellView)

Or one in a variable, not doing the schematic check afterwards:

abConvertComponentParams("conv.config" cellView nil)

Or all the matching views in a library

abConvertComponentParamsForLib("myLib" "schematic" "conv.config")

Or all the matching viewTypes in a library

abConvertComponentParamsForLibByViewType("myLib" "schematic" "conv.config")

Or a whole design hierarchy:

abConvertComponentParamsHier("conv.config")

With some different keyword arguments:

abConvertComponentParamsHier("conv.config" ?cellView cellView 
    ?viewList "schematic schem2 symbol" 
    ?stopList "symbol" ?check nil)

(see the code below for details on the arguments to this function)


Each uses a configuration file which contains the rules for conversion.

The conversion configuration file contains a single list. Each sub-list
is a disembodied property list, containing the following properties:

fromLib:  Name of the library from which the component to
          be changed originates
          Is an anchored regular expression ("nfet" means "^nfet$"; ".*fet" means
          "^.*fet$")
fromCell: Name of the cell of the component to be changed.
          An anchored regular expression.
fromView: Name of the view of the component to be changed.
          Defaults to "symbol" if not specified.
          An anchored regular expression.
toLib:    Name of the library in which the new component is
          to be found.
toCell:   Name of the cell of the new component.
toView:   Name of the view of the new component.
          Defaults to "symbol" if not specified.
propMatch:
          An assoc list of property names and values. If the instance
          matches the cell name, and this key exists, it will match if
          all properties match. The property values are treated as anchored
          regular expressions (so "N.*" will match as "^N.*$").
matchFunction:
          Name of a predicate function (which is passed the instId) to
          decide if this is a match. If neither propMatch nor matchFunction
          keys are provided, it is assumed to match. If both are supplied,
          they must both match.
postProcess:
          Name of a function (passed the instance id) which is used to
          do any post-processing on the instance after conversion (before
          the callbacks are called). 
runCallbacks: Invoke the callbacks after changes if this is t. If it is
          a list, then these are additional arguments to abInvokeInstCdfCallbacks
params:   List of parameters to change. Each sub-list contains
          two to four elements:
            The original parameter name
            The new parameter name (or nil)
            [optional] a function to do some modification. The function
            will be passed the instance id, the original parameter name,
            and the existing parameter value, and will return the 
            updated parameter value.
            [optional] the type of the new parameter (if different from original)
          If the new parameter name is nil, it simply deletes the 
          original property.
addProps: List of properties to add, or set. Each sub-list contains
          two or three elements:
            The name of the property
            The value to set.
            [optional] The type of the property (e.g. "netSet") if cannot be
            inferred from the value.

An example would be:

(
  (nil
   fromLib   "analogLib"
   fromCell  "nmos4"
   toLib     "test"
   toCell    "nmos4"
   runCallbacks t
   params (
           ("w" "width")
           ("l" "length" fixIt)
           )
   addProps (
             ("isnoisy" t)
             )
   )
  (nil
   fromLib   "analogLib"
   fromCell  ".*mos4"
   toLib     "test"
   toCell    "pmos4"
   propMatch (("subtype" "pxyz"))
   runCallbacks (?useInstCDF t ?callInitProc t)
   params (
           ("w" "width")
           ("l" "length")
           )
   )
)

For example, fixIt might be something like:

procedure(fixIt(inst parName val)
  printf("Fixing %s on %s\n" parName inst~>name)
  ; double the value. Note evalstring is not really a good idea,
  ; just in case the parameter value is some legal SKILL which could do
  ; something unexpected (e.g. "exit()" !)
  sprintf(nil "%n" evalstring(val)*2)
  )

Unless anything complicated is needed, there is no need to specify a conversion
function. This may be needed for parameter scaling (e.g. changing from meters to
microns)

***************************************************

SCCS Info: @(#) abConvertComponentParams.il 09/03/19.17:39:04 1.13

*/

/**********************************************************************
*                                                                     *
*           (abReadConvertComponentParamsConfig configFile)           *
*                                                                     *
* Read the configuration parameters for the conversion from the named *
*                      file, and return the list                      *
*                                                                     *
**********************************************************************/

(procedure (abReadConvertComponentParamsConfig configFile)
  (let (prt data)
       (setq prt (infile configFile))
       (while (and (setq data (lineread prt)) (eq data t)) t)
       (car data)
       )
  )

/***************************************************************
*                                                              *
*      (abConvertComponentParamsPropMatch inst propMatch)      *
*                                                              *
* Check an instance against all the entries in the propMatch,  *
*                 and return t if it matches.                  *
*                                                              *
***************************************************************/

(procedure (abConvertComponentParamsPropMatch inst propMatch)
  (let (propVal)
       (forall propPair propMatch
               (and (setq propVal (dbGet inst (car propPair)))
                    (or
                     (and (stringp propVal)
                          (rexMatchp (sprintf nil "^%s$" (cadr propPair))
                                     propVal))
                     (equal (cadr propPair) propVal))
                    ))
       )
  )

/***********************************************************************
*                                                                      *
*                      (abConvertComponentParams                       *
*                              configFile                              *
*          @optional (cellView (geGetEditCellView)) (check t)          *
*                                  )                                   *
*                                                                      *
* Using the conversion configuration information, convert the cellView *
*                              specified.                              *
*                                                                      *
***********************************************************************/

(procedure (abConvertComponentParams
            configFile
            @optional (cellView (geGetEditCellView)) (check t)
            )
  (let (master config newVal changed newParams valueType)
       ;-----------------------------------------------------------------
       ; Read the config file, unless the config parameters were
       ; passed in
       ;-----------------------------------------------------------------
       (setq config
             (if (stringp configFile)
                 (abReadConvertComponentParamsConfig configFile)
                 configFile
                 )
             )
       ;-----------------------------------------------------------------
       ; Try each component in the config
       ;-----------------------------------------------------------------
       (foreach component config
                ;--------------------------------------------------------
                ; Get the master for the replacement component
                ;--------------------------------------------------------
                (setq master 
                      (dbOpenCellViewByType
                       (getq component toLib)
                       (getq component toCell)
                       (or (getq component toView) "symbol")
                       )
                      )
                ;--------------------------------------------------------
                ; Fill in the default values in the config
                ;--------------------------------------------------------
                (unless (getq component fromView)
                        (putpropq component "symbol" fromView))
                ;--------------------------------------------------------
                ; Look at all the instances
                ;--------------------------------------------------------
                (foreach inst (dbGetq cellView instances)
                         ;-----------------------------------------------
                         ; If it matches
                         ;-----------------------------------------------
                         (when
                          (and
                           (rexMatchp 
                            (sprintf nil "^%s$" (getq component fromLib))
                            (dbGetq inst libName))
                           (rexMatchp 
                            (sprintf nil "^%s$" (getq component fromCell))
                            (dbGetq inst cellName))
                           (rexMatchp 
                            (sprintf nil "^%s$" (getq component fromView))
                            (dbGetq inst viewName))
                           (or (null (getq component propMatch))
                               (abConvertComponentParamsPropMatch
                                inst (getq component propMatch)))
                           (or (null (getq component matchFunction))
                               (funcall (getq component matchFunction) inst))
                           )
                          (unless changed
                                  (dbReopen cellView "a")
                                  (setq changed t)
                                  )
                          ;----------------------------------------------
                          ; Initialise the list of new parameters
                          ;----------------------------------------------
                          (setq newParams nil)
                          ;----------------------------------------------
                          ; Then update all the properties
                          ;----------------------------------------------
                          (foreach param (getq component params)
                                   (when (and
                                          (setq newVal (dbGet inst (car param)))
                                          (nequal newVal "")
                                          )
                                         ;-------------------------------
                                         ; If a function was supplied, call it to
                                         ; process the value
                                         ;-------------------------------
                                         (when (caddr param)
                                               (setq newVal 
                                                     (funcall (caddr param) inst 
                                                              (car param) newVal))
                                               )
                                         ;-------------------------------
                                         ; The valueType is either the existing
                                         ; type, or can be overridden with the 
                                         ; fourth entry in the param definition
                                         ;-------------------------------
                                         (setq valueType
                                               (or (cadddr param)
                                                   (dbGetq
                                                     (dbFindProp inst (car param))
                                                     valueType)))
                                         ;-------------------------------
                                         ; Delete the old property, and 
                                         ; store the new
                                         ;-------------------------------
                                         (dbDeletePropByName inst (car param))
                                         ;-------------------------------
                                         ; If the new parameter name is non-nil
                                         ; add the new parameter into the list to add
                                         ;-------------------------------
                                         (when (cadr param)
                                           (setq newParams
                                                 (cons (list (cadr param) newVal 
                                                             valueType) newParams))
                                           ) 
                                         ) 
                                   ) 
                          ;----------------------------------------------
                          ; Update the master to the new component
                          ; Must do this after changing the parameters, as otherwise
                          ; getting default values from the CDF doesn't work (since
                          ; the component changed)
                          ;----------------------------------------------
                          (dbSetq inst master master)
                          ;----------------------------------------------
                          ; Now go through the list of parameters and save them
                          ; trying to preserve the property type
                          ;----------------------------------------------
                          (foreach param newParams
                                   (if (caddr param)
                                     (dbReplaceProp inst (car param) 
                                                    (caddr param) (cadr param))
                                     (dbSet inst (cadr param) (car param))
                                     )
                                   )
                          ;----------------------------------------------
                          ; Now add any parameters that were asked to be set
                          ; Also supports optional type
                          ;----------------------------------------------
                          (foreach param (getq component addProps)
                                   (if (caddr param)
                                     (dbReplaceProp inst (car param) 
                                                    (caddr param) (cadr param))
                                     (dbSet inst (cadr param) (car param))
                                     )
                                   ) 
                          ;----------------------------------------------
                          ; invoke any postProcess function that has been 
                          ; defined
                          ;----------------------------------------------
                          (when (getq component postProcess)
                            (funcall (getq component postProcess) inst)
                            )
                          ;----------------------------------------------
                          ; If the configuration says to invoke the callbacks,
                          ; call them
                          ;----------------------------------------------
                          (when (getq component runCallbacks)
                            (if (listp (getq component runCallbacks))
                              (apply 'abInvokeInstCdfCallbacks
                                     (cons inst (getq component runCallbacks)))
                              (abInvokeInstCdfCallbacks inst)
                              )
                            )
                          )
                         )
                ;--------------------------------------------------------
                ; Close the master replacement object (for tidiness)
                ;--------------------------------------------------------
                (dbClose master)
                )
       ;-----------------------------------------------------------------
       ; If anything changed, need to save it
       ;-----------------------------------------------------------------
       (when changed 
             (when check (schCheck cellView))
             (dbSave cellView)
             )
       ;-----------------------------------------------------------------
       ; Let the caller know if anything got changed
       ;-----------------------------------------------------------------
       changed
       )
  )

/*****************************************************************************
*                                                                            *
*    (abConvertComponentParamsForLib libName viewName configFile [check])    *
*                                                                            *
* Convert all cells in the library libName which have a view viewName, using *
*                     the configuration file passed in.                      *
*                                                                            *
*****************************************************************************/

(procedure (abConvertComponentParamsForLib libName viewName configFile
                                           @optional (check t)
                                           )
  (let (config cellName cellView)
       ;-----------------------------------------------------------------
       ; Read the config file, unless the config parameters were
       ; passed in
       ;-----------------------------------------------------------------
       (setq config
             (if (stringp configFile)
                 (abReadConvertComponentParamsConfig configFile)
                 configFile
                 )
             )
       (foreach cell (getq (ddGetObj libName) cells)
                (setq cellName (getq cell name))
                (when (ddGetObj libName cellName viewName)
                      (setq cellView 
                            (dbOpenCellViewByType
                             libName cellName viewName))
                      (when cellView
                            (printf "Converting %s/%s/%s\n" libName cellName viewName)
                            (abConvertComponentParams config cellView check)
                            (dbClose cellView)
                            )
                      )
                )
       )
  t
  )

/*********************************************************************
*                                                                    *
*             (abConvertComponentParamsForLibByViewType              *
*          libName viewType configFile @optional (check t)           *
*                                 )                                  *
*                                                                    *
* Convert all cells in the library libName which have a view of type *
*         viewType, using the configuration file passed in.          *
*                                                                    *
*********************************************************************/

(procedure (abConvertComponentParamsForLibByViewType
             libName viewType configFile @optional (check t)
             )
  (let (config cellName cellView viewName masterObj)
       ;-----------------------------------------------------------------
       ; Read the config file, unless the config parameters were
       ; passed in
       ;-----------------------------------------------------------------
       (setq config
             (if (stringp configFile)
                 (abReadConvertComponentParamsConfig configFile)
                 configFile
                 )
             )
       (foreach cell (getq (ddGetObj libName) cells)
                (setq cellName (getq cell name))
                (foreach view (getq (ddGetObj libName cellName) views)
                         (setq viewName (getq view name))
                         (setq masterObj
                               (ddGetObj libName cellName viewName "*"))
                         (when (and masterObj 
                                    (equal (ddMapGetFileViewType masterObj)
                                           viewType))
                           (setq cellView 
                                 (dbOpenCellViewByType
                                   libName cellName viewName))
                           (when cellView
                             (printf "Converting %s/%s/%s\n" libName cellName 
                                     viewName)
                             (abConvertComponentParams config cellView check)
                             (dbClose cellView)
                             )
                           )
                         )
                )
       )
  t
  )

/*********************************************************************
*                                                                    *
*                   (abConvertComponentParamsHier                    *
*             configFile [?cellView (geGetEditCellView)]             *
*              [?viewList "schematic cmos.sch symbol"]               *
*                        [?stopList "symbol"]                        *
*                             [?check t]                             *
*                          [?visited table]                          *
*                                 )                                  *
*                                                                    *
* Traverse the design hierarchically, using the specified view list  *
* and stop list, converting the parameters as outlined in the config *
* file supplied. The ?visited argument is only intended to be passed *
*  when this function is called recursively - end users should not   *
*  pass this argument. ?check allows you to turn off the check done  *
*  before saving. The default cellView to start from is the one in   *
*                        the current window.                         *
*                                                                    *
*********************************************************************/

(procedure (abConvertComponentParamsHier
            configFile @key (cellView (geGetEditCellView))
            (viewList "schematic cmos.sch symbol")
            (stopList "symbol")
            (check t)
            (visited (makeTable 'visited nil))
            )
  (let (config switchedView)
       ;-----------------------------------------------------------------
       ; Read the config file, unless the config parameters were
       ; passed in
       ;-----------------------------------------------------------------
       (setq config
             (if (stringp configFile)
                 (abReadConvertComponentParamsConfig configFile)
                 configFile
                 )
             )
       ;-----------------------------------------------------------------
       ; Convert the stop list to a list for convenience
       ;-----------------------------------------------------------------
       (when (stringp stopList)
             (setq stopList (parseString stopList)))
       ;-----------------------------------------------------------------
       ; Convert the parameters for this cellView
       ;-----------------------------------------------------------------
       (abConvertComponentParams config cellView check)
       ;-----------------------------------------------------------------
       ; Traverse the hierarchy
       ;-----------------------------------------------------------------
       (foreach header (dbGetq cellView instHeaders)
                (when (dbGetq header instances)
                  ;------------------------------------------------------
                  ; Switch into a cellView in the view list
                  ;------------------------------------------------------
                  (setq switchedView (dbGetAnyInstSwitchMaster 
                                       (car (dbGetq header instances))
                                       viewList))
                  (when switchedView
                    ;----------------------------------------------------
                    ; Unless I've been here before, or it's a pcell,
                    ; or it's in the stop list, then recursively call
                    ; this function
                    ;----------------------------------------------------
                    (unless
                      (or
                        (arrayref visited switchedView)
                        (dbGetq switchedView isParamCell)
                        (dbGetq switchedView superMaster)
                        (member (dbGetq switchedView viewName) stopList)
                        )
                      (setarray visited switchedView t)
                      (abConvertComponentParamsHier 
                        config
                        ?cellView switchedView
                        ?viewList viewList
                        ?stopList stopList
                        ?check check
                        ?visited visited
                        )
                      )
                    ) 
                  ) 
                )
       t
       )
  )
