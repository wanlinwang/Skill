;ineed('pinLppChange t)
(setq insts geGetSelectedSet())

defun(pinLppChange (@optional (newLay "MET3") (insts geGetSelectedSet()))
; inst=car(insts)
; newLay="MET3"
foreach(mapcar inst insts inst~>layerName=newLay ))


;pinLppChange("MET2");<= change selected label and pins into new layername without changing the purpose of it either pin or drawinf
;pinLppChange("MET2")