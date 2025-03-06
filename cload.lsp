;; 入口函数
(defun c:cload (/ calcType)
  ;; 选择计算类型
  (initget "1 2")
  (setq calcType (getkword "\n选择计算类型 [1-梁墙/2-女儿墙] <1-梁墙>: "))
  (if (not calcType) (setq calcType "1"))

  ;; 根据选择调用对应函数
  (cond
    ((= calcType "1") (calc-wall-load)) ; 调用梁墙荷载计算
    ((= calcType "2") (calc-parapet-load)) ; 调用女儿墙荷载计算
  )
  (princ)
)

;; 梁墙荷载计算函数
(defun calc-wall-load (/ wallDensity wallThickness plasterDensity plasterThickness decorationLoad 
                          windowReductionFactor storyHeight beamHeight lineLoadPerMeter noWindowLoad 
                          windowLoad actualWallHeight insertPoint insertPoint2 insertPoint3)
  ;; 用户输入（带默认值）
  (setq wallDensity (getreal "\n填充墙容重 (KN/m3) <8>: "))
  (if (not wallDensity) (setq wallDensity 8))

  (setq wallThickness (getreal "\n填充墙厚度 (m) <0.2>: "))
  (if (not wallThickness) (setq wallThickness 0.2))

  (setq plasterDensity (getreal "\n抹灰砂浆容重 (KN/m3) <20>: "))
  (if (not plasterDensity) (setq plasterDensity 20))

  (setq plasterThickness (getreal "\n单面抹灰厚度 (m) <0.02>: "))
  (if (not plasterThickness) (setq plasterThickness 0.02))

  (setq decorationLoad (getreal "\n墙面装饰荷载 (KN/m2) <0>: "))
  (if (not decorationLoad) (setq decorationLoad 0))
  
  (initget "Y N")
  (setq hasWindow (getkword "\n是否计算窗洞:N/<Y>"))
  (if (or (not hasWindow) (eq hasWindow "Y"))
      (progn
        (setq windowReductionFactor (getreal "\n窗洞折减系数：外墙0.8内墙0.85 <0.8>: "))
        (if (not windowReductionFactor) (setq windowReductionFactor 0.8)))
    (setq windowReductionFactor 1.0))

  (setq storyHeight (getreal "\n输入本层结构层高 (m): "))
  (while (not storyHeight)
    (setq storyHeight (getreal "\n本层结构层高不能为空，请重新输入 (m): ")))

  (setq beamHeight (getreal "\n输入上层梁高 (m): "))
  (while (not beamHeight)
    (setq beamHeight (getreal "\n上层梁高不能为空，请重新输入 (m): ")))

  ;; 计算每米墙高线荷载（KN/m）
  (setq lineLoadPerMeter
    (+
      (* wallDensity wallThickness)               ; 填充墙
      (* plasterDensity plasterThickness 2)        ; 双面抹灰
      decorationLoad                              ; 装饰荷载
    )
  )

  ;; 计算总荷载（KN/m）考虑实际墙高
  (setq actualWallHeight (- storyHeight beamHeight))
  (setq noWindowLoad (* lineLoadPerMeter actualWallHeight))
  (if (or (not hasWindow) (eq hasWindow "Y"))
    (setq windowLoad (* noWindowLoad windowReductionFactor))) 

  ;; 插入CAD文字
  (setvar "TEXTSTYLE" "TSSD_Label")
  (setq insertPoint (getpoint "\n输入插入点: "))
  (setq insertPoint2 (polar insertPoint (* pi -0.5) 350)) ; 下移350单位
  (setq insertPoint3 (polar insertPoint2 (* pi -0.5) 350))
  (setq insertPoint4 (polar insertPoint3 (* pi -0.5) 350))

  (if (or (not hasWindow) (eq hasWindow "Y"))
    (command "._TEXT" insertPoint4 300 0
    (strcat "有窗洞总荷载: " 
      (rtos noWindowLoad 2 2) "*" (rtos windowReductionFactor 2 2) " = " 
      (rtos windowLoad 2 2) " KN/m")))

  (command "._TEXT" insertPoint3 300 0 
    (strcat "无窗洞总荷载: " 
      (rtos lineLoadPerMeter 2 2) "* (" (rtos storyHeight 2 2) "-" (rtos beamHeight 2 2) ") = " 
      (rtos noWindowLoad 2 2) " KN/m"))

  (command "._TEXT" insertPoint2 300 0 
    (strcat "每米墙高线荷载: " 
      (rtos wallDensity 2 2) "*" (rtos wallThickness 2 2) " + " 
      (rtos plasterDensity 2 2) "*" (rtos plasterThickness 2 2) "*2 + " 
      (rtos decorationLoad 2 2) " = " 
      (rtos lineLoadPerMeter 2 2) " KN/m"))

  (command "._TEXT" insertPoint 300 0 
    (strcat "标高: " "XXX" "~" "XXX"))

  (princ)
)

;; 女儿墙荷载计算函数
(defun calc-parapet-load (/ wallDensity wallThickness plasterDensity plasterThickness decorationLoad 
                             storyHeight lineLoadPerMeter parapetLoad insertPoint)
  ;; 用户输入（带默认值）
  (setq wallDensity (getreal "\n女儿墙容重 (KN/m3) <25>: "))
  (if (not wallDensity) (setq wallDensity 25))

  (setq wallThickness (getreal "\n女儿墙厚度 (m) <0.15>: "))
  (if (not wallThickness) (setq wallThickness 0.15))

  (setq plasterDensity (getreal "\n抹灰砂浆容重 (KN/m3) <20>: "))
  (if (not plasterDensity) (setq plasterDensity 20))

  (setq plasterThickness (getreal "\n单面抹灰厚度 (m) <0.02>: "))
  (if (not plasterThickness) (setq plasterThickness 0.02))

  (setq storyHeight (getreal "\n女儿墙高度 (m): "))
  (while (not storyHeight)
    (setq storyHeight (getreal "\n女儿墙高度不能为空，请重新输入 (m): ")))

  ;; 计算每米墙高线荷载（KN/m）
  (setq lineLoadPerMeter
    (+
      (* wallDensity wallThickness)               ; 女儿墙
      (* plasterDensity plasterThickness 2)        ; 双面抹灰
    )
  )
  (princ lineLoadPerMeter)
  ;; 计算女儿墙总荷载（KN/m）
  (setq parapetLoad (* lineLoadPerMeter storyHeight))

  ;; 输出结果
  (princ (strcat "\n女儿墙总荷载: " (rtos parapetLoad 2 2) " KN/m"))

  ;; 插入CAD文字
  (setvar "TEXTSTYLE" "TSSD_Label")
  (setq insertPoint (getpoint "\n输入插入点: "))

  (command "._TEXT" insertPoint 300 0 
    (strcat "女儿墙总荷载: " 
      "("
      (rtos wallDensity 2 2) "*" (rtos wallThickness 2 2) " + "
      (rtos plasterDensity 2 2) "*" (rtos plasterThickness 2 2)  "*2"
      ")" "*" (rtos storyHeight 2 2) " = " 
      (rtos parapetLoad 2 2) " KN/m"))
  (princ)
)