(defun extract-bracket-text (txt)
  (if (and txt
           (vl-string-search "(" txt)  ;; 检查是否包含左括号
           (vl-string-search ")" txt)) ;; 检查是否包含右括号
    (progn
      (setq start (vl-string-search "(" txt)) ;; 查找左括号的位置
      (setq end (vl-string-search ")" txt))   ;; 查找右括号的位置
      (setq content (substr txt (+ start 2) (- end (+ start 1))))  ;; 提取括号内的内容
      content)
    txt
  )
)

(defun c:DiKanDataExport ( / ss i ent txt content file)
  ;; 选择文本对象
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))  ;; 选择所有的 TEXT 和 MTEXT 对象
  
  (if ss
    (progn
      ;; 打开文件以写入
      (setq file (open "text_output.txt" "w"))
      
      ;; 遍历选中的对象
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))  ;; 获取第i个选择的实体
        (setq txt (cdr (assoc 1 (entget ent))))  ;; 提取文本内容
        (if txt
          (progn
            ;; 提取括号中的内容
            (setq content (extract-bracket-text txt))
            (if content
              (progn
                ;; 将提取的内容写入文件
                (write-line content file)
              )
            )
          )
        )
        (setq i (1+ i))  ;; 继续处理下一个文本
      )
      
      ;; 关闭文件
      (close file)
      (princ "\n文本已导出到 text_output.txt")
    )
    (princ "\n未选择任何文本对象！")
  )
  (princ)
)
