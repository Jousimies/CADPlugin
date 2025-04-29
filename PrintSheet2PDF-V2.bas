Attribute VB_Name = "模块1"
Sub PrintSheets2PDF_NoColor()
    Dim ws As Worksheet
    Dim startSheet As Worksheet, endSheet As Worksheet
    Dim startIndex As Integer, endIndex As Integer
    Dim outputPath As String
    Dim pdfName As String
    Dim printRange As String
    Dim sheetName1 As String, sheetName2 As String
    Dim CellA As String, CellB As String
    Dim fd As FileDialog
    Dim originalInteriorColor() As Variant
    Dim originalFontColor() As Variant
    Dim rng As Range
    Dim i As Integer
    
    ' 获取用户输入
    sheetName1 = InputBox("请输入起始工作表名称：", "打印设置", "Z1")
    If sheetName1 = "" Then Exit Sub
    
    sheetName2 = InputBox("请输入末尾工作表名称：", "打印设置", "6")
    If sheetName2 = "" Then Exit Sub
    
    CellA = InputBox("请输入左上单元格号：", "打印设置", "A1")
    If CellA = "" Then Exit Sub
    
    CellB = InputBox("请输入右下单元格号：", "打印设置", "H40")
    If CellB = "" Then Exit Sub
    
    ' 验证工作表是否存在
    On Error Resume Next
    Set startSheet = ThisWorkbook.Sheets(sheetName1)
    Set endSheet = ThisWorkbook.Sheets(sheetName2)
    On Error GoTo 0
    
    If startSheet Is Nothing Or endSheet Is Nothing Then
        MsgBox "指定的工作表不存在，请检查名称是否正确。", vbExclamation
        Exit Sub
    End If
    
    startIndex = startSheet.Index
    endIndex = endSheet.Index
    printRange = "$" & CellA & ":$" & CellB
    
    ' 选择保存位置
    Set fd = Application.FileDialog(msoFileDialogFolderPicker)
    With fd
        .Title = "选择PDF保存位置"
        .InitialFileName = Environ("UserProfile") & "\Desktop\"
        If .Show <> -1 Then Exit Sub
        outputPath = .SelectedItems(1) & "\"
    End With
    
    Application.ScreenUpdating = False
    Application.PrintCommunication = False
    
    For i = startIndex To endIndex
        Set ws = ThisWorkbook.Sheets(i)
        Set rng = ws.Range(printRange)
        
        ' 保存原始颜色设置
        ReDim originalInteriorColor(1 To rng.Cells.Count)
        ReDim originalFontColor(1 To rng.Cells.Count)
        
        Dim cellIndex As Integer
        cellIndex = 1
        
        For Each cell In rng
            originalInteriorColor(cellIndex) = cell.Interior.Color
            originalFontColor(cellIndex) = cell.Font.Color
            cellIndex = cellIndex + 1
        Next cell
        
        ' 移除所有颜色
        With rng
            .Interior.Color = xlNone  ' 移除背景色
            .Font.Color = vbBlack    ' 设置字体为黑色
            .Font.TintAndShade = 0  ' 移除字体色调
            
            ' 设置边框
            .Borders.LineStyle = xlContinuous
            .Borders.Color = vbBlack
            .Borders.TintAndShade = 0
            .Borders.Weight = xlThin
            
            ' 单独设置各边边框
            .Borders(xlEdgeLeft).LineStyle = xlContinuous
            .Borders(xlEdgeTop).LineStyle = xlContinuous
            .Borders(xlEdgeBottom).LineStyle = xlContinuous
            .Borders(xlEdgeRight).LineStyle = xlContinuous
            .Borders(xlInsideVertical).LineStyle = xlNone
            .Borders(xlInsideHorizontal).LineStyle = xlNone
        End With
        
        ' 设置页面
        ws.PageSetup.PrintArea = printRange
        With ws.PageSetup
            .LeftHeader = ""
            .CenterHeader = ""
            .RightHeader = ""
            .LeftFooter = ""
            .CenterFooter = ""
            .RightFooter = ""
            
            .LeftMargin = Application.InchesToPoints(0.7)
            .RightMargin = Application.InchesToPoints(0.7)
            .TopMargin = Application.InchesToPoints(0.75)
            .BottomMargin = Application.InchesToPoints(0.75)
            .HeaderMargin = Application.InchesToPoints(0.3)
            .FooterMargin = Application.InchesToPoints(0.3)
            
            .Orientation = xlPortrait
            .PaperSize = xlPaperA4
            .FitToPagesWide = 1
            .FitToPagesTall = 1
            .Zoom = True
            .PrintGridlines = True
            .PrintQuality = 600
        End With
        
        ' 生成PDF文件名
        pdfName = outputPath & Format(Now(), "yyyymmdd") & "_" & ws.Name & ".pdf"
        
        ' 导出PDF
        Application.PrintCommunication = True
        ws.ExportAsFixedFormat _
            Type:=xlTypePDF, _
            Filename:=pdfName, _
            Quality:=xlQualityStandard, _
            IncludeDocProperties:=True, _
            IgnorePrintAreas:=False, _
            OpenAfterPublish:=False
        Application.PrintCommunication = False
        
        ' 恢复原始颜色设置
        cellIndex = 1
        For Each cell In rng
            cell.Interior.Color = originalInteriorColor(cellIndex)
            cell.Font.Color = originalFontColor(cellIndex)
            cellIndex = cellIndex + 1
        Next cell
    Next i
    
    Application.ScreenUpdating = True
    Application.PrintCommunication = True
    
    MsgBox "共导出 " & (endIndex - startIndex + 1) & " 个工作表到指定位置", vbInformation
End Sub

