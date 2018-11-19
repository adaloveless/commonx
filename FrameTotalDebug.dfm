object framTotalDebug: TframTotalDebug
  Left = 0
  Top = 0
  Width = 487
  Height = 318
  TabOrder = 0
  object tabCommands: TPageControl
    Left = 0
    Top = 0
    Width = 487
    Height = 318
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    object TabSheet2: TTabSheet
      Caption = 'Commands'
      ImageIndex = 1
      OnShow = TabSheet2Show
      ExplicitLeft = 0
      ExplicitTop = 40
    end
    object TabSheet3: TTabSheet
      Caption = 'Memory'
      ImageIndex = 2
      object Chart1: TChart
        Left = 0
        Top = 0
        Width = 479
        Height = 290
        Title.Text.Strings = (
          'TChart')
        View3DOptions.Elevation = 315
        View3DOptions.Orthogonal = False
        View3DOptions.Perspective = 0
        View3DOptions.Rotation = 360
        Zoom.Pen.Mode = pmNotXor
        Align = alClient
        TabOrder = 0
        DefaultCanvas = 'TGDIPlusCanvas'
        ColorPaletteIndex = 13
        object Series1: TPieSeries
          XValues.Order = loAscending
          YValues.Name = 'Pie'
          YValues.Order = loNone
          Frame.InnerBrush.BackColor = clRed
          Frame.InnerBrush.Gradient.EndColor = clGray
          Frame.InnerBrush.Gradient.MidColor = clWhite
          Frame.InnerBrush.Gradient.StartColor = 4210752
          Frame.InnerBrush.Gradient.Visible = True
          Frame.MiddleBrush.BackColor = clYellow
          Frame.MiddleBrush.Gradient.EndColor = 8553090
          Frame.MiddleBrush.Gradient.MidColor = clWhite
          Frame.MiddleBrush.Gradient.StartColor = clGray
          Frame.MiddleBrush.Gradient.Visible = True
          Frame.OuterBrush.BackColor = clGreen
          Frame.OuterBrush.Gradient.EndColor = 4210752
          Frame.OuterBrush.Gradient.MidColor = clWhite
          Frame.OuterBrush.Gradient.StartColor = clSilver
          Frame.OuterBrush.Gradient.Visible = True
          Frame.Width = 4
          OtherSlice.Legend.Visible = False
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Master Monitor'
      ImageIndex = 3
      object ListView1: TListView
        Left = 0
        Top = 0
        Width = 479
        Height = 290
        Align = alClient
        Columns = <
          item
            Caption = 'Address'
            Width = 100
          end
          item
            Caption = 'ClassName'
            Width = 100
          end
          item
            Caption = 'Status'
            Width = 400
          end>
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object Timer1: TTimer
    Interval = 200
    OnTimer = Timer1Timer
    Left = 232
    Top = 144
  end
end
