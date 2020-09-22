VERSION 5.00
Begin VB.Form FrmMain 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "Form1"
   ClientHeight    =   9000
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   7650
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   9000
   ScaleWidth      =   7650
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
   WindowState     =   2  'Maximized
   Begin VB.TextBox Text2 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      Height          =   285
      Left            =   0
      TabIndex        =   8
      Top             =   0
      Width           =   150
   End
   Begin VB.Timer Timer1 
      Interval        =   10
      Left            =   600
      Top             =   1560
   End
   Begin VB.Label Label12 
      BackColor       =   &H00000000&
      BackStyle       =   0  'Transparent
      Caption         =   "- Press Q to Quit."
      BeginProperty Font 
         Name            =   "MS Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00008000&
      Height          =   375
      Left            =   120
      TabIndex        =   12
      Top             =   8520
      Width           =   3855
   End
   Begin VB.Label Label11 
      BackColor       =   &H00C00000&
      Caption         =   "Label2"
      DragMode        =   1  'Automatic
      ForeColor       =   &H00C00000&
      Height          =   135
      Left            =   3120
      TabIndex        =   11
      Top             =   7920
      Width           =   135
   End
   Begin VB.Label Label10 
      BackColor       =   &H00000000&
      BackStyle       =   0  'Transparent
      Caption         =   "- Drag and Drop The Blue Square's      To Change The Drawing."
      BeginProperty Font 
         Name            =   "MS Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00008000&
      Height          =   375
      Left            =   120
      TabIndex        =   10
      Top             =   7800
      Width           =   5775
   End
   Begin VB.Label Label9 
      BackColor       =   &H00000000&
      BackStyle       =   0  'Transparent
      Caption         =   "- Press A or Z and Hold It to Zoom In And Out."
      BeginProperty Font 
         Name            =   "MS Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00008000&
      Height          =   375
      Left            =   120
      TabIndex        =   9
      Top             =   8160
      Width           =   3855
   End
   Begin VB.Line l12 
      BorderColor     =   &H0000FF00&
      X1              =   5880
      X2              =   6720
      Y1              =   5760
      Y2              =   4680
   End
   Begin VB.Line l11 
      BorderColor     =   &H0000FF00&
      X1              =   3360
      X2              =   4680
      Y1              =   5760
      Y2              =   4680
   End
   Begin VB.Line l10 
      BorderColor     =   &H0000FF00&
      X1              =   4680
      X2              =   6720
      Y1              =   4680
      Y2              =   4680
   End
   Begin VB.Label Label8 
      BackColor       =   &H00C00000&
      Caption         =   "Label8"
      DragMode        =   1  'Automatic
      ForeColor       =   &H00C00000&
      Height          =   135
      Left            =   6720
      TabIndex        =   7
      Top             =   4680
      Width           =   135
   End
   Begin VB.Line l9 
      BorderColor     =   &H0000FF00&
      X1              =   6720
      X2              =   6720
      Y1              =   3000
      Y2              =   4680
   End
   Begin VB.Label Label7 
      BackColor       =   &H00C00000&
      Caption         =   "Label7"
      DragMode        =   1  'Automatic
      ForeColor       =   &H00C00000&
      Height          =   135
      Left            =   4680
      TabIndex        =   6
      Top             =   4680
      Width           =   135
   End
   Begin VB.Line l8 
      BorderColor     =   &H0000FF00&
      X1              =   4680
      X2              =   4680
      Y1              =   3000
      Y2              =   4680
   End
   Begin VB.Line l7 
      BorderColor     =   &H0000FF00&
      X1              =   4680
      X2              =   6720
      Y1              =   3000
      Y2              =   3000
   End
   Begin VB.Label Label6 
      BackColor       =   &H00C00000&
      Caption         =   "Label6"
      DragMode        =   1  'Automatic
      ForeColor       =   &H00800000&
      Height          =   135
      Left            =   6720
      TabIndex        =   5
      Top             =   3000
      Width           =   135
   End
   Begin VB.Line l6 
      BorderColor     =   &H0000FF00&
      X1              =   5880
      X2              =   6720
      Y1              =   3960
      Y2              =   3000
   End
   Begin VB.Label Label5 
      BackColor       =   &H00C00000&
      Caption         =   "Label5"
      DragMode        =   1  'Automatic
      ForeColor       =   &H00C00000&
      Height          =   135
      Left            =   4680
      TabIndex        =   4
      Top             =   3000
      Width           =   135
   End
   Begin VB.Line l5 
      BorderColor     =   &H0000FF00&
      X1              =   3360
      X2              =   4680
      Y1              =   3960
      Y2              =   3000
   End
   Begin VB.Line l4 
      BorderColor     =   &H0000FF00&
      X1              =   5880
      X2              =   5880
      Y1              =   3960
      Y2              =   5760
   End
   Begin VB.Label Label4 
      BackColor       =   &H00C00000&
      Caption         =   "Label4"
      DragMode        =   1  'Automatic
      ForeColor       =   &H00C00000&
      Height          =   135
      Left            =   5880
      TabIndex        =   3
      Top             =   5760
      Width           =   135
   End
   Begin VB.Line l3 
      BorderColor     =   &H0000FF00&
      X1              =   3360
      X2              =   5880
      Y1              =   5760
      Y2              =   5760
   End
   Begin VB.Label Label3 
      BackColor       =   &H00C00000&
      Caption         =   "Label3"
      DragMode        =   1  'Automatic
      ForeColor       =   &H00C00000&
      Height          =   135
      Left            =   5880
      TabIndex        =   2
      Top             =   3960
      Width           =   135
   End
   Begin VB.Label Label2 
      BackColor       =   &H00C00000&
      Caption         =   "Label2"
      DragMode        =   1  'Automatic
      ForeColor       =   &H00C00000&
      Height          =   135
      Left            =   3360
      TabIndex        =   1
      Top             =   5760
      Width           =   135
   End
   Begin VB.Line l2 
      BorderColor     =   &H0000FF00&
      X1              =   5880
      X2              =   3360
      Y1              =   3960
      Y2              =   3960
   End
   Begin VB.Label Label1 
      BackColor       =   &H00C00000&
      Caption         =   "Label1"
      DragMode        =   1  'Automatic
      ForeColor       =   &H00C00000&
      Height          =   135
      Left            =   3360
      TabIndex        =   0
      Top             =   3960
      Width           =   135
   End
   Begin VB.Line l1 
      BorderColor     =   &H0000FF00&
      X1              =   3360
      X2              =   3360
      Y1              =   5760
      Y2              =   3960
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Form_DragDrop(Source As Control, X As Single, Y As Single)
Source.Left = X
Source.Top = Y
End Sub

Private Sub Form_Load()
Text2.Text = ""
End Sub

Private Sub Text2_Change()
If Text2.Text = "a" Then
Text2.Text = ""
Label2.Top = Label2.Top + 100
Label2.Left = Label2.Left - 100
Label4.Top = Label4.Top + 100
Label4.Left = Label4.Left + 100
Label7.Top = Label7.Top + 100
Label7.Left = Label7.Left - 100
Label8.Top = Label8.Top + 100
Label8.Left = Label8.Left + 100
Label3.Top = Label3.Top - 100
Label3.Left = Label3.Left + 100
Label1.Top = Label1.Top - 100
Label1.Left = Label1.Left - 100
Label5.Top = Label5.Top - 100
Label5.Left = Label5.Left - 100
Label6.Top = Label6.Top - 100
Label6.Left = Label6.Left + 100
ElseIf Text2.Text = "z" Then
Text2.Text = ""
Label2.Top = Label2.Top - 100
Label2.Left = Label2.Left + 100
Label4.Top = Label4.Top - 100
Label4.Left = Label4.Left - 100
Label7.Top = Label7.Top - 100
Label7.Left = Label7.Left + 100
Label8.Top = Label8.Top - 100
Label8.Left = Label8.Left - 100
Label3.Top = Label3.Top + 100
Label3.Left = Label3.Left - 100
Label1.Top = Label1.Top + 100
Label1.Left = Label1.Left + 100
Label5.Top = Label5.Top + 100
Label5.Left = Label5.Left + 100
Label6.Top = Label6.Top + 100
Label6.Left = Label6.Left - 100
ElseIf Text2.Text = "q" Then
End
Else: Text2.Text = ""
End If
End Sub

Private Sub Timer1_Timer()
l1.X1 = Label2.Left
l1.Y1 = Label2.Top
l3.X1 = Label2.Left
l3.Y1 = Label2.Top
l11.X1 = Label2.Left
l11.Y1 = Label2.Top

l1.X2 = Label1.Left
l1.Y2 = Label1.Top
l2.X2 = Label1.Left
l2.Y2 = Label1.Top
l5.X1 = Label1.Left
l5.Y1 = Label1.Top

l2.X1 = Label3.Left
l2.Y1 = Label3.Top
l4.X1 = Label3.Left
l4.Y1 = Label3.Top
l6.X1 = Label3.Left
l6.Y1 = Label3.Top

l9.X2 = Label8.Left
l9.Y2 = Label8.Top
l10.X2 = Label8.Left
l10.Y2 = Label8.Top
l12.X2 = Label8.Left
l12.Y2 = Label8.Top

l8.X2 = Label7.Left
l8.Y2 = Label7.Top
l10.X1 = Label7.Left
l10.Y1 = Label7.Top
l11.X2 = Label7.Left
l11.Y2 = Label7.Top

l6.X2 = Label6.Left
l6.Y2 = Label6.Top
l7.X2 = Label6.Left
l7.Y2 = Label6.Top
l9.X1 = Label6.Left
l9.Y1 = Label6.Top

l5.X2 = Label5.Left
l5.Y2 = Label5.Top
l7.X1 = Label5.Left
l7.Y1 = Label5.Top
l8.X1 = Label5.Left
l8.Y1 = Label5.Top

l3.X2 = Label4.Left
l3.Y2 = Label4.Top
l4.X2 = Label4.Left
l4.Y2 = Label4.Top
l12.X1 = Label4.Left
l12.Y1 = Label4.Top

End Sub
