�
 TCOMPILER 0>  TPF0	TCompilerCompilerLeft� Top� Width�HeightECaptionCompilerColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderWindowStatewsMaximizedOnCreate
FormCreate	OnDestroyFormDestroyPixelsPerInch`
TextHeight TPanelPanel1Left� Top WidthKHeightAlignalRightCaptionPanel1TabOrder  TPanelPanel3Left� TopWidth� Height� AlignalClientCaptionPanel3TabOrder  	TGroupBox	GroupBox1LeftTopWidth� Height`AlignalTopCaptionTypesTabOrder  TListBoxTypesLeftTopWidth� HeightOAlignalClient
ItemHeightTabOrder    	TGroupBox	GroupBox2LeftTopaWidth� HeightZAlignalTopCaption	VariablesTabOrder TListBox	VariablesLeftTopWidth� HeightIAlignalClient
ItemHeightTabOrder    	TGroupBox	GroupBox3LeftTop� Width� HeightXAlignalTopCaption	ConstantsTabOrder TListBox	ConstantsLeftTopWidth� HeightGAlignalClient
ItemHeightTabOrder    	TGroupBox	GroupBox5LeftTopWidth� Height� AlignalClientCaption
ProceduresTabOrder TListBox
ProceduresLeftTopWidth� HeightzAlignalClient
ItemHeightTabOrder    	TGroupBox	GroupBox6LeftTop� Width� HeightUAlignalBottomCaption	FunctionsTabOrder TListBox	FunctionsLeftTopWidth� HeightDAlignalClient
ItemHeightTabOrder     TPanelPanel4LeftTopWidth� Height� AlignalLeftCaptionPanel4TabOrder 	TGroupBox	GroupBox4LeftTop� Width� HeightuAlignalClientCaptionTokensTabOrder  TListBoxTokensLeftTopWidth� HeightdAlignalClient
ItemHeightTabOrder    	TGroupBox	GroupBox7LeftTopWidth� Height� AlignalTopCaptionBlocksTabOrder TListBoxBlocksLeftTopWidth� HeightoAlignalClient
ItemHeightTabOrder OnClickBlocksClick    TPanelPanel2LeftTop� WidthIHeightAlignalBottom
BevelOuterbvNoneCaptionPanel2TabOrder TPanelPanel5Left Top Width� HeightAlignalLeft
BevelInnerbvRaised
BevelOuter	bvLoweredCaptionCheck SyntaxTabOrder OnClickPanel2ClickOnMouseDownPanel2MouseDown	OnMouseUpPanel2MouseUp  TPanelRunLeft� Top Width� HeightAlignalClient
BevelInnerbvRaised
BevelOuter	bvLoweredCaptionRunTabOrderOnClickRunClickOnMouseDownPanel2MouseDown	OnMouseUpPanel2MouseUp    
TStatusBar
StatusBar1Left TopWidth�HeightPanels SimplePanel	  TPanelPanel6Left Top Width� HeightAlignalClientCaptionPanel6TabOrder TMemoMemo1LeftTop8WidthaHeight� Lines.StringsModule Caspiola; type  y = array[3..4] of integer; type  TInt = integer;
  Frame =      record        x, y, z: real;	     end; var  i: integer;  r1, r2: real;  F1, F2: Frame; const!  A = 'Esta es mi cadena de oro';
  N = 100;8  Pi = 3.14159;   // Este es un aproximado del numero Pi 5  procedure PerroFrito(Killer: real; Perro: integer);    var       Test: integer;     procedure GatoTuerto;	      var          pescado: real;      begin
      end; 	    begin        if Killer = 'Barreto'          then Perro := 0;    end; &  function Succ( const x: real): real;
     begin         Succ := x + 1;	     end; &  function Pred( const x: real): real;
     begin         Succ := x - 1;	     end;   procedure UpDate;     var        Pos: Frame;
     begin         if r1 > r2            then r1 := r2;
      end; begin   for i := 1 to N do      begin          r1 := Pi * i;          r2 := r1 / N + i;          if  r1 = r2              then                begin$                     r1 := Succ(r2);                     UpDate;                 end !             else r1 := Pred(r2);
      end;end.   TabOrderVisibleWordWrap  TMemoMemo2LeftTopWidth� Height� AlignalClientLines.StringsModule Pelosonte; var  a, b, c: integer;begin   a := 20;
   b := 5;   c := a + b;   output c;   c := a - b;   output c;   c := a * b;   output c;   c := a / b;   output c;   a := a mod b;   output c;end. TabOrder   	TGroupBox	GroupBox8LeftTop� Width� HeighthAlignalBottomCaptionConsoleTabOrder TListBoxConsoleLeftTopWidth� HeightWAlignalClient
ItemHeightTabOrder      