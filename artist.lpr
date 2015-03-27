{ ARTIST - Another paint clone
  Written by Dmitry D. Chernov, 16.01.2015 }

program artist;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  main, f_imagesize, tools;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSizeForm, SizeForm);
  Application.Title := MainForm.Caption;
  Application.Run;
end.

