unit PrjManager;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ComCtrls;

type
  TProjectManager = class(TForm)
      ListView1: TListView;
      procedure ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    private
      function FindModule(const Name: string): TListItem;
      function GetFileName(const i: integer): string;
      function GetFileNameSelected: string;
      function GetFileCount: integer;
    public
      procedure Clear;
      procedure Add(const Dir, Module, Ext: string);
      function RemoveSelection: string;
      function GetModuleIndex(const Module: string): integer;
      procedure Remove(const Module: string);
      procedure Replace(const Module, NewDir, NewName: string);
      property FileName[const i: integer]: string read GetFileName;
      property FileNameSelected: string read GetFileNameSelected;
      property FileCount: integer read GetFileCount;
  end;

var
  ProjectManager: TProjectManager;

implementation

{$R *.DFM}

  procedure TProjectManager.Add(const Dir, Module, Ext: string);
    var
      ListItem: TListItem;
    begin
      with ListView1 do
        begin
          ListItem := Items.Add;
          ListItem.Caption := Module + Ext;
          ListItem.SubItems.Add(Dir);
        end;
    end;

  procedure TProjectManager.Clear;
    begin
      ListView1.Items.Clear;
    end;

  function TProjectManager.RemoveSelection: string;
    begin
      with ListView1 do
        if Assigned(Selected)
          then
            begin
              Result := Selected.Caption;
              Items.Delete(Items.IndexOf(Selected));
            end;
    end;

  procedure TProjectManager.ListView1KeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    begin
      if Key = VK_DELETE
        then RemoveSelection;
    end;

  procedure TProjectManager.Remove(const Module: string);
    var
      ListItem: TListItem;
    begin
      ListItem := FindModule(Module);
      if ListItem <> nil
        then
          with ListView1 do
            Items.Delete(Items.IndexOf(ListItem));
    end;

  procedure TProjectManager.Replace(const Module, NewDir, NewName: string);
    var
      ListItem: TListItem;
    begin
      ListItem := FindModule(Module);
      if ListItem <> nil
        then
          begin
            ListItem.Caption := NewName;
            ListItem.SubItems.Text := NewDir;
          end;
    end;

  function TProjectManager.FindModule(const Name: string): TListItem;
    begin
      Result := ListView1.FindCaption(0, Name, false, true, false);
    end;

  function TProjectManager.GetFileName(const i: integer): string;
    begin
      with ListView1.Items[i] do
        Result := SubItems[0] + '\' + Caption;
    end;

  function TProjectManager.GetFileNameSelected: string;
    begin
      with ListView1.Selected do
        Result := SubItems[0] + '\' + Caption;
    end;

  function TProjectManager.GetFileCount: integer;
    begin
      Result := ListView1.Items.Count;
    end;

  function TProjectManager.GetModuleIndex(const Module: string): integer;
    begin
      Result := ListView1.Items.IndexOf(FindModule(Module));
    end;

end.
