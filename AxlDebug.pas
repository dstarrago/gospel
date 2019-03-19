{
Author:       Medardo Rodríguez
Description:  Debugger helpers
e-mail:       med@merchise.vcl.sld.cu, medx@canada.com, med@nekonline.com
Creation:     January, 1998
Version:      1.00
Support:      Send e-mails directly to me
Legal issues: Copyright (c) 1998 by Medardo Rodriguez
              Instituto de Informática (Institute of Imformatics)
              Universidad Central de Las Villas (Central University of Las Villas)
              Santa Clara, Villa Clara, Cuba CP: 54830
              Tel: +53-(422)-81515

              This software is provided without any warranty.
              The author don't held liable for any damages because the use
              of this software.

              Permission is granted to anyone to use this module for any
              purpose only subject to the following restrictions:

              . The origin of this code must not be changed,
                you must not claim that you produced the original source code.
                If this software is used in a product, an acknowledgment
                in the product and its documentation will be required.

              . This notice may not be removed or altered from any source code
                distribution.

}
unit AxlDebug;

interface

uses
  SysUtils;

{$ifopt D-}
  {$define _NODEBUG}
{$endif}


const
  DEBUGGING = {$ifndef _NODEBUG} true {$else} false {$endif};
  ASSERTING = {$ifopt C+} true {$else} false {$endif};


type
  IDebugConsole =
    interface
      procedure WriteDebugStr(const which : string);
    end;

var
  Console : IDebugConsole = nil;
      
procedure LogThis(const msg : string);
procedure WriteDebugStr(const which : string);     // protected
procedure DebugBreakPoint;


implementation


{$ifndef _NODEBUG}

uses
  Windows;


procedure WriteDebugStr(const which : string);
  begin
    OutputDebugString(pchar(which));
    if IsConsole
      then writeln(which);
    if Console <> nil
      then Console.WriteDebugStr(which);
  end;

procedure DebugBreakPoint;
  begin
    DebugBreak;
  end;

procedure LogThis(const msg : string);
  begin
    WriteDebugStr(FormatDateTime('hh:nn:ss <', Time) + '>' + msg);
  end;

(*
// Debug Memory Manager

type
  PDebugMemoryHeader = ^TDebugMemoryHeader;
  TDebugMemoryHeader =
    record
      Previous : PDebugMemoryHeader;
      Next     : PDebugMemoryHeader;
      Size     : integer;
    end;

const
  Signature : array[0..7] of char = 'MeRcHiSe';

var
  gMemoryBlocks    : integer = 0;
  gLastMemoryBlock : PDebugMemoryHeader = nil;


function GetBlockSize(which : pointer) : integer;
  begin
    
  end;

function CheckBlock(which : pointer) : boolean;
  begin
    assert(which <> nil);
    dec(which, sizeof(Signature));
    if CompareSignature(pchar(which) - sizeof(Signature)) and CompareSignature(pchar() )
      then
        begin
  end;
  
function DebugGetMem(Size : integer) : pointer;
  var
    aux : pointer;
  begin
    inc(gMemoryBlocks);
    if Size > 0
      then
        begin
          aux := SysGetMem(Size + sizeof(TDebugMemoryHeader) + 2*sizeof(Signature));
          if aux <> nil
            then
              begin
                TDebugMemoryHeader(aux^).Next     := nil;
                TDebugMemoryHeader(aux^).Previous := gLastMemoryBlock;
                TDebugMemoryHeader(aux^).Size     := Size;
                gLastMemoryBlock                  := aux;
                inc(pchar(aux), sizeof(TDebugMemoryHeader));
                move(Signature, aux^, sizeof(Signature));
                inc(pchar(aux), sizeof(Signature));
                move(Signature, (pchar(aux) + Size)^, sizeof(Signature));
              end;
          Result := aux;
        end
      else Result := nil;
  end;

function DebugFreeMem(p : pointer) : integer;
  begin
    CheckBlock(p);

  end;

function DebugReallocMem(p : pointer; Size : integer) : pointer;
  begin
  end;


procedure SetDebugMemoryManager;
  var
    Manager : TMemoryManager;
  begin
    assert(not IsMemoryManagerSet);
    Manager.GetMem     := DebugGetMem;
    Manager.FreeMem    := DebugFreeMem;
    Manager.ReallocMem := DebugReallocMem;
    SetMemoryManager(Manager);
  end;

initialization
  SetDebugMemoryManager;
*)  

{$else}

procedure LogThis(const msg : string);
  begin
  end;

procedure WriteDebugStr(const which : string);     // protected
  begin
  end;

procedure DebugBreakPoint;
  begin
  end;


{$endif}

end.

