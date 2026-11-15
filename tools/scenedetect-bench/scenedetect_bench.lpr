{*
 * URUWorks MPVPlayer - Scene detection benchmark
 *
 * The contents of this file are used with permission, subject to
 * the Mozilla Public License Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/2.0.html
 *
 * Copyright (C) 2026 URUWorks, uruworks@gmail.com.
 *}

// Measures TMPVSceneDetector against videos with known cuts.
//
//   scenedetect_bench <list> [options]
//
// <list>: a line per video, "video|cuts" (paths relative to the list file,
// "#" for comments). <cuts>: the first frame of each new shot, a time in
// seconds per line ("#" for comments). A gradual transition is its middle and
// the tolerance in frames (half its length): "12.345 9".
//
// options:
//   --tolerance=N  frames of tolerance to match a cut (default 1)
//   --minscore=X   --ratio=Y   --window=W   --gradualmin=G   the detector's parameters
//   --nogradual    only hard cuts
//   --sweep        F1 of a grid of MinScore x Ratio, from the same scores
//   --verbose      the missed and false cuts of each video
//   --cache=DIR    keep what is measured of each video there (<name>.frames),
//                  the next run doesn't read the video again
//
// Reading the videos is the slow part: the scores of every frame are kept and
// the cuts are decided again for every parameter set.

program scenedetect_bench;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, Types, Math, MPVPlayer.SceneDetect;

type
  TVideo = record
    FileName : String;
    Truth    : TDoubleDynArray;
    TruthTol : TIntegerDynArray; // frames, 0 = --tolerance
    Frames   : TMPVSceneFrames;
    Span     : Integer;
    FrameDur : Double;
  end;

  TCounts = record
    TP, FP, FN: Integer;
  end;

var
  Videos     : array of TVideo;
  Tolerance  : Integer = 1;
  MinScore   : Double = 6;
  Ratio      : Double = 2;
  Window     : Double = 0.25;
  Gradual    : Boolean = True;
  GradualMin : Double = 25;
  Sweep      : Boolean = False;
  Verbose    : Boolean = False;
  CacheDir   : String = '';
  ListFile   : String = '';
  FS         : TFormatSettings;

// -----------------------------------------------------------------------------

procedure ReadTruth(const AFileName: String; var V: TVideo);
var
  sl: TStringList;
  i, p, tol: Integer;
  s: String;
  v1: Double;
begin
  V.Truth := NIL;
  V.TruthTol := NIL;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFileName);
    for i := 0 to sl.Count-1 do
    begin
      s := Trim(sl[i]);
      if (s = '') or (s[1] = '#') then Continue;
      tol := 0;
      p := Pos(' ', s);
      if p > 0 then
      begin
        tol := StrToIntDef(Trim(Copy(s, p+1, MaxInt)), 0);
        s := Copy(s, 1, p-1);
      end;
      if TryStrToFloat(s, v1, FS) then
      begin
        SetLength(V.Truth, Length(V.Truth)+1);
        SetLength(V.TruthTol, Length(V.Truth));
        V.Truth[High(V.Truth)] := v1;
        V.TruthTol[High(V.Truth)] := tol;
      end;
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SortDoubles(var A: TDoubleDynArray; L, R: Integer);
var
  i, j: Integer;
  p, t: Double;
begin
  while L < R do
  begin
    i := L;
    j := R;
    p := A[(L + R) div 2];
    repeat
      while A[i] < p do Inc(i);
      while A[j] > p do Dec(j);
      if i <= j then
      begin
        t := A[i]; A[i] := A[j]; A[j] := t;
        Inc(i);
        Dec(j);
      end;
    until i > j;
    if L < j then SortDoubles(A, L, j);
    L := i;
  end;
end;

// -----------------------------------------------------------------------------

function LoadFrames(const AFileName: String; var V: TVideo): Boolean;
var
  sl, p: TStringList;
  i: Integer;
begin
  Result := False;
  if not FileExists(AFileName) or (FileAge(AFileName) < FileAge(V.FileName)) then Exit;
  sl := TStringList.Create;
  p := TStringList.Create;
  try
    p.Delimiter := ' ';
    p.StrictDelimiter := True;
    sl.LoadFromFile(AFileName);
    if (sl.Count < 2) or not sl[0].StartsWith('# span ') then Exit;
    V.Span := StrToInt(Copy(sl[0], 8, MaxInt));
    SetLength(V.Frames, sl.Count-1);
    for i := 1 to sl.Count-1 do
    begin
      p.DelimitedText := sl[i];
      if p.Count < 6 then Exit;
      V.Frames[i-1].Time      := StrToFloat(p[0], FS);
      V.Frames[i-1].Score     := StrToFloat(p[1], FS);
      V.Frames[i-1].Gradual   := StrToFloat(p[2], FS);
      V.Frames[i-1].Linearity := StrToFloat(p[3], FS);
      V.Frames[i-1].Luma      := StrToFloat(p[4], FS);
      V.Frames[i-1].Contrast  := StrToFloat(p[5], FS);
    end;
    Result := True;
  finally
    p.Free;
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveFrames(const AFileName: String; const V: TVideo);
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    sl.Add('# span ' + IntToStr(V.Span));
    for i := 0 to High(V.Frames) do
      with V.Frames[i] do
        sl.Add(FormatFloat('0.000000', Time, FS) + ' ' + FormatFloat('0.000', Score, FS) + ' ' +
          FormatFloat('0.000', Gradual, FS) + ' ' + FormatFloat('0.000', Linearity, FS) + ' ' +
          FormatFloat('0.000', Luma, FS) + ' ' + FormatFloat('0.000', Contrast, FS));
    sl.SaveToFile(AFileName);
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

function Analyse(var V: TVideo): Boolean;
var
  d: TMPVSceneDetector;
  cache: String;
  i: Integer;
  dt: TDoubleDynArray;
begin
  Result := False;
  cache := '';
  if CacheDir <> '' then
    cache := IncludeTrailingPathDelimiter(CacheDir) + ExtractFileName(V.FileName) + '.frames';

  if (cache = '') or not LoadFrames(cache, V) then
  begin
    d := TMPVSceneDetector.Create;
    try
      if not d.Start(V.FileName) then
      begin
        WriteLn(StdErr, V.FileName, ': libmpv not available');
        Exit;
      end;
      d.WaitFor;
      CheckSynchronize;
      if d.Status <> sdsDone then
      begin
        WriteLn(StdErr, V.FileName, ': ', d.ErrorText);
        Exit;
      end;
      V.Frames := d.GetFrames;
      V.Span   := d.GradualSpan;
    finally
      d.Free;
    end;
    if cache <> '' then
    begin
      ForceDirectories(CacheDir);
      SaveFrames(cache, V);
    end;
  end;

  // the usual frame duration (median of the intervals)
  dt := NIL;
  SetLength(dt, Max(Length(V.Frames)-1, 0));
  for i := 1 to High(V.Frames) do
    dt[i-1] := V.Frames[i].Time - V.Frames[i-1].Time;
  if Length(dt) > 0 then
  begin
    SortDoubles(dt, 0, High(dt));
    V.FrameDur := dt[Length(dt) div 2];
  end
  else
    V.FrameDur := 1 / 25;
  Result := True;
end;

// -----------------------------------------------------------------------------

// greedy: each true cut takes the nearest free detected one within tolerance
function Compare(const V: TVideo; const ACuts: TDoubleDynArray; const AVerbose: Boolean): TCounts;
var
  used: array of Boolean;
  i, j, best: Integer;
  tol, dist: Double;
begin
  Result.TP := 0;
  Result.FP := 0;
  Result.FN := 0;
  used := NIL;
  SetLength(used, Length(ACuts));
  for i := 0 to High(V.Truth) do
  begin
    tol := (Max(Tolerance, V.TruthTol[i]) + 0.5) * V.FrameDur;
    best := -1;
    for j := 0 to High(ACuts) do
      if not used[j] and (Abs(ACuts[j] - V.Truth[i]) <= tol) and
        ((best < 0) or (Abs(ACuts[j] - V.Truth[i]) < Abs(ACuts[best] - V.Truth[i]))) then
        best := j;
    if best >= 0 then
    begin
      used[best] := True;
      Inc(Result.TP);
      if AVerbose then
      begin
        dist := (ACuts[best] - V.Truth[i]) / V.FrameDur;
        if (V.TruthTol[i] = 0) and (Abs(dist) > 0.5) then
          WriteLn(Format('    off  %10.3f  (%d frames)', [V.Truth[i], Round(dist)], FS));
      end;
    end
    else
    begin
      Inc(Result.FN);
      if AVerbose then WriteLn(Format('    MISS %10.3f', [V.Truth[i]], FS));
    end;
  end;
  for j := 0 to High(ACuts) do
    if not used[j] then
    begin
      Inc(Result.FP);
      if AVerbose then WriteLn(Format('    FALSE%10.3f', [ACuts[j]], FS));
    end;
end;

// -----------------------------------------------------------------------------

function F1(const C: TCounts): Double;
begin
  if 2*C.TP + C.FP + C.FN = 0 then
    Result := 1
  else
    Result := 2*C.TP / (2*C.TP + C.FP + C.FN);
end;

// -----------------------------------------------------------------------------

function RunAll(const AMinScore, ARatio: Double; const APrint: Boolean): TCounts;
var
  i: Integer;
  c: TCounts;
  cuts: TDoubleDynArray;
begin
  Result.TP := 0;
  Result.FP := 0;
  Result.FN := 0;
  for i := 0 to High(Videos) do
  begin
    cuts := TMPVSceneDetector.FindCuts(Videos[i].Frames, Videos[i].Span, AMinScore, ARatio, Window, Gradual, GradualMin);
    if APrint then
    begin
      c := Compare(Videos[i], cuts, False);
      WriteLn(Format('%-40s cuts %4d  ok %4d  false %3d  missed %3d  F1 %.3f',
        [ExtractFileName(Videos[i].FileName), Length(Videos[i].Truth), c.TP, c.FP, c.FN, F1(c)], FS));
      if Verbose then Compare(Videos[i], cuts, True);
    end
    else
      c := Compare(Videos[i], cuts, False);
    Inc(Result.TP, c.TP);
    Inc(Result.FP, c.FP);
    Inc(Result.FN, c.FN);
  end;
end;

// -----------------------------------------------------------------------------

procedure LoadList;
var
  sl: TStringList;
  i, p: Integer;
  s, dir: String;
  v: TVideo;
begin
  dir := ExtractFilePath(ExpandFileName(ListFile));
  sl := TStringList.Create;
  try
    sl.LoadFromFile(ListFile);
    for i := 0 to sl.Count-1 do
    begin
      s := Trim(sl[i]);
      if (s = '') or (s[1] = '#') then Continue;
      p := Pos('|', s);
      if p = 0 then Continue;
      v := Default(TVideo);
      v.FileName := ExpandFileName(dir + Trim(Copy(s, 1, p-1)));
      ReadTruth(ExpandFileName(dir + Trim(Copy(s, p+1, MaxInt))), v);
      Write(StdErr, 'Reading ', ExtractFileName(v.FileName), '... ');
      if Analyse(v) then
      begin
        WriteLn(StdErr, Length(v.Frames), ' frames');
        SetLength(Videos, Length(Videos)+1);
        Videos[High(Videos)] := v;
      end;
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure DoSweep;
const
  MinScores: array[0..10] of Double = (1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20);
  Ratios: array[0..7] of Double = (1.5, 2, 2.5, 3, 3.5, 4, 5, 6);
var
  i, j: Integer;
  c: TCounts;
  f, best: Double;
  bi, bj: Integer;
begin
  WriteLn;
  WriteLn('F1 (rows MinScore, columns Ratio)');
  Write('        ');
  for j := 0 to High(Ratios) do Write(Format('%7.1f', [Ratios[j]], FS));
  WriteLn;
  best := -1;
  bi := 0;
  bj := 0;
  for i := 0 to High(MinScores) do
  begin
    Write(Format('%6.1f  ', [MinScores[i]], FS));
    for j := 0 to High(Ratios) do
    begin
      c := RunAll(MinScores[i], Ratios[j], False);
      f := F1(c);
      Write(Format('%7.3f', [f], FS));
      if f > best then
      begin
        best := f;
        bi := i;
        bj := j;
      end;
    end;
    WriteLn;
  end;
  c := RunAll(MinScores[bi], Ratios[bj], False);
  WriteLn(Format('best: MinScore %.1f Ratio %.1f  F1 %.3f (ok %d, false %d, missed %d)',
    [MinScores[bi], Ratios[bj], best, c.TP, c.FP, c.FN], FS));
end;

// -----------------------------------------------------------------------------

var
  i: Integer;
  s: String;
  c: TCounts;

begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';

  for i := 1 to ParamCount do
  begin
    s := ParamStr(i);
    if s.StartsWith('--tolerance=') then Tolerance := StrToInt(s.Substring(12))
    else if s.StartsWith('--minscore=') then MinScore := StrToFloat(s.Substring(11), FS)
    else if s.StartsWith('--ratio=') then Ratio := StrToFloat(s.Substring(8), FS)
    else if s.StartsWith('--window=') then Window := StrToFloat(s.Substring(9), FS)
    else if s.StartsWith('--gradualmin=') then GradualMin := StrToFloat(s.Substring(13), FS)
    else if s = '--nogradual' then Gradual := False
    else if s.StartsWith('--cache=') then CacheDir := s.Substring(8)
    else if s = '--sweep' then Sweep := True
    else if s = '--verbose' then Verbose := True
    else if not s.StartsWith('--') then ListFile := s;
  end;

  if ListFile = '' then
  begin
    WriteLn('scenedetect_bench <list> [--tolerance=N] [--minscore=X] [--ratio=Y] [--window=W] [--gradualmin=G] [--nogradual] [--sweep] [--verbose] [--cache=DIR]');
    Halt(1);
  end;

  LoadList;
  if Length(Videos) = 0 then Halt(2);

  WriteLn(Format('MinScore %.1f  Ratio %.1f  Window %.2f  Gradual %s (%.1f)  tolerance %d frame(s)', [MinScore, Ratio, Window, BoolToStr(Gradual, 'yes', 'no'), GradualMin, Tolerance], FS));
  c := RunAll(MinScore, Ratio, True);
  WriteLn(Format('TOTAL  ok %d  false %d  missed %d  precision %.3f  recall %.3f  F1 %.3f',
    [c.TP, c.FP, c.FN, c.TP / Max(c.TP + c.FP, 1), c.TP / Max(c.TP + c.FN, 1), F1(c)], FS));

  if Sweep then DoSweep;
end.
