unit PrintfUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils, Forms, Controls, LCLType, 
  LoggerBase, VirtualTrees;

type
  // 검색 옵션
  TSearchOptions = record
    SearchText: string;
    MatchCase: Boolean;
    WholeWord: Boolean;
    SearchInTime: Boolean;
    SearchInLevel: Boolean;
    SearchInMessage: Boolean;
    SearchInSource: Boolean; // 소스 검색 옵션 추가
  end;

  // 필터 옵션
  TFilterOptions = record
    Enabled: Boolean;
    ShowInfo: Boolean;
    ShowDebug: Boolean;
    ShowWarning: Boolean;
    ShowError: Boolean;
    ShowFatal: Boolean;
    ShowDevelop: Boolean;
    TimeFrom: TDateTime;
    TimeTo: TDateTime;
    FilterTimeEnabled: Boolean;
    FilterText: string;
    FilterTextEnabled: Boolean;
    FilterTextMatchCase: Boolean;
    FilterSource: string; // 소스 필터 추가
    FilterSourceEnabled: Boolean; // 소스 필터 활성화 여부
  end;

  // 컬럼 가시성 옵션 추가
  TColumnVisibility = record
    ShowTimeColumn: Boolean;
    ShowLevelColumn: Boolean;
    ShowSourceColumn: Boolean;
    ShowMessageColumn: Boolean;
  end;

  // 폼 위치 옵션
  TFormPosition = (fpTopLeft, fpBottomLeft, fpTopRight, fpBottomRight, fpCustom);

  // 로그 추적 모드
  TTraceMode = (tmNormal, tmAutoScroll);

  // 로그 아이템 데이터 구조
  PLogData = ^TLogData;
  TLogData = record
    Message: string;
    Level: TLogLevel;
    Time: TDateTime;  // 로그 시간
    Source: string;   // 소스 정보
  end;

  // 로그 큐 아이템
  TPrintfLogQueueItem = record
    Message: string;
    Level: TLogLevel;
    Source: string;  // 소스 정보
  end;
  PPrintfLogQueueItem = ^TPrintfLogQueueItem;

  { TPrintfHandlerThread - 비동기 로깅용 스레드 }
  TPrintfHandlerThread = class(TThread)
  private
    FOwner: TObject;          // 소유자 (TPrintfHandler)
    FQueueEvent: TEvent;      // 큐 이벤트
    FTerminateEvent: TEvent;  // 종료 이벤트
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;

    procedure SignalQueue;    // 큐 처리 신호
    procedure SignalTerminate; // 종료 신호
  end;

// 디버그 로그 출력 함수
procedure DebugToFile(const Msg: string);

// 날짜/시간 문자열 파싱 헬퍼 함수
function IsValidDateTimeFormat(const DateTimeStr: string): Boolean;

// 로그 레벨 관련 헬퍼 함수
function IsValidLogLevel(const LevelStr: string): Boolean;
function StringToLogLevel(const LevelStr: string): TLogLevel;

// 로그 라인 파싱 관련 함수
function ParseLogLineBasic(const LogLine: string;
                         out TimeStr, LevelStr, SourceStr, MsgStr: string): Boolean;

// 파일 관련 헬퍼 함수
function GetFileSize(const FileName: string): Int64;
function GetLastLinesFromFile(const FileName: string; LineCount: Integer): TStringList;

implementation

{$IFDEF WINDOWS}
uses
  Windows;
{$ENDIF}

procedure DebugToFile(const Msg: string);
var
  F: TextFile;
begin
  AssignFile(F, 'debug.log');
  try
    if FileExists('debug.log') then
      Append(F)
    else
      Rewrite(F);
    WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ': ' + Msg);
  finally
    CloseFile(F);
  end;
end;

function IsValidDateTimeFormat(const DateTimeStr: string): Boolean;
var
  DateTime: TDateTime;
begin
  Result := False;

  try
    // 날짜/시간 형식인지 확인
    DateTime := StrToDateTime(DateTimeStr);
    Result := True;
  except
    // 날짜/시간 형식이 아니면 False 반환
    Result := False;
  end;
end;

function IsValidLogLevel(const LevelStr: string): Boolean;
var
  UpperLevelStr: string;
begin
  UpperLevelStr := UpperCase(LevelStr);

  // 알려진 로그 레벨인지 확인
  Result := (UpperLevelStr = 'INFO') or
            (UpperLevelStr = 'DEBUG') or
            (UpperLevelStr = 'DEVEL') or
            (UpperLevelStr = 'DEVELOP') or
            (UpperLevelStr = 'WARNING') or
            (UpperLevelStr = 'WARN') or
            (UpperLevelStr = 'ERROR') or
            (UpperLevelStr = 'FATAL');
end;

function StringToLogLevel(const LevelStr: string): TLogLevel;
var
  UpperLevelStr: string;
begin
  UpperLevelStr := UpperCase(LevelStr);

  if UpperLevelStr = 'INFO' then
    Result := llInfo
  else if UpperLevelStr = 'DEBUG' then
    Result := llDebug
  else if (UpperLevelStr = 'WARN') or (UpperLevelStr = 'WARNING') then
    Result := llWarning
  else if UpperLevelStr = 'ERROR' then
    Result := llError
  else if UpperLevelStr = 'FATAL' then
    Result := llFatal
  else if (UpperLevelStr = 'DEVEL') or (UpperLevelStr = 'DEVELOP') then
    Result := llDevelop
  else
    Result := llInfo; // 기본값
end;

function ParseLogLineBasic(const LogLine: string;
                         out TimeStr, LevelStr, SourceStr, MsgStr: string): Boolean;
const
  TimeSeparators: array[0..2] of Char = (':', '.', '-');
var
  Parts: TStringList;
  i, TimePart: Integer;
  HasTime, HasLevel: Boolean;
  WordStart, WordEnd, WordLen: Integer;
  Word: string;
begin
  Result := False;
  TimeStr := '';
  LevelStr := '';
  SourceStr := '';
  MsgStr := LogLine;

  // 명확한 구분자가 없는 경우 단어 기반으로 파싱 시도
  Parts := TStringList.Create;
  try
    // 공백으로 분리 (WordCount/ExtractWord 대신 직접 구현)
    WordStart := 1;
    while WordStart <= Length(LogLine) do
    begin
      // 앞쪽 공백 건너뛰기
      while (WordStart <= Length(LogLine)) and
            ((LogLine[WordStart] = ' ') or (LogLine[WordStart] = #9)) do
        Inc(WordStart);

      if WordStart > Length(LogLine) then
        Break;

      // 단어 끝 찾기
      WordEnd := WordStart;
      while (WordEnd <= Length(LogLine)) and
            (LogLine[WordEnd] <> ' ') and (LogLine[WordEnd] <> #9) do
        Inc(WordEnd);

      // 단어 추출 및 추가
      WordLen := WordEnd - WordStart;
      if WordLen > 0 then
      begin
        Word := Copy(LogLine, WordStart, WordLen);
        Parts.Add(Word);
      end;

      WordStart := WordEnd + 1;
    end;

    if Parts.Count < 2 then
      Exit(False); // 최소 2개 부분(시간 + 메시지)이 있어야 함

    // 시간 부분 찾기 (첫 번째 또는 두 번째 토큰)
    HasTime := False;
    for TimePart := 0 to Min(1, Parts.Count - 1) do
    begin
      // 시간 형식인지 확인 (콜론이나 점 포함)
      for i := Low(TimeSeparators) to High(TimeSeparators) do
      begin
        if Pos(TimeSeparators[i], Parts[TimePart]) > 0 then
        begin
          TimeStr := Parts[TimePart];
          HasTime := True;
          Break;
        end;
      end;

      if HasTime then
        Break;
    end;

    // 레벨 부분 찾기 (다음 토큰)
    HasLevel := False;
    if HasTime and (TimePart + 1 < Parts.Count) then
    begin
      if IsValidLogLevel(Parts[TimePart + 1]) then
      begin
        LevelStr := Parts[TimePart + 1];
        HasLevel := True;
      end;
    end;

    // 소스 부분 찾기 (레벨 다음 토큰, 없으면 건너뜀)
    if HasLevel and (TimePart + 2 < Parts.Count) then
    begin
      // 소스는 특별한 형식이 없으므로 그냥 다음 토큰으로 간주
      SourceStr := Parts[TimePart + 2];

      // 메시지는 그 이후 모든 부분
      MsgStr := '';
      for i := TimePart + 3 to Parts.Count - 1 do
      begin
        if i = TimePart + 3 then
          MsgStr := Parts[i]  // 첫 부분은 공백 없이 시작
        else
          MsgStr := MsgStr + ' ' + Parts[i];
      end;
    end
    else if HasLevel then
    begin
      // 소스 없음, 메시지는 레벨 이후 모든 부분
      MsgStr := '';
      for i := TimePart + 2 to Parts.Count - 1 do
      begin
        if i = TimePart + 2 then
          MsgStr := Parts[i]  // 첫 부분은 공백 없이 시작
        else
          MsgStr := MsgStr + ' ' + Parts[i];
      end;
    end
    else if HasTime then
    begin
      // 레벨도 없음, 메시지는 시간 이후 모든 부분
      MsgStr := '';
      for i := TimePart + 1 to Parts.Count - 1 do
      begin
        if i = TimePart + 1 then
          MsgStr := Parts[i]  // 첫 부분은 공백 없이 시작
        else
          MsgStr := MsgStr + ' ' + Parts[i];
      end;
    end;

    // 시간 정보가 있으면 기본 파싱 성공
    Result := HasTime;
  finally
    Parts.Free;
  end;
end;

function GetFileSize(const FileName: string): Int64;
var
  FileHandle: THandle;
begin
  Result := 0;

  if not FileExists(FileName) then
    Exit;

  FileHandle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if FileHandle <> THandle(-1) then
  begin
    try
      Result := FileSeek(FileHandle, 0, soFromEnd);
    finally
      FileClose(FileHandle);
    end;
  end;
end;

function GetLastLinesFromFile(const FileName: string; LineCount: Integer): TStringList;
const
  BUFFER_SIZE = 4096;
var
  Buffer: array[0..BUFFER_SIZE-1] of Char;
  FileHandle: THandle;
  FileSize, ReadPos, BytesRead: Int64;
  TempStr, Line: string;
  Lines: TStringList;
  i, LineCounter, StartPos, EndPos: Integer;
begin
  Result := TStringList.Create;
  Result.Capacity := LineCount;

  if not FileExists(FileName) then
    Exit;

  FileSize := GetFileSize(FileName);
  if FileSize = 0 then
    Exit;

  Lines := TStringList.Create;
  try
    FileHandle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
    if FileHandle = THandle(-1) then
      Exit;

    try
      // 파일 끝에서부터 최대 버퍼 크기만큼 읽기
      ReadPos := FileSize;
      TempStr := '';

      while (ReadPos > 0) and (Lines.Count < LineCount + 1) do
      begin
        if ReadPos < BUFFER_SIZE then
          BytesRead := ReadPos
        else
          BytesRead := BUFFER_SIZE;

        ReadPos := ReadPos - BytesRead;
        FileSeek(FileHandle, ReadPos, soFromBeginning);

        FillChar(Buffer, BUFFER_SIZE, 0);
        FileRead(FileHandle, Buffer, BytesRead);

        TempStr := Buffer + TempStr;

        // 전체 텍스트에서 라인 분리
        for i := Length(TempStr) downto 1 do
        begin
          if (TempStr[i] = #10) or (TempStr[i] = #13) then
          begin
            if i < Length(TempStr) then
            begin
              Line := Copy(TempStr, i + 1, Length(TempStr) - i);
              if Line <> '' then
                Lines.Insert(0, Line);
            end;
            TempStr := Copy(TempStr, 1, i - 1);
          end;

          if Lines.Count >= LineCount + 1 then
            Break;
        end;

        if Lines.Count >= LineCount + 1 then
          Break;
      end;

      // 남은 텍스트 처리
      if (TempStr <> '') and (Lines.Count < LineCount + 1) then
        Lines.Insert(0, TempStr);

      // 마지막 LineCount 개 라인 복사
      LineCounter := Min(LineCount, Lines.Count);
      for i := Lines.Count - LineCounter to Lines.Count - 1 do
        Result.Add(Lines[i]);

    finally
      FileClose(FileHandle);
    end;
  finally
    Lines.Free;
  end;
end;

{ TPrintfHandlerThread }

constructor TPrintfHandlerThread.Create(AOwner: TObject);
begin
  inherited Create(True); // 일시 중단 상태로 생성

  FOwner := AOwner;
  FQueueEvent := TEvent.Create(nil, False, False, '');
  FTerminateEvent := TEvent.Create(nil, True, False, '');

  FreeOnTerminate := False;

  // 스레드 시작
  Start;
end;

destructor TPrintfHandlerThread.Destroy;
begin
  FQueueEvent.Free;
  FTerminateEvent.Free;

  inherited;
end;

procedure TPrintfHandlerThread.Execute;
var
  WaitResult: DWORD;
  Events: array[0..1] of THandle;
begin
  Events[0] := THandle(FQueueEvent.Handle);     // 큐 이벤트 핸들
  Events[1] := THandle(FTerminateEvent.Handle); // 종료 이벤트 핸들

  while not Terminated do
  begin
    // 이벤트 대기 (5초 타임아웃)
    WaitResult := WaitForMultipleObjects(2, @Events[0], False, 5000);

    if Terminated then
      Break;

    case WaitResult of
      WAIT_OBJECT_0:     // FQueueEvent 신호 (인덱스 0)
        begin
          // FOwner를 통해 큐 처리 메서드 호출 - 동적 메서드 호출 방식 사용
          if FOwner <> nil then
          begin
            try
              // TMethod를 사용하여 ProcessLogQueue 메서드 간접 호출
              TMethod(FOwner).Code := @TObject.MethodAddress(FOwner, 'ProcessLogQueue');
              if Assigned(TMethod(FOwner).Code) then
                TMethod(FOwner).Call([]);
            except
              // 오류 무시
            end;
          end;
        end;
      WAIT_OBJECT_0 + 1: // FTerminateEvent 신호 (인덱스 1)
        Break;
      WAIT_TIMEOUT:      // 타임아웃 (5초 경과)
        begin
          // 타임아웃 시에도 큐 처리 시도
          if FOwner <> nil then
          begin
            try
              // TMethod를 사용하여 ProcessLogQueue 메서드 간접 호출
              TMethod(FOwner).Code := @TObject.MethodAddress(FOwner, 'ProcessLogQueue');
              if Assigned(TMethod(FOwner).Code) then
                TMethod(FOwner).Call([]);
            except
              // 오류 무시
            end;
          end;
        end;
      else               // 오류 발생 (WAIT_FAILED 등)
        Break;
    end;
  end;
end;

procedure TPrintfHandlerThread.SignalQueue;
begin
  FQueueEvent.SetEvent;
end;

procedure TPrintfHandlerThread.SignalTerminate;
begin
  FTerminateEvent.SetEvent;
end;

end.