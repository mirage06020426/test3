unit LogConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LoggerBase, McJSON;

type
  { TLogConfiguration - 로그 시스템 설정 관리 클래스 }
  TLogConfiguration = class(TInterfacedObject, ILogConfiguration)
  private
    FLogLevels: TLogLevelSet;             // 로그 레벨 필터
    FIndentMode: TIndentMode;             // 들여쓰기 모드
    FSourceFormat: TSourceFormatInfo;     // 소스 식별자 형식
    FAsyncConfig: TAsyncConfig;           // 비동기 처리 설정
    FExtDataMode: TExtDataMode;           // 확장 데이터 모드
    
    // ILogConfiguration 인터페이스 구현
    function GetLogLevels: TLogLevelSet;
    function GetIndentMode: TIndentMode;
    function GetSourceFormat: TSourceFormatInfo;
    function GetAsyncConfig: TAsyncConfig;
    function GetExtDataMode: TExtDataMode;
    procedure SetLogLevels(Value: TLogLevelSet);
    procedure SetIndentMode(Value: TIndentMode);
    procedure SetSourceFormat(const Value: TSourceFormatInfo);
    procedure SetAsyncConfig(const Value: TAsyncConfig);
    procedure SetExtDataMode(Value: TExtDataMode);
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // 설정 파일 관련 메서드
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    
    // 기본 설정 적용
    procedure ApplyDefaults;
    
    // 속성
    property LogLevels: TLogLevelSet read GetLogLevels write SetLogLevels;
    property IndentMode: TIndentMode read GetIndentMode write SetIndentMode;
    property SourceFormat: TSourceFormatInfo read GetSourceFormat write SetSourceFormat;
    property AsyncConfig: TAsyncConfig read GetAsyncConfig write SetAsyncConfig;
    property ExtDataMode: TExtDataMode read GetExtDataMode write SetExtDataMode;
  end;

implementation

{ 로그 레벨 집합 <-> 문자열 변환 헬퍼 함수 }
function LogLevelSetToStr(const LogLevels: TLogLevelSet): string;
var
  Level: TLogLevel;
  List: TStringList;
begin
  List := TStringList.Create;
  try
    for Level := Low(TLogLevel) to High(TLogLevel) do
    begin
      if Level in LogLevels then
        List.Add(LogLevelNames[Level]);
    end;
    Result := List.CommaText;
  finally
    List.Free;
  end;
end;

function StrToLogLevelSet(const S: string): TLogLevelSet;
var
  List: TStringList;
  i: Integer;
  Level: TLogLevel;
begin
  Result := [];
  List := TStringList.Create;
  try
    List.CommaText := S;
    for i := 0 to List.Count - 1 do
    begin
      for Level := Low(TLogLevel) to High(TLogLevel) do
      begin
        if CompareText(List[i], LogLevelNames[Level]) = 0 then
        begin
          Include(Result, Level);
          Break;
        end;
      end;
    end;
  finally
    List.Free;
  end;
end;

{ TLogConfiguration }

constructor TLogConfiguration.Create;
begin
  inherited Create;
  ApplyDefaults;
end;

destructor TLogConfiguration.Destroy;
begin
  inherited Destroy;
end;

procedure TLogConfiguration.ApplyDefaults;
begin
  // 로그 레벨 기본값 - 모든 레벨 활성화
  FLogLevels := LOG_LEVEL_ALL;
  
  // 들여쓰기 모드 기본값 - 자동
  FIndentMode := imAuto;
  
  // 소스 형식 기본값
  FSourceFormat.DisplayMode := sdmBoth;
  FSourceFormat.Prefix := '[';
  FSourceFormat.Suffix := ']';
  FSourceFormat.Separator := ':';
  FSourceFormat.SourceFirst := True;
  FSourceFormat.UseColor := False;
  
  // 비동기 처리 기본값
  FAsyncConfig.Mode := amNone;
  FAsyncConfig.QueueSize := 100;
  FAsyncConfig.FlushInterval := 1000; // 1초
  
  // 확장 데이터 모드 기본값
  FExtDataMode := edmNone;
end;

function TLogConfiguration.GetLogLevels: TLogLevelSet;
begin
  Result := FLogLevels;
end;

function TLogConfiguration.GetIndentMode: TIndentMode;
begin
  Result := FIndentMode;
end;

function TLogConfiguration.GetSourceFormat: TSourceFormatInfo;
begin
  Result := FSourceFormat;
end;

function TLogConfiguration.GetAsyncConfig: TAsyncConfig;
begin
  Result := FAsyncConfig;
end;

function TLogConfiguration.GetExtDataMode: TExtDataMode;
begin
  Result := FExtDataMode;
end;

procedure TLogConfiguration.SetLogLevels(Value: TLogLevelSet);
begin
  FLogLevels := Value;
end;

procedure TLogConfiguration.SetIndentMode(Value: TIndentMode);
begin
  FIndentMode := Value;
end;

procedure TLogConfiguration.SetSourceFormat(const Value: TSourceFormatInfo);
begin
  FSourceFormat := Value;
end;

procedure TLogConfiguration.SetAsyncConfig(const Value: TAsyncConfig);
begin
  FAsyncConfig := Value;
end;

procedure TLogConfiguration.SetExtDataMode(Value: TExtDataMode);
begin
  FExtDataMode := Value;
end;

procedure TLogConfiguration.LoadFromFile(const FileName: string);
var
  JsonRoot: TMcJsonItem;
  JsonLevels: TMcJsonItem;
  JsonSource: TMcJsonItem;
  JsonAsync: TMcJsonItem;
  LogLevelsStr: string;
  LevelsList: TStringList;
  i: Integer;
  IndentModeStr: string;
  DisplayModeStr: string;
  AsyncModeStr: string;
  ExtDataModeStr: string;
begin
  if not FileExists(FileName) then
    Exit;

  JsonRoot := TMcJsonItem.Create;
  try
    try
      JsonRoot.LoadFromFile(FileName);

      // 로그 레벨 로드
      if JsonRoot.HasKey('logLevels') then
      begin
        JsonLevels := JsonRoot['logLevels'];
        LevelsList := TStringList.Create;
        try
          for i := 0 to JsonLevels.Count - 1 do
            LevelsList.Add(JsonLevels.Items[i].AsString);

          LogLevelsStr := LevelsList.CommaText;
          FLogLevels := StrToLogLevelSet(LogLevelsStr);
        finally
          LevelsList.Free;
        end;
      end;

      // 들여쓰기 모드 로드
      if JsonRoot.HasKey('indentMode') then
      begin
        IndentModeStr := JsonRoot.S['indentMode'];
        if CompareText(IndentModeStr, 'none') = 0 then
          FIndentMode := imNone
        else if CompareText(IndentModeStr, 'auto') = 0 then
          FIndentMode := imAuto
        else if CompareText(IndentModeStr, 'manual') = 0 then
          FIndentMode := imManual;
      end;

      // 소스 형식 로드
      if JsonRoot.HasKey('sourceFormat') then
      begin
        JsonSource := JsonRoot['sourceFormat'];

        if JsonSource.HasKey('displayMode') then
        begin
          DisplayModeStr := JsonSource.S['displayMode'];
          if CompareText(DisplayModeStr, 'none') = 0 then
            FSourceFormat.DisplayMode := sdmNone
          else if CompareText(DisplayModeStr, 'handlerOnly') = 0 then
            FSourceFormat.DisplayMode := sdmHandlerOnly
          else if CompareText(DisplayModeStr, 'tagOnly') = 0 then
            FSourceFormat.DisplayMode := sdmTagOnly
          else if CompareText(DisplayModeStr, 'both') = 0 then
            FSourceFormat.DisplayMode := sdmBoth
          else if CompareText(DisplayModeStr, 'full') = 0 then
            FSourceFormat.DisplayMode := sdmFull;
        end;

        if JsonSource.HasKey('prefix') then
          FSourceFormat.Prefix := JsonSource.S['prefix'];

        if JsonSource.HasKey('suffix') then
          FSourceFormat.Suffix := JsonSource.S['suffix'];

        if JsonSource.HasKey('separator') then
          FSourceFormat.Separator := JsonSource.S['separator'];

        if JsonSource.HasKey('sourceFirst') then
          FSourceFormat.SourceFirst := JsonSource.B['sourceFirst'];

        if JsonSource.HasKey('useColor') then
          FSourceFormat.UseColor := JsonSource.B['useColor'];
      end;

      // 비동기 설정 로드
      if JsonRoot.HasKey('asyncConfig') then
      begin
        JsonAsync := JsonRoot['asyncConfig'];

        if JsonAsync.HasKey('mode') then
        begin
          AsyncModeStr := JsonAsync.S['mode'];
          if CompareText(AsyncModeStr, 'none') = 0 then
            FAsyncConfig.Mode := amNone
          else if CompareText(AsyncModeStr, 'thread') = 0 then
            FAsyncConfig.Mode := amThread
          else if CompareText(AsyncModeStr, 'queue') = 0 then
            FAsyncConfig.Mode := amQueue;
        end;

        if JsonAsync.HasKey('queueSize') then
          FAsyncConfig.QueueSize := JsonAsync.I['queueSize'];

        if JsonAsync.HasKey('flushInterval') then
          FAsyncConfig.FlushInterval := JsonAsync.I['flushInterval'];
      end;

      // 확장 데이터 모드 로드
      if JsonRoot.HasKey('extDataMode') then
      begin
        ExtDataModeStr := JsonRoot.S['extDataMode'];
        if CompareText(ExtDataModeStr, 'none') = 0 then
          FExtDataMode := edmNone
        else if CompareText(ExtDataModeStr, 'basic') = 0 then
          FExtDataMode := edmBasic
        else if CompareText(ExtDataModeStr, 'full') = 0 then
          FExtDataMode := edmFull
        else if CompareText(ExtDataModeStr, 'custom') = 0 then
          FExtDataMode := edmCustom;
      end;

    except
      on E: Exception do
        WriteLn('로그 설정 파일 로드 오류: ', E.Message);
    end;
  finally
    JsonRoot.Free;
  end;
end;

procedure TLogConfiguration.SaveToFile(const FileName: string);
var
  JsonRoot: TMcJsonItem;
  JsonLevels: TMcJsonItem;
  JsonSource: TMcJsonItem;
  JsonAsync: TMcJsonItem;
  Level: TLogLevel;
  LevelsCount: Integer;
begin
  JsonRoot := TMcJsonItem.Create;
  try
    // 로그 레벨 저장
    JsonLevels := JsonRoot.Add('logLevels');
    JsonLevels.ItemType := jitArray;
    LevelsCount := 0;

    for Level := Low(TLogLevel) to High(TLogLevel) do
    begin
      if Level in FLogLevels then
      begin
        JsonLevels.Add(IntToStr(LevelsCount)).AsString := LogLevelNames[Level];
        Inc(LevelsCount);
      end;
    end;

    // 들여쓰기 모드 저장
    case FIndentMode of
      imNone: JsonRoot.Add('indentMode').AsString := 'none';
      imAuto: JsonRoot.Add('indentMode').AsString := 'auto';
      imManual: JsonRoot.Add('indentMode').AsString := 'manual';
    end;

    // 소스 형식 저장
    JsonSource := JsonRoot.Add('sourceFormat');
    JsonSource.ItemType := jitObject;

    case FSourceFormat.DisplayMode of
      sdmNone: JsonSource.Add('displayMode').AsString := 'none';
      sdmHandlerOnly: JsonSource.Add('displayMode').AsString := 'handlerOnly';
      sdmTagOnly: JsonSource.Add('displayMode').AsString := 'tagOnly';
      sdmBoth: JsonSource.Add('displayMode').AsString := 'both';
      sdmFull: JsonSource.Add('displayMode').AsString := 'full';
    end;

    JsonSource.Add('prefix').AsString := FSourceFormat.Prefix;
    JsonSource.Add('suffix').AsString := FSourceFormat.Suffix;
    JsonSource.Add('separator').AsString := FSourceFormat.Separator;
    JsonSource.Add('sourceFirst').AsBoolean := FSourceFormat.SourceFirst;
    JsonSource.Add('useColor').AsBoolean := FSourceFormat.UseColor;

    // 비동기 설정 저장
    JsonAsync := JsonRoot.Add('asyncConfig');
    JsonAsync.ItemType := jitObject;

    case FAsyncConfig.Mode of
      amNone: JsonAsync.Add('mode').AsString := 'none';
      amThread: JsonAsync.Add('mode').AsString := 'thread';
      amQueue: JsonAsync.Add('mode').AsString := 'queue';
    end;

    JsonAsync.Add('queueSize').AsInteger := FAsyncConfig.QueueSize;
    JsonAsync.Add('flushInterval').AsInteger := FAsyncConfig.FlushInterval;

    // 확장 데이터 모드 저장
    case FExtDataMode of
      edmNone: JsonRoot.Add('extDataMode').AsString := 'none';
      edmBasic: JsonRoot.Add('extDataMode').AsString := 'basic';
      edmFull: JsonRoot.Add('extDataMode').AsString := 'full';
      edmCustom: JsonRoot.Add('extDataMode').AsString := 'custom';
    end;

    // 파일 저장
    try
      JsonRoot.SaveToFile(FileName);
    except
      on E: Exception do
        WriteLn('로그 설정 파일 저장 오류: ', E.Message);
    end;
  finally
    JsonRoot.Free;
  end;
end;

end.
