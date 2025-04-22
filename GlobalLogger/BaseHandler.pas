unit BaseHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, LoggerBase;

type
  { 로그 태그 필터링 정보 }
  TLogTagInfo = record
    TagFilterEnabled: Boolean;   // 태그 필터 활성화 여부
    EnabledTags: TStringList;    // 활성화된 태그 목록
  end;

  { 로그 필터링 정보 }
  TLogFilterInfo = record
    FilterEnabled: Boolean;      // 필터 활성화 여부
    FilterText: string;          // 필터링할 텍스트
    CaseSensitive: Boolean;      // 대소문자 구분 여부
  end;

  { TBaseLogHandler - 기본 로그 핸들러 추상 클래스 }
  TBaseLogHandler = class(TInterfacedObject, ILogHandler)
  private
    FActive: Boolean;            // 핸들러 활성화 상태
    FLogLevels: TLogLevelSet;    // 로그 레벨 필터
    FLock: TCriticalSection;     // 스레드 동기화 객체
    FAsyncMode: TAsyncMode;      // 비동기 처리 모드
    FTagInfo: TLogTagInfo;       // 태그 필터링 정보
    FFilterInfo: TLogFilterInfo; // 텍스트 필터링 정보
    FSourceIdentifier: string;   // 핸들러 소스 식별자

    { ILogHandler 인터페이스 구현 }
    procedure SetActive(Value: Boolean);
    procedure SetLogLevels(Value: TLogLevelSet);
    //procedure SetAsyncMode(Mode: TAsyncMode); virtual;
    procedure SetSourceIdentifier(const Value: string);
    function GetActive: Boolean;
    function GetLogLevels: TLogLevelSet;
    function GetAsyncMode: TAsyncMode;
    function GetSourceIdentifier: string;

    { 필터링 관련 내부 메서드 }
    function IsTagEnabled(const Tag: string): Boolean;
    function IsFilterMatched(const Msg: string): Boolean;

  protected
    procedure SetAsyncMode(Mode: TAsyncMode); virtual;
    { 로그 레벨에 따른 로깅 여부 확인 }
    function ShouldLog(Level: TLogLevel; const Tag: string = ''): Boolean; virtual;

    { 실제 로그 메시지 기록 (자식 클래스에서 구현) }
    procedure WriteLog(const Msg: string; Level: TLogLevel); virtual; abstract;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    { 로그 메시지 전달 - 필터링 및 처리 후 WriteLog 호출 }
    procedure Deliver(const Msg: string; Level: TLogLevel; const Tag: string = '');

    { 핸들러 초기화 }
    procedure Init; virtual;

    { 핸들러 종료 }
    procedure Shutdown; virtual;

    { 태그 필터링 관련 메서드 }
    procedure AddTagFilter(const Tag: string);
    procedure RemoveTagFilter(const Tag: string);
    procedure ClearTagFilters;
    procedure EnableTagFilter(const Tags: array of string);
    procedure DisableTagFilter;

    { 텍스트 필터링 관련 메서드 }
    procedure EnableFilter(const FilterText: string; CaseSensitive: Boolean = False);
    procedure DisableFilter;

    { 비동기 큐 플러시 }
    procedure FlushQueue; virtual;

    { 속성 }
    property Active: Boolean read GetActive write SetActive;
    property LogLevels: TLogLevelSet read GetLogLevels write SetLogLevels;
    property AsyncMode: TAsyncMode read GetAsyncMode write SetAsyncMode;
    property SourceIdentifier: string read GetSourceIdentifier write SetSourceIdentifier;
  end;

implementation

{ TBaseLogHandler }

constructor TBaseLogHandler.Create;
begin
  inherited Create;
  
  FActive := True;
  FLogLevels := LOG_LEVEL_ALL;
  FLock := TCriticalSection.Create;
  FAsyncMode := amNone; // 기본값은 동기 처리

  // 태그 필터링 초기화
  FTagInfo.TagFilterEnabled := False;
  FTagInfo.EnabledTags := TStringList.Create;
  FTagInfo.EnabledTags.Sorted := True;
  FTagInfo.EnabledTags.Duplicates := dupIgnore;
  FTagInfo.EnabledTags.CaseSensitive := False;

  // 텍스트 필터링 초기화
  FFilterInfo.FilterEnabled := False;
  FFilterInfo.FilterText := '';
  FFilterInfo.CaseSensitive := False;

  // 소스 식별자 기본값 설정 - 핸들러 클래스 이름에서 'TLog' 또는 'T' 접두사와 'Handler' 접미사를 제거
  FSourceIdentifier := ClassName;
  if Pos('TLog', FSourceIdentifier) = 1 then
    Delete(FSourceIdentifier, 1, 4)
  else if FSourceIdentifier[1] = 'T' then
    Delete(FSourceIdentifier, 1, 1);

  if Copy(FSourceIdentifier, Length(FSourceIdentifier) - 6, 7) = 'Handler' then
    SetLength(FSourceIdentifier, Length(FSourceIdentifier) - 7);
end;

destructor TBaseLogHandler.Destroy;
begin
  Shutdown;

  if Assigned(FTagInfo.EnabledTags) then
    FTagInfo.EnabledTags.Free;

  FLock.Free;

  inherited;
end;

procedure TBaseLogHandler.SetActive(Value: Boolean);
begin
  FLock.Enter;
  try
    FActive := Value;
  finally
    FLock.Leave;
  end;
end;

function TBaseLogHandler.GetActive: Boolean;
begin
  FLock.Enter;
  try
    Result := FActive;
  finally
    FLock.Leave;
  end;
end;

procedure TBaseLogHandler.SetLogLevels(Value: TLogLevelSet);
begin
  FLock.Enter;
  try
    FLogLevels := Value;
  finally
    FLock.Leave;
  end;
end;

function TBaseLogHandler.GetLogLevels: TLogLevelSet;
begin
  FLock.Enter;
  try
    Result := FLogLevels;
  finally
    FLock.Leave;
  end;
end;

procedure TBaseLogHandler.SetAsyncMode(Mode: TAsyncMode);
begin
  FLock.Enter;
  try
    FAsyncMode := Mode;
  finally
    FLock.Leave;
  end;
end;

function TBaseLogHandler.GetAsyncMode: TAsyncMode;
begin
  FLock.Enter;
  try
    Result := FAsyncMode;
  finally
    FLock.Leave;
  end;
end;

procedure TBaseLogHandler.SetSourceIdentifier(const Value: string);
begin
  FLock.Enter;
  try
    FSourceIdentifier := Value;
  finally
    FLock.Leave;
  end;
end;

function TBaseLogHandler.GetSourceIdentifier: string;
begin
  FLock.Enter;
  try
    Result := FSourceIdentifier;
  finally
    FLock.Leave;
  end;
end;

function TBaseLogHandler.ShouldLog(Level: TLogLevel; const Tag: string): Boolean;
begin
  // 기본 조건: 핸들러가 활성화되어 있고, 요청된 로그 레벨이 설정된 레벨에 포함되어야 함
  Result := FActive and (Level in FLogLevels);

  // 태그 필터링 적용
  if Result and (Tag <> '') and FTagInfo.TagFilterEnabled then
    Result := IsTagEnabled(Tag);
end;

function TBaseLogHandler.IsTagEnabled(const Tag: string): Boolean;
begin
  if not FTagInfo.TagFilterEnabled or (FTagInfo.EnabledTags.Count = 0) then
    Result := True
  else
    Result := FTagInfo.EnabledTags.IndexOf(Tag) >= 0;
end;

function TBaseLogHandler.IsFilterMatched(const Msg: string): Boolean;
begin
  if not FFilterInfo.FilterEnabled or (FFilterInfo.FilterText = '') then
    Result := True  // 필터가 비활성화되어 있으면 항상 매치됨
  else
  begin
    if FFilterInfo.CaseSensitive then
      Result := Pos(FFilterInfo.FilterText, Msg) > 0
    else
      Result := Pos(LowerCase(FFilterInfo.FilterText), LowerCase(Msg)) > 0;
  end;
end;

procedure TBaseLogHandler.Deliver(const Msg: string; Level: TLogLevel; const Tag: string);
var
  OriginalSourceId: string;
  CombinedSourceId: string;
begin
  // 로그 레벨과 태그 기준으로 로깅 여부 결정
  if not ShouldLog(Level, Tag) then
    Exit;

  // 텍스트 필터링 적용
  if not IsFilterMatched(Msg) then
    Exit;

  // 스레드 안전성 확보
  FLock.Enter;
  try
    // 태그 정보가 있는 경우, 소스 식별자와 결합
    OriginalSourceId := FSourceIdentifier;
    try
      if (Tag <> '') then
      begin
        if (FSourceIdentifier <> '') then
          CombinedSourceId := Format('%s:%s', [FSourceIdentifier, Tag])
        else
          CombinedSourceId := Tag;

        FSourceIdentifier := CombinedSourceId;
      end;

      // 실제 로깅 작업은 자식 클래스의 WriteLog에서 처리
      WriteLog(Msg, Level);
    finally
      // 원래 소스 식별자 복원
      FSourceIdentifier := OriginalSourceId;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TBaseLogHandler.Init;
begin
  // 기본 구현은 비어 있음, 자식 클래스에서 필요에 따라 오버라이드
end;

procedure TBaseLogHandler.Shutdown;
begin
  // 기본 구현은 비어 있음, 자식 클래스에서 필요에 따라 오버라이드
end;

procedure TBaseLogHandler.AddTagFilter(const Tag: string);
begin
  if Tag = '' then
    Exit;
    
  FLock.Enter;
  try
    FTagInfo.EnabledTags.Add(Tag);
    FTagInfo.TagFilterEnabled := True;
  finally
    FLock.Leave;
  end;
end;

procedure TBaseLogHandler.RemoveTagFilter(const Tag: string);
var
  Index: Integer;
begin
  if Tag = '' then
    Exit;
    
  FLock.Enter;
  try
    Index := FTagInfo.EnabledTags.IndexOf(Tag);
    if Index >= 0 then
      FTagInfo.EnabledTags.Delete(Index);
  finally
    FLock.Leave;
  end;
end;

procedure TBaseLogHandler.ClearTagFilters;
begin
  FLock.Enter;
  try
    FTagInfo.EnabledTags.Clear;
    FTagInfo.TagFilterEnabled := False;
  finally
    FLock.Leave;
  end;
end;

procedure TBaseLogHandler.EnableTagFilter(const Tags: array of string);
var
  i: Integer;
begin
  FLock.Enter;
  try
    FTagInfo.EnabledTags.Clear;
    for i := Low(Tags) to High(Tags) do
      FTagInfo.EnabledTags.Add(Tags[i]);
    FTagInfo.TagFilterEnabled := True;
  finally
    FLock.Leave;
  end;
end;

procedure TBaseLogHandler.DisableTagFilter;
begin
  FLock.Enter;
  try
    FTagInfo.TagFilterEnabled := False;
    FTagInfo.EnabledTags.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TBaseLogHandler.EnableFilter(const FilterText: string; CaseSensitive: Boolean = False);
begin
  FLock.Enter;
  try
    FFilterInfo.FilterEnabled := True;
    FFilterInfo.FilterText := FilterText;
    FFilterInfo.CaseSensitive := CaseSensitive;
  finally
    FLock.Leave;
  end;
end;

procedure TBaseLogHandler.DisableFilter;
begin
  FLock.Enter;
  try
    FFilterInfo.FilterEnabled := False;
    FFilterInfo.FilterText := '';
  finally
    FLock.Leave;
  end;
end;

procedure TBaseLogHandler.FlushQueue;
begin
  // 기본 구현은 비어 있음
  // 비동기 큐를 사용하는 자식 클래스에서 오버라이드
end;

end.
