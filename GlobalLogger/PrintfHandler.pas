unit PrintfHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls,
  Graphics, Dialogs, Menus, Clipbrd, LMessages, LCLType, DateTimePicker,
  SyncObjs, DateUtils,
  LoggerBase, BaseHandler, LogStorage, McJSON,
  PrintfLogForm, PrintfUtils;

type
  { TPrintfHandler - 로그 출력용 핸들러 }
  TPrintfHandler = class(TBaseLogHandler)
  private
    FLogForm: TPrintfLogForm;        // 로그 폼
    FVisible: Boolean;               // 폼 표시 여부
    FFormPosition: TFormPosition;    // 폼 위치
    FFormWidth: Integer;             // 폼 너비
    FFormHeight: Integer;            // 폼 높이
    FFormCaption: string;            // 폼 캡션
    FStayOnTop: Boolean;             // 항상 위에 표시 여부
    FAutoSave: Boolean;              // 자동 저장 여부
    FShowTime: Boolean;              // 시간 표시 여부
    FMaxLogCount: Integer;           // 최대 로그 개수
    FColorByLevel: Boolean;          // 레벨별 색상 적용 여부
    FColorBySource: Boolean;         // 소스별 색상 적용 여부

    // 비동기 처리 관련 필드
    FLogQueue: TThreadList;           // 로그 메시지 큐
    FAsyncThread: TPrintfHandlerThread; // 비동기 처리 스레드
    FQueueMaxSize: Integer;           // 큐 최대 크기
    FQueueFlushInterval: Integer;     // 큐 자동 플러시 간격
    FLastQueueFlush: TDateTime;       // 마지막 큐 플러시 시간

    // 로그 로테이션 관련 필드
    FAutoRotate: Boolean;        // 자동 로테이션 여부
    FRotateLineCount: Integer;   // 로테이션 기준 라인 수
    FRotateSaveBeforeClear: Boolean; // 클리어 전 저장 여부
    FRotateLogFolder: string;    // 로그 저장 폴더

    FShowDateInTimeColumn: Boolean; // 시간 컬럼에 날짜 표시 여부
    FColumnVisibility: TColumnVisibility; // 컬럼 가시성 설정

    // 소스 색상 맵
    FSourceColorMap: TStringList; // 소스별 색상 저장

    // 폼 위치 설정
    procedure SetFormPosition(Value: TFormPosition);

    // 폼 크기 설정
    procedure SetFormWidth(Value: Integer);
    procedure SetFormHeight(Value: Integer);

    // 폼 캡션 설정
    procedure SetFormCaption(Value: string);

    // 항상 위에 표시 설정
    procedure SetStayOnTop(Value: Boolean);

    // 로그 폼 표시 설정
    procedure SetVisible(Value: Boolean);
    // 가시성 상태 관리
    procedure UpdateFormVisibility(Visiblity: Boolean);

    // 색상 관련
    function GetColorForLevel(Level: TLogLevel): TColor;
    function GetColorForSource(const Source: string): TColor; // 소스별 색상

    // 로그 큐 처리
    procedure FlushQueue;
    procedure ProcessLogQueue;

    // 로그 폼 이벤트 핸들러
    procedure HandleFormClear(Sender: TObject);
    procedure HandleFormSave(Sender: TObject);

    // 컬럼 가시성 설정
    procedure SetColumnVisibility(const Value: TColumnVisibility);

    // 현재 로깅 중인 파일 열기 기능
    procedure ViewCurrentLogFile;
    // 가장 최근의 로그 파일 찾기
    function FindMostRecentLogFile(const LogFolder: string): string;
    // TPrintfHandler 클래스 내 메인 메뉴에 항목 추가
    procedure AddViewCurrentLogMenuItem;
    // 메뉴 클릭 이벤트 핸들러
    procedure ViewCurrentLogMenuClick(Sender: TObject);

  protected
    procedure WriteLog(const Msg: string; Level: TLogLevel); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Init; override;
    procedure Shutdown; override;

    // 세션 시작 마커 기록
    procedure WriteSessionStart;

    // 비동기 모드 설정 오버라이드
    procedure SetAsyncMode(Mode: TAsyncMode); override;

    // 폼 디자인 설정
    procedure SetFormDesign(const ACaption: string; AWidth, AHeight: Integer;
                           APosition: TFormPosition = fpTopRight);

    // 폼 표시 설정
    procedure ShowForm(Show: Boolean = True);

    // 폰트 설정
    procedure SetFont(const FontName: string; FontSize: Integer; FontColor: TColor);

    // 배경색 설정
    procedure SetBackgroundColor(Color: TColor);

    // 로그 내용 저장
    procedure SaveToFile(const FileName: string = '');

    // 로그 내용 지우기
    procedure ClearLog;

    // 저장된 로그 파일 열기
    class procedure OpenLogFile(AOwner: TComponent);

    // 소스별 색상 설정 메서드 추가
    procedure SetSourceColor(const Source: string; Color: TColor);
    procedure ClearSourceColors;

    // 소스 통계 표시
    procedure ShowSourceStatistics;

    // 컬럼 가시성 설정
    procedure ConfigureColumnVisibility(ShowTime, ShowLevel, ShowSource, ShowMessage: Boolean);

    // 속성
    property LogForm: TPrintfLogForm read FLogForm;
    property Visible: Boolean read FVisible write SetVisible;
    property FormPosition: TFormPosition read FFormPosition write SetFormPosition;
    property FormWidth: Integer read FFormWidth write SetFormWidth;
    property FormHeight: Integer read FFormHeight write SetFormHeight;
    property FormCaption: string read FFormCaption write SetFormCaption;
    property StayOnTop: Boolean read FStayOnTop write SetStayOnTop;
    property AutoSave: Boolean read FAutoSave write FAutoSave;
    property ShowTime: Boolean read FShowTime write FShowTime;
    property MaxLogCount: Integer read FMaxLogCount write FMaxLogCount;
    property ColorByLevel: Boolean read FColorByLevel write FColorByLevel;
    property ColorBySource: Boolean read FColorBySource write FColorBySource; // 소스별 색상 속성 추가
    property QueueMaxSize: Integer read FQueueMaxSize write FQueueMaxSize;
    property QueueFlushInterval: Integer read FQueueFlushInterval write FQueueFlushInterval;
    property ShowDateInTimeColumn: Boolean read FShowDateInTimeColumn write FShowDateInTimeColumn;
    property ColumnVisibility: TColumnVisibility read FColumnVisibility write SetColumnVisibility;

    // 로그 로테이션 관련 속성
    property AutoRotate: Boolean read FAutoRotate write FAutoRotate;
    property RotateLineCount: Integer read FRotateLineCount write FRotateLineCount;
    property RotateSaveBeforeClear: Boolean read FRotateSaveBeforeClear write FRotateSaveBeforeClear;
    property RotateLogFolder: string read FRotateLogFolder write FRotateLogFolder;
  end;

implementation

{$IFDEF WINDOWS}
uses Windows;
{$ENDIF}

constructor TPrintfHandler.Create;
var
  BasePath, DatePath, PrintfPath: string;
  CurrentDate: TDateTime;
begin
  inherited Create;

  // 기본 설정
  FVisible := True;
  FFormPosition := fpTopRight;
  FFormWidth := 700;  // 폼 너비 증가
  FFormHeight := 500; // 폼 높이 증가
  FFormCaption := '로그 출력';
  FStayOnTop := False;
  FAutoSave := False;
  FShowTime := True;
  FMaxLogCount := 5000;
  FColorByLevel := True;
  FColorBySource := False; // 기본적으로 소스별 색상 비활성화
  FShowDateInTimeColumn := False; // 기본값으로 시간만 표시

  // 컬럼 가시성 기본 설정
  FColumnVisibility.ShowTimeColumn := True;
  FColumnVisibility.ShowLevelColumn := True;
  FColumnVisibility.ShowSourceColumn := True; // 기본적으로 소스 컬럼 표시
  FColumnVisibility.ShowMessageColumn := True;

  // 소스 색상 맵 초기화
  FSourceColorMap := TStringList.Create;
  FSourceColorMap.Sorted := True;
  FSourceColorMap.Duplicates := dupIgnore;

  // 로그 로테이션 기본 설정
  FAutoRotate := False;
  FRotateLineCount := 5000;
  FRotateSaveBeforeClear := True;

  // 날짜별 디렉토리 구조 사용하여 로그 폴더 설정
  BasePath := ExtractFilePath(ParamStr(0)) + 'logs';
  CurrentDate := Date;
  DatePath := IncludeTrailingPathDelimiter(BasePath) +
             FormatDateTime('yyyy', CurrentDate) + PathDelim +
             FormatDateTime('mm', CurrentDate) + PathDelim +
             FormatDateTime('dd', CurrentDate);
  PrintfPath := DatePath + PathDelim + 'Printf';

  // 기본 로그 저장 폴더 설정
  FRotateLogFolder := PrintfPath;

  // 디렉토리가 없으면 생성 (Init에서도 확인)
  if not DirectoryExists(PrintfPath) then
    ForceDirectories(PrintfPath);

  // 로그 폼 생성 및 설정
  if Application <> nil then
  begin
    FLogForm := TPrintfLogForm.Create(Application, Self);
    FLogForm.OnClear := @HandleFormClear;
    FLogForm.OnSave := @HandleFormSave;
    FLogForm.Width := FFormWidth;
    FLogForm.Height := FFormHeight;
    FLogForm.Caption := FFormCaption;

    // 위치 설정
    SetFormPosition(FFormPosition);
  end;

  // 비동기 처리 관련
  FLogQueue := TThreadList.Create;
  FAsyncThread := nil;
  FQueueMaxSize := 1000;       // 기본 큐 크기
  FQueueFlushInterval := 1000; // 기본 플러시 간격 (ms)
  FLastQueueFlush := Now;
end;


destructor TPrintfHandler.Destroy;
begin
  Shutdown;

  // 로그 큐 정리
  FlushQueue;
  FLogQueue.Free;

  // 소스 색상 맵 해제
  if Assigned(FSourceColorMap) then
    FSourceColorMap.Free;

  // 폼 정리
  if Assigned(FLogForm) then
  begin
    FLogForm.Free;
    FLogForm := nil;
  end;

  inherited;
end;

procedure TPrintfHandler.Init;
begin
  inherited;

  // 로그 폴더 확인 및 생성
  if FRotateLogFolder <> '' then
    if not DirectoryExists(FRotateLogFolder) then
      ForceDirectories(FRotateLogFolder);

  // 폼이 생성되었는지 확인
  if not Assigned(FLogForm) and (Application <> nil) then
  begin
    FLogForm := TPrintfLogForm.Create(Application, Self);
    FLogForm.OnClear := @HandleFormClear;
    FLogForm.OnSave := @HandleFormSave;
    FLogForm.Width := FFormWidth;
    FLogForm.Height := FFormHeight;
    FLogForm.Caption := FFormCaption;

    // 위치 설정
    SetFormPosition(FFormPosition);
  end;

  // 항상 위에 표시 설정
  if Assigned(FLogForm) then
  begin
    if FStayOnTop then
      FLogForm.FormStyle := fsStayOnTop
    else
      FLogForm.FormStyle := fsNormal;

    // 폼 표시 설정
    if FVisible then
      FLogForm.Show;
  end;

  // 현재 로그 보기 메뉴 항목 추가
  AddViewCurrentLogMenuItem;
end;

procedure TPrintfHandler.Shutdown;
begin
  // 비동기 스레드 종료
  SetAsyncMode(amNone);

  // 로그 큐 플러시
  FlushQueue;

  // 자동 저장 옵션이 켜져 있으면 저장
  if FAutoSave and Assigned(FLogForm) and (FLogForm.LogTree.TotalCount > 0) then
    SaveToFile;

  // 폼 숨기기 및 정리
  if Assigned(FLogForm) then
    FLogForm.Hide;

  inherited;
end;

// 소스별 색상 관련 메서드 추가
function TPrintfHandler.GetColorForSource(const Source: string): TColor;
var
  Index: Integer;
  ColorStr: string;
begin
  Result := clNone; // 기본값은 색상 없음

  if not FColorBySource or (Source = '') then
    Exit;

  // 소스 색상 맵에서 찾기
  Index := FSourceColorMap.IndexOf(Source);
  if Index >= 0 then
  begin
    ColorStr := FSourceColorMap.ValueFromIndex[Index];
    if ColorStr <> '' then
      Result := StrToIntDef(ColorStr, clNone);
  end;
end;

procedure TPrintfHandler.SetSourceColor(const Source: string; Color: TColor);
begin
  if Source = '' then
    Exit;

  // 소스 색상 저장
  FSourceColorMap.Values[Source] := IntToStr(Color);

  // 폼이 있으면 갱신
  if Assigned(FLogForm) then
    FLogForm.LogTree.Invalidate;
end;

procedure TPrintfHandler.ClearSourceColors;
begin
  FSourceColorMap.Clear;

  // 폼이 있으면 갱신
  if Assigned(FLogForm) then
    FLogForm.LogTree.Invalidate;
end;

// 소스 통계 표시
procedure TPrintfHandler.ShowSourceStatistics;
begin
  if Assigned(FLogForm) then
    FLogForm.ShowSourceStatisticsForm;
end;

// 컬럼 가시성 관련
procedure TPrintfHandler.SetColumnVisibility(const Value: TColumnVisibility);
begin
  FColumnVisibility := Value;

  // 로그 폼에 설정 적용
  if Assigned(FLogForm) then
  begin
    FLogForm.ColumnVisibility := Value;
    FLogForm.ApplyColumnVisibility;
  end;
end;


// 현재 로깅 중인 파일 열기 기능
procedure TPrintfHandler.ViewCurrentLogFile;
var
  LogFileName: string;
  TempFileName: string;
  ViewerForm: TPrintfLogForm;
begin
  if not Assigned(FLogForm) then
    Exit;

  // 현재 로그 파일이 없는 경우 메모리에 있는 내용으로 임시 파일 생성 후 보기
  if (not FAutoSave) or (FRotateLogFolder = '') then
  begin
    TempFileName := GetTempDir + 'current_log_' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '.log';

    // 현재 내용을 임시 파일에 저장
    SaveToFile(TempFileName);

    // 파일 뷰어 폼 생성
    ViewerForm := TPrintfLogForm.CreateForFileViewing(Application, TempFileName);
    ViewerForm.Caption := '현재 로그 보기 (임시 파일)';
    ViewerForm.Show;

    Exit;
  end;

  // 현재 로그 파일 경로 확인
  LogFileName := '';

  // 1. 현재 날짜 기반 파일명 구성
  LogFileName := IncludeTrailingPathDelimiter(FRotateLogFolder) +
               FormatDateTime('yyyymmdd', Now) + '.log';

  // 파일이 없으면 다른 이름 패턴 찾기
  if not FileExists(LogFileName) then
  begin
    LogFileName := IncludeTrailingPathDelimiter(FRotateLogFolder) +
                 FormatDateTime('yyyymmdd-hhnnss', Now) + '.log';

    if not FileExists(LogFileName) then
    begin
      // 디렉토리에서 가장 최근 로그 파일 찾기
      LogFileName := FindMostRecentLogFile(FRotateLogFolder);

      if LogFileName = '' then
      begin
        // 로그 파일을 찾을 수 없음 - 임시 파일 생성
        TempFileName := GetTempDir + 'current_log_' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '.log';
        SaveToFile(TempFileName);
        LogFileName := TempFileName;
      end;
    end;
  end;

  // 파일 뷰어 폼 열기
  ViewerForm := TPrintfLogForm.CreateForFileViewing(Application, LogFileName);
  ViewerForm.Caption := '현재 로그 보기: ' + ExtractFileName(LogFileName);

  // 자동 새로고침 설정 (필요시 활성화)
  // 타이머 기반 자동 새로고침은 TPrintfLogForm에 구현되어 있음

  ViewerForm.Show;
end;

// 가장 최근의 로그 파일 찾기
function TPrintfHandler.FindMostRecentLogFile(const LogFolder: string): string;
var
  SearchRec: TSearchRec;
  MostRecentTime: TDateTime;
  MostRecentFile: string;
  FilePath: string;
  FileTime: TDateTime;
  FileAge: Integer;
begin
  Result := '';

  if not DirectoryExists(LogFolder) then
    Exit;

  MostRecentTime := 0;
  MostRecentFile := '';

  // 디렉토리 내 모든 .log 파일 검색
  if FindFirst(IncludeTrailingPathDelimiter(LogFolder) + '*.log', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        FilePath := IncludeTrailingPathDelimiter(LogFolder) + SearchRec.Name;
        FileAge := SysUtils.FileAge(FilePath);
        if FileAge <> -1 then
        begin
          FileTime := FileDateToDateTime(FileAge);

          if FileTime > MostRecentTime then
          begin
            MostRecentTime := FileTime;
            MostRecentFile := FilePath;
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      SysUtils.FindClose(SearchRec);
    end;
  end;

  Result := MostRecentFile;
end;

// TPrintfHandler 클래스 내 메인 메뉴에 항목 추가
procedure TPrintfHandler.AddViewCurrentLogMenuItem;
var
  LogMenu: TMenuItem;
  MenuItem: TMenuItem;
begin
  if not Assigned(FLogForm) or not Assigned(FLogForm.LogTree) or
     not Assigned(FLogForm.LogTree.PopupMenu) then
    Exit;

  // 로그 보기 메뉴 항목 추가
  MenuItem := TMenuItem.Create(FLogForm.LogTree.PopupMenu);
  MenuItem.Caption := '현재 로그 파일 보기';
  MenuItem.Tag := 7; // 현재 로그 보기 태그
  MenuItem.OnClick := @ViewCurrentLogMenuClick;

  // 적절한 위치에 메뉴 삽입
  FLogForm.LogTree.PopupMenu.Items.Insert(
    FLogForm.LogTree.PopupMenu.Items.Count - 1, MenuItem);  // 마지막 항목 앞에 삽입
end;

// 메뉴 클릭 이벤트 핸들러
procedure TPrintfHandler.ViewCurrentLogMenuClick(Sender: TObject);
begin
  ViewCurrentLogFile;
end;


procedure TPrintfHandler.ConfigureColumnVisibility(ShowTime, ShowLevel, ShowSource, ShowMessage: Boolean);
var
  NewVisibility: TColumnVisibility;
begin
  NewVisibility.ShowTimeColumn := ShowTime;
  NewVisibility.ShowLevelColumn := ShowLevel;
  NewVisibility.ShowSourceColumn := ShowSource;
  NewVisibility.ShowMessageColumn := ShowMessage;

  SetColumnVisibility(NewVisibility);
end;

procedure TPrintfHandler.WriteLog(const Msg: string; Level: TLogLevel);
var
  LogItem: PPrintfLogQueueItem;
  List: TList;
  SourceStr: string;
  CleanMsg: string;
  TimeStart, TimeEnd, LevelStart, LevelEnd, SourceStart, SourceEnd: Integer;
begin
  // 소스 식별자 가져오기
  SourceStr := SourceIdentifier;

  // 메시지 정제 - 이미 [시간] [레벨] [소스] 형식으로 되어 있는지 확인
  CleanMsg := Msg;

  // 메시지에서 이미 [시간] [레벨] [소스] 형식이 있는지 확인하고 제거
  if (Length(Msg) > 2) and (Msg[1] = '[') then
  begin
    TimeStart := 1;
    TimeEnd := Pos(']', Msg, TimeStart);

    if TimeEnd > TimeStart then
    begin
      LevelStart := Pos('[', Msg, TimeEnd);
      if LevelStart > 0 then
      begin
        LevelEnd := Pos(']', Msg, LevelStart);

        if LevelEnd > LevelStart then
        begin
          SourceStart := Pos('[', Msg, LevelEnd);
          if SourceStart > 0 then
          begin
            SourceEnd := Pos(']', Msg, SourceStart);

            if (SourceEnd > SourceStart) and (SourceEnd < Length(Msg)) then
            begin
              // [시간] [레벨] [소스] 형식의 접두어가 있음 - 실제 메시지만 추출
              CleanMsg := Trim(Copy(Msg, SourceEnd + 1, Length(Msg)));
            end;
          end;
        end;
      end;
    end;
  end;

  if AsyncMode = amThread then
  begin
    // 비동기 모드: 큐에 메시지 추가
    New(LogItem);
    LogItem^.Message := CleanMsg; // 정제된 메시지 사용
    LogItem^.Level := Level;
    LogItem^.Source := SourceStr; // 소스 정보

    List := FLogQueue.LockList; // 스레드 안전한 Lock 획득
    try
      List.Add(LogItem);

      // 큐 크기 확인
      if List.Count >= FQueueMaxSize then
        FAsyncThread.SignalQueue;
    finally
      FLogQueue.UnlockList; // Lock 해제
    end;
  end
  else
  begin
    // 동기 모드: 직접 로그 추가
    if Assigned(FLogForm) then
    begin
      try
        FLogForm.AddLog(CleanMsg, Level, SourceStr); // 정제된 메시지 및 소스 정보 전달
      except
        // 추가 실패 무시
      end;
    end;
  end;
end;

procedure TPrintfHandler.WriteSessionStart;
var
  AppName: string;
  SessionStartMsg: string;
begin
  AppName := ExtractFileName(ParamStr(0));
  SessionStartMsg := Format('=== Log Session Started at %s by %s ===',
                           [DateTimeToStr(Now), AppName]);

  // 세션 시작 메시지 기록
  WriteLog(SessionStartMsg, llInfo);
end;

procedure TPrintfHandler.SetAsyncMode(Mode: TAsyncMode);
begin
  // 현재 모드와 같으면 아무것도 하지 않음
  if Mode = AsyncMode then
    Exit;

  inherited SetAsyncMode(Mode);

  case Mode of
    amNone:
      begin
        // 비동기 모드 비활성화
        if Assigned(FAsyncThread) then
        begin
          FAsyncThread.SignalTerminate;
          FAsyncThread.Terminate;
          FAsyncThread.WaitFor;
          FAsyncThread.Free;
          FAsyncThread := nil;
        end;

        // 큐에 남은 로그 처리
        FlushQueue;
      end;

    amThread:
      begin
        // 비동기 스레드 모드 활성화
        if not Assigned(FAsyncThread) then
          FAsyncThread := TPrintfHandlerThread.Create(Self);
      end;
  end;
end;

function TPrintfHandler.GetColorForLevel(Level: TLogLevel): TColor;
begin
  case Level of
    llDevelop:  Result := TColor($AAAAFF); // 연한 노란색
    llDebug:    Result := TColor($AAFFAA); // 연한 녹색
    llInfo:     Result := TColor($FFFFFF); // 흰색
    llWarning:  Result := TColor($55AAFF); // 주황색
    llError:    Result := TColor($5555FF); // 빨간색
    llFatal:    Result := TColor($0000FF); // 진한 빨간색
    else        Result := TColor($FFFFFF); // 기본 흰색
  end;
end;

procedure TPrintfHandler.FlushQueue;
var
  List: TList;
  i: Integer;
  LogItem: PPrintfLogQueueItem;
begin
  if not Assigned(FLogForm) then
    Exit;

  List := FLogQueue.LockList;
  try
    if List.Count = 0 then
      Exit;

    // 모든 큐 항목 처리
    for i := 0 to List.Count - 1 do
    begin
      LogItem := PPrintfLogQueueItem(List[i]);

      // 로그 추가 (소스 정보 포함)
      try
        FLogForm.AddLog(LogItem^.Message, LogItem^.Level, LogItem^.Source);
      except
        // 추가 실패 무시
      end;

      // 메모리 해제
      Dispose(LogItem);
    end;

    // 처리 완료된 항목 제거
    List.Clear;

    // 마지막 플러시 시간 갱신
    FLastQueueFlush := Now;
  finally
    FLogQueue.UnlockList;
  end;
end;

procedure TPrintfHandler.ProcessLogQueue;
var
  CurrentTime: TDateTime;
  ElapsedMS: Int64;
  List: TList;
begin
  CurrentTime := Now;
  ElapsedMS := MilliSecondsBetween(CurrentTime, FLastQueueFlush);

  // 큐 크기 또는 시간 간격 확인
  List := FLogQueue.LockList;
  try
    if (List.Count > 0) and
       ((List.Count >= FQueueMaxSize) or (ElapsedMS >= FQueueFlushInterval)) then
    begin
      // 큐 플러시 필요
      FLogQueue.UnlockList;
      FlushQueue;
    end;
  finally
    if List <> nil then
      FLogQueue.UnlockList;
  end;
end;

procedure TPrintfHandler.HandleFormClear(Sender: TObject);
begin
  // 로그 지우기 이벤트 처리
end;

procedure TPrintfHandler.HandleFormSave(Sender: TObject);
begin
  // 로그 저장 이벤트 처리
end;

procedure TPrintfHandler.SetFormPosition(Value: TFormPosition);
begin
  FFormPosition := Value;

  if not Assigned(FLogForm) then
    Exit;

  case Value of
    fpTopLeft:
      FLogForm.SetBounds(0, 0, FFormWidth, FFormHeight);

    fpBottomLeft:
      FLogForm.SetBounds(0, Screen.Height - FFormHeight - 40, FFormWidth, FFormHeight);

    fpTopRight:
      FLogForm.SetBounds(Screen.Width - FFormWidth, 0, FFormWidth, FFormHeight);

    fpBottomRight:
      FLogForm.SetBounds(Screen.Width - FFormWidth, Screen.Height - FFormHeight - 40, FFormWidth, FFormHeight);

    fpCustom:
      ; // 사용자 정의 위치는 변경하지 않음
  end;
end;

procedure TPrintfHandler.SetFormWidth(Value: Integer);
begin
  FFormWidth := Value;

  if Assigned(FLogForm) then
  begin
    FLogForm.Width := Value;
    SetFormPosition(FFormPosition); // 위치 재조정
  end;
end;

procedure TPrintfHandler.SetFormHeight(Value: Integer);
begin
  FFormHeight := Value;

  if Assigned(FLogForm) then
  begin
    FLogForm.Height := Value;
    SetFormPosition(FFormPosition); // 위치 재조정
  end;
end;

procedure TPrintfHandler.SetFormCaption(Value: string);
begin
  FFormCaption := Value;

  if Assigned(FLogForm) then
    FLogForm.Caption := Value;
end;

procedure TPrintfHandler.SetStayOnTop(Value: Boolean);
begin
  FStayOnTop := Value;

  if Assigned(FLogForm) then
  begin
    if Value then
    begin
      FLogForm.FormStyle := fsStayOnTop;
      FLogForm.StatusBar.Panels[0].Text := '윈도우 상태: 항상 위';
    end
    else
    begin
      FLogForm.FormStyle := fsNormal;
      FLogForm.StatusBar.Panels[0].Text := '윈도우 상태: 표준';
    end;
  end;
end;

procedure TPrintfHandler.SetVisible(Value: Boolean);
begin
  FVisible := Value;

  if Assigned(FLogForm) then
  begin
    if Value then
    begin
      FLogForm.Show;

      // 폼이 다시 표시될 때 트리 갱신
      FLogForm.LogTree.Invalidate;

      // 자동 스크롤이 활성화되어 있다면 마지막 항목으로 스크롤
      if FLogForm.TraceMode = tmAutoScroll then
      begin
        if FLogForm.LogTree.TotalCount > 0 then
          FLogForm.LogTree.ScrollIntoView(FLogForm.LogTree.GetLast, True);
      end;
    end
    else
      FLogForm.Hide;
  end;
end;

procedure TPrintfHandler.UpdateFormVisibility(Visiblity: Boolean);
begin
  FVisible := Visiblity;
end;

procedure TPrintfHandler.SetFormDesign(const ACaption: string; AWidth, AHeight: Integer;
                                     APosition: TFormPosition);
begin
  FFormCaption := ACaption;
  FFormWidth := AWidth;
  FFormHeight := AHeight;
  FFormPosition := APosition;

  if Assigned(FLogForm) then
  begin
    FLogForm.Caption := ACaption;
    FLogForm.Width := AWidth;
    FLogForm.Height := AHeight;
    SetFormPosition(APosition);
  end;
end;

procedure TPrintfHandler.ShowForm(Show: Boolean);
var
  i: Integer;
  MenuItem: TMenuItem;
begin
  SetVisible(Show);

  // 로그 폼 트레이 아이콘 메뉴 업데이트
  if Assigned(FLogForm) and Assigned(FLogForm.FTrayIcon) and
     Assigned(FLogForm.FTrayIcon.PopupMenu) and
     (FLogForm.FTrayIcon.PopupMenu.Items.Count > 0) then
  begin
    if Show then
      FLogForm.FTrayIcon.PopupMenu.Items[0].Caption := '로그 창 숨기기'
    else
      FLogForm.FTrayIcon.PopupMenu.Items[0].Caption := '로그 창 보이기';
  end;

  // 로그 폼의 팝업 메뉴에서 '로그 보이기' 메뉴 활성화/비활성화
  if Assigned(FLogForm) and Assigned(FLogForm.LogTree) and
     Assigned(FLogForm.LogTree.PopupMenu) then
  begin
    for i := 0 to FLogForm.LogTree.PopupMenu.Items.Count - 1 do
    begin
      MenuItem := FLogForm.LogTree.PopupMenu.Items[i];
      if MenuItem.Tag = 4 then // 로그 보이기 메뉴
      begin
        MenuItem.Enabled := not Show; // 폼이 보이면 비활성화, 숨겨져 있으면 활성화
        Break;
      end;
    end;
  end;
end;

procedure TPrintfHandler.SetFont(const FontName: string; FontSize: Integer; FontColor: TColor);
begin
  if Assigned(FLogForm) and Assigned(FLogForm.LogTree) then
  begin
    FLogForm.LogTree.Font.Name := FontName;
    FLogForm.LogTree.Font.Size := FontSize;
    FLogForm.LogTree.Font.Color := FontColor;

    // 노드 갱신을 위한 다시 그리기
    FLogForm.LogTree.Invalidate;
  end;
end;

procedure TPrintfHandler.SetBackgroundColor(Color: TColor);
begin
  if Assigned(FLogForm) and Assigned(FLogForm.LogTree) then
  begin
    FLogForm.LogTree.Color := Color;
    // 노드 갱신을 위한 다시 그리기
    FLogForm.LogTree.Invalidate;
  end;
end;

procedure TPrintfHandler.SaveToFile(const FileName: string);
begin
  if Assigned(FLogForm) then
    FLogForm.SaveLogs(FileName);
end;

procedure TPrintfHandler.ClearLog;
begin
  if Assigned(FLogForm) then
    FLogForm.ClearLogs;
end;

class procedure TPrintfHandler.OpenLogFile(AOwner: TComponent);
var
  OpenDialog: TOpenDialog;
  ViewerForm: TPrintfLogForm;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Title := '로그 파일 열기';
    OpenDialog.Filter := '로그 파일 (*.log)|*.log|텍스트 파일 (*.txt)|*.txt|모든 파일 (*.*)|*.*';
    OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);

    if OpenDialog.Execute then
    begin
      // 새 로그 뷰어 폼 생성
      ViewerForm := TPrintfLogForm.CreateForFileViewing(AOwner, OpenDialog.FileName);
      ViewerForm.Show;
    end;
  finally
    OpenDialog.Free;
  end;
end;

end.