unit PrintfLogForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls,
  Graphics, Dialogs, Menus, Clipbrd, LCLType, DateTimePicker,
  SyncObjs, DateUtils, VirtualTrees, LCLProc,
  LoggerBase, McJSON, PrintfUtils;

type
  // 소스 통계 폼 선언
  TSourceStatForm = class(TForm)
  private
    FListView: TListView;
    FCloseButton: TButton;
    FButtonPanel: TPanel;

    procedure CloseButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    procedure AddSourceItem(const Source: string; Count: Integer; ErrorCount: Integer);
    procedure ClearItems;
  end;

  // 소스 팝업 메뉴 클래스 선언
  TSourcePopupMenu = class(TPopupMenu)
  private
    FLogForm: TObject;
  public
    constructor Create(ALogForm: TObject); reintroduce;
    procedure FilterSourceClick(Sender: TObject);
  end;

  { TPrintfLogForm - 로그 표시 폼 (VirtualTreeView 사용) }
  TPrintfLogForm = class(TForm)
  private
    FLogTree: TVirtualStringTree;  // 로그 표시용 VirtualStringTree
    FStatusBar: TStatusBar;        // 상태바
    FTraceMode: TTraceMode;        // 추적 모드
    FOnClear: TNotifyEvent;        // 클리어 이벤트
    FOnSave: TNotifyEvent;         // 저장 이벤트
    FOwnerHandler: TObject;        // 소유 핸들러

    FTrayIcon: TTrayIcon;          // 시스템 트레이 아이콘

    // 검색 및 필터링 관련 필드
    FSearchPanel: TPanel;
    FFilterPanel: TPanel;
    FSearchEdit: TEdit;
    FSearchCaseSensitive: TCheckBox;
    FSearchWholeWord: TCheckBox;
    FSearchInTime: TCheckBox;
    FSearchInLevel: TCheckBox;
    FSearchInSource: TCheckBox;    // 소스 검색 체크박스
    FSearchInMessage: TCheckBox;
    FSearchButton: TButton;
    FCloseSearchButton: TButton;

    FFilterButton: TButton;
    FFilterInfoCheck: TCheckBox;
    FFilterDebugCheck: TCheckBox;
    FFilterWarningCheck: TCheckBox;
    FFilterErrorCheck: TCheckBox;
    FFilterFatalCheck: TCheckBox;
    FFilterDevelopCheck: TCheckBox;
    FFilterTimeFromCheck: TCheckBox;
    FFilterTimeFrom: TDateTimePicker;
    FFilterTimeToCheck: TCheckBox;
    FFilterTimeTo: TDateTimePicker;
    FFilterTextCheck: TCheckBox;
    FFilterText: TEdit;
    FFilterTextCaseSensitive: TCheckBox;
    // 소스 필터링 필드
    FFilterSourceCheck: TCheckBox;
    FFilterSource: TEdit;
    FFilterSourceCaseSensitive: TCheckBox;
    FApplyFilterButton: TButton;
    FResetFilterButton: TButton;
    FCloseFilterButton: TButton;

    // 컬럼 가시성 관련 필드
    FColumnVisibilityPanel: TPanel;
    FShowTimeColumnCheck: TCheckBox;
    FShowLevelColumnCheck: TCheckBox;
    FShowSourceColumnCheck: TCheckBox;
    FShowMessageColumnCheck: TCheckBox;
    FApplyColumnVisibilityButton: TButton;
    FCloseColumnVisibilityButton: TButton;

    FSearchOptions: TSearchOptions;
    FFilterOptions: TFilterOptions;
    FColumnVisibility: TColumnVisibility; // 컬럼 가시성 옵션
    FOriginalNodes: TList; // 필터링 전 모든 노드 참조 저장
    FIsFileViewMode: Boolean; // 파일 뷰 모드인지 여부
    FCurrentSearchNode: PVirtualNode; // 현재 검색 노드
    FUniqueSourceList: TStringList; // 고유 소스 목록 저장 (통계 및 자동완성용)

    // 자동 새로고침 관련 필드
    FAutoRefreshTimer: TTimer;
    FAutoRefreshEnabled: Boolean;
    FAutoRefreshInterval: Integer; // 밀리초 단위
    FLastLoadedFileName: string;
    FPreserveScrollPos: Boolean;
    FLastVisibleNode: PVirtualNode;
    FLastFileSize: Int64;              // 마지막으로 로드한 파일 크기
    FLastFileModified: TDateTime;      // 마지막 파일 수정 시간
    FLastLineBuffer: array of string;  // 마지막으로 읽은 파일의 마지막 몇 줄
    FFirstRefreshDone: Boolean;        // 첫 새로고침 완료 여부
    FSkipNextRefresh: Boolean;         // 다음 새로고침 건너뛰기 플래그

    procedure StatusBarDblClick(Sender: TObject);
    procedure CreatePopupMenu;
    procedure PopupMenuClick(Sender: TObject);
    procedure CopySelectedLogs;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    // 트레이 아이콘 이벤트 핸들러
    procedure TrayIconClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure CreateTrayIcon;

    // VirtualTreeView 관련 이벤트 핸들러
    procedure LogTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                             Column: TColumnIndex; TextType: TVSTTextType;
                             var CellText: String);
    procedure LogTreeGetNodeDataSize(Sender: TBaseVirtualTree;
                                    var NodeDataSize: Integer);
    procedure LogTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure LogTreeDrawText(Sender: TBaseVirtualTree;
                             TargetCanvas: TCanvas; Node: PVirtualNode;
                             Column: TColumnIndex; const CellText: String;
                             const CellRect: TRect; var DefaultDraw: Boolean);
    procedure LogTreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);

    // 검색 및 필터링 관련
    procedure CreateSearchPanel;
    procedure CreateFilterPanel;
    // 컬럼 가시성 패널 생성
    procedure CreateColumnVisibilityPanel;
    procedure SearchButtonClick(Sender: TObject);
    procedure CloseSearchButtonClick(Sender: TObject);
    procedure FilterButtonClick(Sender: TObject);
    procedure ApplyFilterButtonClick(Sender: TObject);
    procedure ResetFilterButtonClick(Sender: TObject);
    procedure CloseFilterButtonClick(Sender: TObject);
    procedure FilterCheckChange(Sender: TObject);
    // 컬럼 가시성 관련
    procedure ColumnVisibilityButtonClick(Sender: TObject);
    procedure ApplyColumnVisibilityButtonClick(Sender: TObject);
    procedure CloseColumnVisibilityButtonClick(Sender: TObject);

    function MatchesFilter(Node: PVirtualNode): Boolean;
    function FindNextMatch(StartNode: PVirtualNode; SearchOptions: TSearchOptions): PVirtualNode;
    procedure HighlightSearchMatch(Node: PVirtualNode);
    procedure StoreOriginalNodes;
    procedure RestoreNodes;
    procedure ApplyFilter;
    // 컬럼 가시성 적용
    procedure ApplyColumnVisibility;

    // 소스 통계 관련
    procedure UpdateSourceStatistics;
    procedure ShowSourceStatistics;
    procedure AddSourceToList(const SourceStr: string);
    
    // JSON 내보내기/가져오기 관련
    procedure ExportToJSON(const FileName: string);
    procedure ImportFromJSON(const FileName: string);
    procedure SaveLogsToJSON(const JsonFileName: string);
    procedure LoadLogsFromJSON(const JsonFileName: string);
    procedure HandleExportJSONClick(Sender: TObject);
    procedure HandleImportJSONClick(Sender: TObject);
    
  public
    constructor Create(AOwner: TComponent; AHandler: TObject); reintroduce;
    destructor Destroy; override;

    // 파일 뷰 모드용 생성자
    constructor CreateForFileViewing(AOwner: TComponent; const FileName: string);

    // 로그 파일 로드 메서드
    procedure LoadLogsFromFile(const FileName: string);
    // 자동 새로고침 메뉴 추가
    procedure AddAutoRefreshMenuItem;
    // 스크롤 위치 유지 옵션 클릭 이벤트
    procedure OnPreserveScrollPosClick(Sender: TObject);
    // 자동 새로고침 메뉴 클릭 이벤트
    procedure OnAutoRefreshMenuClick(Sender: TObject);
    // 메뉴 항목 체크 상태 업데이트
    procedure UpdateAutoRefreshMenuChecks(SelectedInterval: Integer);
    // 타이머 이벤트 핸들러
    procedure OnAutoRefreshTimer(Sender: TObject);
    // 파일 정보 갱신 메서드
    procedure UpdateFileInfo(const FileName: string);
    // 파일 속성 변경 확인 메서드
    function HasFileAttributesChanged(const FileName: string): Boolean;
    // 수동 새로고침 메뉴 클릭 이벤트
    procedure OnManualRefreshClick(Sender: TObject);
    // 파일 변경 감지 함수 - 파일 끝 부분만 읽어서 비교
    function HasFileContentChanged(const FileName: string): Boolean;
    // 로그 파일 다시 로드 메서드
    procedure ReloadLogFile;

    // 검색 및 필터링 관련 메서드
    procedure ShowSearchPanel;
    procedure HideSearchPanel;
    procedure ShowFilterPanel;
    procedure HideFilterPanel;
    // 컬럼 가시성 패널 표시/숨기기 메서드
    procedure ShowColumnVisibilityPanel;
    procedure HideColumnVisibilityPanel;
    procedure FindNext;
    procedure FindPrev;

    // 로그 항목
    procedure AddLog(const LogText: string; Level: TLogLevel = llInfo; const Source: string = '');

    // 로그 저장
    procedure SaveLogs(const FileName: string = '');

    // 로그 클리어
    procedure ClearLogs;

    // 소스 통계 표시
    procedure ShowSourceStatisticsForm;

    // 속성
    property LogTree: TVirtualStringTree read FLogTree;
    property StatusBar: TStatusBar read FStatusBar;
    property TraceMode: TTraceMode read FTraceMode write FTraceMode;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
    property OnSave: TNotifyEvent read FOnSave write FOnSave;
    property ColumnVisibility: TColumnVisibility read FColumnVisibility write FColumnVisibility;
    property FTrayIcon;
  end;

implementation

{$IFDEF WINDOWS}
uses Windows;
{$ENDIF}

{ TSourceStatForm }

constructor TSourceStatForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);

  Caption := '소스별 로그 통계';
  Width := 450;         // 폼 너비 줄임 (500 -> 450)
  Height := 300;        // 폼 높이 줄임 (400 -> 300)
  Position := poScreenCenter;
  BorderStyle := bsSizeable;

  // 리스트뷰 생성
  FListView := TListView.Create(Self);
  with FListView do
  begin
    Parent := Self;
    Align := alClient;
    ViewStyle := vsReport;
    GridLines := True;
    RowSelect := True;
    ReadOnly := True;

    // 컬럼 설정 수정
    with Columns.Add do
    begin
      Caption := '소스';
      Width := 150;     // 소스 컬럼 너비 줄임 (200 -> 150)
    end;
    with Columns.Add do
    begin
      Caption := '로그 수';
      Width := 80;
      Alignment := taRightJustify;
    end;
    with Columns.Add do
    begin
      Caption := '오류/경고 수';
      Width := 100;
      Alignment := taRightJustify;
    end;
    with Columns.Add do
    begin
      Caption := '비율 (%)';
      Width := 80;
      Alignment := taRightJustify;
    end;
  end;

  // 닫기 버튼 위치 수정
  FCloseButton := TButton.Create(Self);
  with FCloseButton do
  begin
    Parent := Self;
    Caption := '닫기';
    ModalResult := mrOk;
    Default := True;
    Cancel := True;
    Width := 80;
    Height := 30;

    // 버튼 위치를 폼 하단 중앙으로 수정
    Left := (Width - 80) div 2;        // 폼 중앙에 배치
    Top := Height - 40;                // 하단에서 40픽셀 위
    Anchors := [akBottom];             // 하단에 고정

    OnClick := @CloseButtonClick;
  end;

  // 버튼 패널 추가 (닫기 버튼을 담을 패널)
  FButtonPanel := TPanel.Create(Self);
  with FButtonPanel do
  begin
    Parent := Self;
    Align := alBottom;
    Height := 40;
    BevelOuter := bvNone;

    // 닫기 버튼을 패널로 이동
    FCloseButton.Parent := FButtonPanel;
    FCloseButton.Anchors := [akTop, akRight];
    FCloseButton.Left := Width - 90;
    FCloseButton.Top := 5;
  end;
end;

procedure TSourceStatForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSourceStatForm.AddSourceItem(const Source: string; Count: Integer; ErrorCount: Integer);
var
  Item: TListItem;
  Percentage: Double;
begin
  Item := FListView.Items.Add;
  Item.Caption := Source;
  Item.SubItems.Add(IntToStr(Count));
  Item.SubItems.Add(IntToStr(ErrorCount));

  if Count > 0 then
    Percentage := (ErrorCount / Count) * 100
  else
    Percentage := 0;

  Item.SubItems.Add(Format('%.2f', [Percentage]));
end;

procedure TSourceStatForm.ClearItems;
begin
  FListView.Items.Clear;
end;

{ TSourcePopupMenu }

constructor TSourcePopupMenu.Create(ALogForm: TObject);
begin
  inherited Create(TComponent(ALogForm));
  FLogForm := ALogForm;
end;

procedure TSourcePopupMenu.FilterSourceClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  SourceText: string;
  LogForm: TPrintfLogForm;
begin
  if not (Sender is TMenuItem) then
    Exit;

  LogForm := TPrintfLogForm(FLogForm);
  if not Assigned(LogForm) then
    Exit;

  MenuItem := TMenuItem(Sender);
  SourceText := MenuItem.Caption;

  // 필터 패널 표시
  LogForm.ShowFilterPanel;

  // 소스 필터 설정
  LogForm.FFilterSourceCheck.Checked := True;
  LogForm.FFilterSource.Text := SourceText;
  LogForm.FFilterSource.Enabled := True;
  LogForm.FFilterSourceCaseSensitive.Enabled := True;

  // 필터 적용
  LogForm.ApplyFilterButtonClick(nil);
end;

{ TPrintfLogForm }

constructor TPrintfLogForm.Create(AOwner: TComponent; AHandler: TObject);
begin
  inherited CreateNew(AOwner);
  FOwnerHandler := AHandler;

  // 기본 폼 설정
  Caption := '로그 출력';
  Width := 700; // 폼 너비 증가
  Height := 500; // 폼 높이 증가
  Position := poDesigned;
  BorderStyle := bsSizeable;

  // 고유 소스 목록 초기화
  FUniqueSourceList := TStringList.Create;
  FUniqueSourceList.Sorted := True;
  FUniqueSourceList.Duplicates := dupIgnore;

  // 상태바 생성
  FStatusBar := TStatusBar.Create(Self);
  with FStatusBar do
  begin
    Parent := Self;
    SimplePanel := False;
    Panels.Add;
    Panels.Add;
    Panels[0].Width := 300; // 너비 증가
    Panels[0].Text := '윈도우 상태: 표준';
    Panels[1].Width := 300; // 너비 증가
    Panels[1].Text := '더블클릭: 자동 스크롤 활성화';
    OnDblClick := @StatusBarDblClick;
  end;

  // VirtualStringTree 생성
  FLogTree := TVirtualStringTree.Create(Self);

  with FLogTree do
  begin
    Parent := Self;
    Align := alClient;

    // 기본 설정
    Font.Name := '맑은 고딕';
    Font.Size := 9;
    Color := $00644300;  // 어두운 색상
    Font.Color := clSilver;

    // VirtualTreeView 설정
    Header.Options := [hoColumnResize, hoVisible, hoHotTrack];
    Header.Height := 20;

    // 중요: 전체 행 선택 설정
    TreeOptions.SelectionOptions := [toFullRowSelect, toMultiSelect, toRightClickSelect];

    // 기타 옵션 설정
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toFullRepaintOnResize, toGridExtensions, toWheelPanning];
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowVertGridLines, toShowHorzGridLines, toThemeAware];

    // 이벤트 설정
    OnGetText := @LogTreeGetText;
    OnGetNodeDataSize := @LogTreeGetNodeDataSize;
    OnFreeNode := @LogTreeFreeNode;
    OnDrawText := @LogTreeDrawText;
    OnHeaderClick := @LogTreeHeaderClick;

    // 컬럼 설정
    Header.Columns.Clear;

    // 시간 컬럼 - hh:nn:ss.zzz가 모두 보이도록 충분히 넓게 설정
    with Header.Columns.Add do
    begin
      Text := '시간';
      Width := 105;  // 밀리초까지 보이도록 더 넓게 설정
      MinWidth := 105;
      Alignment := taLeftJustify;
      Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
                  coParentColor, coResizable, coVisible, coFixed];
    end;

    // 레벨 컬럼 - 현재 적당함
    with Header.Columns.Add do
    begin
      Text := '레벨';
      Width := 65;
      MinWidth := 55;
      Alignment := taLeftJustify;
      Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
                  coParentColor, coResizable, coVisible, coFixed];
    end;

    // 소스 컬럼 - 더 좁게 조정
    with Header.Columns.Add do
    begin
      Text := '소스';
      Width := 65;  // 더 좁게 설정
      MinWidth := 55;
      Alignment := taLeftJustify;
      Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
                  coParentColor, coResizable, coVisible, coFixed];
    end;

    // 메시지 컬럼 - 더 넓게
    with Header.Columns.Add do
    begin
      Text := '메시지';
      Width := 440;
      MinWidth := 200;
      Alignment := taLeftJustify;
      Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode,
                  coParentColor, coResizable, coVisible]; // coFixed 제외
    end;

    // 컬럼 설정 적용
    Header.AutoSizeIndex := 3; // 메시지 컬럼이 확장되도록 설정

    // 노드 라인 높이 설정
    DefaultNodeHeight := 18;

    // 강제 갱신
    Invalidate;

    // 추가로 컬럼 너비를 초기 설정대로 강제 적용
    BeginUpdate;
    try
      // 각 컬럼의 너비를 명시적으로 설정하고 개별 갱신
      Header.Columns[0].Width := 105;  // 시간 (밀리초까지 표시)
      InvalidateColumn(0);

      Header.Columns[1].Width := 65;  // 레벨 (적당함)
      InvalidateColumn(1);

      Header.Columns[2].Width := 65;  // 소스 (더 좁게)
      InvalidateColumn(2);

      // 메시지 컬럼은 자동 크기 조정됨
      InvalidateColumn(3);
    finally
      EndUpdate;
    end;
  end;

  // 컬럼 가시성 초기 설정
  FColumnVisibility.ShowTimeColumn := True;
  FColumnVisibility.ShowLevelColumn := True;
  FColumnVisibility.ShowSourceColumn := True;  // 기본적으로 소스 컬럼 표시
  FColumnVisibility.ShowMessageColumn := True;

  // 팝업 메뉴 생성
  CreatePopupMenu;

  // 트레이 아이콘 생성
  CreateTrayIcon;

  // 패널 생성
  CreateSearchPanel;
  CreateFilterPanel;
  CreateColumnVisibilityPanel;

  // 기본값 설정
  FTraceMode := tmAutoScroll;

  // 단축키 설정 - 메인 메뉴 없이 단축키 사용
  KeyPreview := True;
  OnKeyDown := @FormKeyDown;
  OnClose := @FormClose;

  // 검색 옵션 초기화
  FSearchOptions.SearchInSource := True;  // 소스 컬럼 검색 활성화

  // 확인을 위한 디버그 코드 - 이 코드는 초기화 후 실행되어야 함
  StatusBar.Panels[0].Text := Format('칼럼 수: %d', [FLogTree.Header.Columns.Count]);
  // 상태바에 단축키 힌트 표시
  FStatusBar.Panels[1].Text := 'Ctrl+S: 저장, Ctrl+L: 지우기, Ctrl+V: 컬럼 표시/숨김, Ctrl+G: 소스통계, Ctrl+J: JSON저장';
end;

destructor TPrintfLogForm.Destroy;
begin
  // 마지막 라인 버퍼 해제
  SetLength(FLastLineBuffer, 0);

  // 타이머 해제
  if Assigned(FAutoRefreshTimer) then
  begin
    FAutoRefreshTimer.Enabled := False;
    FreeAndNil(FAutoRefreshTimer);
  end;

  // 원본 노드 리스트 해제
  if Assigned(FOriginalNodes) then
    FOriginalNodes.Free;

  // 소스 목록 해제
  if Assigned(FUniqueSourceList) then
    FUniqueSourceList.Free;

  // 필요한 다른 정리 작업...

  inherited;
end;

procedure TPrintfLogForm.CreateTrayIcon;
var
  TrayPopupMenu: TPopupMenu;
  MenuItem: TMenuItem;
  IconHandle: TIcon;
begin
  // 트레이 아이콘 생성
  FTrayIcon := TTrayIcon.Create(Self);

  // 아이콘 생성 및 설정
   IconHandle := TIcon.Create;
   try
     // 리소스에서 아이콘 로드
     IconHandle.LoadFromResourceName(HInstance, 'LOGVIEWER_ICON');

     // 파일에서 아이콘 로드
     // IconHandle.LoadFromFile('logviewer.ico');

     FTrayIcon.Icon.Assign(IconHandle);
   finally
     IconHandle.Free;
   end;

  // 트레이 아이콘 기본 설정 {16x16 ico 파일만 된다. 명심}
  // 아이콘 파일을 프로젝트에 추가할 때는 .ico 파일을 프로젝트 폴더에 넣고, 프로젝트 옵션에서 리소스로 추가함.
  FTrayIcon.Visible := True;
  FTrayIcon.Hint := '로그 뷰어';
  FTrayIcon.BalloonTitle := '로그 뷰어';
  FTrayIcon.BalloonHint := '로그 뷰어가 실행 중입니다. 클릭하여 보기/숨기기를 토글할 수 있습니다.';

  // 아이콘 설정 (기본 아이콘 사용)
  //FTrayIcon.Icon.Assign(Application.Icon);

  // 클릭 이벤트 설정
  FTrayIcon.OnClick := @TrayIconClick;
  FTrayIcon.OnDblClick := @TrayIconDblClick;

  // 트레이 아이콘 전용 팝업 메뉴
  TrayPopupMenu := TPopupMenu.Create(Self);

  // 로그 보이기/숨기기 메뉴
  MenuItem := TMenuItem.Create(TrayPopupMenu);
  MenuItem.Caption := '로그 창 보이기';
  MenuItem.Tag := 1;
  MenuItem.OnClick := @TrayIconClick;
  TrayPopupMenu.Items.Add(MenuItem);

  // 구분선
  MenuItem := TMenuItem.Create(TrayPopupMenu);
  MenuItem.Caption := '-';
  TrayPopupMenu.Items.Add(MenuItem);

  // 로그 지우기 메뉴
  MenuItem := TMenuItem.Create(TrayPopupMenu);
  MenuItem.Caption := '로그 지우기';
  MenuItem.Tag := 2;
  MenuItem.OnClick := @PopupMenuClick;
  TrayPopupMenu.Items.Add(MenuItem);

  // 로그 저장 메뉴
  MenuItem := TMenuItem.Create(TrayPopupMenu);
  MenuItem.Caption := '로그 저장...';
  MenuItem.Tag := 3;
  MenuItem.OnClick := @PopupMenuClick;
  TrayPopupMenu.Items.Add(MenuItem);

  // JSON 내보내기 메뉴
  MenuItem := TMenuItem.Create(TrayPopupMenu);
  MenuItem.Caption := 'JSON으로 저장...';
  MenuItem.Tag := 8;
  MenuItem.OnClick := @HandleExportJSONClick;
  TrayPopupMenu.Items.Add(MenuItem);

  // 구분선
  MenuItem := TMenuItem.Create(TrayPopupMenu);
  MenuItem.Caption := '-';
  TrayPopupMenu.Items.Add(MenuItem);

  // 소스 통계 메뉴
  MenuItem := TMenuItem.Create(TrayPopupMenu);
  MenuItem.Caption := '소스별 통계 보기...';
  MenuItem.Tag := 6;
  MenuItem.OnClick := @PopupMenuClick;
  TrayPopupMenu.Items.Add(MenuItem);

  // 구분선
  MenuItem := TMenuItem.Create(TrayPopupMenu);
  MenuItem.Caption := '-';
  TrayPopupMenu.Items.Add(MenuItem);

  // 종료 메뉴
  MenuItem := TMenuItem.Create(TrayPopupMenu);
  MenuItem.Caption := '로그 창 종료';
  MenuItem.Tag := 4;
  MenuItem.OnClick := @PopupMenuClick;
  TrayPopupMenu.Items.Add(MenuItem);

  // 팝업 메뉴 설정
  FTrayIcon.PopupMenu := TrayPopupMenu;
end;

procedure TPrintfLogForm.TrayIconClick(Sender: TObject);
begin
  // 로그 폼 표시/숨기기 토글
  if Visible then
  begin
    Hide;
    if Sender is TMenuItem then
      TMenuItem(Sender).Caption := '로그 창 보이기';
    // 트레이 아이콘 메뉴 캡션 업데이트
    if Assigned(FTrayIcon) and Assigned(FTrayIcon.PopupMenu) and
       (FTrayIcon.PopupMenu.Items.Count > 0) then
    begin
      FTrayIcon.PopupMenu.Items[0].Caption := '로그 창 보이기';
    end;
  end
  else
  begin
    Show;
    BringToFront;
    if Sender is TMenuItem then
      TMenuItem(Sender).Caption := '로그 창 숨기기';
    // 트레이 아이콘 메뉴 캡션 업데이트
    if Assigned(FTrayIcon) and Assigned(FTrayIcon.PopupMenu) and
       (FTrayIcon.PopupMenu.Items.Count > 0) then
    begin
      FTrayIcon.PopupMenu.Items[0].Caption := '로그 창 숨기기';
    end;
  end;

  // 소유자 핸들러에게 가시성 변경 알림
  if Assigned(FOwnerHandler) then
    TMethod(FOwnerHandler).Code := @TObject.MethodAddress(FOwnerHandler, 'UpdateFormVisibility');
    if Assigned(TMethod(FOwnerHandler).Code) then
      TMethod(FOwnerHandler).Call([Visible]);
end;

procedure TPrintfLogForm.TrayIconDblClick(Sender: TObject);
begin
  // 더블클릭 시 로그 폼 표시/숨기기 토글 (click과 동일하게 동작)
  TrayIconClick(Sender);
end;

procedure TPrintfLogForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // 폼이 닫힐 때 처리 - 항상 트레이로 숨기기
  CloseAction := caNone; // 폼이 실제로 닫히지 않도록 함
  Hide; // 대신 폼을 숨김

  // 트레이 아이콘 메뉴 캡션 업데이트
  if Assigned(FTrayIcon) and Assigned(FTrayIcon.PopupMenu) and
     (FTrayIcon.PopupMenu.Items.Count > 0) then
  begin
    FTrayIcon.PopupMenu.Items[0].Caption := '로그 창 보이기';
  end;

  // 소유자 핸들러에게 폼이 숨겨졌음을 알림
  if Assigned(FOwnerHandler) then
    TMethod(FOwnerHandler).Code := @TObject.MethodAddress(FOwnerHandler, 'UpdateFormVisibility');
    if Assigned(TMethod(FOwnerHandler).Code) then
      TMethod(FOwnerHandler).Call([False]);
end;

procedure TPrintfLogForm.LogTreeGetNodeDataSize(Sender: TBaseVirtualTree;
                                              var NodeDataSize: Integer);
begin
  // 노드 데이터 크기 반환
  NodeDataSize := SizeOf(TLogData);
end;

procedure TPrintfLogForm.LogTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PLogData;
begin
  // 노드 데이터 해제
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    Data^.Message := ''; // 문자열 해제
    Data^.Source := '';  // 소스 문자열 해제
  end;
end;

procedure TPrintfLogForm.LogTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
                                       Column: TColumnIndex; TextType: TVSTTextType;
                                       var CellText: String);
var
  Data: PLogData;
begin
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then
    Exit;

  // 저장된 데이터에서 직접 필드 값 사용 - 파싱 재시도 없음
  case Column of
    0: // 시간 컬럼
      begin
        // 시간 형식 수정 - 파일 뷰 모드가 아니고 ShowDateInTimeColumn이 꺼져 있으면 시간만 표시
        if (not FIsFileViewMode) and Assigned(FOwnerHandler) then
        begin
          // 소유 핸들러에서 ShowDateInTimeColumn 속성 확인
          TMethod(FOwnerHandler).Code := @TObject.MethodAddress(FOwnerHandler, 'GetShowDateInTimeColumn');
          if Assigned(TMethod(FOwnerHandler).Code) then
          begin
            if not Boolean(TMethod(FOwnerHandler).Call([])) then
              CellText := FormatDateTime('hh:nn:ss.zzz', Data^.Time)
            else
              CellText := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Data^.Time);
          end
          else
            CellText := FormatDateTime('hh:nn:ss.zzz', Data^.Time);
        end
        else
          CellText := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Data^.Time);
      end;

    1: // 레벨 컬럼
      begin
        case Data^.Level of
          llDevelop: CellText := 'DEVEL';
          llDebug:   CellText := 'DEBUG';
          llInfo:    CellText := 'INFO';
          llWarning: CellText := 'WARN';
          llError:   CellText := 'ERROR';
          llFatal:   CellText := 'FATAL';
          else       CellText := '???';
        end;
      end;

    2: // 소스 컬럼
      begin
        // 저장된 소스 정보 사용
        CellText := Data^.Source;
      end;

    3: // 메시지 컬럼
      begin
        CellText := Data^.Message;
      end;
  end;
end;

procedure TPrintfLogForm.LogTreeDrawText(Sender: TBaseVirtualTree;
                                       TargetCanvas: TCanvas; Node: PVirtualNode;
                                       Column: TColumnIndex; const CellText: String;
                                       const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PLogData;
  SourceColor: TColor;
begin
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then
    Exit;

  // 로그 레벨에 따른 텍스트 색상 변경
  if FIsFileViewMode or (Assigned(FOwnerHandler) and 
      ((TMethod(FOwnerHandler).Code := @TObject.MethodAddress(FOwnerHandler, 'GetColorByLevel')),
       Assigned(TMethod(FOwnerHandler).Code) and Boolean(TMethod(FOwnerHandler).Call([])))) then
  begin
    case Data^.Level of
      llDevelop:  TargetCanvas.Font.Color := TColor($AAAAFF); // 연한 노란색
      llDebug:    TargetCanvas.Font.Color := TColor($AAFFAA); // 연한 녹색
      llInfo:     TargetCanvas.Font.Color := TColor($FFFFFF); // 흰색
      llWarning:  TargetCanvas.Font.Color := TColor($55AAFF); // 주황색
      llError:    TargetCanvas.Font.Color := TColor($5555FF); // 빨간색
      llFatal:    TargetCanvas.Font.Color := TColor($0000FF); // 진한 빨간색
      else        TargetCanvas.Font.Color := TColor($FFFFFF); // 기본 흰색
    end;
  end;

  // 소스별 색상 적용 (소스 컬럼일 경우)
  if (Column = 2) and Assigned(FOwnerHandler) and
     ((TMethod(FOwnerHandler).Code := @TObject.MethodAddress(FOwnerHandler, 'GetColorBySource')),
      Assigned(TMethod(FOwnerHandler).Code)) then
  begin
    SourceColor := TColor(TMethod(FOwnerHandler).Call([Data^.Source]));
    if SourceColor <> clNone then
      TargetCanvas.Font.Color := SourceColor;
  end;

  // 기본 그리기 허용
  DefaultDraw := True;
end;

// 로그 트리 헤더 클릭 이벤트 - 소스 컬럼 정렬 및 필터링 팝업
procedure TPrintfLogForm.LogTreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
var
  Node: PVirtualNode;
  Data: PLogData;
  SourcePopup: TSourcePopupMenu;
  MenuItem: TMenuItem;
  SourceList: TStringList;
  i: Integer;
  SourceStr: string;
  P: TPoint;
begin
  // 헤더 클릭 이벤트 처리 (정렬 등)
  if HitInfo.Button = mbLeft then
  begin
    // 컬럼 정렬 등의 작업 구현 가능
    case HitInfo.Column of
      0: // 시간 컬럼
        begin
          // 시간 정렬 구현
        end;
      1: // 레벨 컬럼
        begin
          // 레벨 정렬 구현
        end;
      2: // 소스 컬럼
        begin
          // 소스 정렬 구현
        end;
      3: // 메시지 컬럼
        begin
          // 메시지 정렬 구현
        end;
    end;
  end
  else if (HitInfo.Button = mbRight) and (HitInfo.Column = 2) then
  begin
    // 소스 컬럼에서 우클릭 시 소스 필터링 팝업 메뉴 표시
    SourceList := TStringList.Create;
    try
      SourceList.Sorted := True;
      SourceList.Duplicates := dupIgnore;

      // 고유 소스 목록
      Node := FLogTree.GetFirst;
      while Assigned(Node) do
      begin
        Data := FLogTree.GetNodeData(Node);
        if Assigned(Data) and (Data^.Source <> '') then
          SourceList.Add(Data^.Source);
        Node := FLogTree.GetNext(Node);
      end;

      if SourceList.Count > 0 then
      begin
        // 소스 필터링 팝업 메뉴 생성
        SourcePopup := TSourcePopupMenu.Create(Self);

        // 메뉴 항목: 모든 소스 표시
        MenuItem := TMenuItem.Create(SourcePopup);
        MenuItem.Caption := '모든 소스 표시';
        MenuItem.OnClick := @ResetFilterButtonClick;
        SourcePopup.Items.Add(MenuItem);

        // 구분선
        MenuItem := TMenuItem.Create(SourcePopup);
        MenuItem.Caption := '-';
        SourcePopup.Items.Add(MenuItem);

        // 각 소스별 필터 메뉴 항목 추가
        for i := 0 to SourceList.Count - 1 do
        begin
          SourceStr := SourceList[i];
          MenuItem := TMenuItem.Create(SourcePopup);
          MenuItem.Caption := SourceStr;
          MenuItem.OnClick := @SourcePopup.FilterSourceClick;
          SourcePopup.Items.Add(MenuItem);
        end;

        // 팝업 메뉴 표시
        P := Mouse.CursorPos;
        SourcePopup.Popup(P.X, P.Y);
      end;
    finally
      SourceList.Free;
    end;
  end;
end;

// 소스 통계 관련 메서드
procedure TPrintfLogForm.AddSourceToList(const SourceStr: string);
begin
  if (SourceStr <> '') then
  begin
    // 이미 목록에 있는지 확인한 후 추가
    if FUniqueSourceList.IndexOf(SourceStr) < 0 then
      FUniqueSourceList.Add(SourceStr);
  end;
end;

procedure TPrintfLogForm.UpdateSourceStatistics;
var
  Node: PVirtualNode;
  Data: PLogData;
  SourceStr: string;
begin
  // 기존 코드 대체

  // 통계 수집을 위해 모든 노드 순회
  FUniqueSourceList.Clear; // 기존 목록 초기화
  FUniqueSourceList.Sorted := True; // 정렬 활성화
  FUniqueSourceList.Duplicates := dupIgnore; // 중복 무시

  Node := FLogTree.GetFirst;
  while Assigned(Node) do
  begin
    Data := FLogTree.GetNodeData(Node);
    if Assigned(Data) then
    begin
      SourceStr := Data^.Source;
      if SourceStr <> '' then
      begin
        // TStringList.Add 메서드 사용 (인덱스 지정 없음)
        if FUniqueSourceList.IndexOf(SourceStr) < 0 then
          FUniqueSourceList.Add(SourceStr);
      end;
    end;
    Node := FLogTree.GetNext(Node);
  end;
end;

procedure TPrintfLogForm.ShowSourceStatistics;
var
  StatForm: TSourceStatForm;
  Node: PVirtualNode;
  Data: PLogData;
  SourceCounts: TStringList; // 소스별 로그 수
  SourceErrorCounts: TStringList; // 소스별 오류/경고 수
  i: Integer;
  SourceStr: string;
  Count, ErrorCount: Integer;
  TempList: TStringList; // 정렬되지 않은 임시 리스트
begin
  // 통계 데이터 수집
  SourceCounts := TStringList.Create;
  SourceErrorCounts := TStringList.Create;
  TempList := TStringList.Create; // 정렬되지 않은 임시 리스트 추가

  try
    // 초기 설정
    SourceCounts.Sorted := False; // 정렬 비활성화
    SourceCounts.Duplicates := dupAccept;
    SourceErrorCounts.Sorted := False; // 정렬 비활성화
    SourceErrorCounts.Duplicates := dupAccept;

    // 먼저 모든 고유 소스 수집 (정렬되지 않은 임시 리스트 사용)
    TempList.Clear;
    TempList.Sorted := False;
    TempList.Duplicates := dupIgnore;

    // 모든 소스 수집
    Node := FLogTree.GetFirst;
    while Assigned(Node) do
    begin
      Data := FLogTree.GetNodeData(Node);
      if Assigned(Data) then
      begin
        SourceStr := Data^.Source;
        if SourceStr = '' then
          SourceStr := '(없음)';

        // 소스가 이미 TempList에 있는지 확인
        if TempList.IndexOf(SourceStr) < 0 then
          TempList.Add(SourceStr);
      end;
      Node := FLogTree.GetNext(Node);
    end;

    // 소스별 카운트 초기화
    for i := 0 to TempList.Count - 1 do
    begin
      SourceCounts.Values[TempList[i]] := '0';
      SourceErrorCounts.Values[TempList[i]] := '0';
    end;

    // 로그 항목 분석
    Node := FLogTree.GetFirst;
    while Assigned(Node) do
    begin
      Data := FLogTree.GetNodeData(Node);
      if Assigned(Data) then
      begin
        SourceStr := Data^.Source;
        if SourceStr = '' then
          SourceStr := '(없음)';

        // 총 카운트 증가
        Count := StrToIntDef(SourceCounts.Values[SourceStr], 0);
        SourceCounts.Values[SourceStr] := IntToStr(Count + 1);

        // 오류/경고 카운트 증가 (ERROR, WARNING, FATAL 레벨)
        if Data^.Level in [llWarning, llError, llFatal] then
        begin
          ErrorCount := StrToIntDef(SourceErrorCounts.Values[SourceStr], 0);
          SourceErrorCounts.Values[SourceStr] := IntToStr(ErrorCount + 1);
        end;
      end;
      Node := FLogTree.GetNext(Node);
    end;

    // 통계 폼 생성 및 표시
    StatForm := TSourceStatForm.Create(Self);
    try
      StatForm.ClearItems;

      // 데이터 추가
      for i := 0 to SourceCounts.Count - 1 do
      begin
        SourceStr := SourceCounts.Names[i];
        Count := StrToIntDef(SourceCounts.Values[SourceStr], 0);
        ErrorCount := StrToIntDef(SourceErrorCounts.Values[SourceStr], 0);

        if Count > 0 then
          StatForm.AddSourceItem(SourceStr, Count, ErrorCount);
      end;

      StatForm.ShowModal;
    finally
      StatForm.Free;
    end;

  finally
    SourceCounts.Free;
    SourceErrorCounts.Free;
    TempList.Free; // 임시 리스트 해제
  end;
end;

procedure TPrintfLogForm.ShowSourceStatisticsForm;
begin
  // 소스 통계 정보 업데이트 및 표시
  UpdateSourceStatistics;
  ShowSourceStatistics;
end;

// 컬럼 가시성 패널 구현
procedure TPrintfLogForm.CreateColumnVisibilityPanel;
begin
  // 컬럼 가시성 패널 생성
  FColumnVisibilityPanel := TPanel.Create(Self);
  with FColumnVisibilityPanel do
  begin
    Parent := Self;
    Align := alTop;
    Height := 80;
    BevelOuter := bvNone;
    BorderStyle := bsSingle;
    Color := clBtnFace;
    Visible := False;
  end;

  // 컬럼 체크박스 생성
  FShowTimeColumnCheck := TCheckBox.Create(FColumnVisibilityPanel);
  with FShowTimeColumnCheck do
  begin
    Parent := FColumnVisibilityPanel;
    Left := 10;
    Top := 10;
    Width := 120;
    Caption := '시간 컬럼 표시';
    Checked := True;
  end;

  FShowLevelColumnCheck := TCheckBox.Create(FColumnVisibilityPanel);
  with FShowLevelColumnCheck do
  begin
    Parent := FColumnVisibilityPanel;
    Left := 140;
    Top := 10;
    Width := 120;
    Caption := '레벨 컬럼 표시';
    Checked := True;
  end;

  FShowSourceColumnCheck := TCheckBox.Create(FColumnVisibilityPanel);
  with FShowSourceColumnCheck do
  begin
    Parent := FColumnVisibilityPanel;
    Left := 10;
    Top := 40;
    Width := 120;
    Caption := '소스 컬럼 표시';
    Checked := True;
  end;

  FShowMessageColumnCheck := TCheckBox.Create(FColumnVisibilityPanel);
  with FShowMessageColumnCheck do
  begin
    Parent := FColumnVisibilityPanel;
    Left := 140;
    Top := 40;
    Width := 150;
    Caption := '메시지 컬럼 표시';
    Checked := True;
  end;

  // 적용 버튼
  FApplyColumnVisibilityButton := TButton.Create(FColumnVisibilityPanel);
  with FApplyColumnVisibilityButton do
  begin
    Parent := FColumnVisibilityPanel;
    Left := FColumnVisibilityPanel.Width - 180;
    Top := 20;
    Width := 80;
    Height := 25;
    Caption := '적용';
    OnClick := @ApplyColumnVisibilityButtonClick;
    Anchors := [akTop, akRight];
  end;

  // 닫기 버튼
  FCloseColumnVisibilityButton := TButton.Create(FColumnVisibilityPanel);
  with FCloseColumnVisibilityButton do
  begin
    Parent := FColumnVisibilityPanel;
    Left := FColumnVisibilityPanel.Width - 90;
    Top := 20;
    Width := 80;
    Height := 25;
    Caption := '닫기';
    OnClick := @CloseColumnVisibilityButtonClick;
    Anchors := [akTop, akRight];
  end;
end;

// 컬럼 가시성 패널 표시/숨기기
procedure TPrintfLogForm.ShowColumnVisibilityPanel;
begin
  // 다른 패널 숨기기
  HideSearchPanel;
  HideFilterPanel;

  // 현재 설정 반영
  FShowTimeColumnCheck.Checked := FColumnVisibility.ShowTimeColumn;
  FShowLevelColumnCheck.Checked := FColumnVisibility.ShowLevelColumn;
  FShowSourceColumnCheck.Checked := FColumnVisibility.ShowSourceColumn;
  FShowMessageColumnCheck.Checked := FColumnVisibility.ShowMessageColumn;

  // 패널 표시
  FColumnVisibilityPanel.Visible := True;
end;

procedure TPrintfLogForm.HideColumnVisibilityPanel;
begin
  FColumnVisibilityPanel.Visible := False;

  // 포커스 트리로 이동
  if FLogTree.CanFocus then
    FLogTree.SetFocus;
end;

// 컬럼 가시성 적용
procedure TPrintfLogForm.ApplyColumnVisibilityButtonClick(Sender: TObject);
begin
  // 설정 업데이트
  FColumnVisibility.ShowTimeColumn := FShowTimeColumnCheck.Checked;
  FColumnVisibility.ShowLevelColumn := FShowLevelColumnCheck.Checked;
  FColumnVisibility.ShowSourceColumn := FShowSourceColumnCheck.Checked;
  FColumnVisibility.ShowMessageColumn := FShowMessageColumnCheck.Checked;

  // 설정 적용
  ApplyColumnVisibility;

  // 상태바 업데이트
  StatusBar.Panels[0].Text := '컬럼 가시성 설정이 적용되었습니다.';
end;

procedure TPrintfLogForm.CloseColumnVisibilityButtonClick(Sender: TObject);
begin
  HideColumnVisibilityPanel;
end;

// 컬럼 가시성 메뉴 버튼 클릭
procedure TPrintfLogForm.ColumnVisibilityButtonClick(Sender: TObject);
begin
  ShowColumnVisibilityPanel;
end;

// 컬럼 가시성 적용 메서드
procedure TPrintfLogForm.ApplyColumnVisibility;
begin
  with FLogTree.Header do
  begin
    // 시간 컬럼 가시성
    if Columns.Count > 0 then
    begin
      if FColumnVisibility.ShowTimeColumn then
        Columns[0].Options := Columns[0].Options + [coVisible]
      else
        Columns[0].Options := Columns[0].Options - [coVisible];
    end;

    // 레벨 컬럼 가시성
    if Columns.Count > 1 then
    begin
      if FColumnVisibility.ShowLevelColumn then
        Columns[1].Options := Columns[1].Options + [coVisible]
      else
        Columns[1].Options := Columns[1].Options - [coVisible];
    end;

    // 소스 컬럼 가시성
    if Columns.Count > 2 then
    begin
      if FColumnVisibility.ShowSourceColumn then
        Columns[2].Options := Columns[2].Options + [coVisible]
      else
        Columns[2].Options := Columns[2].Options - [coVisible];
    end;

    // 메시지 컬럼 가시성
    if Columns.Count > 3 then
    begin
      if FColumnVisibility.ShowMessageColumn then
        Columns[3].Options := Columns[3].Options + [coVisible]
      else
        Columns[3].Options := Columns[3].Options - [coVisible];
    end;
  end;

  // 컬럼 가시성 변경 후 갱신
  FLogTree.Invalidate;

  // 소유 핸들러에 설정 전달 (실시간 로깅 폼인 경우)
  if Assigned(FOwnerHandler) then
  begin
    TMethod(FOwnerHandler).Code := @TObject.MethodAddress(FOwnerHandler, 'SetColumnVisibility');
    if Assigned(TMethod(FOwnerHandler).Code) then
      TMethod(FOwnerHandler).Call([FColumnVisibility]);
  end;
end;

// 검색 패널 - 소스 검색
procedure TPrintfLogForm.CreateSearchPanel;
begin
  // 검색 패널 생성
  FSearchPanel := TPanel.Create(Self);
  with FSearchPanel do
  begin
    Parent := Self;
    Align := alTop;
    Height := 80;  // 높이 증가
    BevelOuter := bvNone;
    BorderStyle := bsSingle;
    Color := clBtnFace;
    Visible := False;
  end;

  // 검색어 입력
  FSearchEdit := TEdit.Create(FSearchPanel);
  with FSearchEdit do
  begin
    Parent := FSearchPanel;
    Left := 10;
    Top := 10;
    Width := 300;  // 너비 증가
    Anchors := [akLeft, akTop, akRight];
    Text := '';
    TabOrder := 0;
  end;

  // 검색 옵션 체크박스
  FSearchCaseSensitive := TCheckBox.Create(FSearchPanel);
  with FSearchCaseSensitive do
  begin
    Parent := FSearchPanel;
    Left := 10;
    Top := 40;
    Width := 120;
    Caption := '대소문자 구분';
    Checked := False;
  end;

  FSearchWholeWord := TCheckBox.Create(FSearchPanel);
  with FSearchWholeWord do
  begin
    Parent := FSearchPanel;
    Left := 140;
    Top := 40;
    Width := 120;
    Caption := '전체 단어만';
    Checked := False;
  end;

  FSearchInTime := TCheckBox.Create(FSearchPanel);
  with FSearchInTime do
  begin
    Parent := FSearchPanel;
    Left := 270;
    Top := 40;
    Width := 80;
    Caption := '시간';
    Checked := True;
  end;

  FSearchInLevel := TCheckBox.Create(FSearchPanel);
  with FSearchInLevel do
  begin
    Parent := FSearchPanel;
    Left := 350;
    Top := 40;
    Width := 80;
    Caption := '레벨';
    Checked := True;
  end;

  // 소스 검색 체크박스
  FSearchInSource := TCheckBox.Create(FSearchPanel);
  with FSearchInSource do
  begin
    Parent := FSearchPanel;
    Left := 430;
    Top := 40;
    Width := 80;
    Caption := '소스';
    Checked := True;
  end;

  FSearchInMessage := TCheckBox.Create(FSearchPanel);
  with FSearchInMessage do
  begin
    Parent := FSearchPanel;
    Left := 510;
    Top := 40;
    Width := 80;
    Caption := '메시지';
    Checked := True;
  end;

  // 검색 버튼
  FSearchButton := TButton.Create(FSearchPanel);
  with FSearchButton do
  begin
    Parent := FSearchPanel;
    Left := FSearchPanel.Width - 180;
    Top := 10;
    Width := 80;
    Height := 25;
    Caption := '다음 찾기';
    Default := True;
    OnClick := @SearchButtonClick;
    Anchors := [akTop, akRight];
  end;

  // 닫기 버튼
  FCloseSearchButton := TButton.Create(FSearchPanel);
  with FCloseSearchButton do
  begin
    Parent := FSearchPanel;
    Left := FSearchPanel.Width - 90;
    Top := 10;
    Width := 80;
    Height := 25;
    Caption := '닫기';
    OnClick := @CloseSearchButtonClick;
    Anchors := [akTop, akRight];
  end;
end;

// 필터 패널 - 소스 필터링
procedure TPrintfLogForm.CreateFilterPanel;
begin
  // 필터 패널 생성
  FFilterPanel := TPanel.Create(Self);
  with FFilterPanel do
  begin
    Parent := Self;
    Align := alTop;
    Height := 130;  // 높이 증가
    BevelOuter := bvNone;
    BorderStyle := bsSingle;
    Color := clBtnFace;
    Visible := False;
  end;

  // 필터 옵션 - 로그 레벨
  FFilterInfoCheck := TCheckBox.Create(FFilterPanel);
  with FFilterInfoCheck do
  begin
    Parent := FFilterPanel;
    Left := 10;
    Top := 5;
    Width := 60;
    Caption := 'INFO';
    Checked := True;
    OnClick := @FilterCheckChange;
  end;

  FFilterDebugCheck := TCheckBox.Create(FFilterPanel);
  with FFilterDebugCheck do
  begin
    Parent := FFilterPanel;
    Left := 80;
    Top := 5;
    Width := 70;
    Caption := 'DEBUG';
    Checked := True;
    OnClick := @FilterCheckChange;
  end;

  FFilterWarningCheck := TCheckBox.Create(FFilterPanel);
  with FFilterWarningCheck do
  begin
    Parent := FFilterPanel;
    Left := 160;
    Top := 5;
    Width := 60;
    Caption := 'WARN';
    Checked := True;
    OnClick := @FilterCheckChange;
  end;

  FFilterErrorCheck := TCheckBox.Create(FFilterPanel);
  with FFilterErrorCheck do
  begin
    Parent := FFilterPanel;
    Left := 230;
    Top := 5;
    Width := 70;
    Caption := 'ERROR';
    Checked := True;
    OnClick := @FilterCheckChange;
  end;

  FFilterFatalCheck := TCheckBox.Create(FFilterPanel);
  with FFilterFatalCheck do
  begin
    Parent := FFilterPanel;
    Left := 310;
    Top := 5;
    Width := 70;
    Caption := 'FATAL';
    Checked := True;
    OnClick := @FilterCheckChange;
  end;

  FFilterDevelopCheck := TCheckBox.Create(FFilterPanel);
  with FFilterDevelopCheck do
  begin
    Parent := FFilterPanel;
    Left := 390;
    Top := 5;
    Width := 80;
    Caption := 'DEVEL';
    Checked := True;
    OnClick := @FilterCheckChange;
  end;

  // 시간 필터
  FFilterTimeFromCheck := TCheckBox.Create(FFilterPanel);
  with FFilterTimeFromCheck do
  begin
    Parent := FFilterPanel;
    Left := 10;
    Top := 35;
    Width := 60;
    Caption := '시작:';
    Checked := False;
    OnClick := @FilterCheckChange;
  end;

  FFilterTimeFrom := TDateTimePicker.Create(FFilterPanel);
  with FFilterTimeFrom do
  begin
    Parent := FFilterPanel;
    Left := 80;
    Top := 33;
    Width := 150;
    Kind := dtkDateTime;
    Time := StartOfTheDay(Now);
    Enabled := False;
  end;

  FFilterTimeToCheck := TCheckBox.Create(FFilterPanel);
  with FFilterTimeToCheck do
  begin
    Parent := FFilterPanel;
    Left := 240;
    Top := 35;
    Width := 60;
    Caption := '종료:';
    Checked := False;
    OnClick := @FilterCheckChange;
  end;

  FFilterTimeTo := TDateTimePicker.Create(FFilterPanel);
  with FFilterTimeTo do
  begin
    Parent := FFilterPanel;
    Left := 310;
    Top := 33;
    Width := 150;
    Kind := dtkDateTime;
    Time := EndOfTheDay(Now);
    Enabled := False;
  end;

  // 텍스트 필터
  FFilterTextCheck := TCheckBox.Create(FFilterPanel);
  with FFilterTextCheck do
  begin
    Parent := FFilterPanel;
    Left := 10;
    Top := 65;
    Width := 80;
    Caption := '텍스트:';
    Checked := False;
    OnClick := @FilterCheckChange;
  end;

  FFilterText := TEdit.Create(FFilterPanel);
  with FFilterText do
  begin
    Parent := FFilterPanel;
    Left := 100;
    Top := 63;
    Width := 250;
    Enabled := False;
  end;

  FFilterTextCaseSensitive := TCheckBox.Create(FFilterPanel);
  with FFilterTextCaseSensitive do
  begin
    Parent := FFilterPanel;
    Left := 360;
    Top := 65;
    Width := 120;
    Caption := '대소문자 구분';
    Checked := False;
    Enabled := False;
  end;

  // 소스 필터
  FFilterSourceCheck := TCheckBox.Create(FFilterPanel);
  with FFilterSourceCheck do
  begin
    Parent := FFilterPanel;
    Left := 10;
    Top := 95;
    Width := 80;
    Caption := '소스:';
    Checked := False;
    OnClick := @FilterCheckChange;
  end;

  FFilterSource := TEdit.Create(FFilterPanel);
  with FFilterSource do
  begin
    Parent := FFilterPanel;
    Left := 100;
    Top := 93;
    Width := 250;
    Enabled := False;
  end;

  FFilterSourceCaseSensitive := TCheckBox.Create(FFilterPanel);
  with FFilterSourceCaseSensitive do
  begin
    Parent := FFilterPanel;
    Left := 360;
    Top := 95;
    Width := 120;
    Caption := '대소문자 구분';
    Checked := False;
    Enabled := False;
  end;

  // 필터 적용 및 초기화 버튼
  FApplyFilterButton := TButton.Create(FFilterPanel);
  with FApplyFilterButton do
  begin
    Parent := FFilterPanel;
    Left := FFilterPanel.Width - 240;
    Top := 98;//125;
    Width := 80;
    Height := 25;
    Caption := '필터 적용';
    OnClick := @ApplyFilterButtonClick;
    Anchors := [akRight, akBottom];
  end;

  FResetFilterButton := TButton.Create(FFilterPanel);
  with FResetFilterButton do
  begin
    Parent := FFilterPanel;
    Left := FFilterPanel.Width - 150;
    Top := 98;
    Width := 80;
    Height := 25;
    Caption := '필터 초기화';
    OnClick := @ResetFilterButtonClick;
    Anchors := [akRight, akBottom];
  end;

  // 닫기 버튼
  FCloseFilterButton := TButton.Create(FFilterPanel);
  with FCloseFilterButton do
  begin
    Parent := FFilterPanel;
    Left := FFilterPanel.Width - 60;
    Top := 98;
    Width := 50;
    Height := 25;
    Caption := '닫기';
    OnClick := @CloseFilterButtonClick;
    Anchors := [akRight, akBottom];
  end;
end;

// 필터 체크박스 변경 이벤트 - 소스 필터 체크박스
procedure TPrintfLogForm.FilterCheckChange(Sender: TObject);
begin
  if Sender = FFilterTimeFromCheck then
    FFilterTimeFrom.Enabled := FFilterTimeFromCheck.Checked;

  if Sender = FFilterTimeToCheck then
    FFilterTimeTo.Enabled := FFilterTimeToCheck.Checked;

  if Sender = FFilterTextCheck then
  begin
    FFilterText.Enabled := FFilterTextCheck.Checked;
    FFilterTextCaseSensitive.Enabled := FFilterTextCheck.Checked;
  end;

  // 소스 필터 체크박스 처리
  if Sender = FFilterSourceCheck then
  begin
    FFilterSource.Enabled := FFilterSourceCheck.Checked;
    FFilterSourceCaseSensitive.Enabled := FFilterSourceCheck.Checked;
  end;
end;

procedure TPrintfLogForm.ShowSearchPanel;
begin
  // 필터 패널 숨기기
  HideFilterPanel;
  HideColumnVisibilityPanel;

  // 검색 패널 표시
  FSearchPanel.Visible := True;
  FSearchEdit.SetFocus;
end;

procedure TPrintfLogForm.HideSearchPanel;
begin
  FSearchPanel.Visible := False;

  // 포커스 트리로 이동
  if FLogTree.CanFocus then
    FLogTree.SetFocus;
end;

procedure TPrintfLogForm.ShowFilterPanel;
begin
  // 검색 패널 숨기기
  HideSearchPanel;
  HideColumnVisibilityPanel;

  // 필터 패널 표시
  FFilterPanel.Visible := True;
end;