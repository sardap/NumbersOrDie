program nodHost;
uses sgTypes, SwinGame, sysutils, Process;

const
	SEVER_NAME = 'leaderBoard';
	SCORE_FILE_NAME = 'scores.nod';
	DEFUALT_PORT = 1917;
	SEND_SCORE_BOARD_PREFIX = 'Score:';
	UPLOAD_PREFIX = 'Upload:';

type
	Score = record
		playerName : String;
		netWorth : Extended;
	end;
	ScoreArray = array of Score;

//
// Purpose: Checks If the scoreBoard Should be sent if true it will then send the scoreboard
//
procedure CheckSendScoreBoard(const toCheck: Message; const scoreData: ScoreArray);
var
	i, amountToSend : Integer;
	responseConn : Connection;
	toSend, temp : String;

begin
	// Checks if it is an Send Score Message
	if (Pos(SEND_SCORE_BOARD_PREFIX, MessageData(toCheck)) > 0) then
	begin
		amountToSend := StrToInt(Copy(MessageData(toCheck), Pos(':', MessageData(toCheck))+1, Length(MessageData(toCheck))));
		responseConn := MessageConnection(toCheck);
		WriteLn('Sending ', amountToSend, ' Scores');
		//sends the top scores
		i := High(scoreData);
		while (High(scoreData) - i < amountToSend) and not (i <= 0) do
		begin
			Str(scoreData[i].netWorth:0:3, toSend);
			toSend := scoreData[i].playerName + ' = ' + toSend;
			if SendMessageTo(toSend, responseConn) then
				WriteLn('Message ', i , ' Sent')
			else
				WriteLn('FAILED TO SEND');
			i -= 1;
		end;
		if CloseConnection(responseConn) then
			WriteLn('Connection Closed');
	end;
end;
//
// Purpose: Sorts scoreData
//
procedure SelectionSort(var scoreData: ScoreArray);
var
	i, j: Integer;
	temp : Score;

begin
	//using SelectionSort Because it's nearly Sorted
	for i:=0 to High(scoreData) do
		for j:=i+1 to High(scoreData) do
			if scoreData[i].netWorth > scoreData[j].netWorth then
			begin
				temp := scoreData[i];
				scoreData[i] := scoreData[j];
				scoreData[j] := temp;
			end;
end;
//
// Purpose: Reads a score sent to the Server
//
function ReadScore(const toCheck: String): Score;
begin
	//Remeber File Format is name:netWorth and the ID is the same as the index
	result.playerName := Copy(toCheck, 0, Pos(':', toCheck)-1);
	result.netWorth := StrToFloat(Copy(toCheck, Pos(':', toCheck)+1, Length(toCheck)));
end;
//
// Purpose: Checks if the Upload is vaild
//
function CheckUploadVaild(toCheck: String; checkChar: Char; target: Integer): Boolean;
var
	i, count : Integer;

begin
	count := 0;
	for i:=0 to Length(toCheck) do
		if toCheck[i] = checkChar then
			count += 1;

	if count > target then
		result := false
	else
		result := true;
end;
//
// Purpose: Checks if the message is an upload if so it will then add it to the scoreData Record
//
procedure CheckUpload(toCheck: String; var scoreData: ScoreArray);
var
	startPos : Integer;

begin
	// Checks if it is an upload Message and if Vaild
	if (Pos(UPLOAD_PREFIX, toCheck) > 0) and (CheckUploadVaild(toCheck, ':', 2)) then
	begin
		startPos := Pos(UPLOAD_PREFIX, toCheck) + Length(UPLOAD_PREFIX);
		SetLength(scoreData, Length(scoreData)+1);
		//Copys the stirng past the Upload: then passes that to the read function
		toCheck := Copy(toCheck, startPos, Length(toCheck));
		scoreData[High(scoreData)] := ReadScore(toCheck);
		WriteLn(scoreData[High(scoreData)].playername, scoreData[High(scoreData)].netWorth);
		// Then Sorts the Array With The new Value
		SelectionSort(scoreData);
	end;
end;
//
// Purpose: Returns the number of lines in a file
//
function GetNumberOfLines(const fileName: UniCodeString): Integer;
var
	toCheckF : TextFile;
	temp : String;

begin
	result := 0;
	AssignFile(toCheckF, fileName);
	Reset(toCheckF);
	while not eof(toCheckF) do
  begin
    ReadLn(toCheckF, temp);
		result += 1;
  end;
	close(toCheckF);
end;
//
// Purpose: Updates the score File with new scores
//
procedure UpdateScoreFile(const scoreData: ScoreArray; fileName: UniCodeString);
var
	scoreFile : TextFile;
	temp : string;
	i : Integer;
	lineStart : Integer;

begin
	//Appends the Scores to the file
	lineStart := GetNumberOfLines(fileName);
	WriteLn('There Are ', Length(scoreData) - lineStart, ' scores To add');
	// Checks if there are less lines in text file than entrys in array
	if lineStart < High(scoreData) then
	begin
		WriteLn('File Open');
		AssignFile(scoreFile, fileName);
		Append(scoreFile);
		for i:=lineStart to High(scoreData) do
		begin
			temp := scoreData[i].playerName + ':' + FloatToStr(scoreData[i].netWorth);
			WriteLn(scoreFile, temp);
		end;
		Close(scoreFile);
		WriteLn('File Closed');
	end;
end;
//
// Purpose: Reads the score file and adds it all to the ScoreData Record
//
procedure ReadScoreFile(var scoreData: ScoreArray; fileName: UniCodeString);
var
	scoreFile : TextFile;
	temp : String;

begin
	AssignFile(scoreFile, fileName);
	Reset(scoreFile);
	while not Eof(scoreFile) do
	begin
		// adds entrys into the array
		ReadLn(scoreFile, temp);
		SetLength(scoreData, Length(scoreData)+1);
		scoreData[High(scoreData)] := ReadScore(temp);
	end;
	Close(scoreFile);
	WriteLn('There are ', Length(scoreData), ' Scores');
end;
//
// Purpose: Creates a score file
//
procedure CreateScoreFile(fileName: UniCodeString);
var
	scoreFile : TextFile;

begin
	WriteLn('Creating File ');
	AssignFile(scoreFile, fileName);
	Rewrite(scoreFile);
	Close(scoreFile);
	if FileExists(fileName) then
		Write('Sucess')
	else
		Write('Failed');
end;
//
// Purpose: the main server loop
//
procedure SeverLoop(var server: ServerSocket);
var
	updateTimer : Timer;
	i : Integer;
	temp : Message;
	scoreData : ScoreArray;
	tempMessage : Message;

begin
	updateTimer := CreateTimer();
	StartTimer(updateTimer);
	SetLength(scoreData, 0);
	//Checks if there is a score file
	if not FileExists(SCORE_FILE_NAME) then
		CreateScoreFile(SCORE_FILE_NAME);
	ReadScoreFile(scoreData, SCORE_FILE_NAME);
	SelectionSort(scoreData);
	// Loops Forever
	while (true) do
	begin
		CheckNetworkActivity();
		while HasMessages() do
		begin
			WriteLn(ConnectionCount(server));
			tempMessage := ReadMessage(server);
			CheckSendScoreBoard(tempMessage, scoreData);
			CheckUpload(MessageData(tempMessage), scoreData);
		end;
		ClearMessages(server);
		if TimerTicks(updateTimer) > 10000 then
		begin
			UpdateScoreFile(scoreData, SCORE_FILE_NAME);
			ResetTimer(updateTimer);
		end;
	end;
end;

procedure Main();
var
	port : Word;
	boardServer : ServerSocket;

begin
	port := DEFUALT_PORT;
	boardServer := CreateServer('leaderBoard', port, TCP);
	WriteLn('Started');
	SeverLoop(boardServer);
end;

begin
	Main();
end.
