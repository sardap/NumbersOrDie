unit nodNetworking;

interface
	uses SwinGame, sgTypes, nodTypes, sysutils;

	procedure UploadScore(const ipAddress, playerName, netWorth: String);

	function GetScoreBoard(const ipAddress: String; var scoreBoardData: StringArray; const numberOfEntrys: Word): Boolean;

implementation

	function ConnectToServer(const ipAddress: String; var conn: Connection): Boolean;
	var
		port : WORD;

	begin
		result := false;
		port := 1917;
		WriteLn('Connecting @', ipAddress);
		conn := OpenConnection('upload', ipAddress, port, TCP);
		if ConnectionOpen(conn) then
		begin
			result := true;
			WriteLn('Connection Open!');
		end
		else
			WriteLn('Connection Failed');
	end;

	procedure UploadScore(const ipAddress, playerName, netWorth: String);
	var
		toSend : String;
		failure : Boolean;
		conn : Connection;

	begin
		if ConnectToServer(ipAddress, conn) then
		begin
			//Guitly until proven otherwise
			failure := true;
			// Concats Data to Be Sent
			toSend := UPLOAD_PREFIX + playerName + ':' + netWorth;
			if SendMessageTo(toSend, conn) then
				failure := false;

			if failure = true then
				WriteLn('Shits Fucked')
			else
				WriteLn('Message Sent!');
			CloseConnection(conn);
		end;
	end;

	function GetScoreBoard(const ipAddress: String; var scoreBoardData: StringArray; const numberOfEntrys: Word): Boolean;
	var
		i : Integer;
		conn : Connection;

	begin
		result := false;
		if ConnectToServer(ipAddress, conn) then
		begin
			SetLength(scoreBoardData, 0);
			if SendMessageTo(SEND_SCORE_BOARD_PREFIX + IntToStr(numberOfEntrys), conn) then
			begin
				WriteLn('Asking for ', numberOfEntrys, ' Score(s)');
				i := 0;
				while (ConnectionOpen(conn)) do
				begin
					CheckNetworkActivity();
					while HasMessages() do
					begin
						// Sets Length to the amount of scores Received
						SetLength(scoreBoardData, Length(scoreBoardData)+1);
						scoreBoardData[i] := ReadMessageData(conn);
						i += 1;
					end;
					result := true;
				end;
			end;
			WriteLn('Connection Closed Got ', Length(scoreBoardData), ' Score)');
			if Length(scoreBoardData) > 1 then Write('s');
			CloseConnection(conn);
			//fiils empty slots with a message
			while (numberOfEntrys >= Length(scoreBoardData)) do
			begin
				SetLength(scoreBoardData, Length(scoreBoardData)+1);
				scoreBoardData[High(scoreBoardData)] := 'This Could be you ;)';
			end;
		end;
	end;
end.
