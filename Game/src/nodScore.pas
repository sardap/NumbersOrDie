unit nodScore;

interface
	uses SwinGame, sgTypes, SysUtils, nodTypes, nodNetworking, nodMisc, nodDraw;

	procedure OpenUploadScreen(const netWorth: Single; const playername, ipAddress: String);

	procedure OpenScoreScreen(const ipAddress: String; var timeData: Times);

implementation

	procedure InitialiseUploadBox(var uploadMessage: MessageBox; netWorth, playername: String);
	var
		i : Integer;

	begin
		uploadMessage.whiteBox.width := CalcProportionWhole(0.234375, ScreenWidth());
		uploadMessage.whiteBox.height := CalcProportionWhole(0.139, ScreenHeight());
		uploadMessage.whiteBox.x := CenterInsideBox(0, screenWidth(), uploadMessage.whiteBox.width);
		uploadMessage.whiteBox.y := CenterInsideBox(0, ScreenHeight(), uploadMessage.whiteBox.height);
		// Title
		uploadMessage.Title.x := uploadMessage.whiteBox.x + 10;
		uploadMessage.Title.y := uploadMessage.whiteBox.y + 10;
		uploadMessage.Title.avatar := DrawTextToBitmap(LoadFont('Calibri', CalcProportionWhole(0.05, uploadMessage.whiteBox.width)), 'Name: ' + playername + ' Networth: ' + netWorth, ColorBlack, ColorWhite);
		// there will be two buttons
		SetLength(uploadMessage.buttonData, 2);
		uploadMessage.buttonData[0].text.avatar := DrawTextToBitmap(LoadFont('Calibri', CalcProportionWhole(0.063, uploadMessage.whiteBox.width)), 'Upload', ColorBlack, TransparencyOf(ColorRed));
		uploadMessage.buttonData[1].text.avatar := DrawTextToBitmap(LoadFont('Calibri', CalcProportionWhole(0.063, uploadMessage.whiteBox.width)), 'PlayAgain', ColorBlack, TransparencyOf(ColorRed));

		for i:=0 to High(uploadMessage.buttonData) do
		begin
			uploadMessage.buttonData[i].postion.y := uploadMessage.whiteBox.y + CalcProportionWhole(0.45, uploadMessage.whiteBox.height);
			uploadMessage.buttonData[i].postion.width := CalcProportionWhole(0.36, uploadMessage.whiteBox.width);
			uploadMessage.buttonData[i].postion.height := CalcProportionWhole(0.3, uploadMessage.whiteBox.height);
			uploadMessage.buttonData[i].backgroundColor := ColorGrey;

			uploadMessage.buttonData[i].text.y := uploadMessage.buttonData[i].postion.y +  BitmapHeight(uploadMessage.buttonData[i].text.avatar) div 3;

		end;
		// Sets X postions
		uploadMessage.buttonData[0].postion.x := uploadMessage.whiteBox.x + CalcProportionWhole(0.10, uploadMessage.whiteBox.width);
		uploadMessage.buttonData[1].postion.x := uploadMessage.buttonData[0].postion.x + uploadMessage.buttonData[0].postion.width + CalcProportionWhole(0.10, uploadMessage.whiteBox.width);
		for i:=0 to High(uploadMessage.buttonData) do
		begin
			WriteLn(BitmapWidth(uploadMessage.buttonData[i].text.avatar));
			uploadMessage.buttonData[i].text.x := CenterInsideBox(uploadMessage.buttonData[i].postion.x, uploadMessage.buttonData[i].postion.width, BitmapWidth(uploadMessage.buttonData[i].text.avatar));
		end;
	end;

	procedure HandleUploadInput(var exit: Boolean; var uploadMessage: MessageBox; const playername, Networth, ipAddress: String);
	var
		i : Integer;

	begin
		for i:=0 to High(uploadMessage.buttonData) do
		begin
			if CheckButtonPressed(uploadMessage.buttonData[i]) then
				case i of
					0 :
					begin
						UploadScore(ipAddress, playername, Networth);
						exit := True;
					end;
					1 : exit := True;
				end;
		end;
	end;

	procedure OpenUploadScreen(const netWorth: Single; const playername, ipAddress: String);
	var
		uploadMessage : MessageBox;
		exit : Boolean;
		temp : String;

	begin
		exit := false;
		Str(netWorth:0:3, temp);
		InitialiseUploadBox(uploadMessage, temp, playername);
		repeat
			ProcessEvents();
			HandleUploadInput(exit, uploadMessage, playername, FloatToStr(netWorth), ipAddress);
			DrawMessageBox(uploadMessage);
			RefreshScreen();
		until (exit) or (KeyReleased(KeyCode(InUpload))) or (WindowCloseRequested());
	end;

	procedure InitialiseScoreBoardBox(var scoreBoardBoxPostion: MessageBox);
	begin
		scoreBoardBoxPostion.whiteBox.width := ScreenWidth() >> 1;
		scoreBoardBoxPostion.whiteBox.height := scoreBoardBoxPostion.whiteBox.width;
		scoreBoardBoxPostion.whiteBox.x := CenterInsideBox(0, ScreenWidth(), scoreBoardBoxPostion.whiteBox.width);
		scoreBoardBoxPostion.whiteBox.y := CenterInsideBox(0, ScreenHeight(), scoreBoardBoxPostion.whiteBox.height);
		// Title
		scoreBoardBoxPostion.Title.x := scoreBoardBoxPostion.whiteBox.x + 10;
		scoreBoardBoxPostion.Title.y := scoreBoardBoxPostion.whiteBox.y + 10;
		scoreBoardBoxPostion.Title.avatar := DrawTextToBitmap(LoadFont('Calibri', CalcProportionWhole(0.085, scoreBoardBoxPostion.whiteBox.width)), 'HighScores:', ColorBlack, ColorWhite);
		// there will be One button
		SetLength(scoreBoardBoxPostion.buttonData, 1);
		scoreBoardBoxPostion.buttonData[0].text.avatar := DrawTextToBitmap(LoadFont('Calibri', CalcProportionWhole(0.03, scoreBoardBoxPostion.whiteBox.width)), 'Close', ColorBlack, ColorGrey);

		scoreBoardBoxPostion.buttonData[0].postion.y := scoreBoardBoxPostion.whiteBox.y + CalcProportionWhole(0.94, scoreBoardBoxPostion.whiteBox.height);

		scoreBoardBoxPostion.buttonData[0].postion.x := scoreBoardBoxPostion.whiteBox.x + CalcProportionWhole(0.009, scoreBoardBoxPostion.whiteBox.width);

		scoreBoardBoxPostion.buttonData[0].postion.height := CalcProportionWhole(0.05, scoreBoardBoxPostion.whiteBox.width);

		scoreBoardBoxPostion.buttonData[0].postion.width := CalcProportionWhole(0.095, scoreBoardBoxPostion.whiteBox.width);

		scoreBoardBoxPostion.buttonData[0].backgroundColor := ColorGrey;

		scoreBoardBoxPostion.buttonData[0].text.y := CenterInsideBox(scoreBoardBoxPostion.buttonData[0].postion.y, scoreBoardBoxPostion.buttonData[0].postion.height, BitmapHeight(scoreBoardBoxPostion.buttonData[0].text.avatar));

		scoreBoardBoxPostion.buttonData[0].text.x := CenterInsideBox(scoreBoardBoxPostion.buttonData[0].postion.x, scoreBoardBoxPostion.buttonData[0].postion.width, BitmapWidth(scoreBoardBoxPostion.buttonData[0].text.avatar));

	end;

	procedure InitialiseScorePostion(var scoreDataPostion: ItemPostionArray; const scoreBoardBoxPostion : MessageBox);
	var
		i : Integer;
		yStart, yEnd, x, width, height : Integer;

	begin
		yStart := scoreBoardBoxPostion.whiteBox.y + BitmapHeight(scoreBoardBoxPostion.title.avatar) + 20;

		yEnd := scoreBoardBoxPostion.buttonData[0].postion.y;

		width := CalcProportionWhole(0.90, scoreBoardBoxPostion.whiteBox.width);
		x := CenterInsideBox(scoreBoardBoxPostion.whiteBox.x, scoreBoardBoxPostion.whiteBox.width, width);
		height := 25;

		SetLength(scoreDataPostion, 0);
		i := 0;
		WriteLn(yStart, ':', yEnd);
		repeat
			SetLength(scoreDataPostion, Length(scoreDataPostion)+1);
			scoreDataPostion[i].x := x;
			scoreDataPostion[i].width := width;
			scoreDataPostion[i].height := height;

			scoreDataPostion[i].y := yStart;
			yStart := scoreDataPostion[i].y + height;
			i+=1;
		until (yStart + height > yEnd);
	end;

	procedure DrawScoreData(const scoreDataPostion: ItemPostionArray; const scoreBoardData: StringArray);
	var
		i : Integer;

	begin
		for i:=0 to High(scoreDataPostion) do
		begin
			DrawRectangle(ColorBlack, scoreDataPostion[i].x, scoreDataPostion[i].y, scoreDataPostion[i].width, scoreDataPostion[i].height);
			DrawText(scoreBoardData[i], ColorBlack, LoadFont('Calibri', 20), scoreDataPostion[i].x, CenterInsideBox(scoreDataPostion[i].y, scoreDataPostion[i].height, 20));
		end;
	end;

	procedure OpenScoreScreen(const ipAddress: String; var timeData: Times);
	var
		scoreBoardBoxPostion : MessageBox;
		scoreDataPostion : ItemPostionArray;
		scoreBoardData : StringArray;

	begin
		PauseAllTimers(timeData);
		InitialiseScoreBoardBox(scoreBoardBoxPostion);
		InitialiseScorePostion(scoreDataPostion, scoreBoardBoxPostion);
		if GetScoreBoard(ipAddress, scoreBoardData, Length(scoreDataPostion)) then
		begin
			repeat
				ProcessEvents();
				DrawMessageBox(scoreBoardBoxPostion);
				DrawScoreData(scoreDataPostion, scoreBoardData);
				RefreshScreen();
			until (CheckButtonPressed(scoreBoardBoxPostion.buttonData[0])) or (KeyReleased(KeyCode(InScoreBoard))) or (WindowCloseRequested());
		end;
		ResumeAllTimers(timeData);
	end;

end.
