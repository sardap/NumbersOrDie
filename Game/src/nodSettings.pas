unit nodSettings;

interface
	uses SwinGame, sgInput, sgTypes, SysUtils, nodDraw, nodTypes, nodMisc;
	//
	// Settings Functions/procudres for use everywhere
	//
	procedure UpdateSettings(var settingsData: Settings);

	procedure InitialiseSettings(var settingsData: Settings);

	procedure OpenSettingsScreen(var settingsData: Settings; var timeData: Times);

implementation
	//
	// Settings Functions/procudres
	//
	procedure CreateSettingsFile(startingDifficulty, screenWidth, ScreenHeight: Integer; playername, ipAddress: String);
	var
		settingsF: TextFile;

	begin
		AssignFile(settingsF, SETTINGS_FILE_NAME);
		Rewrite(settingsF);
		//Writes the vaules passed into the file
		WriteLn(settingsF, 'Difficulty:', IntToStr(startingDifficulty));
		WriteLn(settingsF, 'ScreenWidth:', IntToStr(screenWidth));
		WriteLn(settingsF, 'ScreenHeight:', IntToStr(screenHeight));
		WriteLn(settingsF, 'PlayerName:', playername);
		WriteLn(settingsF, 'Server IP:', ipAddress);
		CloseFile(settingsF);
	end;

	procedure CleanInput(var toCheck: String);
	var
		i : Integer;
		invaildChars : array of Char;

	begin
		SetLength(invaildChars, 10);
		invaildChars[0] := ':';
		invaildChars[1] := '@';
		invaildChars[2] := '?';
		invaildChars[3] := '(';
		invaildChars[4] := ')';
		for i:=0 to High(invaildChars) do
		begin
			while (Pos(invaildChars[i], toCheck) > 0)  do
			begin
				Delete(toCheck, Pos(invaildChars[i], toCheck), 1);
			end;
		end;
	end;

	procedure ReadSettingsLine(var line: String; seprator: Char);
	begin
		line := Copy(line, pos(seprator, line)+1, Length(line));
	end;

	procedure UpdateSettings(var settingsData: Settings);
	var
		settingsF: TextFile;
		temp : String;

	begin
		AssignFile(settingsF, SETTINGS_FILE_NAME);
		Reset(settingsF);
		//copys the vaule from settingsF into settingsData
		// Reading diffcilty
		ReadLn(settingsF, temp);
		ReadSettingsLine(temp, ':');
		settingsData.startingDifficulty := StrToIntDef(temp, 1000);
		// Reading screenWidth
		ReadLn(settingsF, temp);
		ReadSettingsLine(temp, ':');
		settingsData.screenWidth := StrToIntDef(temp, 1280);
		// Reading ScreenHeight
		ReadLn(settingsF, temp);
		ReadSettingsLine(temp, ':');
		settingsData.screenHeight := StrToIntDef(temp, 720);
		// Reading playerName
		ReadLn(settingsF, temp);
		ReadSettingsLine(temp, ':');
		CleanInput(temp);
		settingsData.playerName := temp;
		// Reading Ip address
		ReadLn(settingsF, temp);
		ReadSettingsLine(temp, ':');
		CleanInput(temp);
		settingsData.ipAddress := temp;
		// WriteLn('IP address: ', settingsData.ipAddress);
		CloseFile(settingsF);
	end;

	procedure InitialiseSettings(var settingsData: Settings);
	begin
		if not FileExists(SETTINGS_FILE_NAME) then
			CreateSettingsFile(DEFAULT_DIFFICULTY, 1280, 720, 'Paul_is_Cool', 'dotaridoo.com');
		UpdateSettings(settingsData);
	end;

	procedure HandleSettingsInput(var settingsData: Settings; var settingsBox: MessageBox);
	var
		i : Integer;

	begin
		for i:=0 to High(settingsBox.buttonData) do
			if (CheckButtonPressed(settingsBox.buttonData[i]))  then
			begin
				case i of
					0 :
					 	begin
					 		settingsData.screenWidth := 1280;
							settingsData.screenHeight := 720;
					 	end;
					1 :
						begin
							settingsData.screenWidth := 1366;
							settingsData.screenHeight := 768;
						end;
					2 :
						begin
							settingsData.screenWidth := 1920;
							settingsData.screenHeight := 1080;
						end;
					3 :
						begin
							settingsData.screenWidth := 2560;
							settingsData.screenHeight := 1440;
						end;
					4 :
						begin
							if not ReadingText() then
								StartReadingTextWithText(settingsData.playerName ,ColorBlack, 25, LoadFont('Calibri', CalcProportionWhole(0.13, settingsBox.buttonData[4].postion.width)), settingsBox.buttonData[i].postion.x, settingsBox.buttonData[i].postion.y);
						end;
				end;
			end;
			if ReadingText() then
			begin
				settingsData.playerName := GetReadingText();
			end;
	end;

	procedure InitialisesettingsBox(var settingsBox: MessageBox; const playerName: String);
	var
	  spaceBetweenButtons, i: Integer;

	begin
		// WhiteBox
		settingsBox.whiteBox.width := CalcProportionWhole(0.41, ScreenWidth());
		settingsBox.whiteBox.height := CalcProportionWhole(0.51, ScreenHeight());
		settingsBox.whiteBox.x := CenterInsideBox(0, ScreenWidth(), settingsBox.whiteBox.width);
		settingsBox.whiteBox.y := CenterInsideBox(0, ScreenHeight(), settingsBox.whiteBox.height);
		// Title of box
		settingsBox.title.x := settingsBox.whiteBox.x + CalcProportionWhole(0.05, settingsBox.whiteBox.width);
		settingsBox.title.y := settingsBox.whiteBox.y + CalcProportionWhole(0.05, settingsBox.whiteBox.height);
		settingsBox.title.avatar := DrawTextToBitmap(LoadFont('Calibri', CalcProportionWhole(0.08, settingsBox.whiteBox.width)), 'Settings', ColorBlack, ColorWhite);

		//4 Resoltuon Buttons 1 Player Text Input
		SetLength(settingsBox.buttonData, 5);
		// Resoltuon ResoltuonButton locations
		settingsBox.buttonData[0].postion.x := settingsBox.title.x;
		settingsBox.buttonData[0].postion.y := settingsBox.title.y + BitmapHeight(settingsBox.title.avatar) + 10;
		settingsBox.buttonData[0].postion.width := CalcProportionWhole(0.25, settingsBox.whiteBox.width);
		settingsBox.buttonData[0].postion.height := settingsBox.buttonData[0].postion.width >> 2;
		// Text vaules of Resoltuon Buttons
		settingsBox.buttonData[0].text.avatar := DrawTextToBitmap(LoadFont('Calibri', 20), '1280x720', ColorBlack, TransparencyOf(ColorGrey));
		settingsBox.buttonData[1].text.avatar := DrawTextToBitmap(LoadFont('Calibri', 20), '1366x768', ColorBlack, TransparencyOf(ColorWhite));
		settingsBox.buttonData[2].text.avatar := DrawTextToBitmap(LoadFont('Calibri', 20), '1920x1080', ColorBlack, TransparencyOf(ColorWhite));
		settingsBox.buttonData[3].text.avatar := DrawTextToBitmap(LoadFont('Calibri', 20), '2560x1440', ColorBlack, TransparencyOf(ColorWhite));
		// Text locations Resoltuon Buttons
		settingsBox.buttonData[0].text.x := CenterInsideBox(settingsBox.buttonData[0].postion.x, settingsBox.buttonData[0].postion.width, BitmapWidth(settingsBox.buttonData[0].text.avatar));

		settingsBox.buttonData[0].text.y := CenterInsideBox(settingsBox.buttonData[0].postion.y, settingsBox.buttonData[0].postion.height,  BitmapHeight(settingsBox.buttonData[0].text.avatar));

		spaceBetweenButtons := CalcProportionWhole(0.05, settingsBox.whiteBox.height);
		for i:=1 to 4 do
		begin
			// Makes x the same for all Resoltuon buttons
			settingsBox.buttonData[i].postion.x := settingsBox.buttonData[0].postion.x;
			// Makes y = height of the first button + y of the prevouis button + 39 for whitespace
			settingsBox.buttonData[i].postion.y := settingsBox.buttonData[i-1].postion.y + settingsBox.buttonData[0].postion.height + spaceBetweenButtons;
			// Makes width the same for all Resoltuon buttons
			settingsBox.buttonData[i].postion.width := settingsBox.buttonData[0].postion.width;
			// Makes height the same for all Resoltuon buttons
			settingsBox.buttonData[i].postion.height :=
			settingsBox.buttonData[0].postion.height;
			// assign vaule of x for text inside of the box
			settingsBox.buttonData[i].text.x := CenterInsideBox(settingsBox.buttonData[i].postion.x, settingsBox.buttonData[i].postion.width, BitmapWidth(settingsBox.buttonData[i].text.avatar));

			settingsBox.buttonData[i].text.y := settingsBox.buttonData[i].postion.y + BitmapHeight(settingsBox.buttonData[i].text.avatar) >> 1;
		end;

		//sets the vaule of the Resoltuon button
		settingsBox.buttonData[0].valueChk := 1280;
		settingsBox.buttonData[1].valueChk := 1366;
		settingsBox.buttonData[2].valueChk := 1920;
		settingsBox.buttonData[3].valueChk := 2560;

		// defualt color for Resoltuon buttons
		for i:=0 to 4 do
			settingsBox.buttonData[i].backgroundColor := ColorGrey;

		//Text Input Button
		settingsBox.buttonData[4].postion.x := settingsBox.buttonData[0].postion.x + settingsBox.buttonData[0].postion.width + CalcProportionWhole(0.05, settingsBox.whiteBox.width);
		settingsBox.buttonData[4].postion.y := settingsBox.buttonData[1].postion.y;
		settingsBox.buttonData[4].postion.width := CalcProportionWhole(0.25, settingsBox.whiteBox.width);
		settingsBox.buttonData[4].postion.height := CalcProportionWhole(0.09, settingsBox.whiteBox.height);
		// Color for Text Input Button
		settingsBox.buttonData[4].backgroundColor := ColorLightGrey;
		// Text Data
		settingsBox.buttonData[4].text.x := settingsBox.buttonData[4].postion.x;
		settingsBox.buttonData[4].text.y := settingsBox.buttonData[4].postion.y - settingsBox.buttonData[4].postion.height;
		settingsBox.buttonData[4].text.avatar := DrawTextToBitmap(LoadFont('Calibri', CalcProportionWhole(0.13, settingsBox.buttonData[4].postion.width)), 'PlayerName:' + playerName, ColorBlack, ColorWhite);
		settingsBox.buttonData[4].valueChk := -1;
	end;

	procedure OpenSettingsScreen(var settingsData: Settings; var timeData: Times);
	var
		settingsBox: MessageBox;
		playerNameText : TextData;

	begin
		PauseAllTimers(timeData);
		InitialisesettingsBox(settingsBox, settingsData.playerName);
		WriteLn(settingsData.playerName);
		repeat
			ProcessEvents();
			DrawMessageBox(settingsBox);
			HandleSettingsInput(settingsData, settingsBox);
			RefreshScreen();
		until (KeyReleased(EscapeKey)) or (WindowCloseRequested());
		ResumeAllTimers(timeData);
		CreateSettingsFile(settingsData.startingDifficulty, settingsData.screenWidth, settingsData.screenHeight, settingsData.playerName, settingsData.ipAddress);
		UpdateSettings(settingsData);
	end;
end.
